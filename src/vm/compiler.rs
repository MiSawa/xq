use crate::{
    ast::{
        self, BinaryOp, BindPattern, FuncArg, FuncDef, Identifier, ObjectBindPatternEntry, Query,
        StringFragment, Suffix, Term,
    },
    data_structure::{PHashMap, PVector},
    intrinsic,
    module_loader::{ModuleLoadError, ModuleLoader},
    vm::{bytecode::Closure, Address, ByteCode, Program, ScopeId, ScopedSlot},
    Number, Value,
};
use itertools::Itertools;
use num::bigint::ToBigInt;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
};
use thiserror::Error;

/// # Function calling convention
/// ## Caller
/// for `func(arg0, arg1, closure2)`
/// - push arg0
/// - push arg1
/// - push_closure { (copy of the current scope), (start address) }
/// - call (&func, return address)
///
/// ## Callee
/// for `func($arg0, $arg1, closure2)`
/// - new_frame { scope id, return address }
/// - pop_closure
/// - pop slot_1
/// - pop slot_0
/// - (function body here)
/// - return // pop frame
///

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Use of unknown variable `{0:}`")]
    UnknownVariable(Identifier),
    #[error("Use of unknown function `{0:}`")]
    UnknownFunction(Identifier),
    #[error("Bind pattern has the same variable `{0:}`")]
    SameVariableInPattern(Identifier),
    #[error(transparent)]
    ModuleLoadError(#[from] ModuleLoadError),
}

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionIdentifier(pub(crate) Identifier, pub(crate) usize);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct DeclaredFunction(Address, Vec<ArgType>);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) enum ArgType {
    /// arg
    Closure,
    /// $arg
    Value,
}
#[derive(Clone)]
enum FunctionLike {
    Function(DeclaredFunction),
    Closure(ScopedSlot),
    Intrinsic(ByteCode, Vec<ArgType>),
    ManuallyImplemented(
        &'static str,
        fn(&mut Compiler, &[Query], Address) -> Result<Address>,
    ),
}
impl Debug for FunctionLike {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionLike::Function(func) => f.debug_tuple("DeclaredFunction").field(func).finish(),
            FunctionLike::Closure(slot) => f.debug_tuple("Closure").field(slot).finish(),
            FunctionLike::Intrinsic(code, args) => {
                f.debug_tuple("Intrinsic").field(code).field(args).finish()
            }
            FunctionLike::ManuallyImplemented(name, _) => {
                f.debug_tuple("ManuallyImplemented").field(name).finish()
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct PlaceHolder(Address);

struct CodeEmitter {
    code: Vec<ByteCode>,
}

impl CodeEmitter {
    fn new() -> Self {
        Self {
            code: vec![ByteCode::Output, ByteCode::Backtrack, ByteCode::Unreachable],
        }
    }

    fn address(&self) -> Address {
        Address(self.code.len() - 1)
    }

    fn output(&self) -> Address {
        Address(0)
    }

    fn backtrack(&self) -> Address {
        Address(1)
    }

    fn follow_jump(&mut self, mut address: Address) -> Address {
        while let ByteCode::Jump(next) = self.code[address.0] {
            address = next;
        }
        address
    }

    fn jump_or_follow(&mut self, address: Address) {
        if address.0 + 1 == self.code.len() {
            return;
        }
        let address = self.follow_jump(address);
        let code = match self.code.get(address.0) {
            Some(ByteCode::Unreachable) => ByteCode::Unreachable,
            Some(ByteCode::Call {
                function,
                return_address,
            }) => ByteCode::Call {
                function: *function,
                return_address: *return_address,
            },
            Some(ByteCode::Backtrack) => ByteCode::Backtrack,
            Some(ByteCode::Ret) => ByteCode::Ret,
            Some(ByteCode::Output) => ByteCode::Output,
            _ => ByteCode::Jump(address),
        };
        self.code.push(code);
    }

    fn emit_normal_op(&mut self, code: ByteCode, next: Address) -> Address {
        self.jump_or_follow(next);
        self.code.push(code);
        self.address()
    }

    fn emit_terminal_op(&mut self, code: ByteCode) -> Address {
        self.code.push(code);
        self.address()
    }

    fn emit_function_call(&mut self, function: Address, return_address: Address) -> Address {
        let function = self.follow_jump(function);
        self.emit_terminal_op(ByteCode::Call {
            function,
            return_address,
        })
    }

    fn emit_constant(&mut self, value: Value, next: Address) -> Address {
        self.emit_normal_op(ByteCode::Const(value), next)
    }

    fn emit_fork(&mut self, fork_pc: Address, next: Address) -> Address {
        self.emit_normal_op(ByteCode::Fork { fork_pc }, next)
    }

    fn emit_placeholder(&mut self, next: Address) -> (Address, PlaceHolder) {
        let address = self.emit_normal_op(ByteCode::PlaceHolder, next);
        (address, PlaceHolder(address))
    }

    fn emit_terminal_placeholder(&mut self) -> (Address, PlaceHolder) {
        let address = self.emit_terminal_op(ByteCode::PlaceHolder);
        (address, PlaceHolder(address))
    }

    fn replace_placeholder(&mut self, placeholder: PlaceHolder, code: ByteCode) {
        assert!(matches!(
            self.code.get(placeholder.0 .0),
            Some(ByteCode::PlaceHolder)
        ));
        self.code[placeholder.0 .0] = code;
    }
}

#[derive(Debug, Clone)]
struct Scope {
    id: ScopeId,
    next_variable_slot_id: usize,
    next_closure_slot_id: usize,
    functions: PHashMap<FunctionIdentifier, FunctionLike>,
    variables: PHashMap<Identifier, ScopedSlot>,
}

impl Scope {
    fn new(id: ScopeId) -> Self {
        Self {
            id,
            next_variable_slot_id: 0,
            next_closure_slot_id: 0,
            functions: Default::default(),
            variables: Default::default(),
        }
    }

    fn nested(id: ScopeId, previous: &Self) -> Self {
        Self {
            id,
            next_variable_slot_id: 0,
            next_closure_slot_id: 0,
            functions: previous.functions.clone(),
            variables: previous.variables.clone(),
        }
    }

    fn allocate_variable(&mut self) -> ScopedSlot {
        let slot = ScopedSlot(self.id, self.next_variable_slot_id);
        self.next_variable_slot_id += 1;
        slot
    }

    fn register_variable(&mut self, name: Identifier) -> ScopedSlot {
        let slot = self.allocate_variable();
        self.variables.insert(name, slot);
        slot
    }

    fn register_function(&mut self, name: Identifier, function: DeclaredFunction) {
        self.functions.insert(
            FunctionIdentifier(name, function.1.len()),
            FunctionLike::Function(function),
        );
    }

    fn register_closure(&mut self, name: Identifier) -> ScopedSlot {
        let slot = ScopedSlot(self.id, self.next_closure_slot_id);
        self.next_closure_slot_id += 1;
        self.functions
            .insert(FunctionIdentifier(name, 0), FunctionLike::Closure(slot));
        slot
    }

    fn lookup_variable(&self, name: &Identifier) -> Option<&ScopedSlot> {
        self.variables.get(name)
    }

    fn lookup_function(&self, identifier: &FunctionIdentifier) -> Option<&FunctionLike> {
        self.functions.get(identifier)
    }
}

pub struct Compiler {
    emitter: CodeEmitter,
    next_scope_id: ScopeId,
    scope_stack: Vec<Scope>,
}

struct SavedScope(Scope);

trait Compile {
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address>;
}

impl Compile for Query {
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        compiler.compile_query(self, next)
    }
}

impl Compile for Term {
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        compiler.compile_term(self, next)
    }
}

impl Compile for Address {
    fn compile(&self, _compiler: &mut Compiler, _next: Address) -> Result<Address> {
        Ok(*self)
    }
}

impl<T> Compile for T
where
    T: AsRef<Query>,
{
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        compiler.compile_query(self.as_ref(), next)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            emitter: CodeEmitter::new(),
            next_scope_id: ScopeId(1),
            scope_stack: vec![Scope::new(ScopeId(0))],
        }
    }

    fn current_scope(&self) -> &Scope {
        self.scope_stack
            .last()
            .expect("Scope stack shouldn't be empty")
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope_stack
            .last_mut()
            .expect("Scope stack shouldn't be empty")
    }

    fn save_scope(&mut self) -> SavedScope {
        SavedScope(self.current_scope_mut().clone())
    }

    fn restore_scope(&mut self, save: SavedScope) {
        let mut save = save.0;
        let current = self.current_scope_mut();
        assert_eq!(current.id, save.id);
        save.next_variable_slot_id = current.next_variable_slot_id;
        save.next_closure_slot_id = current.next_closure_slot_id;
        *current = save;
    }

    fn allocate_variable(&mut self) -> ScopedSlot {
        self.current_scope_mut().allocate_variable()
    }

    fn register_variable(&mut self, name: Identifier) -> ScopedSlot {
        self.current_scope_mut().register_variable(name)
    }

    fn register_closure(&mut self, name: Identifier) -> ScopedSlot {
        self.current_scope_mut().register_closure(name)
    }

    fn register_function(&mut self, name: Identifier, function: DeclaredFunction) {
        self.current_scope_mut().register_function(name, function)
    }

    fn enter_scope(&mut self) -> ScopeId {
        let new_scope = Scope::nested(self.next_scope_id, self.current_scope());
        self.scope_stack.push(new_scope);
        let ret = self.next_scope_id;
        self.next_scope_id.0 += 1;
        ret
    }

    fn exit_scope(&mut self, id: ScopeId) -> (usize, usize) {
        let scope = self
            .scope_stack
            .pop()
            .expect("Scope stack shouldn't be empty");
        assert_eq!(id, scope.id);
        (scope.next_variable_slot_id, scope.next_closure_slot_id)
    }

    fn exit_scope_and_emit_new_scope(&mut self, id: ScopeId, next: Address) -> Address {
        let scope_info = self.exit_scope(id);
        self.emitter.emit_normal_op(
            ByteCode::NewScope {
                id,
                variable_cnt: scope_info.0,
                closure_cnt: scope_info.1,
            },
            next,
        )
    }

    fn exit_global_scope_and_emit_new_scope(&mut self, next: Address) -> Address {
        self.exit_scope_and_emit_new_scope(ScopeId(0), next)
    }

    fn lookup_variable(&self, name: &Identifier) -> Result<&ScopedSlot> {
        self.current_scope()
            .lookup_variable(name)
            .ok_or_else(|| CompileError::UnknownVariable(name.clone()))
    }

    fn lookup_compilable_intrinsic(function: &FunctionIdentifier) -> Option<FunctionLike> {
        Some(match function {
            FunctionIdentifier(Identifier(name), 0) => match name.as_str() {
                "empty" => FunctionLike::Intrinsic(ByteCode::Backtrack, vec![]),
                _ => return None,
            },
            FunctionIdentifier(Identifier(name), 1) => match name.as_str() {
                "path" => FunctionLike::ManuallyImplemented("path", |compiler, args, next| {
                    assert_eq!(1, args.len());
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::ExitPathTracking, next);
                    let next = compiler.compile_query(&args[0], next)?;
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::EnterPathTracking, next);
                    Ok(next)
                }),
                _ => return None,
            },
            _ => return None,
        })
    }

    fn lookup_function(&self, function: &FunctionIdentifier) -> Result<FunctionLike> {
        self.current_scope()
            .lookup_function(function)
            .cloned()
            .or_else(|| Self::lookup_compilable_intrinsic(function))
            .or_else(|| {
                intrinsic::lookup_intrinsic_fn(function)
                    .map(|(code, args)| FunctionLike::Intrinsic(code, args))
            })
            .ok_or_else(|| CompileError::UnknownFunction(function.0.clone()))
    }

    /// Consumes nothing, produces nothing. Just places the code.
    fn compile_function_inner(&mut self, args: &[FuncArg], body: &Query) -> Result<Address> {
        let scope_id = self.enter_scope();
        #[allow(clippy::needless_collect)] // This collect is needed to unborrow `self`
        let slots: Vec<_> = args
            .iter()
            .map(|arg| match arg {
                FuncArg::Variable(name) => self.register_variable(name.clone()),
                FuncArg::Closure(name) => self.register_closure(name.clone()),
            })
            .collect();

        let next = self.emitter.emit_terminal_op(ByteCode::Ret);
        let mut next = self.compile_query(body, next)?;
        for (arg, slot) in args.iter().zip(slots.into_iter()) {
            next = match arg {
                FuncArg::Variable(_) => self.emitter.emit_normal_op(ByteCode::Store(slot), next),
                FuncArg::Closure(_) => self
                    .emitter
                    .emit_normal_op(ByteCode::StoreClosure(slot), next),
            };
        }
        let next = self.exit_scope_and_emit_new_scope(scope_id, next);
        Ok(next)
    }

    /// Consumes nothing, produces nothing. Just places the code.
    fn compile_closure(&mut self, closure: &Query) -> Result<Address> {
        self.compile_function_inner(&[], closure)
    }

    /// Consumes nothing, produces nothing. Registers function to the current scope.
    fn compile_funcdef(&mut self, func: &FuncDef) -> Result<()> {
        let (func_address, placeholder) = self.emitter.emit_terminal_placeholder();
        let types = func
            .args
            .iter()
            .map(|arg| match arg {
                FuncArg::Variable(_) => ArgType::Value,
                FuncArg::Closure(_) => ArgType::Closure,
            })
            .collect();
        self.register_function(func.name.clone(), DeclaredFunction(func_address, types));
        let real_address = self.compile_function_inner(&func.args, &func.body)?;
        self.emitter
            .replace_placeholder(placeholder, ByteCode::Jump(real_address));
        Ok(())
    }

    fn compile_func_call_args(
        &mut self,
        args: &[Query],
        types: &[ArgType],
        mut next: Address,
    ) -> Result<Address> {
        assert_eq!(args.len(), types.len());
        // We need to evaluate all value-typed arguments on the same current context (stack top).
        // In order to do so, we store the stack top to a slot temporarily if there's a value-typed arg.
        let context_slot = if types.iter().any(|s| s == &ArgType::Value) {
            Some(self.allocate_variable())
        } else {
            None
        };
        for (arg, ty) in args.iter().zip(types.iter()).rev() {
            next = match ty {
                ArgType::Closure => {
                    let closure_address = self.compile_closure(arg)?;
                    self.emitter
                        .emit_normal_op(ByteCode::PushClosure(Closure(closure_address)), next)
                }
                ArgType::Value => {
                    let next = self.compile_query(arg, next)?;
                    if let Some(slot) = context_slot {
                        self.emitter.emit_normal_op(ByteCode::Load(slot), next)
                    } else {
                        next
                    }
                }
            }
        }
        Ok(if let Some(slot) = context_slot {
            next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
            self.emitter.emit_normal_op(ByteCode::Dup, next)
        } else {
            next
        })
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_func_call(
        &mut self,
        function: FunctionLike,
        args: &[Query],
        next: Address,
    ) -> Result<Address> {
        Ok(match function {
            FunctionLike::Function(DeclaredFunction(address, types)) => {
                let next = self.emitter.emit_function_call(address, next);
                self.compile_func_call_args(args, &types, next)?
            }
            FunctionLike::Closure(slot) => {
                assert!(args.is_empty());
                self.emitter.emit_terminal_op(ByteCode::CallClosure {
                    slot,
                    return_address: next,
                })
            }
            FunctionLike::Intrinsic(bytecode, types) => {
                let next = self.emitter.emit_normal_op(bytecode, next);
                self.compile_func_call_args(args, &types, next)?
            }
            FunctionLike::ManuallyImplemented(_, implementation) => {
                implementation(self, args, next)?
            }
        })
    }

    fn lookup_and_compile_func_call(
        &mut self,
        name: Identifier,
        args: &[Query],
        next: Address,
    ) -> Result<Address> {
        let resolved = self.lookup_function(&FunctionIdentifier(name, args.len()))?;
        self.compile_func_call(resolved, args, next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_try<T: Compile, U: Compile>(
        &mut self,
        body: &T,
        catch: Option<&U>,
        next: Address,
    ) -> Result<Address> {
        let try_end = self.emitter.emit_normal_op(ByteCode::ForkTryEnd, next);
        let catch_pc = catch.map(|c| c.compile(self, next)).transpose()?;
        let body = body.compile(self, try_end)?;
        Ok(self
            .emitter
            .emit_normal_op(ByteCode::ForkTryBegin { catch_pc }, body))
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_if<T: Compile, U: Compile, V: Compile>(
        &mut self,
        cond: &T,
        positive: &U,
        negative: Option<&V>,
        next: Address,
    ) -> Result<Address> {
        let negative_address = if let Some(negative) = negative {
            negative.compile(self, next)?
        } else {
            next
        };
        let positive_address = positive.compile(self, next)?;
        let next = self
            .emitter
            .emit_normal_op(ByteCode::JumpUnless(negative_address), positive_address);
        let next = self
            .emitter
            .emit_normal_op(ByteCode::ExitNonPathTracking, next);
        let next = cond.compile(self, next)?;
        let next = self
            .emitter
            .emit_normal_op(ByteCode::EnterNonPathTracking, next);
        let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_index<T: Compile, U: Compile>(
        &mut self,
        body: &T,
        index: &U,
        next: Address,
    ) -> Result<Address> {
        let indexing = self.emitter.emit_normal_op(ByteCode::Index, next);
        let index = index.compile(self, indexing)?;
        let swap = self.emitter.emit_normal_op(ByteCode::Swap, index);
        let body = body.compile(self, swap)?;
        Ok(self.emitter.emit_normal_op(ByteCode::Dup, body))
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_slice<T: Compile, U: Compile, V: Compile>(
        &mut self,
        body: &T,
        start: Option<&U>,
        end: Option<&V>,
        next: Address,
    ) -> Result<Address> {
        assert!(start.is_some() || end.is_some());
        let next = self.emitter.emit_normal_op(
            ByteCode::Slice {
                start: start.is_some(),
                end: end.is_some(),
            },
            next,
        );
        let mut need_val = false;
        let next = if let Some(end) = end {
            need_val = true;
            end.compile(self, next)?
        } else {
            next
        };
        let next = if let Some(start) = start {
            if need_val {
                // Don't consume the value; put `start` penultimate of the stack, and leave the value top.
                let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                let next = start.compile(self, next)?;
                self.emitter.emit_normal_op(ByteCode::Dup, next)
            } else {
                // Consume the value and produce the start
                need_val = true;
                start.compile(self, next)?
            }
        } else {
            next
        };
        // Same as the above.... although need_val = true.
        let next = if need_val {
            let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
            let next = body.compile(self, next)?;
            self.emitter.emit_normal_op(ByteCode::Dup, next)
        } else {
            body.compile(self, next)?
        };
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_iterate<T: Compile>(&mut self, body: &T, next: Address) -> Result<Address> {
        let next = self.emitter.emit_normal_op(ByteCode::Each, next);
        body.compile(self, next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_term_suffix(
        &mut self,
        term: &Term,
        suffix: &Suffix,
        next: Address,
    ) -> Result<Address> {
        match suffix {
            Suffix::Optional => self.compile_try::<_, Query>(term, None, next),
            Suffix::Iterate => self.compile_iterate(term, next),
            Suffix::Index(ident) => {
                self.compile_index(term, &Term::Constant(Value::string(ident.0.clone())), next)
            }
            Suffix::Query(q) => self.compile_index(term, q, next),
            Suffix::Slice(start, end) => {
                self.compile_slice(term, start.as_ref(), end.as_ref(), next)
            }
        }
    }

    /// Consumes a value (object) from the stack, and produces a single value (updated object) onto the stack.
    fn compile_object_entry(
        &mut self,
        context: ScopedSlot,
        key: &Query,
        value: &Option<Query>,
        next: Address,
    ) -> Result<Address> {
        let next = self.emitter.emit_normal_op(ByteCode::AppendObject, next);
        let next = match value {
            Some(value) => {
                let next = self.compile_query(value, next)?;
                self.emitter.emit_normal_op(ByteCode::Load(context), next)
            }
            None => {
                if let Query::Term(term) = key {
                    if let Term::Variable(ident) = term.as_ref() {
                        let slot = *self.lookup_variable(ident)?;
                        let next = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                        let next = self
                            .emitter
                            .emit_normal_op(ByteCode::Push(Value::string(ident.0.clone())), next);
                        return Ok(next);
                    }
                }
                let next = self.emitter.emit_normal_op(ByteCode::Index, next);
                let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                let next = self.emitter.emit_normal_op(ByteCode::Load(context), next);
                self.emitter.emit_normal_op(ByteCode::Dup, next)
            }
        };
        let next = self.compile_query(key, next)?;
        let next = self.emitter.emit_normal_op(ByteCode::Load(context), next);
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_object(
        &mut self,
        kvs: &[(Query, Option<Query>)],
        mut next: Address,
    ) -> Result<Address> {
        let slot = self.allocate_variable();
        for (key, value) in kvs.iter().rev() {
            next = self.compile_object_entry(slot, key, value, next)?
        }
        next = self
            .emitter
            .emit_normal_op(ByteCode::Push(Value::Object(Default::default())), next);
        Ok(self.emitter.emit_normal_op(ByteCode::Store(slot), next))
    }

    fn compile_bind(
        &mut self,
        source: &Term,
        patterns: &[BindPattern],
        body: &Query,
        next: Address,
    ) -> Result<Address> {
        assert!(!patterns.is_empty());
        fn collect_variable_occurrences<'a>(
            pattern: &'a BindPattern,
            occurrences: &mut HashMap<&'a Identifier, usize>,
        ) {
            match pattern {
                BindPattern::Variable(ident) => {
                    *occurrences.entry(ident).or_insert(0) += 1;
                }
                BindPattern::Array(arr) => {
                    for p in arr {
                        collect_variable_occurrences(p, occurrences);
                    }
                }
                BindPattern::Object(obj) => {
                    for entry in obj {
                        match entry {
                            ObjectBindPatternEntry::KeyValue(_, p) => {
                                collect_variable_occurrences(p, occurrences);
                            }
                            ObjectBindPatternEntry::KeyOnly(ident) => {
                                *occurrences.entry(ident).or_insert(0) += 1;
                            }
                        }
                    }
                }
            }
        }

        impl Compile for BindPattern {
            fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
                let next = match self {
                    BindPattern::Variable(ident) => {
                        let slot = *compiler.lookup_variable(ident)?;
                        compiler.emitter.emit_normal_op(ByteCode::Store(slot), next)
                    }
                    BindPattern::Array(v) => {
                        let mut tmp = next;
                        for (i, pattern) in v.iter().enumerate().rev() {
                            tmp = pattern.compile(compiler, tmp)?;
                            tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                            tmp = compiler.emitter.emit_normal_op(
                                ByteCode::Push(Value::number(Number::from_integer(
                                    i.to_bigint().unwrap(),
                                ))),
                                tmp,
                            );
                            tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                        }
                        tmp
                    }
                    BindPattern::Object(entries) => {
                        let mut tmp = next;
                        for entry in entries.iter().rev() {
                            match entry {
                                ObjectBindPatternEntry::KeyValue(key, value) => {
                                    tmp = value.compile(compiler, tmp)?;
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Swap, tmp);
                                    tmp = compiler.compile_query(key, tmp)?;
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                                }
                                ObjectBindPatternEntry::KeyOnly(key) => {
                                    let slot = *compiler.lookup_variable(key)?;
                                    tmp =
                                        compiler.emitter.emit_normal_op(ByteCode::Store(slot), tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                                    tmp = compiler.emitter.emit_normal_op(
                                        ByteCode::Push(Value::string(key.0.clone())),
                                        tmp,
                                    );
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                                }
                            }
                        }
                        tmp
                    }
                };
                Ok(next)
            }
        }

        let mut variables: Vec<HashSet<&Identifier>> = vec![];
        for pattern in patterns {
            let mut map = HashMap::new();
            collect_variable_occurrences(pattern, &mut map);
            if let Some((&key, _)) = map.iter().find(|(_, &v)| v > 1) {
                return Err(CompileError::SameVariableInPattern(key.clone()));
            }
            variables.push(map.keys().cloned().collect())
        }

        let saved = self.save_scope();
        for &v in variables.iter().flatten().unique() {
            self.register_variable(v.clone());
        }
        let body = self.compile_query(body, next)?;
        let body = self
            .emitter
            .emit_normal_op(ByteCode::ExitNonPathTracking, body);
        let mut next_alt: Option<Address> = None;

        for (i, pattern) in patterns.iter().enumerate().rev() {
            let mut tmp = if let Some(next_alt) = next_alt {
                self.compile_try(pattern, Some(&next_alt), body)?
            } else {
                pattern.compile(self, body)?
            };
            if i > 0 {
                for prev_ident in variables[i - 1].iter() {
                    let slot = *self.lookup_variable(prev_ident)?;
                    tmp = self.emitter.emit_normal_op(ByteCode::Store(slot), tmp);
                    self.emitter
                        .emit_normal_op(ByteCode::Push(Value::Null), tmp);
                }
            }
            if i + 1 != patterns.len() {
                tmp = self.emitter.emit_normal_op(ByteCode::Dup, tmp);
            }
            next_alt = Some(tmp)
        }
        let next = next_alt.unwrap();
        self.restore_scope(saved);
        let next = self.compile_term(source, next)?;
        let next = self
            .emitter
            .emit_normal_op(ByteCode::EnterNonPathTracking, next);
        let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_term(&mut self, term: &ast::Term, next: Address) -> Result<Address> {
        let ret = match term {
            Term::Constant(value) => self.emitter.emit_constant(value.clone(), next),
            Term::String(s) => {
                let ret: Address = if s.is_empty() {
                    self.emitter
                        .emit_constant(Value::string("".to_string()), next)
                } else if s.len() == 1 {
                    if let StringFragment::String(s) = &s[0] {
                        self.emitter.emit_constant(Value::string(s.clone()), next)
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                };
                ret
            }
            Term::Identity => next,
            Term::Recurse => {
                self.lookup_and_compile_func_call(Identifier("recurse".to_string()), &[], next)?
            }
            Term::Suffix(term, suffix) => self.compile_term_suffix(term, suffix, next)?,
            Term::Variable(name) => {
                let slot = *self.lookup_variable(name)?;
                let load = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                self.emitter.emit_normal_op(ByteCode::Pop, load)
            }
            Term::FunctionCall { name, args } => {
                self.lookup_and_compile_func_call(name.clone(), args, next)?
            }
            Term::Format(_) => todo!(),
            Term::Query(query) => self.compile_query(query, next)?,
            Term::Unary(operator, term) => {
                let operator = intrinsic::unary(operator);
                let next = self
                    .emitter
                    .emit_normal_op(ByteCode::Intrinsic1(operator), next);
                self.compile_term(term, next)?
            }
            Term::Object(kvs) => self.compile_object(kvs, next)?,
            Term::Array(query) => match query {
                None => self
                    .emitter
                    .emit_constant(Value::Array(PVector::new()), next),
                Some(query) => {
                    let slot = self.allocate_variable();
                    let load = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                    let backtrack = self.emitter.backtrack();
                    let append = self
                        .emitter
                        .emit_normal_op(ByteCode::Append(slot), backtrack);
                    let query = self.compile_query(query, append)?;
                    let next = self.emitter.emit_fork(load, query);
                    let next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
                    let next = self
                        .emitter
                        .emit_constant(Value::Array(PVector::new()), next);
                    self.emitter.emit_normal_op(ByteCode::Dup, next)
                }
            },
            Term::Break(_) => todo!(),
        };
        Ok(ret)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_query(&mut self, query: &ast::Query, next: Address) -> Result<Address> {
        let ret = match query {
            Query::Term(term) => self.compile_term(term, next)?,
            Query::WithFunc { function, query } => {
                let saved = self.save_scope();
                self.compile_funcdef(function)?;
                let next = self.compile_query(query, next)?;
                self.restore_scope(saved);
                next
            }
            Query::Pipe { lhs, rhs } => {
                let rhs_address = self.compile_query(rhs, next)?;
                self.compile_query(lhs, rhs_address)?
            }
            Query::Concat { lhs, rhs } => {
                let rhs_address = self.compile_query(rhs, next)?;
                let lhs_address = self.compile_query(lhs, next)?;
                self.emitter.emit_fork(rhs_address, lhs_address)
            }
            Query::Bind {
                source,
                patterns,
                body,
            } => self.compile_bind(source, patterns, body, next)?,
            Query::Reduce { .. } => todo!(),
            Query::ForEach { .. } => todo!(),
            Query::If {
                cond,
                positive,
                negative,
            } => self.compile_if(cond, positive, negative.as_ref(), next)?,
            Query::Try { body, catch } => self.compile_try(body, catch.as_ref(), next)?,
            Query::Label { .. } => todo!(),
            Query::Operate { lhs, operator, rhs } => match operator {
                BinaryOp::Arithmetic(operator) => {
                    let operator = intrinsic::binary(operator);
                    let next = self
                        .emitter
                        .emit_normal_op(ByteCode::Intrinsic2(operator), next);
                    let next = self.compile_query(lhs, next)?;
                    let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                    let next = self.compile_query(rhs, next)?;
                    self.emitter.emit_normal_op(ByteCode::Dup, next)
                }
                BinaryOp::Alt => {
                    let found = self.allocate_variable();
                    let backtrack = self.emitter.backtrack();
                    let rhs = self.compile_query(rhs, next)?;
                    let rhs = self
                        .emitter
                        .emit_normal_op(ByteCode::JumpUnless(rhs), backtrack);
                    let rhs = self.emitter.emit_normal_op(ByteCode::Load(found), rhs);

                    let next = self.emitter.emit_normal_op(ByteCode::Store(found), next);
                    let next = self
                        .emitter
                        .emit_normal_op(ByteCode::Push(Value::True), next);
                    let next = self
                        .emitter
                        .emit_normal_op(ByteCode::JumpUnless(backtrack), next);
                    let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
                    let lhs = self.compile_query(lhs, next)?;
                    let fork = self.emitter.emit_fork(rhs, lhs);
                    let next = self.emitter.emit_normal_op(ByteCode::Store(found), fork);
                    self.emitter
                        .emit_normal_op(ByteCode::Push(Value::False), next)
                }
                BinaryOp::And => {
                    let rhs = self.compile_if(
                        rhs,
                        &Term::Constant(Value::True),
                        Some(&Term::Constant(Value::False)),
                        next,
                    )?;
                    self.compile_if(lhs, &rhs, Some(&Term::Constant(Value::False)), next)?
                }
                BinaryOp::Or => {
                    let rhs = self.compile_if(
                        rhs,
                        &Term::Constant(Value::True),
                        Some(&Term::Constant(Value::False)),
                        next,
                    )?;
                    self.compile_if(lhs, &Term::Constant(Value::True), Some(&rhs), next)?
                }
            },
            Query::Update { .. } => todo!(),
            Query::Compare {
                lhs,
                comparator: operator,
                rhs,
            } => {
                let operator = intrinsic::comparator(operator);
                let next = self
                    .emitter
                    .emit_normal_op(ByteCode::Intrinsic2(operator), next);
                let next = self.compile_query(lhs, next)?;
                let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                let next = self.compile_query(rhs, next)?;
                self.emitter.emit_normal_op(ByteCode::Dup, next)
            }
        };
        Ok(ret)
    }

    fn compile_prelude(&mut self, ast: &ast::Program) -> Result<()> {
        assert!(ast.module_header.is_none());
        assert!(ast.imports.is_empty());
        assert_eq!(ast.query, Term::Identity.into());
        for func in &ast.functions {
            self.compile_funcdef(func)?
        }
        Ok(())
    }

    pub fn compile<M: ModuleLoader>(
        &mut self,
        ast: &ast::Program,
        module_loader: &M,
    ) -> Result<Program> {
        let preludes = module_loader.prelude()?;
        for prelude in preludes {
            self.compile_prelude(&prelude)?;
        }
        if !ast.imports.is_empty() {
            todo!()
        }
        if ast.module_header.is_some() {
            todo!()
        }
        for func in &ast.functions {
            self.compile_funcdef(func)?
        }
        let output = self.emitter.output();
        let backtrack = self.emitter.backtrack();
        let query_start = self.compile_query(&ast.query, output)?;
        let new_scope = self.exit_global_scope_and_emit_new_scope(query_start);
        let entry_point = self.emitter.emit_terminal_op(ByteCode::Call {
            function: new_scope,
            return_address: backtrack,
        });
        Ok(Program {
            code: self.emitter.code.clone(),
            entry_point,
        })
    }
}
