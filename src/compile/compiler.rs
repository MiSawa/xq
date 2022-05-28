use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    rc::Rc,
    slice::from_ref,
};

use itertools::Itertools;
use thiserror::Error;
use xq_lang::ast::{
    self, BinaryArithmeticOp, BinaryOp, BindPattern, ConstantPrimitive, FuncArg, FuncDef,
    Identifier, ObjectBindPatternEntry, Query, StringFragment, Suffix, Term, UpdateOp,
};

use crate::{
    data_structure::PHashMap,
    intrinsic,
    module_loader::{ModuleLoadError, ModuleLoader},
    value::Array,
    vm::{
        bytecode::{ClosureAddress, NamedFn0, NamedFn1, NamedFn2},
        Address, ByteCode, Program, ScopeId, ScopedSlot,
    },
    Number, Value,
};

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
/// # Important note
/// Things are compiled backwards. This makes us handling jump-ish code much easier, since most of
/// the jumps (or reference to an address of a code to be precise) are going forward, so we don't
/// have to do something like "acquire a placeholder, replace it with jump when the jump target
/// address was determined".

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Use of unknown variable `{0:}`")]
    UnknownVariable(Identifier),
    #[error("Use of unknown function `{0:}` that takes {1:} arguments")]
    UnknownFunction(Identifier, usize),
    #[error("Bind pattern has the same variable `{0:}`")]
    SameVariableInPattern(Identifier),
    #[error("Unknown label `{0:}`")]
    UnknownLabel(Identifier),
    #[error(transparent)]
    ModuleLoadError(#[from] ModuleLoadError),
    #[error("Unknown string formatter `{0:?}`")]
    UnknownStringFormatter(Identifier),
}

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionIdentifier(pub(crate) Identifier, pub(crate) usize);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct DeclaredFunction {
    /// The address for normal function call. [ByteCode::NewFrame] will be there.
    address: Address,
    /// The address for tail call that doesn't require caller's frame anymore.
    tail_call_discard_frame: Address,
    /// The address for tail call in case the caller's frame has to be preserved.
    /// Compiles either to a [ByteCode::Jump] or to the normal path [ByteCode::NewFrame].
    tail_call_preserve_frame: Address,
    /// The list of arg type.
    arg_types: Vec<ArgType>,
}
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

    fn follow_jump(&self, mut address: Address) -> Address {
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
            Some(ByteCode::Call(func_address)) => {
                let func_address = *func_address;
                self.jump_or_follow(address.get_next());
                ByteCode::Call(func_address)
            }
            Some(ByteCode::TailCall(address)) => ByteCode::TailCall(*address),
            Some(ByteCode::Backtrack) => ByteCode::Backtrack,
            Some(ByteCode::Ret) => ByteCode::Ret,
            Some(ByteCode::Output) => ByteCode::Output,
            _ => ByteCode::Jump(address),
        };
        self.code.push(code);
    }

    fn get_next_op(&self, next: Address) -> &ByteCode {
        &self.code[self.follow_jump(next).0]
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

    fn emit_constant_primitive<V>(&mut self, value: V, next: Address) -> Address
    where
        V: Into<ConstantPrimitive>,
    {
        match value.into() {
            ConstantPrimitive::Null => self.emit_constant(Value::Null, next),
            ConstantPrimitive::False => self.emit_constant(false, next),
            ConstantPrimitive::True => self.emit_constant(true, next),
            ConstantPrimitive::Number(v) => self.emit_constant(Number::from(v.0), next),
            ConstantPrimitive::String(s) => self.emit_constant(s, next),
        }
    }

    fn emit_constant<V>(&mut self, value: V, next: Address) -> Address
    where
        V: Into<Value>,
    {
        self.emit_normal_op(ByteCode::Const(value.into()), next)
    }

    fn emit_push<V>(&mut self, value: V, next: Address) -> Address
    where
        V: Into<Value>,
    {
        self.emit_normal_op(ByteCode::Push(value.into()), next)
    }

    fn emit_fork(&mut self, fork_pc: Address, next: Address) -> Address {
        self.emit_normal_op(ByteCode::Fork { fork_pc }, next)
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
    next_label_slot_id: usize,
    functions: PHashMap<FunctionIdentifier, FunctionLike>,
    variables: PHashMap<Identifier, ScopedSlot>,
    labels: PHashMap<Identifier, ScopedSlot>,
    leaked_scope_ids: Rc<RefCell<HashSet<ScopeId>>>,
}

impl Scope {
    fn new(id: ScopeId) -> Self {
        Self {
            id,
            next_variable_slot_id: 0,
            next_closure_slot_id: 0,
            next_label_slot_id: 0,
            functions: Default::default(),
            variables: Default::default(),
            labels: Default::default(),
            leaked_scope_ids: Rc::new(RefCell::new(Default::default())),
        }
    }

    fn nested(id: ScopeId, previous: &Self) -> Self {
        Self {
            id,
            next_variable_slot_id: 0,
            next_closure_slot_id: 0,
            next_label_slot_id: 0,
            functions: previous.functions.clone(),
            variables: previous.variables.clone(),
            labels: previous.labels.clone(),
            leaked_scope_ids: previous.leaked_scope_ids.clone(),
        }
    }

    fn require_slot(&self) -> bool {
        self.next_variable_slot_id > 0
            || self.next_closure_slot_id > 0
            || self.next_label_slot_id > 0
    }

    fn has_slot_leaked_scope(&self) -> bool {
        self.leaked_scope_ids.borrow().contains(&self.id)
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
            FunctionIdentifier(name, function.arg_types.len()),
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

    fn allocate_label(&mut self) -> ScopedSlot {
        let slot = ScopedSlot(self.id, self.next_label_slot_id);
        self.next_label_slot_id += 1;
        slot
    }

    fn register_label(&mut self, name: Identifier) -> ScopedSlot {
        let slot = ScopedSlot(self.id, self.next_label_slot_id);
        self.next_label_slot_id += 1;
        self.labels.insert(name, slot);
        slot
    }

    fn lookup_variable(&self, name: &Identifier) -> Option<&ScopedSlot> {
        let ret = self.variables.get(name);
        if let Some(ScopedSlot(id, _)) = ret {
            if id != &self.id {
                self.leaked_scope_ids.borrow_mut().insert(*id);
            }
        }
        ret
    }

    fn lookup_function(&self, identifier: &FunctionIdentifier) -> Option<&FunctionLike> {
        let ret = self.functions.get(identifier);
        if let Some(FunctionLike::Closure(ScopedSlot(id, _))) = ret {
            if id != &self.id {
                self.leaked_scope_ids.borrow_mut().insert(*id);
            }
        }
        ret
    }

    fn lookup_label(&self, name: &Identifier) -> Option<&ScopedSlot> {
        let ret = self.labels.get(name);
        if let Some(ScopedSlot(id, _)) = ret {
            if id != &self.id {
                self.leaked_scope_ids.borrow_mut().insert(*id);
            }
        }
        ret
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

impl Compile for Value {
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        Ok(compiler.emitter.emit_constant(self.clone(), next))
    }
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

impl Compile for Box<Query> {
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        compiler.compile_query(self.as_ref(), next)
    }
}

impl<F> Compile for F
where
    F: Fn(&mut Compiler, Address) -> Result<Address>,
{
    fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
        self(compiler, next)
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
        save.next_label_slot_id = current.next_label_slot_id;
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

    fn current_scope_require_slot(&self) -> bool {
        self.current_scope().require_slot()
    }

    fn exit_scope(&mut self, id: ScopeId) -> (usize, usize, usize) {
        let scope = self
            .scope_stack
            .pop()
            .expect("Scope stack shouldn't be empty");
        assert_eq!(id, scope.id);
        (
            scope.next_variable_slot_id,
            scope.next_closure_slot_id,
            scope.next_label_slot_id,
        )
    }

    fn exit_scope_and_emit_new_frame(&mut self, id: ScopeId, next: Address) -> Address {
        let scope_info = self.exit_scope(id);
        self.emitter.emit_normal_op(
            ByteCode::NewFrame {
                id,
                variable_cnt: scope_info.0,
                closure_cnt: scope_info.1,
                label_cnt: scope_info.2,
            },
            next,
        )
    }

    fn exit_global_scope_and_emit_new_frame(&mut self, next: Address) -> Address {
        self.exit_scope_and_emit_new_frame(ScopeId(0), next)
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
                "input" => FunctionLike::Intrinsic(ByteCode::Input, vec![]),
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
                "getpath" => {
                    FunctionLike::ManuallyImplemented("getpath", |compiler, args, next| {
                        assert_eq!(1, args.len());
                        let next = compiler.emitter.emit_normal_op(ByteCode::Access, next);
                        let next = compiler.compile_query(&args[0], next)?;
                        let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                        Ok(next)
                    })
                }
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
            .ok_or_else(|| CompileError::UnknownFunction(function.0.clone(), function.1))
    }

    /// Consumes nothing, produces nothing. Just places the code.
    fn compile_function_inner(&mut self, args: &[FuncArg], body: &Query) -> Result<Address> {
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
        Ok(next)
    }

    /// Consumes nothing, produces nothing. Just places the code.
    fn compile_closure(&mut self, closure: &Query) -> Result<Address> {
        let scope_id = self.enter_scope();
        let next = self.compile_function_inner(&[], closure)?;
        let next = self.exit_scope_and_emit_new_frame(scope_id, next);
        Ok(next)
    }

    /// Consumes nothing, produces nothing. Registers function to the current scope.
    fn compile_funcdef(&mut self, func: &FuncDef) -> Result<()> {
        let (func_address, placeholder) = self.emitter.emit_terminal_placeholder();
        let (tail_call_discard_frame, tail_call_discard_frame_placeholder) =
            self.emitter.emit_terminal_placeholder();
        let (tail_call_preserve_frame, tail_call_preserve_frame_placeholder) =
            self.emitter.emit_terminal_placeholder();
        let arg_types = func
            .args
            .iter()
            .map(|arg| match arg {
                FuncArg::Variable(_) => ArgType::Value,
                FuncArg::Closure(_) => ArgType::Closure,
            })
            .collect();
        self.register_function(
            func.name.clone(),
            DeclaredFunction {
                address: func_address,
                tail_call_discard_frame,
                tail_call_preserve_frame,
                arg_types,
            },
        );

        let scope_id = self.enter_scope();
        let function_body = self.compile_function_inner(&func.args, &func.body)?;
        let require_slot = self.current_scope_require_slot();
        let real_address = self.exit_scope_and_emit_new_frame(scope_id, function_body);
        self.emitter
            .replace_placeholder(placeholder, ByteCode::Jump(real_address));
        if require_slot {
            self.emitter.replace_placeholder(
                tail_call_discard_frame_placeholder,
                ByteCode::TailCall(real_address),
            );
            self.emitter.replace_placeholder(
                tail_call_preserve_frame_placeholder,
                ByteCode::CallChainRet(real_address),
            );
        } else {
            self.emitter.replace_placeholder(
                tail_call_discard_frame_placeholder,
                ByteCode::Jump(function_body),
            );
            self.emitter.replace_placeholder(
                tail_call_preserve_frame_placeholder,
                ByteCode::Jump(function_body),
            );
        }
        Ok(())
    }

    fn compile_func_call_args(
        &mut self,
        args: &[Query],
        types: &[ArgType],
        mut next: Address,
    ) -> Result<Address> {
        assert_eq!(args.len(), types.len());
        // Compile closures first to reduce number of jumps
        let mut closure_addresses = vec![];
        for (arg, ty) in args.iter().zip(types.iter()) {
            if ty == &ArgType::Closure {
                closure_addresses.push(Some(self.compile_closure(arg)?));
            } else {
                closure_addresses.push(None);
            }
        }
        // We need to evaluate all value-typed arguments on the same current context (stack top).
        // To do so, we dup, calc and swap to shift a copy of the context up
        let mut require_context = false;
        for ((arg, ty), closure) in args.iter().zip(types.iter()).zip(closure_addresses).rev() {
            match ty {
                ArgType::Closure => {
                    let closure =
                        ClosureAddress(closure.expect("closures should be compiled already"));
                    /*
                    TODO: Uncomment on #14
                    if require_context {
                        next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                    }
                     */
                    // TODO: Optimize closures that doesn't require frames saved.
                    next = self
                        .emitter
                        .emit_normal_op(ByteCode::PushClosure(closure), next);
                }
                ArgType::Value => {
                    if require_context {
                        next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                    }
                    next = self.compile_query(arg, next)?;
                    if require_context {
                        next = self.emitter.emit_normal_op(ByteCode::Dup, next);
                    } else {
                        require_context = true;
                    }
                }
            }
        }
        if require_context {
            next = self.emitter.emit_normal_op(ByteCode::Dup, next);
        }
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_func_call(
        &mut self,
        function: FunctionLike,
        args: &[Query],
        next: Address,
    ) -> Result<Address> {
        Ok(match function {
            FunctionLike::Function(DeclaredFunction {
                address,
                tail_call_discard_frame,
                tail_call_preserve_frame,
                arg_types,
            }) => {
                let call = if matches!(self.emitter.get_next_op(next), ByteCode::Ret) {
                    // The destination `tail_call_*_frame` will be (or is already) replaced with either
                    // a `ByteCode::TailCall(new_frame_address)` or a `ByteCode::Jump(after_new_frame_address)`
                    // depending on whether the function requires a slot.
                    // To use `ByteCode::TailCall`, we can't have any slot that are referenced from the function we're calling,
                    // since we'll discard the current frame.
                    // As an estimation, we care if there's any slot on the current scope already referenced from another scope.
                    // The function we call, and functions that can be invoked from there
                    // are either a function that we've already compiled, or a function currently in the scope stack.
                    // Either way, this heuristic should catch the usage of a slot by them.
                    let can_discard_frame = !self.current_scope().has_slot_leaked_scope();
                    if can_discard_frame {
                        self.emitter
                            .emit_terminal_op(ByteCode::Jump(tail_call_discard_frame))
                    } else {
                        self.emitter
                            .emit_terminal_op(ByteCode::Jump(tail_call_preserve_frame))
                    }
                } else {
                    self.emitter.emit_normal_op(ByteCode::Call(address), next)
                };
                // Since value-args doesn't have to be preserved and closure-args save their frames,
                // we don't need these args to be tracked for the above condition.
                self.compile_func_call_args(args, &arg_types, call)?
            }
            FunctionLike::Closure(slot) => {
                assert!(args.is_empty());
                if matches!(self.emitter.get_next_op(next), ByteCode::Ret)
                    && !self.current_scope().has_slot_leaked_scope()
                {
                    // log::info!("Tail call closure for slot {:?}", slot);
                    self.emitter
                        .emit_terminal_op(ByteCode::TailCallClosure(slot))
                } else {
                    self.emitter
                        .emit_normal_op(ByteCode::CallClosure(slot), next)
                }
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
        let next = self.compile_without_path_tracking(cond, next)?;
        let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    /// Execution order: `index`, `body`
    fn compile_index<T: Compile, U: Compile>(
        &mut self,
        body: &T,
        index: &U,
        next: Address,
    ) -> Result<Address> {
        let indexing = self.emitter.emit_normal_op(ByteCode::Index, next);
        let body = body.compile(self, indexing)?;
        let swap = self.emitter.emit_normal_op(ByteCode::Swap, body);
        let index = self.compile_without_path_tracking(index, swap)?;
        Ok(self.emitter.emit_normal_op(ByteCode::Dup, index))
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    /// Execution order: `start`, `end`, `body`
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
        let next = body.compile(self, next)?;

        let next = self
            .emitter
            .emit_normal_op(ByteCode::ExitNonPathTracking, next);
        let next = if let Some(end) = end {
            let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
            let next = end.compile(self, next)?;
            self.emitter.emit_normal_op(ByteCode::Dup, next)
        } else {
            next
        };
        let next = if let Some(start) = start {
            let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
            let next = start.compile(self, next)?;
            self.emitter.emit_normal_op(ByteCode::Dup, next)
        } else {
            next
        };

        let next = self
            .emitter
            .emit_normal_op(ByteCode::EnterNonPathTracking, next);
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
            Suffix::Optional => match term {
                Term::Suffix(term, suffix) if suffix != &Suffix::Optional => {
                    let next = self.compile_try::<_, Query>(
                        &Term::Suffix(Term::Identity.into(), suffix.clone()),
                        None,
                        next,
                    )?;
                    self.compile_term(term, next)
                }
                _ => self.compile_try::<_, Query>(term, None, next),
            },
            Suffix::Iterate => self.compile_iterate(term, next),
            Suffix::Index(ident) => self.compile_index(
                term,
                &Term::Constant(ConstantPrimitive::String(ident.0.clone())),
                next,
            ),
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

    fn compile_bind<T: Compile>(
        &mut self,
        source: &Term,
        patterns: &[BindPattern],
        body: &T,
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
                            ObjectBindPatternEntry::KeyOnly(ident) => {
                                *occurrences.entry(ident).or_insert(0) += 1;
                            }
                            ObjectBindPatternEntry::ValueOnly(_, p) => {
                                collect_variable_occurrences(p, occurrences);
                            }
                            ObjectBindPatternEntry::KeyAndValue(key, p) => {
                                *occurrences.entry(key).or_insert(0) += 1;
                                collect_variable_occurrences(p, occurrences);
                            }
                        }
                    }
                }
            }
        }

        /// Consumes a value from the stack
        impl Compile for BindPattern {
            fn compile(&self, compiler: &mut Compiler, next: Address) -> Result<Address> {
                let next = match self {
                    BindPattern::Variable(ident) => {
                        let slot = *compiler.lookup_variable(ident)?;
                        compiler.emitter.emit_normal_op(ByteCode::Store(slot), next)
                    }
                    BindPattern::Array(v) => {
                        assert!(!v.is_empty());
                        let mut tmp = next;
                        for (i, pattern) in v.iter().enumerate().rev() {
                            tmp = pattern.compile(compiler, tmp)?;
                            tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                            tmp = compiler.emitter.emit_normal_op(ByteCode::Swap, tmp);
                            tmp = compiler
                                .emitter
                                .emit_normal_op(ByteCode::Push(Value::number(i)), tmp);
                            if i + 1 != v.len() {
                                tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                            }
                        }
                        tmp
                    }
                    BindPattern::Object(entries) => {
                        assert!(!entries.is_empty());
                        let mut tmp = next;
                        for (i, entry) in entries.iter().rev().enumerate() {
                            match entry {
                                ObjectBindPatternEntry::KeyOnly(key) => {
                                    let slot = *compiler.lookup_variable(key)?;
                                    tmp =
                                        compiler.emitter.emit_normal_op(ByteCode::Store(slot), tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Swap, tmp);
                                    tmp = compiler.emitter.emit_normal_op(
                                        ByteCode::Push(Value::string(key.0.clone())),
                                        tmp,
                                    );
                                }
                                ObjectBindPatternEntry::ValueOnly(key, value) => {
                                    tmp = value.compile(compiler, tmp)?;
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Swap, tmp);
                                    tmp = compiler.compile_query(key, tmp)?;
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                                }
                                ObjectBindPatternEntry::KeyAndValue(key, value) => {
                                    tmp = value.compile(compiler, tmp)?;
                                    let slot = *compiler.lookup_variable(key)?;
                                    tmp =
                                        compiler.emitter.emit_normal_op(ByteCode::Store(slot), tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Index, tmp);
                                    tmp = compiler.emitter.emit_normal_op(ByteCode::Swap, tmp);
                                    tmp = compiler.emitter.emit_normal_op(
                                        ByteCode::Push(Value::string(key.0.clone())),
                                        tmp,
                                    );
                                }
                            }
                            if i != 0 {
                                tmp = compiler.emitter.emit_normal_op(ByteCode::Dup, tmp);
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
        let body = body.compile(self, next)?;
        let body = self
            .emitter
            .emit_normal_op(ByteCode::ExitNonPathTracking, body);
        let mut next_alt: Option<Address> = None;

        for (i, pattern) in patterns.iter().enumerate().rev() {
            let mut tmp = if let Some(next_alt) = next_alt {
                let tmp = pattern.compile(self, body)?;
                self.emitter
                    .emit_normal_op(ByteCode::ForkAlt { fork_pc: next_alt }, tmp)
            } else {
                pattern.compile(self, body)?
            };
            if i > 0 {
                for prev_ident in variables[i - 1].iter() {
                    let slot = *self.lookup_variable(prev_ident)?;
                    tmp = self.emitter.emit_normal_op(ByteCode::Store(slot), tmp);
                    tmp = self
                        .emitter
                        .emit_normal_op(ByteCode::Push(Value::Null), tmp);
                }
            }
            if i + 1 != patterns.len() {
                tmp = self.emitter.emit_normal_op(ByteCode::Dup, tmp);
            }
            next_alt = Some(tmp)
        }
        let mut next = next_alt.unwrap();

        for v in variables[1..]
            .iter()
            .flatten()
            .unique()
            .filter(|&&v| !variables[0].contains(v))
        {
            let slot = *self.lookup_variable(v)?;
            next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
            next = self
                .emitter
                .emit_normal_op(ByteCode::Push(Value::Null), next);
        }

        self.restore_scope(saved);
        let next = self.compile_term(source, next)?;
        let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
        let next = self
            .emitter
            .emit_normal_op(ByteCode::EnterNonPathTracking, next);
        Ok(next)
    }

    fn compile_string<T: Compile>(
        &mut self,
        fragments: &[StringFragment],
        stringifier: T,
        mut next: Address,
    ) -> Result<Address> {
        if fragments.is_empty() {
            return Ok(self
                .emitter
                .emit_constant(Value::string("".to_string()), next));
        } else if fragments.len() == 1 {
            if let StringFragment::String(s) = &fragments[0] {
                return Ok(self.emitter.emit_constant(Value::string(s.clone()), next));
            }
        }

        let add = intrinsic::binary(&BinaryArithmeticOp::Add);
        let slot = self.allocate_variable();

        for (i, fragment) in fragments.iter().enumerate() {
            if i + 1 != fragments.len() {
                next = self
                    .emitter
                    .emit_normal_op(ByteCode::Intrinsic1(add.clone()), next);
            }
            match fragment {
                StringFragment::String(s) => {
                    next = self
                        .emitter
                        .emit_normal_op(ByteCode::Push(Value::string(s.clone())), next);
                }
                StringFragment::Query(q) => {
                    next = stringifier.compile(self, next)?;
                    next = self.compile_query(q, next)?;
                    next = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                }
            }
        }
        next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
        Ok(next)
    }

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_term(&mut self, term: &ast::Term, next: Address) -> Result<Address> {
        let ret = match term {
            Term::Constant(value) => self.emitter.emit_constant_primitive(value.clone(), next),
            Term::String(s) => self.compile_string(
                s,
                |compiler: &mut Compiler, next| {
                    Ok(compiler.emitter.emit_normal_op(
                        ByteCode::Intrinsic0(NamedFn0 {
                            name: "text",
                            func: intrinsic::text,
                        }),
                        next,
                    ))
                },
                next,
            )?,
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
            Term::Format(format, str) => {
                let stringifier = intrinsic::stringifier(&format.0)
                    .ok_or_else(|| CompileError::UnknownStringFormatter(format.clone()))?;
                match str {
                    Some(s) => self.compile_string(
                        s,
                        &|compiler: &mut Compiler, next| {
                            Ok(compiler
                                .emitter
                                .emit_normal_op(ByteCode::Intrinsic0(stringifier.clone()), next))
                        },
                        next,
                    )?,
                    None => self
                        .emitter
                        .emit_normal_op(ByteCode::Intrinsic0(stringifier), next),
                }
            }
            Term::Query(query) => self.compile_query(query, next)?,
            Term::Unary(operator, term) => {
                let operator = intrinsic::unary(operator);
                let next = self
                    .emitter
                    .emit_normal_op(ByteCode::Intrinsic0(operator), next);
                self.compile_term(term, next)?
            }
            Term::Object(kvs) => self.compile_object(kvs, next)?,
            Term::Array(query) => match query {
                None => self.emitter.emit_constant(Array::new(), next),
                Some(query) => {
                    let slot = self.allocate_variable();
                    let load = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                    let load = self.emitter.emit_normal_op(ByteCode::Pop, load);
                    let backtrack = self.emitter.backtrack();
                    let append = self
                        .emitter
                        .emit_normal_op(ByteCode::Append(slot), backtrack);
                    let query = self.compile_query(query, append)?;
                    let next = self.emitter.emit_fork(load, query);
                    let next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
                    self.emitter.emit_push(Array::new(), next)
                }
            },
            Term::Break(name) => {
                let label_slot = *self
                    .current_scope()
                    .lookup_label(name)
                    .ok_or_else(|| CompileError::UnknownLabel(name.clone()))?;
                self.emitter.emit_terminal_op(ByteCode::Break(label_slot))
            }
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
            } => self.compile_bind(source, patterns.as_slice(), body, next)?,
            Query::Reduce {
                source,
                pattern,
                initial,
                accumulator,
            } => {
                let slot = self.allocate_variable();
                let after = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                let after = self.emitter.emit_normal_op(ByteCode::Pop, after);

                let next = self.compile_bind(
                    source,
                    from_ref(pattern),
                    &|compiler: &mut Compiler, next: Address| -> Result<Address> {
                        let body = compiler.emitter.emit_normal_op(ByteCode::Store(slot), next);
                        let body = compiler.compile_query(accumulator, body)?;
                        Ok(compiler.emitter.emit_normal_op(ByteCode::Load(slot), body))
                    },
                    self.emitter.backtrack(),
                )?;
                let next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
                // TODO: What to do if `initial` produced more than single value?
                let next = self.compile_query(initial, next)?;
                let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
                self.emitter.emit_fork(after, next)
            }
            Query::ForEach {
                source,
                pattern,
                initial,
                update,
                extract,
            } => {
                let slot = self.allocate_variable();

                let next = self.compile_bind(
                    source,
                    from_ref(pattern),
                    &|compiler: &mut Compiler, next: Address| -> Result<Address> {
                        let next = if let Some(extract) = extract {
                            compiler.compile_query(extract, next)?
                        } else {
                            next
                        };
                        let next = compiler.emitter.emit_normal_op(ByteCode::Pop, next);
                        let next = compiler.emitter.emit_normal_op(ByteCode::Swap, next);
                        let body = compiler.emitter.emit_normal_op(ByteCode::Store(slot), next);
                        let body = compiler.emitter.emit_normal_op(ByteCode::Dup, body);
                        let body = compiler.compile_query(update, body)?;
                        Ok(compiler.emitter.emit_normal_op(ByteCode::Load(slot), body))
                    },
                    next,
                )?;
                let next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
                // TODO: What to do if `initial` produced more than single value?
                let next = self.compile_query(initial, next)?;
                self.emitter.emit_normal_op(ByteCode::Dup, next)
            }
            Query::If {
                cond,
                positive,
                negative,
            } => self.compile_if(cond, positive, negative.as_ref(), next)?,
            Query::Try { body, catch } => self.compile_try(body, catch.as_ref(), next)?,
            Query::Label { label, body } => {
                let saved = self.save_scope();
                let label_slot = self.current_scope_mut().register_label(label.clone());
                let body = self.compile_query(body, next)?;
                self.restore_scope(saved);
                self.emitter
                    .emit_normal_op(ByteCode::ForkLabel(label_slot), body)
            }
            Query::Operate { lhs, operator, rhs } => match operator {
                BinaryOp::Arithmetic(operator) => {
                    let operator = intrinsic::binary(operator);
                    let next = self
                        .emitter
                        .emit_normal_op(ByteCode::Intrinsic1(operator), next);
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
                    let next = self.emitter.emit_push(true, next);
                    let next = self
                        .emitter
                        .emit_normal_op(ByteCode::JumpUnless(backtrack), next);
                    let next = self.emitter.emit_normal_op(ByteCode::Dup, next);
                    let lhs = self.compile_query(lhs, next)?;
                    let fork = self.emitter.emit_fork(rhs, lhs);
                    let next = self.emitter.emit_normal_op(ByteCode::Store(found), fork);
                    self.emitter.emit_push(false, next)
                }
                BinaryOp::And => self.compile_if(
                    lhs,
                    &|compiler: &mut Compiler, next| {
                        compiler.compile_if(
                            rhs,
                            &Value::from(true),
                            Some(&Value::from(false)),
                            next,
                        )
                    },
                    Some(&Value::from(false)),
                    next,
                )?,
                BinaryOp::Or => self.compile_if(
                    lhs,
                    &Value::from(true),
                    Some(&|compiler: &mut Compiler, next| {
                        compiler.compile_if(
                            rhs,
                            &Value::from(true),
                            Some(&Value::from(false)),
                            next,
                        )
                    }),
                    next,
                )?,
            },
            Query::Update { lhs, operator, rhs } => {
                // TODO: Research on the evaluation order.... maybe using `input`?
                fn compile_update<T: Compile>(
                    compiler: &mut Compiler,
                    path_expression: &Query,
                    modification: &T,
                    next: Address,
                ) -> Result<Address> {
                    let slot = compiler.allocate_variable();
                    let tmp_slot = compiler.allocate_variable();
                    let del_slot = compiler.allocate_variable();
                    let label_slot = compiler.current_scope_mut().allocate_label();

                    let after = compiler.emitter.emit_normal_op(
                        ByteCode::Intrinsic1(NamedFn1 {
                            name: "delpaths",
                            func: intrinsic::del_paths,
                        }),
                        next,
                    );
                    let after = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Load(del_slot), after);
                    let after = compiler.emitter.emit_normal_op(ByteCode::Load(slot), after);
                    let after = compiler.emitter.emit_normal_op(ByteCode::Pop, after);

                    let on_empty = compiler
                        .emitter
                        .emit_terminal_op(ByteCode::Break(label_slot));
                    let on_empty = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(slot), on_empty);
                    let on_empty = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Append(del_slot), on_empty);
                    let on_empty = compiler.emitter.emit_normal_op(ByteCode::Pop, on_empty);

                    // for each path(lhs), call set_path(., path, . | getpath(path) | rhs) for the first value produced
                    let next = compiler
                        .emitter
                        .emit_terminal_op(ByteCode::Break(label_slot));
                    let next = compiler.emitter.emit_normal_op(ByteCode::Store(slot), next);
                    let next = compiler.emitter.emit_normal_op(
                        ByteCode::Intrinsic2(NamedFn2 {
                            name: "setpath",
                            func: intrinsic::set_path,
                        }),
                        next,
                    );
                    let next = modification.compile(compiler, next)?;
                    let next = compiler.emitter.emit_fork(on_empty, next);
                    // value, path, indexed value
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Load(tmp_slot), next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Swap, next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Load(slot), next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::ExitPathTracking, next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(tmp_slot), next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::ForkLabel(label_slot), next);
                    let next = compiler.compile_query(path_expression, next)?;
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::EnterPathTracking, next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Store(slot), next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                    let next = compiler.emitter.emit_fork(after, next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(del_slot), next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Push(Array::new().into()), next);
                    Ok(next)
                }

                fn compile_assign<T: Compile, U: Compile>(
                    compiler: &mut Compiler,
                    path_expression: &Query,
                    rhs: &T,
                    // Called in state [..., value, path, rhs, lhs]. Should combine the top two into one.
                    combiner: Option<&U>,
                    next: Address,
                ) -> Result<Address> {
                    let updated_value_slot = compiler.allocate_variable();
                    let rhs_slot = compiler.allocate_variable();
                    let lhs_slot = combiner.map(|_| compiler.allocate_variable());

                    let after = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Load(updated_value_slot), next);
                    let after = compiler.emitter.emit_normal_op(ByteCode::Pop, after);

                    let next = compiler.emitter.backtrack();
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(updated_value_slot), next);
                    let next = compiler.emitter.emit_normal_op(
                        ByteCode::Intrinsic2(NamedFn2 {
                            name: "setpath",
                            func: intrinsic::set_path,
                        }),
                        next,
                    );
                    let next = if let Some(combiner) = combiner {
                        let next = combiner.compile(compiler, next)?;
                        let next = compiler
                            .emitter
                            .emit_normal_op(ByteCode::Load(lhs_slot.unwrap()), next);
                        compiler
                            .emitter
                            .emit_normal_op(ByteCode::Load(rhs_slot), next)
                    } else {
                        compiler
                            .emitter
                            .emit_normal_op(ByteCode::Load(rhs_slot), next)
                    };
                    let next = compiler.emitter.emit_normal_op(ByteCode::Swap, next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Load(updated_value_slot), next);
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::ExitPathTracking, next);
                    let next = if let Some(slot) = lhs_slot {
                        let next = compiler.emitter.emit_normal_op(ByteCode::Store(slot), next);
                        compiler.emitter.emit_normal_op(ByteCode::Dup, next)
                    } else {
                        next
                    };
                    let next = path_expression.compile(compiler, next)?;
                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::EnterPathTracking, next);

                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(updated_value_slot), next);
                    let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                    let next = compiler.emitter.emit_fork(after, next);

                    let next = compiler
                        .emitter
                        .emit_normal_op(ByteCode::Store(rhs_slot), next);
                    let next = rhs.compile(compiler, next)?;
                    let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                    Ok(next)
                }

                match operator {
                    UpdateOp::Modify => compile_update(self, lhs, rhs, next)?,
                    UpdateOp::Assign => compile_assign::<_, Query>(self, lhs, rhs, None, next)?,
                    UpdateOp::Alt => compile_assign(
                        self,
                        lhs,
                        rhs,
                        Some(&|compiler: &mut Compiler, next| -> Result<Address> {
                            let on_both = compiler.emitter.emit_normal_op(ByteCode::Pop, next);
                            let on_falsy = on_both;
                            let on_truthy =
                                compiler.emitter.emit_normal_op(ByteCode::Swap, on_both);

                            let next = compiler
                                .emitter
                                .emit_normal_op(ByteCode::JumpUnless(on_falsy), on_truthy);
                            let next = compiler.emitter.emit_normal_op(ByteCode::Dup, next);
                            Ok(next)
                        }),
                        next,
                    )?,
                    UpdateOp::Arithmetic(op) => compile_assign(
                        self,
                        lhs,
                        rhs,
                        Some(&|compiler: &mut Compiler, next| -> Result<Address> {
                            let func = intrinsic::binary(op);
                            let next = compiler
                                .emitter
                                .emit_normal_op(ByteCode::Intrinsic1(func), next);
                            Ok(next)
                        }),
                        next,
                    )?,
                }
            }
            Query::Compare {
                lhs,
                comparator: operator,
                rhs,
            } => {
                let operator = intrinsic::comparator(operator);
                let next = self
                    .emitter
                    .emit_normal_op(ByteCode::Intrinsic1(operator), next);
                let next = self.compile_query(lhs, next)?;
                let next = self.emitter.emit_normal_op(ByteCode::Swap, next);
                let next = self.compile_query(rhs, next)?;
                self.emitter.emit_normal_op(ByteCode::Dup, next)
            }
        };
        Ok(ret)
    }

    fn compile_without_path_tracking<T: Compile>(
        &mut self,
        body: &T,
        next: Address,
    ) -> Result<Address> {
        let next = self
            .emitter
            .emit_normal_op(ByteCode::ExitNonPathTracking, next);
        let next = body.compile(self, next)?;
        let next = self
            .emitter
            .emit_normal_op(ByteCode::EnterNonPathTracking, next);
        Ok(next)
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
        let new_frame = self.exit_global_scope_and_emit_new_frame(query_start);
        let entry_point = self
            .emitter
            .emit_normal_op(ByteCode::Call(new_frame), backtrack);
        Ok(Program {
            code: self.emitter.code.clone(),
            entry_point,
        })
    }
}
