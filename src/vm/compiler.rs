use crate::{
    ast::{self, BinaryOp, FuncArg, FuncDef, Identifier, Query, StringFragment, Suffix, Term},
    data_structure::{PHashMap, PVector},
    intrinsic,
    vm::{bytecode::Closure, Address, ByteCode, Program, ScopeId, ScopedSlot},
    Value,
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

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum CompileError {
    #[error("Use of unknown variable `{0:}`")]
    UnknownVariable(Identifier),
    #[error("Use of unknown function `{0:}`")]
    UnknownFunction(Identifier),
}

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FunctionIdentifier(Identifier, usize);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct DeclaredFunction(Address, Vec<ArgType>);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum ArgType {
    /// arg
    Closure,
    /// $arg
    Value,
}
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum FunctionOrClosure {
    Function(DeclaredFunction),
    Closure(ScopedSlot),
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
            Some(ByteCode::Ret) => todo!(),
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
    functions: PHashMap<FunctionIdentifier, FunctionOrClosure>,
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
            FunctionOrClosure::Function(function),
        );
    }

    fn register_closure(&mut self, name: Identifier) -> ScopedSlot {
        let slot = ScopedSlot(self.id, self.next_closure_slot_id);
        self.next_closure_slot_id += 1;
        self.functions.insert(
            FunctionIdentifier(name, 0),
            FunctionOrClosure::Closure(slot),
        );
        slot
    }

    fn lookup_variable(&self, name: &Identifier) -> Option<&ScopedSlot> {
        self.variables.get(name)
    }

    fn lookup_function(&self, identifier: &FunctionIdentifier) -> Option<&FunctionOrClosure> {
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

    fn lookup_function(&self, function: &FunctionIdentifier) -> Result<&FunctionOrClosure> {
        self.current_scope()
            .lookup_function(function)
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

    /// Consumes a value from the stack, and produces a single value onto the stack.
    fn compile_func_call(
        &mut self,
        function: &FunctionOrClosure,
        args: &[Query],
        next: Address,
    ) -> Result<Address> {
        Ok(match function {
            FunctionOrClosure::Function(DeclaredFunction(address, types)) => {
                let mut next = self.emitter.emit_function_call(*address, next);
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
                            self.emitter.emit_normal_op(
                                ByteCode::PushClosure(Closure(closure_address)),
                                next,
                            )
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
                if let Some(slot) = context_slot {
                    next = self.emitter.emit_normal_op(ByteCode::Store(slot), next);
                    self.emitter.emit_normal_op(ByteCode::Dup, next)
                } else {
                    next
                }
            }
            FunctionOrClosure::Closure(slot) => {
                self.emitter.emit_terminal_op(ByteCode::CallClosure {
                    slot: *slot,
                    return_address: next,
                })
            }
        })
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
        let next = cond.compile(self, next)?;
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

    /// Consumes a value from the stack, and produces a single value onto the stack.
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
        kvs: &Vec<(Query, Option<Query>)>,
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
            Term::Recurse => todo!(),
            Term::Suffix(term, suffix) => self.compile_term_suffix(term, suffix, next)?,
            Term::Variable(name) => {
                let slot = *self.lookup_variable(name)?;
                let load = self.emitter.emit_normal_op(ByteCode::Load(slot), next);
                self.emitter.emit_normal_op(ByteCode::Pop, load)
            }
            Term::FunctionCall { name, args } => {
                // TODO: How to avoid name.clone()?
                // TODO: How to avoid resolved.clone()?....
                let resolved = self
                    .lookup_function(&FunctionIdentifier(name.clone(), args.len()))?
                    .clone();
                self.compile_func_call(&resolved, args, next)?
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
            Query::Bind { .. } => todo!(),
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
                    todo!()
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

    pub fn compile(&mut self, ast: &ast::Program) -> Result<Program> {
        if !ast.functions.is_empty() {
            todo!()
        }
        if !ast.imports.is_empty() {
            todo!()
        }
        if ast.module_header.is_some() {
            todo!()
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
