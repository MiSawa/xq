use crate::{
    data_structure::{PStack, PVector},
    vm::{
        bytecode::{Closure, NamedFunction},
        error::QueryExecutionError,
        intrinsic::truthy,
        Address, ByteCode, Program, Result, ScopeId, ScopedSlot, Value,
    },
};
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub(crate) enum ProgramError {
    #[error("tried to pop an empty stack")]
    PopEmptyStack,
    #[error("tried to use an uninitialized scope")]
    UninitializedScope,
    #[error("tried to load from an uninitialized slot of a scope")]
    UnknownSlot,
    #[error("tried to load from an uninitialized slot of a scope")]
    UninitializedSlot,
    #[error("tried to pop a scope but there was no scope to pop")]
    PopEmptyScope,
    #[error("tried to restore an unknown scope")]
    PopUnknownScope,
}

#[derive(Debug, Clone)]
struct Scope {
    slots: PVector<Option<Value>>,
    closure_slots: PVector<Option<Closure>>,
}

impl Scope {
    fn new(variable_cnt: usize, closure_cnt: usize) -> Self {
        Self {
            slots: std::iter::repeat(None).take(variable_cnt).collect(),
            closure_slots: std::iter::repeat(None).take(closure_cnt).collect(),
        }
    }
}

#[derive(Debug)]
struct Machine {
    env: Environment,
}

#[derive(Debug)]
struct Environment {
    program: Program,
    forks: Vec<(State, OnFork)>,
}

enum PathElement {
    Array(usize),
    Object(Rc<String>),
}

#[derive(Debug, Clone)]
enum PathValueIterator {
    Array {
        array: PVector<Value>,
        next: usize,
        limit: usize,
    },
    Object {
        sorted_elements: PVector<(Rc<String>, Value)>,
        next: usize,
    },
}

impl Iterator for PathValueIterator {
    type Item = (PathElement, Value);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            PathValueIterator::Array { array, next, limit } => {
                if *next >= *limit {
                    None
                } else {
                    let ret = (PathElement::Array(*next), array[*next].clone());
                    *next += 1;
                    Some(ret)
                }
            }
            PathValueIterator::Object {
                sorted_elements,
                next,
            } => {
                if *next >= sorted_elements.len() {
                    None
                } else {
                    let (index, value) = sorted_elements[*next].clone();
                    *next += 1;
                    Some((PathElement::Object(index), value))
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    pc: Address,
    stack: PStack<Value>,

    scopes: PVector<(Option<Scope>, PStack<Scope>)>,
    scope_stack: PStack<(Address, ScopeId)>, // (pc, pushed_scope)
    closure_stack: PStack<Closure>,

    iterators: PStack<PathValueIterator>,
}

#[derive(Debug, Clone)]
enum OnFork {
    Nop,
    IgnoreError,
    CatchError,
    SkipCatch,
}

impl Environment {
    fn push_fork(&mut self, state: &State, on_fork: OnFork, new_pc: Address) {
        let mut new_state = state.clone();
        new_state.pc = new_pc;
        self.forks.push((new_state, on_fork));
    }

    fn pop_fork(&mut self) -> Option<(State, OnFork)> {
        self.forks.pop()
    }
}

impl State {
    fn new(pc: Address) -> Self {
        State {
            pc,
            stack: Default::default(),
            scopes: Default::default(),
            scope_stack: Default::default(),
            closure_stack: Default::default(),
            iterators: Default::default(),
        }
    }

    fn push(&mut self, item: Value) {
        self.stack.push(item)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().ok_or(ProgramError::PopEmptyStack).unwrap()
    }

    fn dup(&mut self) {
        let value = self.pop();
        self.push(value.clone());
        self.push(value);
    }

    fn push_closure(&mut self, closure: Closure) {
        self.closure_stack.push(closure)
    }

    fn pop_closure(&mut self) -> Closure {
        self.closure_stack
            .pop()
            .ok_or(ProgramError::PopEmptyStack)
            .unwrap()
    }

    fn slot(&mut self, scoped_slot: &ScopedSlot) -> &mut Option<Value> {
        let scope = self
            .scopes
            .get_mut(scoped_slot.0 .0)
            .and_then(|(x, _)| x.as_mut())
            .ok_or(ProgramError::UninitializedScope)
            .unwrap();
        scope
            .slots
            .get_mut(scoped_slot.1)
            .ok_or(ProgramError::UnknownSlot)
            .unwrap()
    }

    fn closure_slot(&mut self, scoped_slot: &ScopedSlot) -> &mut Option<Closure> {
        let scope = self
            .scopes
            .get_mut(scoped_slot.0 .0)
            .and_then(|(x, _)| x.as_mut())
            .ok_or(ProgramError::UninitializedScope)
            .unwrap();
        scope
            .closure_slots
            .get_mut(scoped_slot.1)
            .ok_or(ProgramError::UnknownSlot)
            .unwrap()
    }

    fn push_scope(
        &mut self,
        scope_id: ScopeId,
        variable_cnt: usize,
        closure_cnt: usize,
        pc: Address,
    ) {
        if self.scopes.len() <= scope_id.0 {
            self.scopes.extend(
                std::iter::repeat_with(|| (None, PStack::new()))
                    .take(scope_id.0 - self.scopes.len() + 1),
            )
        }
        let (scope, stack) = &mut self.scopes[scope_id.0];
        if let Some(prev_scope) = scope.replace(Scope::new(variable_cnt, closure_cnt)) {
            stack.push(prev_scope);
        }
        self.scope_stack.push((pc, scope_id));
    }

    fn pop_scope(&mut self) -> Address {
        let (pc, scope_id) = self
            .scope_stack
            .pop()
            .ok_or(ProgramError::PopEmptyScope)
            .unwrap();
        let (current, prev) = self
            .scopes
            .get_mut(scope_id.0)
            .ok_or(ProgramError::PopUnknownScope)
            .unwrap();
        assert!(current.is_some(), "Pop unknown scope");
        *current = prev.pop();
        pc
    }
}

impl Machine {
    fn new(program: Program) -> Self {
        let state = State::new(program.entry_point);
        Self {
            env: Environment {
                program,
                forks: vec![(state, OnFork::Nop)],
            },
        }
    }
}

impl Iterator for Machine {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        run_code(&mut self.env)
    }
}

fn run_code(env: &mut Environment) -> Option<Result<Value>> {
    let mut err: Option<QueryExecutionError> = None;
    let mut call_pc: Option<Address> = None;
    let mut catch_skip: usize = 0;
    'backtrack: loop {
        let (mut state, on_fork) = if let Some(x) = env.pop_fork() {
            x
        } else {
            return err.map(Err);
        };
        match on_fork {
            OnFork::Nop => {}
            OnFork::IgnoreError => {
                if catch_skip == 0 {
                    err = None
                } else {
                    catch_skip -= 1
                }
            }
            OnFork::CatchError => {
                if catch_skip == 0 {
                    match err.take() {
                        None => continue 'backtrack,
                        Some(e) => state.push(Value::String(Rc::new(format!("{:?}", e)))),
                    }
                } else {
                    catch_skip -= 1
                }
            }
            OnFork::SkipCatch => {
                catch_skip += 1;
                continue 'backtrack;
            }
        }
        assert_eq!(catch_skip, 0);
        'cycle: loop {
            if err.is_some() {
                continue 'backtrack;
            }
            let code = env.program.fetch_code(state.pc)?;
            use ByteCode::*;
            match code {
                Unreachable => panic!("Reached to the unreachable"),
                PlaceHolder => panic!("Reached to a place holder"),
                Nop => {}
                Push(v) => {
                    state.push(v.clone());
                }
                Pop => {
                    state.pop();
                }
                Dup => {
                    state.dup();
                }
                Const(v) => {
                    state.pop();
                    state.push(v.clone())
                }
                Load(scoped_slot) => {
                    let value = state
                        .slot(scoped_slot)
                        .as_ref()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap()
                        .clone();
                    state.push(value);
                }
                Store(scoped_slot) => {
                    let value = state.pop();
                    state.slot(scoped_slot).replace(value);
                }
                PushClosure(closure) => {
                    state.push_closure(*closure);
                }
                StoreClosure(slot) => {
                    let closure = state.pop_closure();
                    state.closure_slot(slot).replace(closure);
                }
                Object => todo!("Implement {:?}", code),
                Append(scoped_slot) => {
                    let value = state.pop();
                    let slot_item = state
                        .slot(scoped_slot)
                        .as_mut()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap();
                    match slot_item {
                        Value::Array(v) => {
                            v.push_back(value);
                        }
                        _ => {
                            panic!("expected a array to append to, but was not an array");
                        }
                    }
                }
                Fork { fork_pc } => {
                    let fork_pc = *fork_pc;
                    env.push_fork(&state, OnFork::Nop, fork_pc);
                }
                ForkTryBegin { catch_pc } => match catch_pc {
                    None => env.push_fork(&state, OnFork::IgnoreError, state.pc.get_next()),
                    Some(pc) => {
                        let new_pc = *pc;
                        env.push_fork(&state, OnFork::CatchError, new_pc)
                    }
                },
                ForkTryEnd => env.push_fork(&state, OnFork::SkipCatch, state.pc.get_next()),
                ForkAlt => todo!("Implement {:?}", code),
                ForkLabel => todo!("Implement {:?}", code),
                Backtrack => continue 'backtrack,
                Jump(address) => {
                    state.pc = *address;
                    continue 'cycle;
                }
                JumpUnless(address) => {
                    let value = state.pop();
                    if !truthy(value) {
                        state.pc = *address;
                        continue 'cycle;
                    }
                }
                CallClosure {
                    slot,
                    return_address,
                } => {
                    let closure = state
                        .closure_slot(slot)
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap();
                    assert_eq!(call_pc.replace(*return_address), None);
                    state.pc = closure.0;
                    continue 'cycle;
                }
                Call {
                    function,
                    return_address,
                } => {
                    assert_eq!(call_pc.replace(*return_address), None);
                    state.pc = *function;
                    continue 'cycle;
                }
                PushPC => todo!("Implement {:?}", code),
                CallPC => todo!("Implement {:?}", code),
                NewScope {
                    id,
                    variable_cnt,
                    closure_cnt,
                } => {
                    let return_address = call_pc
                        .take()
                        .expect("NewScope should be called after Call");
                    state.push_scope(*id, *variable_cnt, *closure_cnt, return_address);
                }
                Ret => {
                    let return_address = state.pop_scope();
                    state.pc = return_address;
                    continue 'cycle;
                }
                Output => {
                    let value = state.pop();
                    return Some(Ok(value));
                }
                Each => todo!("Implement {:?}", code),
                ExpBegin => todo!("Implement {:?}", code),
                ExpEnd => todo!("Implement {:?}", code),
                PathBegin => todo!("Implement {:?}", code),
                PathEnd => todo!("Implement {:?}", code),
                Intrinsic1(NamedFunction { name: _name, func }) => {
                    let arg = state.pop();
                    match func(arg) {
                        Ok(value) => state.push(value),
                        Err(e) => err = Some(e),
                    }
                }
                Intrinsic2(NamedFunction { name: _name, func }) => {
                    let rhs = state.pop();
                    let lhs = state.pop();
                    match func(lhs, rhs) {
                        Ok(value) => state.push(value),
                        Err(e) => err = Some(e),
                    }
                }
            }
            state.pc.next();
        }
    }
}
