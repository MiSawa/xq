use crate::{
    data_structure::{
        undo::{
            stack::{UStack, UStackToken},
            Undo,
        },
        PStack, PVector,
    },
    intrinsic,
    vm::{
        bytecode::{Closure, Label, NamedFunction},
        error::QueryExecutionError,
        Address, ByteCode, Program, Result, ScopeId, ScopedSlot, Value,
    },
    Number,
};
use itertools::Itertools;
use num::bigint::ToBigInt;
use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};
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
pub(crate) enum PathElement {
    Array(isize),
    Object(Rc<String>),
    Any(Value),
}

impl From<PathElement> for Value {
    fn from(elem: PathElement) -> Self {
        match elem {
            PathElement::Array(i) => Value::number(Number::from_integer(i.to_bigint().unwrap())),
            PathElement::Object(key) => Value::String(key),
            PathElement::Any(value) => value,
        }
    }
}

#[derive(Debug, Clone)]
enum PathValueIterator {
    Array {
        array: PVector<Value>,
        next: usize,
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
            PathValueIterator::Array { array, next } => {
                if *next >= array.len() {
                    None
                } else {
                    let ret = (PathElement::Array(*next as isize), array[*next].clone());
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
struct Scope {
    slots: Rc<RefCell<Vec<Option<Value>>>>,
    closure_slots: Rc<RefCell<Vec<Option<Closure>>>>,
}

impl Scope {
    fn new(variable_cnt: usize, closure_cnt: usize) -> Self {
        Self {
            slots: Rc::new(RefCell::new(
                std::iter::repeat(None).take(variable_cnt).collect(),
            )),
            closure_slots: Rc::new(RefCell::new(
                std::iter::repeat(None).take(closure_cnt).collect(),
            )),
        }
    }
}

#[derive(Debug)]
pub struct Machine {
    program: Rc<Program>,
}

#[derive(Debug)]
struct Environment {
    forks: Vec<(<State as Undo>::UndoToken, OnFork)>,
}

#[derive(Debug)]
struct State {
    pc: Address,
    stack: UStack<Value>,

    scopes: PVector<(Option<Scope>, PStack<Scope>)>,
    scope_stack: UStack<(Address, ScopeId)>, // (pc, pushed_scope)
    closure_stack: UStack<Closure>,

    paths: UStack<Option<(Value, Value, PStack<PathElement>)>>, // origin, current, path stack

    iterators: UStack<PathValueIterator>,
}

#[derive(Debug, Clone)]
enum OnFork {
    Nop,
    IgnoreError,
    CatchError,
    SkipCatch,
    TryAlternative,
    CatchLabel(Label),
    Iterate,
}

impl Environment {
    fn new(state: <State as Undo>::UndoToken) -> Self {
        Self {
            forks: vec![(state, OnFork::Nop)],
        }
    }

    fn push_fork(&mut self, state: &mut State, on_fork: OnFork, mut new_pc: Address) {
        std::mem::swap(&mut state.pc, &mut new_pc);
        let token = state.save();
        self.forks.push((token, on_fork));
        std::mem::swap(&mut state.pc, &mut new_pc);
    }

    fn pop_fork(&mut self) -> Option<(<State as Undo>::UndoToken, OnFork)> {
        self.forks.pop()
    }
}

impl State {
    fn new(pc: Address) -> Self {
        State {
            pc,
            stack: Default::default(),
            paths: Default::default(),
            scopes: Default::default(),
            scope_stack: Default::default(),
            closure_stack: Default::default(),
            iterators: Default::default(),
        }
    }

    fn top(&mut self) -> Option<&Value> {
        self.stack.top()
    }

    fn push(&mut self, item: Value) {
        self.stack.push(item)
    }

    fn check_origin_of_path(&mut self, origin: &Value) -> Result<()> {
        if let Some(Some((_, current, _))) = self.paths.top_mut() {
            if current == origin {
                Ok(())
            } else {
                Err(QueryExecutionError::InvalidPathError(origin.clone()))
            }
        } else {
            Ok(())
        }
    }

    fn push_with_path(&mut self, item: Value, path_elem: PathElement) {
        self.push(item.clone());
        if let Some(Some((_, current, path))) = self.paths.top_mut() {
            *current = item;
            path.push(path_elem);
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().ok_or(ProgramError::PopEmptyStack).unwrap()
    }

    fn dup(&mut self) {
        self.stack.dup();
    }

    fn swap(&mut self) {
        self.stack.swap();
    }

    fn enter_path_tracking(&mut self, base_value: Value) {
        self.paths
            .push(Some((base_value.clone(), base_value, Default::default())))
    }

    fn exit_tracked_path(&mut self) -> (Value, PStack<PathElement>) {
        match self.paths.pop() {
            Some(Some((value, _, path))) => (value, path),
            x => {
                panic!("Expected a path tracking thing but got {:?}", x);
            }
        }
    }

    fn enter_non_path_tracking(&mut self) {
        self.paths.push(None);
    }

    fn exit_non_path_tracking(&mut self) {
        assert!(matches!(self.paths.pop(), Some(None)));
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

    fn push_iterator(&mut self, iter: PathValueIterator) {
        self.iterators.push(iter)
    }

    fn top_iterator(&mut self) -> Option<&mut PathValueIterator> {
        self.iterators.top_mut()
    }

    fn slot(&mut self, scoped_slot: &ScopedSlot) -> RefMut<Option<Value>> {
        let scope = self
            .scopes
            .get_mut(scoped_slot.0 .0)
            .and_then(|(x, _)| x.as_mut())
            .ok_or(ProgramError::UninitializedScope)
            .unwrap();
        let slots = scope.slots.borrow_mut();
        RefMut::map(slots, |v| {
            v.get_mut(scoped_slot.1)
                .ok_or(ProgramError::UnknownSlot)
                .unwrap()
        })
    }

    fn closure_slot(&mut self, scoped_slot: &ScopedSlot) -> RefMut<Option<Closure>> {
        let scope = self
            .scopes
            .get_mut(scoped_slot.0 .0)
            .and_then(|(x, _)| x.as_mut())
            .ok_or(ProgramError::UninitializedScope)
            .unwrap();
        let slots = scope.closure_slots.borrow_mut();
        RefMut::map(slots, |v| {
            v.get_mut(scoped_slot.1)
                .ok_or(ProgramError::UnknownSlot)
                .unwrap()
        })
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

#[derive(Debug)]
struct StateToken {
    pc: Address,
    stack: UStackToken,

    scopes: PVector<(Option<Scope>, PStack<Scope>)>,
    scope_stack: UStackToken,
    closure_stack: UStackToken,

    paths: UStackToken,

    iterators: UStackToken,
}

impl Undo for State {
    type UndoToken = StateToken;

    fn save(&mut self) -> Self::UndoToken {
        log::debug!("Save with scope stack {:?}", self.scope_stack);
        StateToken {
            pc: self.pc,
            stack: self.stack.save(),
            scopes: self.scopes.clone(),
            scope_stack: self.scope_stack.save(),
            closure_stack: self.closure_stack.save(),
            paths: self.paths.save(),
            iterators: self.iterators.save(),
        }
    }

    fn undo(&mut self, token: Self::UndoToken) {
        self.pc = token.pc;
        self.stack.undo(token.stack);
        self.scopes = token.scopes;
        self.scope_stack.undo(token.scope_stack);
        self.closure_stack.undo(token.closure_stack);
        self.paths.undo(token.paths);
        self.iterators.undo(token.iterators);

        log::debug!("Undo to scope stack {:?}", self.scope_stack);
    }
}

impl Machine {
    pub fn new(program: Program) -> Self {
        Self {
            program: Rc::new(program),
        }
    }

    pub fn run(&mut self, value: Value) -> ResultIterator {
        let mut state = State::new(self.program.entry_point);
        state.push(value);
        let env = Environment::new(state.save());
        ResultIterator {
            program: self.program.clone(),
            env,
            state,
        }
    }
}

pub struct ResultIterator {
    program: Rc<Program>,
    env: Environment,
    state: State,
}

impl Iterator for ResultIterator {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        run_code(&self.program, &mut self.state, &mut self.env)
    }
}

fn run_code(program: &Program, state: &mut State, env: &mut Environment) -> Option<Result<Value>> {
    let mut err: Option<QueryExecutionError> = None;
    log::trace!("Start from environment {:?}", env);
    'backtrack: loop {
        log::trace!(
            "Fork stack: {:?}",
            env.forks.iter().map(|(_, f)| f).collect_vec()
        );
        let mut catch_skip: usize = 0;
        let token = 'select_fork: loop {
            let (token, on_fork) = if let Some(x) = env.pop_fork() {
                x
            } else {
                return err.map(Err);
            };
            log::trace!(
                "On fork {:?} with err {:?} and token {:?}",
                on_fork,
                err,
                token
            );
            match (on_fork, &err) {
                (
                    OnFork::CatchLabel(catch_label),
                    Some(QueryExecutionError::Breaking(breaking_label)),
                ) if catch_label == *breaking_label => {
                    err = None;
                    continue 'select_fork;
                }
                (OnFork::CatchLabel(_), _) => {
                    continue 'select_fork;
                }
                (_, Some(QueryExecutionError::Breaking(_))) => {
                    continue 'select_fork;
                }
                (OnFork::Nop, None) => {} // nop
                (OnFork::IgnoreError, _) => {
                    if catch_skip == 0 {
                        err = None;
                    } else {
                        catch_skip -= 1;
                    }
                    continue 'select_fork;
                }
                (OnFork::CatchError, _) => {
                    if catch_skip == 0 {
                        match err.take() {
                            None => continue 'select_fork,
                            Some(QueryExecutionError::UserDefinedError(s)) => {
                                state.undo(token);
                                state.push(Value::string(s));
                                break 'select_fork state.save();
                            }
                            Some(e) => {
                                state.undo(token);
                                state.push(Value::string(format!("{:?}", e)));
                                break 'select_fork state.save();
                            }
                        }
                    } else {
                        catch_skip -= 1;
                        continue 'select_fork;
                    }
                }
                (OnFork::SkipCatch, _) => {
                    catch_skip += 1;
                    continue 'select_fork;
                }
                (OnFork::TryAlternative, None) => continue 'select_fork,
                (OnFork::TryAlternative, Some(_)) => {
                    err = None;
                }
                (OnFork::Iterate, None) => {
                    state.undo(token);
                    let it = state.top_iterator().expect("No iterator to iterate on");
                    match it.next() {
                        None => continue 'select_fork,
                        Some((path_elem, value)) => {
                            env.push_fork(state, OnFork::Iterate, state.pc);
                            state.push_with_path(value, path_elem);
                            break 'select_fork state.save();
                        }
                    }
                }
                (_, Some(_)) => continue 'select_fork,
            }
            break 'select_fork token;
        };
        state.undo(token);
        let mut call_pc: Option<Address> = None;
        log::trace!("Start fork with state {:?}", state);
        'cycle: loop {
            if err.is_some() {
                continue 'backtrack;
            }
            let code = program.fetch_code(state.pc)?;
            log::trace!(
                "Execute code {:?} on stack = {:?}, slots = {:?}",
                code,
                state.stack,
                state.scopes.iter().enumerate().collect_vec()
            );
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
                Swap => {
                    state.swap();
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
                AppendObject => {
                    let value = state.pop();
                    let key = state.pop();
                    let obj = state.pop();
                    match obj {
                        Value::Object(mut map) => match key {
                            Value::String(s) => {
                                map.insert(s, value);
                                state.push(Value::Object(map));
                            }
                            value => {
                                err.replace(QueryExecutionError::ObjectNonStringKey(value));
                                continue 'backtrack;
                            }
                        },
                        _ => panic!("Expected an object to append to, but was not an object"),
                    }
                }
                Append(scoped_slot) => {
                    let value = state.pop();
                    let mut slot = state.slot(scoped_slot);
                    let slot_item = slot
                        .as_mut()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap();
                    match slot_item {
                        Value::Array(v) => {
                            v.push_back(value);
                        }
                        _ => {
                            panic!("expected an array to append to, but was not an array");
                        }
                    }
                }
                Index => {
                    let index = state.pop();
                    let value = state.pop();
                    if let Err(e) = state.check_origin_of_path(&value) {
                        err.replace(e);
                        continue 'backtrack;
                    }
                    match intrinsic::index(value, index) {
                        Ok((value, path_elem)) => {
                            state.push_with_path(value, path_elem);
                        }
                        Err(e) => {
                            err.replace(e);
                        }
                    }
                }
                Slice { start, end } => {
                    let end = if *end { Some(state.pop()) } else { None };
                    let start = if *start { Some(state.pop()) } else { None };
                    let value = state.pop();
                    if let Err(e) = state.check_origin_of_path(&value) {
                        err = Some(e);
                        continue 'backtrack;
                    }
                    match intrinsic::slice(value, start, end) {
                        Ok((value, path_elem)) => {
                            state.push_with_path(value, path_elem);
                        }
                        Err(e) => {
                            err.replace(e);
                        }
                    }
                }
                Each => {
                    let value = state.pop();
                    if let Err(e) = state.check_origin_of_path(&value) {
                        err = Some(e);
                        continue 'backtrack;
                    }
                    let iter = match value {
                        value
                        @
                        (Value::Null
                        | Value::True
                        | Value::False
                        | Value::Number(_)
                        | Value::String(_)) => {
                            err.replace(QueryExecutionError::IterateOnNonIterable(value));
                            continue 'backtrack;
                        }
                        Value::Array(array) => PathValueIterator::Array { array, next: 0 },
                        Value::Object(map) => {
                            let sorted_elements: PVector<_> = map
                                .into_iter()
                                .sorted_by(|(lhs, _), (rhs, _)| Ord::cmp(lhs, rhs))
                                .collect();
                            PathValueIterator::Object {
                                sorted_elements,
                                next: 0,
                            }
                        }
                    };
                    state.push_iterator(iter);
                    env.push_fork(state, OnFork::Iterate, state.pc.get_next());
                    continue 'backtrack;
                }
                EnterPathTracking => {
                    state.dup(); // TODO: Push some kind of token and swap instead?
                    let current = state
                        .top()
                        .ok_or(ProgramError::PopEmptyStack)
                        .unwrap()
                        .clone();
                    state.enter_path_tracking(current);
                }
                ExitPathTracking => {
                    let (origin, mut path) = state.exit_tracked_path();
                    state.pop(); // Discard indexed value
                    let supposed_to_be_origin = state.pop();
                    if origin != supposed_to_be_origin {
                        err = Some(QueryExecutionError::InvalidPathError(origin));
                        continue 'backtrack;
                    }
                    let mut elems = PVector::new();
                    while let Some(elem) = path.pop() {
                        elems.push_front(elem.into());
                    }
                    state.push(Value::Array(elems));
                }
                EnterNonPathTracking => {
                    state.enter_non_path_tracking();
                }
                ExitNonPathTracking => {
                    state.exit_non_path_tracking();
                }
                Fork { fork_pc } => {
                    let fork_pc = *fork_pc;
                    env.push_fork(state, OnFork::Nop, fork_pc);
                }
                ForkTryBegin { catch_pc } => match catch_pc {
                    None => env.push_fork(state, OnFork::IgnoreError, state.pc.get_next()),
                    Some(pc) => {
                        let new_pc = *pc;
                        env.push_fork(state, OnFork::CatchError, new_pc)
                    }
                },
                ForkTryEnd => env.push_fork(state, OnFork::SkipCatch, state.pc.get_next()),
                ForkAlt { fork_pc } => env.push_fork(state, OnFork::TryAlternative, *fork_pc),
                ForkLabel(label) => {
                    env.push_fork(state, OnFork::CatchLabel(*label), state.pc.get_next());
                    // pc doesn't really matter
                }
                Break(label) => {
                    err = Some(QueryExecutionError::Breaking(*label));
                    continue 'backtrack;
                }
                Backtrack => continue 'backtrack,
                Jump(address) => {
                    state.pc = *address;
                    continue 'cycle;
                }
                JumpUnless(address) => {
                    let value = state.pop();
                    if !intrinsic::truthy(value) {
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
                Intrinsic0(NamedFunction { name, func }) => {
                    let context = state.pop();
                    log::trace!("Calling function {} with context {:?}", name, context);
                    match func(context) {
                        Ok(value) => state.push(value),
                        Err(e) => err = Some(e),
                    }
                }
                Intrinsic1(NamedFunction { name, func }) => {
                    let arg1 = state.pop();
                    let context = state.pop();
                    log::trace!(
                        "Calling function {} with context {:?} and arg {:?}",
                        name,
                        context,
                        arg1
                    );
                    match func(context, arg1) {
                        Ok(value) => state.push(value),
                        Err(e) => err = Some(e),
                    }
                }
                Intrinsic2(NamedFunction { name, func }) => {
                    let arg2 = state.pop();
                    let arg1 = state.pop();
                    let context = state.pop();
                    log::trace!(
                        "Calling function {} with context {:?} and arg1 {:?} and arg2 {:?}",
                        name,
                        context,
                        arg1,
                        arg2
                    );
                    match func(context, arg1, arg2) {
                        Ok(value) => state.push(value),
                        Err(e) => err = Some(e),
                    }
                }
            }
            state.pc.next();
        }
    }
}
