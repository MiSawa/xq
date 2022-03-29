use std::{
    cell::{RefCell, RefMut},
    iter::Fuse,
    rc::Rc,
};

use itertools::Itertools;
use thiserror::Error;

use crate::{
    data_structure::{
        undo::{
            stack::{UStack, UStackToken},
            Undo,
        },
        PStack, PVector,
    },
    intrinsic,
    util::make_owned,
    vm::{
        bytecode::{ClosureAddress, NamedFunction},
        error::QueryExecutionError,
        Address, ByteCode, Program, Result, ScopeId, ScopedSlot, Value,
    },
    Array, InputError,
};

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub(crate) enum ProgramError {
    #[error("tried to pop an empty stack")]
    PopEmptyStack,
    #[error("tried to use an uninitialized frame")]
    UninitializedFrame,
    #[error("tried to load from an uninitialized slot of a frame")]
    UnknownSlot,
    #[error("tried to load from an uninitialized slot of a frame")]
    UninitializedSlot,
    #[error("tried to pop a frame but there was no frame to pop")]
    PopEmptyFrame,
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
            PathElement::Array(i) => Value::number(i),
            PathElement::Object(key) => Value::String(key),
            PathElement::Any(value) => value,
        }
    }
}

#[derive(Debug, Clone)]
enum PathValueIterator {
    Array {
        array: Rc<Array>,
        next: usize,
    },
    Object {
        sorted_elements: Rc<Vec<(Rc<String>, Value)>>,
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

type Frames = PVector<Option<Frame>>;
type Closure = (ClosureAddress, Frames);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LabelId(usize);

#[derive(Debug, Clone)]
struct Frame {
    slots: Rc<RefCell<Vec<Option<Value>>>>,
    closure_slots: Rc<RefCell<Vec<Option<Closure>>>>,
    label_slots: Rc<RefCell<Vec<Option<LabelId>>>>,
}

impl Frame {
    fn new(variable_cnt: usize, closure_cnt: usize, label_cnt: usize) -> Self {
        Self {
            slots: Rc::new(RefCell::new(
                std::iter::repeat(None).take(variable_cnt).collect(),
            )),
            closure_slots: Rc::new(RefCell::new(
                std::iter::repeat(None).take(closure_cnt).collect(),
            )),
            label_slots: Rc::new(RefCell::new(
                std::iter::repeat(None).take(label_cnt).collect(),
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
    next_label_id: usize,
    forks: Vec<(<State as Undo>::UndoToken, OnFork)>,
}

#[derive(Debug)]
struct State {
    pc: Address,
    stack: UStack<Value>,

    frames: Frames,
    frame_stack: UStack<(Address, Frames, bool)>, // (return pc, saved frames, chain pop)
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
    CatchLabel(LabelId),
    Iterate,
    IterateContext,
}

impl Environment {
    fn new(state: <State as Undo>::UndoToken) -> Self {
        Self {
            next_label_id: 0,
            forks: vec![(state, OnFork::IterateContext)],
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

    fn gen_label_id(&mut self) -> LabelId {
        let ret = LabelId(self.next_label_id);
        self.next_label_id += 1;
        ret
    }
}

impl State {
    fn new(pc: Address) -> Self {
        State {
            pc,
            stack: Default::default(),
            paths: Default::default(),
            frames: Default::default(),
            frame_stack: Default::default(),
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

    fn push_with_path_element(&mut self, item: Value, path_elem: PathElement) {
        self.push(item.clone());
        if let Some(Some((_, current, path))) = self.paths.top_mut() {
            *current = item;
            path.push(path_elem);
        }
    }

    fn push_with_path(&mut self, item: Value, path_elements: Vec<PathElement>) {
        self.push(item.clone());
        if let Some(Some((_, current, path))) = self.paths.top_mut() {
            *current = item;
            for path_element in path_elements {
                path.push(path_element);
            }
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

    fn push_closure(&mut self, closure: ClosureAddress) {
        self.closure_stack.push((closure, self.frames.clone()));
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
        let frame = self
            .frames
            .get_mut(scoped_slot.0 .0)
            .and_then(Option::as_mut)
            .ok_or(ProgramError::UninitializedFrame)
            .unwrap();
        let slots = frame.slots.borrow_mut();
        RefMut::map(slots, |v| {
            v.get_mut(scoped_slot.1)
                .ok_or(ProgramError::UnknownSlot)
                .unwrap()
        })
    }

    fn closure_slot(&mut self, scoped_slot: &ScopedSlot) -> RefMut<Option<Closure>> {
        let frame = self
            .frames
            .get_mut(scoped_slot.0 .0)
            .and_then(Option::as_mut)
            .ok_or(ProgramError::UninitializedFrame)
            .unwrap();
        let slots = frame.closure_slots.borrow_mut();
        RefMut::map(slots, |v| {
            v.get_mut(scoped_slot.1)
                .ok_or(ProgramError::UnknownSlot)
                .unwrap()
        })
    }

    fn label_slot(&mut self, scoped_slot: &ScopedSlot) -> RefMut<Option<LabelId>> {
        let frame = self
            .frames
            .get_mut(scoped_slot.0 .0)
            .and_then(Option::as_mut)
            .ok_or(ProgramError::UninitializedFrame)
            .unwrap();
        let slots = frame.label_slots.borrow_mut();
        RefMut::map(slots, |v| {
            v.get_mut(scoped_slot.1)
                .ok_or(ProgramError::UnknownSlot)
                .unwrap()
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn push_frame(
        &mut self,
        context_frames: Option<Frames>,
        scope_id: ScopeId,
        variable_cnt: usize,
        closure_cnt: usize,
        label_cnt: usize,
        chain_ret: bool,
        return_address: Address,
    ) {
        let saved = if let Some(frames) = context_frames {
            std::mem::replace(&mut self.frames, frames)
        } else {
            self.frames.clone()
        };
        if self.frames.len() <= scope_id.0 {
            self.frames
                .extend(std::iter::repeat(None).take(scope_id.0 - self.frames.len() + 1))
        }
        self.frames[scope_id.0] = Some(Frame::new(variable_cnt, closure_cnt, label_cnt));
        self.frame_stack.push((return_address, saved, chain_ret));
    }

    fn pop_frame(&mut self) -> Address {
        loop {
            let (return_address, frames, chain) = self
                .frame_stack
                .pop()
                .ok_or(ProgramError::PopEmptyFrame)
                .unwrap();
            self.frames = frames;
            if !chain {
                break return_address;
            }
        }
    }

    fn current_return_address(&self) -> Address {
        let frame = self
            .frame_stack
            .top()
            .expect("Tried to obtain return address but the frame stack was empty");
        frame.0
    }
}

#[derive(Debug)]
struct StateToken {
    pc: Address,
    stack: UStackToken,

    frames: Frames,
    frame_stack: UStackToken,
    closure_stack: UStackToken,

    paths: UStackToken,

    iterators: UStackToken,
}

impl Undo for State {
    type UndoToken = StateToken;

    fn save(&mut self) -> Self::UndoToken {
        StateToken {
            pc: self.pc,
            stack: self.stack.save(),
            frames: self.frames.clone(),
            frame_stack: self.frame_stack.save(),
            closure_stack: self.closure_stack.save(),
            paths: self.paths.save(),
            iterators: self.iterators.save(),
        }
    }

    fn undo(&mut self, token: Self::UndoToken) {
        self.pc = token.pc;
        self.stack.undo(token.stack);
        self.frames = token.frames;
        self.frame_stack.undo(token.frame_stack);
        self.closure_stack.undo(token.closure_stack);
        self.paths.undo(token.paths);
        self.iterators.undo(token.iterators);
    }
}

impl Machine {
    pub fn new(program: Program) -> Self {
        Self {
            program: Rc::new(program),
        }
    }

    pub fn start<
        C: Iterator<Item = Result<Value, InputError>>,
        I: Iterator<Item = Result<Value, InputError>>,
    >(
        &mut self,
        context: C,
        input: I,
    ) -> ResultIterator<C, I> {
        let mut state = State::new(self.program.entry_point);
        let env = Environment::new(state.save());
        ResultIterator {
            program: self.program.clone(),
            env,
            state,
            context: context.fuse(),
            input: input.fuse(),
        }
    }
}

pub struct ResultIterator<
    C: Iterator<Item = Result<Value, InputError>>,
    I: Iterator<Item = Result<Value, InputError>>,
> {
    program: Rc<Program>,
    env: Environment,
    state: State,
    context: Fuse<C>,
    input: Fuse<I>,
}

impl<
        C: Iterator<Item = Result<Value, InputError>>,
        I: Iterator<Item = Result<Value, InputError>>,
    > Iterator for ResultIterator<C, I>
{
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        run_code(
            &self.program,
            &mut self.state,
            &mut self.env,
            &mut self.context,
            &mut self.input,
        )
    }
}

fn run_code(
    program: &Program,
    state: &mut State,
    env: &mut Environment,
    context: &mut impl Iterator<Item = Result<Value, InputError>>,
    input: &mut impl Iterator<Item = Result<Value, InputError>>,
) -> Option<Result<Value>> {
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
                            Some(QueryExecutionError::UserDefinedError(v)) => {
                                state.undo(token);
                                state.push(v);
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
                            state.push_with_path_element(value, path_elem);
                            break 'select_fork state.save();
                        }
                    }
                }
                (OnFork::IterateContext, None) => {
                    match context.next() {
                        None => return None,
                        Some(Err(e)) => {
                            // Don't push fork again so we don't infinitely see input errors
                            // even when the context stream gave us infinite errors.
                            return Some(Err(e.into()));
                        }
                        Some(Ok(v)) => {
                            state.undo(token);
                            env.push_fork(state, OnFork::IterateContext, state.pc);
                            state.push(v);
                            break 'select_fork state.save();
                        }
                    }
                }
                (OnFork::IterateContext, Some(_e)) => {
                    env.push_fork(state, OnFork::IterateContext, state.pc);
                    return Some(Err(err.take().unwrap()));
                }
                (_, Some(_)) => continue 'select_fork,
            }
            break 'select_fork token;
        };
        state.undo(token);
        let mut call_pc: Option<Address> = None;
        let mut context_frame: Option<Frames> = None;
        let mut chain_ret = false;

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
                state.frames.iter().enumerate().collect_vec()
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
                        Value::Object(map) => match key {
                            Value::String(s) => {
                                let mut map = make_owned(map);
                                map.insert(s, value);
                                state.push(map.into());
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
                            Rc::make_mut(v).push(value);
                        }
                        _ => {
                            panic!("expected an array to append to, but was not an array");
                        }
                    }
                }
                Index => {
                    let value = state.pop();
                    let index = state.pop();
                    if let Err(e) = state.check_origin_of_path(&value) {
                        err.replace(e);
                        continue 'backtrack;
                    }
                    match intrinsic::index(value, index) {
                        Ok((value, path_elem)) => {
                            state.push_with_path_element(value, path_elem);
                        }
                        Err(e) => {
                            err.replace(e);
                        }
                    }
                }
                Slice { start, end } => {
                    let value = state.pop();
                    let end = if *end { Some(state.pop()) } else { None };
                    let start = if *start { Some(state.pop()) } else { None };
                    if let Err(e) = state.check_origin_of_path(&value) {
                        err = Some(e);
                        continue 'backtrack;
                    }
                    match intrinsic::slice(value, start, end) {
                        Ok((value, path_elem)) => {
                            state.push_with_path_element(value, path_elem);
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
                        value @ (Value::Null
                        | Value::Boolean(_)
                        | Value::Number(_)
                        | Value::String(_)) => {
                            err.replace(QueryExecutionError::IterateOnNonIterable(value));
                            continue 'backtrack;
                        }
                        Value::Array(array) => PathValueIterator::Array { array, next: 0 },
                        Value::Object(map) => {
                            let sorted_elements = map
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone())) // TODO: Is there a better way?
                                .sorted_unstable_by(|(lhs, _), (rhs, _)| Ord::cmp(lhs, rhs))
                                .collect();
                            PathValueIterator::Object {
                                sorted_elements: Rc::new(sorted_elements),
                                next: 0,
                            }
                        }
                    };
                    state.push_iterator(iter);
                    env.push_fork(state, OnFork::Iterate, state.pc.get_next());
                    continue 'backtrack;
                }
                Access => {
                    let path = state.pop();
                    let value = state.pop();
                    match intrinsic::get_path(value, path.clone()) {
                        Ok(value) => match path {
                            Value::Array(arr) => {
                                let path = arr.iter().cloned().map(PathElement::Any).collect_vec();
                                state.push_with_path(value, path);
                            }
                            _ => {
                                err.replace(QueryExecutionError::PathNotArray(path));
                            }
                        },
                        Err(e) => {
                            err.replace(e);
                        }
                    }
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
                    let mut elems = Vec::new();
                    while let Some(elem) = path.pop() {
                        elems.push(elem.into());
                    }
                    elems.reverse();
                    state.push(Array::from_vec(elems).into());
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
                ForkLabel(label_slot) => {
                    let label_id = env.gen_label_id();
                    state.label_slot(label_slot).replace(label_id);
                    // pc doesn't really matter
                    env.push_fork(state, OnFork::CatchLabel(label_id), state.pc.get_next());
                }
                Break(label_slot) => {
                    let label_id = *state
                        .label_slot(label_slot)
                        .as_ref()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap();
                    err = Some(QueryExecutionError::Breaking(label_id));
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
                CallClosure(slot) => {
                    let closure = state
                        .closure_slot(slot)
                        .as_ref()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap()
                        .clone();
                    assert!(call_pc.replace(state.pc.get_next()).is_none());
                    assert!(context_frame.replace(closure.1).is_none());
                    state.pc = closure.0 .0;
                    continue 'cycle;
                }
                Call(function) => {
                    assert!(call_pc.replace(state.pc.get_next()).is_none());
                    assert!(context_frame.is_none());
                    state.pc = *function;
                    continue 'cycle;
                }
                TailCallClosure(slot) => {
                    let closure = state
                        .closure_slot(slot)
                        .as_ref()
                        .ok_or(ProgramError::UninitializedSlot)
                        .unwrap()
                        .clone();
                    let return_address = state.pop_frame();
                    assert!(call_pc.replace(return_address).is_none());
                    assert!(context_frame.replace(closure.1).is_none());
                    state.pc = closure.0 .0;
                    continue 'cycle;
                }
                TailCall(function) => {
                    let return_address = state.pop_frame();
                    assert!(call_pc.replace(return_address).is_none());
                    assert!(context_frame.is_none());
                    state.pc = *function;
                    continue 'cycle;
                }
                CallChainRet(function) => {
                    let return_address = state.current_return_address();
                    assert!(call_pc.replace(return_address).is_none());
                    assert!(context_frame.is_none());
                    chain_ret = true;
                    state.pc = *function;
                    continue 'cycle;
                }
                NewFrame {
                    id,
                    variable_cnt,
                    closure_cnt,
                    label_cnt,
                } => {
                    let return_address = call_pc
                        .take()
                        .expect("NewFrame should be called after Call");
                    let context_frame = context_frame.take();
                    state.push_frame(
                        context_frame,
                        *id,
                        *variable_cnt,
                        *closure_cnt,
                        *label_cnt,
                        chain_ret,
                        return_address,
                    );
                    chain_ret = false;
                }
                Ret => {
                    let return_address = state.pop_frame();
                    state.pc = return_address;
                    continue 'cycle;
                }
                Output => {
                    let value = state.pop();
                    return Some(Ok(value));
                }
                Input => match input.next() {
                    Some(Err(e)) => {
                        return Some(Err(e.into()));
                    }
                    Some(Ok(v)) => {
                        state.pop();
                        state.push(v.clone())
                    }
                    None => err = Some(QueryExecutionError::NoMoreInputError),
                },
                Intrinsic0(NamedFunction { name, func }) => {
                    let context = state.pop();
                    log::trace!("Calling function {} with context {:?}", name, context);
                    match func(context) {
                        Ok(value) => state.push(value),
                        Err(QueryExecutionError::UserDefinedError(Value::Null)) => {
                            continue 'backtrack
                        }
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
                        Err(QueryExecutionError::UserDefinedError(Value::Null)) => {
                            continue 'backtrack
                        }
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
