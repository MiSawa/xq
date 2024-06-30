use std::fmt::{Debug, Formatter};

use crate::{
    vm::{Address, Result, ScopeId, ScopedSlot},
    Value,
};

#[derive(Clone, Eq, PartialEq)]
pub struct NamedFunction<F: Clone> {
    pub name: &'static str,
    pub func: F,
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ClosureAddress(pub(crate) Address);

pub type NamedFn0 = NamedFunction<fn(Value) -> Result<Value>>;
pub type NamedFn1 = NamedFunction<fn(Value, Value) -> Result<Value>>;
pub type NamedFn2 = NamedFunction<fn(Value, Value, Value) -> Result<Value>>;
pub type NamedFn3 = NamedFunction<fn(Value, Value, Value, Value) -> Result<Value>>;

impl<F: Clone> Debug for NamedFunction<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Intrinsic {}", self.name))
    }
}

/// Byte code of the virtual machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ByteCode {
    /// Just to prevent a pc overrun...
    Unreachable,
    /// This byte code is used temporarily while compiling,
    /// e.g. to replace to a jump to a not-yet-emitted address.
    PlaceHolder,
    /// Does nothing. Can be useful if we want to patch byte code for optimization.
    Nop,
    /// Pushes an immediate value to the top of the stack.
    Push(Value),
    /// Pops and discard the top of the stack.
    /// # Panics
    /// Panics if the stack was empty.
    Pop,
    /// Duplicates the top of the stack and pushes it.
    /// # Panics
    /// Panics if the stack was empty.
    Dup,
    /// Swaps the top 2 values of the stack.
    /// # Panics
    /// Panics if the stack had less than 2 elements.
    Swap,
    /// Exactly equals to Pop + Push(Value).
    /// # Panics
    /// Panics it the stack was empty.
    Const(Value),
    /// Loads the current value of the slot, and pushes a copy of that to the stack.
    /// # Panics
    /// Panics if the slot didn't have a value.
    Load(ScopedSlot),
    /// Pops a value from the stack, and store the value to the slot.
    /// # Panics
    /// Panics if the stack was empty.
    Store(ScopedSlot),
    /// Pushes the closure (= program address) to the closure stack.
    PushClosure(ClosureAddress),
    /// Pops a closure (= program address) from the closure stack the closure, and stores to a closure slot.
    /// # Panics
    /// Panics if the closure stack was empty.
    StoreClosure(ScopedSlot),
    /// Pops three values, `value`, `key` and `obj` from the stack, set `obj[key] = value` and push the new obj.
    /// # Panics
    /// Panics if the stack had less than 3 elements.
    AppendObject,
    /// Pops a value from the stack, and append to the array stored in the slot.
    /// # Panics
    /// Panics if the stack was empty, the slot didn't have a value or the value in the slot was not an array.
    Append(ScopedSlot),

    /// Pops a value `value`, and another value `index` from the stack, and pushes `value[index]` to the stack.
    /// # Panics
    /// Panics if the stack had less than 2 elements.
    Index,
    /// Pops an element from the stack and use it as a `value`.
    /// Then pops an element from the stack if `end` was true. Then pops an element if `start` was true.
    /// Pushes the slice `value[start?:end?]` to the stack.
    /// # Panics
    /// Panics if the stack didn't have enough elements.
    Slice {
        start: bool,
        end: bool,
    },
    /// Pops a value and run forks with each "value" of it pushed to the stack.
    /// # Panics
    /// Panics if the stack was empty.
    Each,
    /// Pops an element `path` from the stack, another element `value` from the stack, and pushes an element
    /// at the `path` of `value`.
    /// # Panics
    /// Panics if the stack had less than 2 elements.
    Access,

    EnterPathTracking,
    ExitPathTracking,
    EnterNonPathTracking,
    ExitNonPathTracking,

    /// Pushes a fork that runs from `fork_pc` to the fork stack.
    Fork {
        fork_pc: Address,
    },
    /// Pushes a fork to the fork stack.
    /// When the fork starts, it checks if the current state is error state, and discards the fork and continues from the next fork if it wasn't.
    /// If it was the error state and has `catch_pc`, it pushes the error message to the stack, clear error state and run again from `catch_pc`.
    /// If it was the error state and `catch_pc` was [Option::None], it discards the error and continue from the next fork.
    ForkTryBegin {
        catch_pc: Option<Address>,
    },
    /// Pushes a mark that let fork-search procedure to ignore the next [Self::ForkTryBegin] fork.
    ForkTryEnd,
    /// Pushes a mark that let fork-search procedure to run from the fork if the current state is an error state,
    /// or otherwise ignore the fork.
    /// Note that [Self::ForkTryEnd] doesn't take this [Self::ForkAlt] into account.
    ForkAlt {
        fork_pc: Address,
    },
    /// Pushes a fork that catches break-type error for the same scope/slot, and swallow the error to continue from the next fork.
    ForkLabel(ScopedSlot),
    /// Search for the fork emitted by the corresponding [Self::ForkLabel].
    Break(ScopedSlot),
    /// Discard the current fork, and continues from the next fork.
    Backtrack,
    /// Change the current pc to the given address.
    Jump(Address),
    /// Pops a value from the stack, jumps to the address if it wasn't [crate::intrinsic::truthy()].
    /// # Panics
    /// Panics if the stack was empty.
    JumpUnless(Address),
    /// Lookup the closure stored in the closure slot, and invokes it with setting the next return address as the `return_address`.
    /// # Panics
    /// Panics if the call pc was already set, i.e. no [Self::NewFrame] was called after the previous [Self::CallClosure]/[Self::Call]/[Self::TailCall]/[Self::TailCallClosure].
    CallClosure(ScopedSlot),
    /// Calls function in the given address, and invokes it with setting the next address as the `return_address`.
    /// # Panics
    /// Panics if the call pc was already set, i.e. no [Self::NewFrame] was called after the previous Call bytecode family.
    Call(Address),
    /// Abandon the current frame, obtain the return address from there and set that as `return_address`, and calls the function as [Self::Call].
    /// # Panics
    /// Panics if the call pc was already set, i.e. no [Self::NewFrame] was called after the previous Call bytecode family.
    TailCall(Address),
    /// _Don't_ abandon the current frame, obtain the return address from the current frame and set that as `return_address`,
    /// and calls the function. A flag will be set on the next [Self::NewFrame] to indicate that the [Self::Ret] that pops the frame
    /// should also pop the next (i.e. the current as of the [Self::CallDoubleRet]) frame.
    CallChainRet(Address),
    /// This is to [Self::CallClosure] what [Self::TailCall] is to [Self::Call].
    /// # Panics
    /// Panics if the call pc was already set, i.e. no [Self::NewFrame] was called after the previous Call bytecode family.
    TailCallClosure(ScopedSlot),
    /// Creates a frame with the scope id, variable slots, closure slots, label slots, and the call pc specified in the previous Call bytecode family.
    /// # Panics
    /// Panics if this was not preceded by Call bytecode family
    NewFrame {
        id: ScopeId,
        variable_cnt: usize,
        closure_cnt: usize,
        label_cnt: usize,
    },
    /// Discards the current frame, and start from the return address.
    /// # Panics
    /// Panics if the frame stack was empty.
    Ret,
    /// Pops a value from the stack and output it.
    /// # Panics
    /// Panics if the stack was empty.
    Output,
    /// Pops a value from the stack
    /// If there's no more input, produce a [QueryExecutionError::NoMoreInputError] instead.
    Input,

    /// Pops a value `context` from the stack, invokes the function with the arg `context`, and pushes the resulting value to the stack.
    /// # Panics
    /// Panics if the stack was empty, or the invoked function panicked.
    Intrinsic0(NamedFn0),
    /// Pops a value `arg1` from the stack, pops another value `context` from the stack,
    /// and invokes the function with the arg `context, arg1`, and pushes the resulting value to the stack.
    /// # Panics
    /// Panics if the stack had less than 2 elements, or the invoked function panicked.
    Intrinsic1(NamedFn1),
    /// Pops a value `arg2` from the stack, pops another value `arg1` from the stack, and another value `context` from the stack,
    /// and invokes the function with the arg `context, arg1, arg2`, and pushes the resulting value to the stack.
    /// # Panics
    /// Panics if the stack had less than 3 elements, or the invoked function panicked.
    Intrinsic2(NamedFn2),
    /// Pops a value `arg3` from the stack, pops another value `arg2` from the stack, pops another value `arg1` from the stack,
    /// and another value `context` from the stack, and invokes the function with the arg `context, arg1, arg2, arg3`,
    /// and pushes the resulting value to the stack.
    /// # Panics
    /// Panics if the stack had less than 4 elements, or the invoked function panicked.
    Intrinsic3(NamedFn3),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub(crate) code: Vec<ByteCode>,
    pub(crate) entry_point: Address,
}

impl Program {
    pub(crate) fn fetch_code(&self, pc: Address) -> Option<&ByteCode> {
        self.code.get(pc.0)
    }
}
