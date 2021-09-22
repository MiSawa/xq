#[allow(dead_code)]
pub mod bytecode;
#[allow(dead_code)]
pub mod compiler;
#[allow(dead_code)]
pub mod error;
#[allow(dead_code)]
mod intrinsic;
pub mod machine;
pub mod value;

pub(crate) use bytecode::{ByteCode, Program};
pub use {error::QueryExecutionError, error::Result, value::Value};

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct Address(usize);

impl Address {
    fn next(&mut self) {
        self.0 -= 1;
    }
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopeId(usize);

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopedSlot(ScopeId, usize);
