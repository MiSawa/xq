pub(crate) use bytecode::{ByteCode, Program};
pub use error::{QueryExecutionError, Result};

use crate::value::Value;

#[allow(dead_code)]
pub mod bytecode;
#[allow(dead_code)]
pub mod compiler;
#[allow(dead_code)]
pub mod error;
pub mod machine;

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct Address(usize);

impl Address {
    fn next(&mut self) {
        self.0 -= 1;
    }

    fn get_next(&self) -> Self {
        Self(self.0 - 1)
    }
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopeId(usize);

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopedSlot(ScopeId, usize);
