#[allow(dead_code)]
pub mod bytecode;
#[allow(dead_code)]
pub mod error;
pub mod machine;

pub(crate) use bytecode::{ByteCode, Program};
pub use error::{QueryExecutionError, Result};

use crate::Value;

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct Address(pub(crate) usize);

impl Address {
    fn next(&mut self) {
        self.0 -= 1;
    }

    pub(crate) fn get_next(&self) -> Self {
        Self(self.0 - 1)
    }
}

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopeId(pub(crate) usize);

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct ScopedSlot(pub(crate) ScopeId, pub(crate) usize);
