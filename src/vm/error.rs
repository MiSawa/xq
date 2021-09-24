use crate::vm::Value;
use thiserror::Error;

pub type Result<T, E = QueryExecutionError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum QueryExecutionError {
    #[error("Object was indexed by non-string value `{0:?}`")]
    ObjectIndexByNonString(Value),
    #[error("Object was indexed by non-integer value `{0:?}`")]
    ArrayIndexByNonInt(Value),
    #[error("Unary {0:?} negation was applied to non-numeric value `{1:?}`")]
    UnaryOnNonNumeric(&'static str, Value),
    #[error("Cannot iterate over non-iterable value `{0:?}`")]
    IterateOnNonIterable(Value),
}
