use crate::{vm::bytecode::Label, Number, Value};
use thiserror::Error;

pub type Result<T, E = QueryExecutionError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum QueryExecutionError {
    #[error("Object was indexed by non-string value `{0:?}`")]
    ObjectIndexByNonString(Value),
    #[error("Object was indexed by non-integer value `{0:?}`")]
    ArrayIndexByNonInt(Value),
    #[error("Slice on non-array `{0:?}`")]
    SliceByNonInt(Value),
    #[error("Cannot iterate over non-iterable value `{0:?}`")]
    IterateOnNonIterable(Value),
    #[error("Cannot index on non-indexable value `{0:?}`")]
    IndexOnNonIndexable(Value),
    #[error("Slice on not an array nor a string `{0:?}`")]
    SliceOnNonArrayNorString(Value),
    #[error("Expected an integer but got a non-integral value `{0:?}`")]
    NonIntegralNumber(Number),
    #[error("Expected a number convertible to isize")]
    NonIndexableNumber(Number),
    #[error("Unary {0:?} negation was applied to non-numeric value `{1:?}`")]
    UnaryOnNonNumeric(&'static str, Value),
    #[error("Cannot {0:?} `{1:?}` and `{2:?}`")]
    IncompatibleBinaryOperator(&'static str, Value, Value),
    #[error("Cannot repeat string `{0:?}` times")]
    StringRepeatByNonUSize(Number),
    #[error("Cannot divide/modulo by zero")]
    DivModByZero,
    #[error("Tried to construct an object with non-string key `{0:?}`")]
    ObjectNonStringKey(Value),
    #[error("Invalid path for `{0:?}`")]
    InvalidPathError(Value),
    #[error("Breaking on label `{0:?}`")]
    Breaking(Label),
    #[error("Path should be an array but was `{0:?}`")]
    PathNotArray(Value),
    #[error("Invalid index `{0:?}`")]
    InvalidIndex(Value),
    #[error("Invalid indexing for value `{0:?}` and index `{0:?}`")]
    InvalidIndexing(Value, Value),
    #[error("Expected an array but was `{0:?}`")]
    ExpectedAnArray(Value),
    #[error("Expected slicing but got an invalid one `{0:?}`")]
    InvalidSlicing(Value),
    #[error("At least one of range start or end has to be specified")]
    UnboundedRange,
    #[error("Invalid as base64")]
    InvalidAsBase64(#[from] base64::DecodeError),
    #[error("Invalid as a UTF-8-encoded byte array")]
    InvalidUTF8Bytes(#[from] std::string::FromUtf8Error),
    #[error("Invalid as a (c|t)sv entry: `{0:?}`")]
    InvalidAsXSVEntry(Value),
    #[error("{0:?}")]
    UserDefinedError(String),
}
