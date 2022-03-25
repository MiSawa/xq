use std::rc::Rc;

use thiserror::Error;

use crate::{value::RcString, vm::machine::LabelId, Number, Value};

pub type Result<T, E = QueryExecutionError> = std::result::Result<T, E>;

#[derive(Debug, Error)]
#[error(transparent)]
pub struct InputError(#[from] Box<dyn std::error::Error + Send + Sync>);
impl InputError {
    pub fn new<E: 'static + std::error::Error + Send + Sync>(e: E) -> Self {
        Self(Box::new(e))
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
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
    Breaking(LabelId),
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
    #[error("Invalid as a unicode scalar value `{0:}`")]
    InvalidNumberAsChar(Number),
    #[error("utf8bytelength is applied to non-string value`{0:?}`")]
    InvalidUTF8ByteLength(Value),
    #[error("{0:?} was called invalidly with arg `{1:?}`")]
    InvalidArgType(&'static str, Value),
    #[error("`{0:?}` can't be parsed as a number")]
    InvalidStringToNumber(RcString),
    #[error("Given string was invalid as a json `{0:}`")]
    InvalidJson(RcString),
    #[error("Unknown string formatter `{0:?}`")]
    UnknownStringFormatter(RcString),
    #[error("Unable to parse date time")]
    DateTimeParseError(RcString),
    #[error("Unable to format date time")]
    DateTimeFormatError(RcString),
    #[error("Invalid date time")]
    InvalidDateTime(#[from] time::error::ComponentRange),
    #[error("Unable to determine local date time offset")]
    IndeterminateOffset(#[from] time::error::IndeterminateOffset),
    #[error("Unable to determine local time zone")]
    TimeZoneLookupFailure(RcString),
    #[error("Failed to compile regex: {0:?}")]
    OnigurumaCompileError(#[from] onig::Error),
    #[error("Invalid regex flag: {0}")]
    InvalidRegexFlag(char),
    #[error("Input source gave an error")]
    InputError(#[from] InputError),
    #[error("There's no more input")]
    NoMoreInputError,
    #[error("{0:?}")]
    UserDefinedError(Value),
}

impl From<time_fmt::parse::ParseError> for QueryExecutionError {
    fn from(e: time_fmt::parse::ParseError) -> Self {
        Self::DateTimeParseError(Rc::new(format!("{}", e)))
    }
}

impl From<time_fmt::format::FormatError> for QueryExecutionError {
    fn from(e: time_fmt::format::FormatError) -> Self {
        Self::DateTimeParseError(Rc::new(format!("{}", e)))
    }
}

impl From<time::error::Parse> for QueryExecutionError {
    fn from(e: time::error::Parse) -> Self {
        Self::DateTimeParseError(Rc::new(format!("{}", e)))
    }
}

impl From<time_tz::system::Error> for QueryExecutionError {
    fn from(e: time_tz::system::Error) -> Self {
        Self::TimeZoneLookupFailure(Rc::new(format!("{}", e)))
    }
}
