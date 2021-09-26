use crate::Value;

mod binary;
mod comparator;
mod index;
mod unary;

pub(crate) use self::{
    binary::binary,
    comparator::comparator,
    index::{index, slice},
    unary::unary,
};
use crate::vm::{
    bytecode::NamedFn1,
    compiler::{ArgType, FunctionIdentifier},
    ByteCode, QueryExecutionError,
};
use phf::phf_map;

static INTRINSICS1: phf::Map<&'static str, (NamedFn1, ArgType)> = phf_map! {
    "error" => (NamedFn1 { name: "error", func: error }, ArgType::Value),
};

pub(crate) fn lookup_intrinsic_fn(func: &FunctionIdentifier) -> Option<(ByteCode, Vec<ArgType>)> {
    if func.1 == 1 {
        INTRINSICS1
            .get(&func.0 .0)
            .cloned()
            .map(|(f, t)| (ByteCode::Intrinsic1(f), vec![t]))
    } else {
        None
    }
}

pub(crate) fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::False)
}

pub(crate) fn stringify(value: Value) -> Result<Value, QueryExecutionError> {
    Ok(Value::string(serde_json::to_string(&value).unwrap()))
}

pub(crate) fn error(value: Value) -> Result<Value, QueryExecutionError> {
    match value {
        Value::String(s) => Err(QueryExecutionError::UserDefinedError((*s).clone())),
        value => Err(QueryExecutionError::UserDefinedError(format!(
            "{:?}",
            value
        ))),
    }
}
