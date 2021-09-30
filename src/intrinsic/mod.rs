use crate::Value;

mod binary;
mod comparator;
mod index;
mod path;
mod unary;

pub(crate) use self::{
    binary::binary,
    comparator::comparator,
    index::{index, slice},
    path::{del_path, set_path},
    unary::unary,
};
use crate::vm::{
    bytecode::{NamedFn0, NamedFn1, NamedFn2},
    compiler::{ArgType, FunctionIdentifier},
    ByteCode, QueryExecutionError,
};
use phf::phf_map;
use std::rc::Rc;

static INTRINSICS0: phf::Map<&'static str, NamedFn0> = phf_map! {
    "error" => NamedFn0 { name: "error", func: error },
    "type" => NamedFn0 { name: "type", func: get_type },
};
static INTRINSICS1: phf::Map<&'static str, (NamedFn1, ArgType)> = phf_map! {
    "error" => (NamedFn1 { name: "error", func: error1 }, ArgType::Value),
};
static INTRINSICS2: phf::Map<&'static str, (NamedFn2, ArgType, ArgType)> = phf_map! {
    "setpath" => (NamedFn2 { name: "setpath", func: path::set_path }, ArgType::Value, ArgType::Value),
};

pub(crate) fn lookup_intrinsic_fn(
    FunctionIdentifier(ident, n_args): &FunctionIdentifier,
) -> Option<(ByteCode, Vec<ArgType>)> {
    if *n_args == 0 {
        INTRINSICS0
            .get(&ident.0)
            .cloned()
            .map(|f| (ByteCode::Intrinsic0(f), vec![]))
    } else if *n_args == 1 {
        INTRINSICS1
            .get(&ident.0)
            .cloned()
            .map(|(f, t)| (ByteCode::Intrinsic1(f), vec![t]))
    } else if *n_args == 2 {
        INTRINSICS2
            .get(&ident.0)
            .cloned()
            .map(|(f, t1, t2)| (ByteCode::Intrinsic2(f), vec![t1, t2]))
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

fn error(value: Value) -> Result<Value, QueryExecutionError> {
    match value {
        Value::String(s) => Err(QueryExecutionError::UserDefinedError((*s).clone())),
        value => Err(QueryExecutionError::UserDefinedError(format!(
            "{:?}",
            value
        ))),
    }
}

fn error1(_: Value, arg: Value) -> Result<Value, QueryExecutionError> {
    error(arg)
}

fn get_type(context: Value) -> Result<Value, QueryExecutionError> {
    let ret = match context {
        Value::Null => "null",
        Value::True | Value::False => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    };
    Ok(Value::String(Rc::new(ret.to_string())))
}
