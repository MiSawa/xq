use crate::{
    compile::compiler::{ArgType, FunctionIdentifier},
    util::make_owned,
    vm::{
        bytecode::{NamedFn0, NamedFn1, NamedFn2},
        error::Result,
        ByteCode, QueryExecutionError,
    },
    Array, Value,
};
use itertools::Itertools;
use num::ToPrimitive;
use phf::phf_map;
use std::rc::Rc;

pub(crate) use self::{
    binary::binary,
    comparator::comparator,
    index::{index, slice},
    path::{del_paths, get_path, set_path},
    string::{stringifier, text},
    unary::unary,
};

mod binary;
mod comparator;
mod index;
#[macro_use]
mod math;
mod path;
mod string;
mod unary;

static INTRINSICS0: phf::Map<&'static str, NamedFn0> = phf_map! {
    "error" => NamedFn0 { name: "error", func: error },
    "type" => NamedFn0 { name: "type", func: get_type },
    "length" => NamedFn0 { name: "length", func: length },
    "utf8bytelength" => NamedFn0 { name: "utf8bytelength", func: utf8_byte_length },
    "sort" => NamedFn0 { name: "sort", func: sort },
    "tostring" => NamedFn0 { name: "tostring", func: text },
    "tonumber" => NamedFn0 { name: "to_number", func: string::to_number },
    "nan" => NamedFn0 { name: "nan", func: math::nan },
    "infinite" => NamedFn0 { name: "infinite", func: math::infinite },
    "floor" => as_math_fn!(floor),
    "sqrt" => as_math_fn!(sqrt),
    "isnan" => as_math_fn!(is_nan),
    "isfinite" => as_math_fn!(is_finite),
    "isnormal" => as_math_fn!(is_normal),
};
static INTRINSICS1: phf::Map<&'static str, (NamedFn1, ArgType)> = phf_map! {
    "error" => (NamedFn1 { name: "error", func: error1 }, ArgType::Value),
    "has" => (NamedFn1 { name: "has", func: has }, ArgType::Value),
    "in" => (NamedFn1 { name: "in", func: |c, i| has(i, c) }, ArgType::Value),
    "delpaths" => (NamedFn1 { name: "delpaths", func: path::del_paths }, ArgType::Value),
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
    !matches!(value, Value::Null | Value::Boolean(false))
}

fn error(value: Value) -> Result<Value> {
    match value {
        Value::String(s) => Err(QueryExecutionError::UserDefinedError((*s).clone())),
        value => Err(QueryExecutionError::UserDefinedError(format!(
            "{:?}",
            value
        ))),
    }
}

fn error1(_: Value, arg: Value) -> Result<Value> {
    error(arg)
}

fn get_type(context: Value) -> Result<Value> {
    let ret = match context {
        Value::Null => "null",
        Value::Boolean(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    };
    Ok(Value::String(Rc::new(ret.to_string())))
}

fn length(context: Value) -> Result<Value> {
    Ok(Value::number(match context {
        Value::Null => 0,
        Value::Boolean(_) => return Err(QueryExecutionError::InvalidArgType("length", context)),
        Value::Number(_) => return Ok(context),
        Value::String(s) => s.chars().count(),
        Value::Array(a) => a.len(),
        Value::Object(o) => o.len(),
    }))
}

fn utf8_byte_length(context: Value) -> Result<Value> {
    match context {
        Value::String(s) => Ok(Value::number(s.len())),
        _ => Err(QueryExecutionError::InvalidUTF8ByteLength(context)),
    }
}

fn has(context: Value, index: Value) -> Result<Value> {
    Ok(match (context, index) {
        (Value::Array(arr), Value::Number(n)) => {
            if let Some(n) = n.to_usize() {
                n < arr.len()
            } else {
                false
            }
        }
        (Value::Object(obj), Value::String(key)) => obj.get(&*key).is_some(),
        _ => false,
    }
    .into())
}

fn sort(context: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("sort", context)),
    };
    let mut arr = arr.into_iter().collect_vec();
    arr.sort();
    Ok(Array::from_vec(arr).into())
}

/// Receives `[ [ group key, original value ] ]`.
pub(crate) fn group_by(context: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("group_by", context)),
    };
    let mut arr: Vec<(Value, Value)> = arr
        .into_iter()
        .map(|v| match v {
            Value::Array(v) => {
                let mut v = make_owned(v).to_vec();
                assert_eq!(v.len(), 2);
                let original = v.pop().unwrap();
                let group = v.pop().unwrap();
                Ok((group, original))
            }
            _ => Err(QueryExecutionError::InvalidArgType("group_by", v)),
        })
        .collect::<Result<Vec<_>>>()?;
    arr.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
    let arr = arr
        .into_iter()
        .fold::<(Option<Value>, Vec<Vec<Value>>), _>(
            (None, Vec::new()),
            |(current_group, mut groups), (grp, value)| {
                if let Some(current_group) = current_group {
                    if current_group == grp {
                        groups.last_mut().expect("Shouldn't be empty").push(value);
                        return (Some(current_group), groups);
                    }
                }
                groups.push(vec![value]);
                (Some(grp), groups)
            },
        )
        .1
        .into_iter()
        .map(Array::from_vec)
        .map(Into::into)
        .collect_vec();
    Ok(Array::from_vec(arr).into())
}
