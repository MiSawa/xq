use std::rc::Rc;

use itertools::Itertools;
use num::{Float, ToPrimitive};
use phf::phf_map;

pub(crate) use self::{
    binary::binary,
    comparator::comparator,
    index::{index, slice},
    path::{del_paths, get_path, set_path},
    string::{stringifier, text},
    unary::unary,
};
use crate::{
    compile::compiler::{ArgType, FunctionIdentifier},
    util::make_owned,
    vm::{
        bytecode::{NamedFn0, NamedFn1, NamedFn2, NamedFn3},
        error::Result,
        ByteCode, QueryExecutionError,
    },
    Array, Value,
};

mod binary;
mod comparator;
mod index;
#[macro_use]
mod math;
mod path;
mod regex;
mod string;
mod time;
mod unary;

static INTRINSICS0: phf::Map<&'static str, NamedFn0> = phf_map! {
    "error" => NamedFn0 { name: "error", func: error },
    "type" => NamedFn0 { name: "type", func: get_type },
    "length" => NamedFn0 { name: "length", func: length },
    "utf8bytelength" => NamedFn0 { name: "utf8bytelength", func: utf8_byte_length },
    "keys_unsorted" =>  NamedFn0 { name: "keys_unsorted", func: keys_unsorted },
    "keys" =>  NamedFn0 { name: "keys", func: keys },
    "sort" => NamedFn0 { name: "sort", func: sort },
    "reverse" => NamedFn0 { name: "reverse", func: reverse },
    "tostring" => NamedFn0 { name: "tostring", func: text },
    "tonumber" => NamedFn0 { name: "to_number", func: string::to_number },
    "fromjson" => NamedFn0 { name: "fromjson", func: string::from_json },
    "tojson" => NamedFn0 { name: "tojson", func: string::to_json },
    "explode" => NamedFn0 { name: "explode", func: string::explode },
    "implode" => NamedFn0 { name: "implode", func: string::implode },

    "gmtime" => NamedFn0 { name: "gmtime", func: time::gm_time },
    "localtime" => NamedFn0 { name: "localtime", func: time::gm_time_local },
    "mktime" => NamedFn0 { name: "mktime", func: time::mk_time },
    "now" => NamedFn0 { name: "now", func: time::now },
    "fromdateiso8601" => NamedFn0 { name: "fromdateiso8601", func: time::fromdateiso8601 },

    "nan" => NamedFn0 { name: "nan", func: math::nan },
    "infinite" => NamedFn0 { name: "infinite", func: math::infinite },
    "isnan" => as_math_fn!(is_nan),
    "isnormal" => as_math_fn!(is_normal),
    "isinfinite" => as_math_fn!(is_infinite),
    "floor" => as_math_fn!(floor),
    "round" => as_math_fn!(round),
    "ceil" => as_math_fn!(ceil),
    "trunc" => as_math_fn!(trunc),
    "fabs" => as_math_fn!(abs),
    "sqrt" => as_math_fn!(sqrt),
    "cbrt" => as_math_fn!(cbrt),
    "sin" => as_math_fn!(sin),
    "cos" => as_math_fn!(cos),
    "tan" => as_math_fn!(tan),
    "asin" => as_math_fn!(asin),
    "acos" => as_math_fn!(acos),
    "atan" => as_math_fn!(atan),
    "sinh" => as_math_fn!(sinh),
    "cosh" => as_math_fn!(cosh),
    "tanh" => as_math_fn!(tanh),
    "asinh" => as_math_fn!(asinh),
    "acosh" => as_math_fn!(acosh),
    "atanh" => as_math_fn!(atanh),
    "exp" => as_math_fn!(exp),
    "exp2" => as_math_fn!(exp2),
    "exp10" => as_math_fn!(exp10),
    "expm1" => as_math_fn!(exp_m1),
    "log" => as_math_fn!(ln),
    "log2" => as_math_fn!(log2),
    "log10" => as_math_fn!(log10),
};
static INTRINSICS1: phf::Map<&'static str, NamedFn1> = phf_map! {
    "error" => NamedFn1 { name: "error", func: error1 },
    "has" => NamedFn1 { name: "has", func: has },
    "in" => NamedFn1 { name: "in", func: |i, c| has(c, i) },
    "contains" => NamedFn1 { name: "contains", func: contains },
    "inside" => NamedFn1 { name: "inside", func: |i, c| contains(c, i) },
    "indices" => NamedFn1 { name: "indices", func: indices },
    "startswith" => NamedFn1 { name: "startswith", func: starts_with },
    "endswith" => NamedFn1 { name: "endswith", func: ends_with },
    "split" => NamedFn1 { name: "split", func: split1 },
    "_min_by" => NamedFn1 { name: "_min_by", func: min_by },
    "_max_by" => NamedFn1 { name: "_max_by", func: max_by },
    "_group_by" => NamedFn1 { name: "_group_by", func: group_by },
    "_unique_by" => NamedFn1 { name: "_unique_by", func: unique_by },
    "delpaths" => NamedFn1 { name: "delpaths", func: path::del_paths },
    "bsearch" => NamedFn1 { name: "bsearch", func: binary_search },
    "format" => NamedFn1 { name: "format", func: format },

    "strftime" => NamedFn1 { name: "strftime", func: time::format_time },
    "strptime" => NamedFn1 { name: "strptime", func: time::parse_time },
    "strflocaltime" => NamedFn1 { name: "strflocaltime", func: time::format_time_local },
};
static INTRINSICS2: phf::Map<&'static str, NamedFn2> = phf_map! {
    "setpath" => NamedFn2 { name: "setpath", func: path::set_path },
    "__split_match_impl" => NamedFn2 { name: "__split_match_impl", func: regex::split_match_impl },

    "fmax" => as_math_fn2!(fmax),
    "fmin" => as_math_fn2!(fmin),
    "copysign" => as_math_fn2!(copysign),
    "atan2" => as_math_fn2!(atan2),
    "hypot" => as_math_fn2!(hypot),
    "pow" => as_math_fn2!(powf),
};
static INTRINSICS3: phf::Map<&'static str, NamedFn3> = phf_map! {
    "fma" => as_math_fn3!(fma),
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
            .map(|f| (ByteCode::Intrinsic1(f), vec![ArgType::Value]))
    } else if *n_args == 2 {
        INTRINSICS2.get(&ident.0).cloned().map(|f| {
            (
                ByteCode::Intrinsic2(f),
                vec![ArgType::Value, ArgType::Value],
            )
        })
    } else if *n_args == 3 {
        INTRINSICS3.get(&ident.0).cloned().map(|f| {
            (
                ByteCode::Intrinsic3(f),
                vec![ArgType::Value, ArgType::Value, ArgType::Value],
            )
        })
    } else {
        None
    }
}

pub(crate) fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::Boolean(false))
}

fn error(value: Value) -> Result<Value> {
    Err(QueryExecutionError::UserDefinedError(value))
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
        Value::Number(n) => return Ok(n.abs().into()),
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

fn keys_unsorted(context: Value) -> Result<Value> {
    match context {
        Value::Array(arr) => Ok((0..arr.len()).map(Value::number).collect::<Array>().into()),
        Value::Object(obj) => Ok(obj
            .keys()
            .map(|k| k.to_string().into())
            .collect::<Array>()
            .into()),
        _ => Err(QueryExecutionError::InvalidArgType(
            "keys_unsorted",
            context,
        )),
    }
}

fn keys(context: Value) -> Result<Value> {
    match context {
        Value::Array(arr) => Ok((0..arr.len()).map(Value::number).collect::<Array>().into()),
        Value::Object(obj) => Ok(obj
            .keys()
            .sorted_unstable()
            .map(|k| k.to_string().into())
            .collect::<Array>()
            .into()),
        _ => Err(QueryExecutionError::InvalidArgType("keys", context)),
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

fn contains(context: Value, element: Value) -> Result<Value> {
    fn contains_rec(lhs: &Value, rhs: &Value) -> Result<bool> {
        Ok(match (lhs, rhs) {
            (Value::Null, Value::Null) => true,
            (Value::Boolean(lhs), Value::Boolean(rhs)) if lhs == rhs => true,
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs, // TODO: nan handling in JQ semantics...
            (Value::String(lhs), Value::String(rhs)) => lhs.contains(rhs.as_ref()),
            (Value::Array(lhs), Value::Array(rhs)) => rhs
                .iter()
                .all(|r| lhs.iter().any(|l| contains_rec(l, r).unwrap_or(false))),
            (Value::Object(lhs), Value::Object(rhs)) => rhs.iter().all(|(k, r)| {
                lhs.get(k)
                    .map(|l| contains_rec(l, r).unwrap_or(false))
                    .unwrap_or(false)
            }),
            (_, _) => {
                return Err(QueryExecutionError::InvalidArgType(
                    "contains",
                    Array::from_vec(vec![lhs.clone(), rhs.clone()]).into(),
                ))
            }
        })
    }
    contains_rec(&context, &element).map(Into::into)
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

fn reverse(context: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("reverse", context)),
    };
    let mut arr = arr.into_iter().collect_vec();
    arr.reverse();
    Ok(Array::from_vec(arr).into())
}

pub(crate) fn min_by(context: Value, keys: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("min_by", context)),
    };
    let keys = match keys {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("min_by", keys)),
    };
    if arr.len() != keys.len() {
        return Err(QueryExecutionError::LengthMismatch("min_by"));
    }
    Ok(keys
        .into_iter()
        .zip(arr)
        .min_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs))
        .map_or(Value::Null, |(_, v)| v))
}

pub(crate) fn max_by(context: Value, keys: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("max_by", context)),
    };
    let keys = match keys {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("max_by", keys)),
    };
    if arr.len() != keys.len() {
        return Err(QueryExecutionError::LengthMismatch("max_by"));
    }
    Ok(keys
        .into_iter()
        .zip(arr)
        .max_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs))
        .map_or(Value::Null, |(_, v)| v))
}

pub(crate) fn group_by(context: Value, keys: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("group_by", context)),
    };
    let keys = match keys {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("group_by", keys)),
    };
    if arr.len() != keys.len() {
        return Err(QueryExecutionError::LengthMismatch("group_by"));
    }
    let mut arr: Vec<(Value, Value)> = keys.into_iter().zip(arr).collect();
    arr.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
    let arr = arr
        .into_iter()
        .fold::<(Option<Value>, Vec<Vec<Value>>), _>(
            (None, Vec::new()),
            |(prev_key, mut groups), (key, value)| {
                if prev_key.is_some_and(|prev_key| prev_key == key) {
                    groups.last_mut().expect("Shouldn't be empty").push(value);
                } else {
                    groups.push(vec![value]);
                }
                (Some(key), groups)
            },
        )
        .1
        .into_iter()
        .map(Array::from_vec)
        .map(Into::into)
        .collect_vec();
    Ok(Array::from_vec(arr).into())
}

pub(crate) fn unique_by(context: Value, keys: Value) -> Result<Value> {
    let arr = match context {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("unique_by", context)),
    };
    let keys = match keys {
        Value::Array(arr) => make_owned(arr),
        _ => return Err(QueryExecutionError::InvalidArgType("unique_by", keys)),
    };
    if arr.len() != keys.len() {
        return Err(QueryExecutionError::LengthMismatch("unique_by"));
    }
    let mut arr: Vec<(Value, Value)> = keys.into_iter().zip(arr).collect();
    arr.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
    let arr = arr
        .into_iter()
        .fold::<(Option<Value>, Vec<Value>), _>(
            (None, Vec::new()),
            |(prev_key, mut values), (key, value)| {
                if prev_key.is_none_or(|prev_key| prev_key != key) {
                    values.push(value);
                }
                (Some(key), values)
            },
        )
        .1
        .into_iter()
        .collect_vec();
    Ok(Array::from_vec(arr).into())
}

fn indices(context: Value, s: Value) -> Result<Value> {
    let ret = match (context, s) {
        (Value::Null, _) => Value::Null,
        (Value::String(lhs), Value::String(rhs)) => Array::from_vec(if rhs.is_empty() {
            vec![]
        } else {
            lhs.match_indices(rhs.as_ref())
                .map(|(pos, _)| Value::Number(pos.into()))
                .collect()
        })
        .into(),
        (Value::Array(lhs), Value::Array(rhs)) => Array::from_vec(if rhs.is_empty() {
            vec![]
        } else {
            lhs.windows(rhs.len())
                .enumerate()
                .filter(|(_, lhs)| lhs.iter().eq(rhs.iter())) // TODO: NaN
                .map(|(pos, _)| Value::Number(pos.into()))
                .collect()
        })
        .into(),
        (Value::Array(lhs), rhs) => Array::from_vec(
            lhs.iter()
                .enumerate()
                .filter(|(_, v)| *v == &rhs) // TODO: NaN
                .map(|(pos, _)| Value::Number(pos.into()))
                .collect(),
        )
        .into(),
        (context, _) => return Err(QueryExecutionError::InvalidArgType("indices", context)),
    };
    Ok(ret)
}

fn starts_with(context: Value, s: Value) -> Result<Value> {
    match (context, s) {
        (Value::String(lhs), Value::String(rhs)) => Ok(lhs.starts_with(rhs.as_ref()).into()),
        (context, _) => Err(QueryExecutionError::InvalidArgType("startswith", context)),
    }
}

fn ends_with(context: Value, s: Value) -> Result<Value> {
    match (context, s) {
        (Value::String(lhs), Value::String(rhs)) => Ok(lhs.ends_with(rhs.as_ref()).into()),
        (context, _) => Err(QueryExecutionError::InvalidArgType("endswith", context)),
    }
}

fn split1(context: Value, s: Value) -> Result<Value> {
    match (context, s) {
        (Value::String(lhs), Value::String(rhs)) => Ok(string::split(lhs.as_ref(), rhs.as_ref())),
        (context, _) => Err(QueryExecutionError::InvalidArgType("split", context)),
    }
}

fn binary_search(context: Value, x: Value) -> Result<Value> {
    match context {
        Value::Array(arr) => Ok(match arr.binary_search(&x) {
            Ok(i) => Value::number(i),
            Err(i) => Value::number(-1 - (i as isize)),
        }),
        _ => Err(QueryExecutionError::InvalidArgType("bsearch", context)),
    }
}

fn format(context: Value, s: Value) -> Result<Value> {
    match (context, s) {
        (lhs, Value::String(fmt)) => (stringifier(&fmt)
            .ok_or(QueryExecutionError::UnknownStringFormatter(fmt))?
            .func)(lhs),
        (_, s) => Err(QueryExecutionError::InvalidArgType("format", s)),
    }
}
