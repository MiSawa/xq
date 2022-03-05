use crate::{
    util::make_owned,
    vm::{bytecode::NamedFn0, QueryExecutionError, Result},
    Array, Number, Value,
};
use itertools::Itertools;
use num::Float;
use num::ToPrimitive;
use std::{borrow::Cow, rc::Rc};

pub(crate) fn to_number(value: Value) -> Result<Value> {
    match value {
        Value::Number(_) => Ok(value),
        Value::String(s) => match s.parse() {
            Ok(n) => Ok(Value::Number(n)),
            Err(_) => Err(QueryExecutionError::InvalidStringToNumber(s)),
        },
        _ => Err(QueryExecutionError::InvalidArgType("to_number", value)),
    }
}

pub(crate) fn stringifier(fmt: &String) -> Option<NamedFn0> {
    match fmt.as_str() {
        "text" => NamedFn0 {
            name: "text",
            func: text,
        },
        "json" => NamedFn0 {
            name: "json",
            func: to_json,
        },
        "html" => NamedFn0 {
            name: "html",
            func: html,
        },
        "uri" => NamedFn0 {
            name: "uri",
            func: uri,
        },
        "csv" => NamedFn0 {
            name: "csv",
            func: csv,
        },
        "tsv" => NamedFn0 {
            name: "tsv",
            func: tsv,
        },
        "sh" => NamedFn0 {
            name: "sh",
            func: sh,
        },
        "base64" => NamedFn0 {
            name: "base64",
            func: base64,
        },
        "base64d" => NamedFn0 {
            name: "base64d",
            func: base64d,
        },
        _ => return None,
    }
    .into()
}

pub(crate) fn explode(value: Value) -> Result<Value> {
    match value {
        Value::String(s) => Ok(Array::from_vec(
            s.chars()
                .map(|c| Number::from(c as u32).into())
                .collect_vec(),
        )
        .into()),
        _ => Err(QueryExecutionError::InvalidArgType("explode", value)),
    }
}

pub(crate) fn implode(value: Value) -> Result<Value> {
    match value {
        Value::Array(s) => {
            let s = s
                .iter()
                .map(|c| match c {
                    Value::Number(c) => {
                        let c = c
                            .to_u32()
                            .ok_or(QueryExecutionError::InvalidNumberAsChar(*c))?;
                        let c = char::try_from(c)
                            .map_err(|_| QueryExecutionError::InvalidNumberAsChar(c.into()))?;
                        Ok(c)
                    }
                    _ => Err(QueryExecutionError::InvalidArgType("implode", c.clone())),
                })
                .collect::<Result<String>>()?;
            Ok(Value::String(Rc::new(s)))
        }
        _ => Err(QueryExecutionError::InvalidArgType("implode", value)),
    }
}

pub(crate) fn split(lhs: &String, rhs: &String) -> Value {
    if rhs.is_empty() {
        lhs.chars()
            .map(|c| c.to_string().into())
            .collect::<Array>()
            .into()
    } else {
        lhs.split(rhs)
            .into_iter()
            .map(|s| s.to_string().into())
            .collect::<Array>()
            .into()
    }
}

fn stringify_inner(value: Value) -> Rc<String> {
    match value {
        Value::String(s) => s,
        _ => serde_json::to_string(&value)
            .expect("Unable to encode a value to json")
            .into(),
    }
}

pub(crate) fn text(value: Value) -> Result<Value> {
    Ok(stringify_inner(value).into())
}

pub(crate) fn from_json(value: Value) -> Result<Value> {
    match value {
        Value::String(s) => {
            serde_json::from_str(&*s).map_err(|_| QueryExecutionError::InvalidJson(s.clone()))
        }
        _ => Err(QueryExecutionError::InvalidArgType("fromjson", value)),
    }
}

pub(crate) fn to_json(value: Value) -> Result<Value> {
    Ok(serde_json::to_string(&value)
        .expect("Unable to encode a value to json")
        .into())
}

fn html(value: Value) -> Result<Value> {
    let s = stringify_inner(value);
    let mut ret = String::new();
    html_escape::encode_quoted_attribute_to_string(&*s, &mut ret);
    Ok(ret.into())
}

fn uri(value: Value) -> Result<Value> {
    let s = stringify_inner(value);
    let ret = urlencoding::encode(&s);
    Ok(ret.to_string().into())
}

fn xsv<F>(value: Value, delim: char, add_string: F) -> Result<Value>
where
    F: Fn(&mut String, &str),
{
    let entries = match value {
        Value::Array(arr) => arr,
        value => return Err(QueryExecutionError::ExpectedAnArray(value)),
    };
    let mut ret = String::new();
    for entry in entries.iter() {
        match entry {
            Value::Null => {}
            Value::Boolean(v) => ret.push_str(if *v { "true" } else { "false" }),
            Value::Number(v) => {
                if !v.is_nan() {
                    ret.push_str(&format!("{}", v))
                }
            }
            Value::String(s) => add_string(&mut ret, s),
            v => return Err(QueryExecutionError::InvalidArgType("(c|t)sv", v.clone())),
        }
        ret.push(delim);
    }
    if !ret.is_empty() {
        ret.pop();
    }
    Ok(ret.into())
}

fn csv(value: Value) -> Result<Value> {
    fn append_quoted_string(s: &mut String, v: &str) {
        s.push('"');
        for c in v.chars() {
            if c == '"' {
                s.push_str(r#""""#);
            } else {
                s.push(c);
            }
        }
        s.push('"');
    }
    xsv(value, ',', append_quoted_string)
}

fn tsv(value: Value) -> Result<Value> {
    fn append_quoted_string(s: &mut String, v: &str) {
        for c in v.chars() {
            if c == '\n' {
                s.push_str(r#"\n"#);
            } else if c == '\r' {
                s.push_str(r#"\r"#);
            } else if c == '\t' {
                s.push_str(r#"\t"#);
            } else if c == '\\' {
                s.push_str(r#"\\"#);
            } else {
                s.push(c);
            }
        }
    }
    xsv(value, '\t', append_quoted_string)
}

fn sh(value: Value) -> Result<Value> {
    let escape = |value: Value| {
        shell_escape::escape(Cow::from(make_owned(stringify_inner(value)))).to_string()
    };
    let ret = match value {
        Value::Array(arr) => arr.iter().map(|value| escape(value.clone())).join(" "),
        _ => escape(value),
    };
    Ok(ret.into())
}

fn base64(value: Value) -> Result<Value> {
    let s = stringify_inner(value);
    let ret = base64::encode(&*s);
    Ok(ret.into())
}

fn base64d(value: Value) -> Result<Value> {
    let s = stringify_inner(value);
    let ret = base64::decode(&*s)?;
    let ret = String::from_utf8(ret)?;
    Ok(ret.into())
}
