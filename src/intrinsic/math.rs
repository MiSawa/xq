use num::Float;

use crate::{vm::Result, Number, Value};

macro_rules! as_math_fn {
    ($name: ident) => {
        crate::vm::bytecode::NamedFn0 {
            name: stringify!($name),
            func: |v: Value| match v {
                crate::Value::Number(v) => crate::intrinsic::math::$name(v).map(Into::into),
                _ => crate::vm::Result::Err(crate::vm::QueryExecutionError::InvalidArgType(
                    stringify!($name),
                    v,
                )),
            },
        }
    };
}

pub(crate) fn nan(_: Value) -> Result<Value> {
    Ok(Number::nan().into())
}

pub(crate) fn infinite(_: Value) -> Result<Value> {
    Ok(Number::infinity().into())
}

pub(crate) fn is_nan(v: Number) -> Result<bool> {
    Ok(v.is_nan())
}

pub(crate) fn is_normal(v: Number) -> Result<bool> {
    Ok(v.is_normal())
}

pub(crate) fn is_infinite(v: Number) -> Result<bool> {
    Ok(v.is_infinite())
}

macro_rules! pub_math_fn {
    ($($name: ident),*) => {
        $(
            pub(crate) fn $name(v: Number) -> Result<Number> {
                Ok(v.$name())
            }
        )*
    };
}

pub_math_fn!(floor, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh);
