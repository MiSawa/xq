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

macro_rules! as_math_fn2 {
    ($name: ident) => {
        crate::vm::bytecode::NamedFn2 {
            name: stringify!($name),
            func: |_: Value, v: Value, w: Value| match (v, w) {
                (crate::Value::Number(v), crate::Value::Number(w)) => {
                    crate::intrinsic::math::$name(v, w).map(Into::into)
                }
                (v, crate::Value::Number(_)) => crate::vm::Result::Err(
                    crate::vm::QueryExecutionError::InvalidArgType(stringify!($name), v),
                ),
                (_, w) => crate::vm::Result::Err(crate::vm::QueryExecutionError::InvalidArgType(
                    stringify!($name),
                    w,
                )),
            },
        }
    };
}

macro_rules! as_math_fn3 {
    ($name: ident) => {
        crate::vm::bytecode::NamedFn3 {
            name: stringify!($name),
            func: |_: Value, v: Value, w: Value, x: Value| match (v, w, x) {
                (crate::Value::Number(v), crate::Value::Number(w), crate::Value::Number(x)) => {
                    crate::intrinsic::math::$name(v, w, x).map(Into::into)
                }
                (v, crate::Value::Number(_), crate::Value::Number(_)) => crate::vm::Result::Err(
                    crate::vm::QueryExecutionError::InvalidArgType(stringify!($name), v),
                ),
                (_, w, crate::Value::Number(_)) => crate::vm::Result::Err(
                    crate::vm::QueryExecutionError::InvalidArgType(stringify!($name), w),
                ),
                (_, _, x) => crate::vm::Result::Err(
                    crate::vm::QueryExecutionError::InvalidArgType(stringify!($name), x),
                ),
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

pub(crate) fn exp10(v: Number) -> Result<Number> {
    Ok(Number::from(10).powf(v))
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

pub_math_fn!(
    floor, round, ceil, trunc, abs, sqrt, cbrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh,
    asinh, acosh, atanh, exp, exp2, exp_m1, ln, log2, log10
);

pub(crate) fn fmax(v: Number, w: Number) -> Result<Number> {
    Ok(Float::max(v, w))
}

pub(crate) fn fmin(v: Number, w: Number) -> Result<Number> {
    Ok(Float::min(v, w))
}

macro_rules! pub_math_fn2 {
    ($($name: ident),*) => {
        $(
            pub(crate) fn $name(v: Number, w: Number) -> Result<Number> {
                Ok(v.$name(w))
            }
        )*
    };
}

pub_math_fn2!(copysign, atan2, hypot, powf);

pub(crate) fn fma(v: Number, w: Number, x: Number) -> Result<Number> {
    Ok(v.mul_add(w, x))
}
