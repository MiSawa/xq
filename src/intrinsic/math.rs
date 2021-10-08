use crate::{vm::Result, Number};
use num::Float;

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

pub(crate) fn floor(v: Number) -> Result<Number> {
    Ok(v.floor())
}

pub(crate) fn sqrt(v: Number) -> Result<Number> {
    Ok(v.sqrt())
}
