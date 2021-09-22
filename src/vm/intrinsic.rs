use crate::vm::{Result, Value};
use std::cmp::Ordering;

pub fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::False)
}

pub fn compare(_lhs: Value, _rhs: Value) -> Ordering {
    todo!()
}

pub fn unary_plus(_value: Value) -> Result<Value> {
    todo!()
}

pub fn unary_minus(_value: Value) -> Result<Value> {
    todo!()
}

pub fn add(_lhs: Value, _rhs: Value) -> Result<Value> {
    todo!()
}

pub fn subtract(_lhs: Value, _rhs: Value) -> Result<Value> {
    todo!()
}
