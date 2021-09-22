use crate::{
    ast::Comparator,
    vm::{bytecode::NamedFn2, Result, Value},
};
use std::cmp::Ordering;

pub fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::False)
}

pub fn compare(_lhs: Value, _rhs: Value) -> Ordering {
    todo!()
}

pub fn comparator(comparator: &Comparator) -> NamedFn2 {
    match comparator {
        Comparator::Eq => NamedFn2 {
            name: "Equal",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_eq() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
        Comparator::Neq => NamedFn2 {
            name: "NotEqual",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_ne() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
        Comparator::Gt => NamedFn2 {
            name: "GreaterThan",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_gt() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
        Comparator::Ge => NamedFn2 {
            name: "GreaterOrEqual",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_ge() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
        Comparator::Lt => NamedFn2 {
            name: "LessThan",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_lt() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
        Comparator::Le => NamedFn2 {
            name: "LessOrEqual",
            func: Box::new(|lhs, rhs| {
                Ok(if compare(lhs, rhs).is_le() {
                    Value::True
                } else {
                    Value::False
                })
            }),
        },
    }
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
