use crate::{
    ast::{Comparator, UnaryOp},
    vm::{
        bytecode::{NamedFn1, NamedFn2},
        QueryExecutionError, Result, Value,
    },
};
use std::{borrow::Borrow, cmp::Ordering, rc::Rc};

pub fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::False)
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

pub fn unary(operator: &UnaryOp) -> NamedFn1 {
    match operator {
        UnaryOp::Plus => NamedFn1 {
            name: "UnaryPlus",
            func: Box::new(unary_plus),
        },
        UnaryOp::Minus => NamedFn1 {
            name: "UnaryMinus",
            func: Box::new(unary_minus),
        },
    }
}

fn compare(_lhs: Value, _rhs: Value) -> Ordering {
    todo!()
}

fn unary_plus(value: Value) -> Result<Value> {
    match value {
        Value::Number(_) => Ok(value),
        _ => Err(QueryExecutionError::UnaryOnNonNumeric("plus", value)),
    }
}

fn unary_minus(value: Value) -> Result<Value> {
    match value {
        Value::Number(n) => Ok(Value::Number(Rc::new(-(*n).clone()))),
        _ => Err(QueryExecutionError::UnaryOnNonNumeric("minus", value)),
    }
}

fn add(_lhs: Value, _rhs: Value) -> Result<Value> {
    todo!()
}

fn subtract(_lhs: Value, _rhs: Value) -> Result<Value> {
    todo!()
}
