use crate::{
    ast::{Comparator, UnaryOp},
    vm::{
        bytecode::{NamedFn1, NamedFn2},
        QueryExecutionError, Result, Value,
    },
};
use itertools::Itertools;
use std::{cmp::Ordering, rc::Rc};

struct ComparableValue<'a>(&'a Value);

impl<'a> Eq for ComparableValue<'a> {}

impl<'a> PartialEq<Self> for ComparableValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        Ord::cmp(self, other).is_eq()
    }
}

impl<'a> PartialOrd<Self> for ComparableValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl<'a> Ord for ComparableValue<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        use Ordering::*;
        use Value::*;
        fn type_ord(value: &Value) -> u8 {
            match value {
                Null => 0,
                False => 1,
                True => 2,
                Number(_) => 3,
                String(_) => 4,
                Array(_) => 5,
                Object(_) => 6,
            }
        }
        if let res @ (Less | Greater) = type_ord(self.0).cmp(&type_ord(other.0)) {
            return res;
        }
        match (&self.0, &other.0) {
            (Null, Null) | (True, True) | (False, False) => Equal,
            (Number(lhs), Number(rhs)) => Ord::cmp(&lhs, &rhs),
            (String(lhs), String(rhs)) => Ord::cmp(&lhs, &rhs),
            (Array(lhs), Array(rhs)) => Iterator::cmp(
                lhs.iter().map(ComparableValue),
                rhs.iter().map(ComparableValue),
            ),
            (Object(lhs), Object(rhs)) => {
                let lhs_keys = lhs.keys().sorted().collect_vec();
                let rhs_keys = rhs.keys().sorted().collect_vec();
                if let res @ (Less | Greater) = Iterator::cmp(lhs_keys.iter(), rhs_keys.iter()) {
                    return res;
                }
                for key in lhs_keys {
                    if let res @ (Less | Greater) = Ord::cmp(
                        &lhs.get(key).map(ComparableValue),
                        &rhs.get(key).map(ComparableValue),
                    ) {
                        return res;
                    }
                }
                Equal
            }
            _ => unreachable!(),
        }
    }
}

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

pub fn index(_lhs: Value, _rhs: Value) -> Result<Value> {
    todo!()
}

fn compare(lhs: Value, rhs: Value) -> Ordering {
    Ord::cmp(&ComparableValue(&lhs), &ComparableValue(&rhs))
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
