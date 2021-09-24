use crate::{
    ast::{Comparator, UnaryOp},
    number::IntOrReal,
    vm::{
        bytecode::{NamedFn1, NamedFn2},
        machine::PathElement,
        QueryExecutionError, Result, Value,
    },
    Number,
};
use itertools::Itertools;
use num::ToPrimitive;
use std::{cmp::Ordering, ops::Bound, rc::Rc};

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

fn try_into_isize(n: &Number) -> Result<isize> {
    match n.as_int_or_real() {
        IntOrReal::Integer(n) => {
            // TODO: actually should be positive usize and negative usize
            // but I believe it's fine.
            Ok(n.to_isize().unwrap_or(isize::MAX))
        }
        _ => Err(QueryExecutionError::NonIntegralNumber(n.clone())),
    }
}

fn get_array_index<F: Fn(Value) -> QueryExecutionError>(
    array_length: usize,
    index: Value,
    err: F,
) -> Result<(Option<usize>, isize)> {
    let i = match index {
        Value::Number(i) => i,
        value => return Err(err(value)),
    };
    let i = try_into_isize(&i)?;
    let idx = if i < 0 {
        let shifted = i + (array_length as isize);
        if shifted < 0 {
            return Ok((None, i));
        } else {
            shifted as usize
        }
    } else {
        i as usize
    };
    Ok((Some(idx), i))
}

pub(crate) fn index(value: Value, index: Value) -> Result<(Value, PathElement)> {
    match value {
        value
        @ (Value::Null | Value::True | Value::False | Value::Number(_) | Value::String(_)) => {
            Err(QueryExecutionError::IndexOnNonIndexable(value))
        }
        Value::Array(array) => {
            let (idx, path_idx) =
                get_array_index(array.len(), index, QueryExecutionError::ArrayIndexByNonInt)?;
            Ok((
                idx.and_then(|i| array.get(i).cloned())
                    .unwrap_or(Value::Null),
                PathElement::Array(path_idx),
            ))
        }
        Value::Object(map) => {
            let i = match index {
                Value::String(i) => i,
                value => return Err(QueryExecutionError::ObjectIndexByNonString(value)),
            };
            Ok((
                map.get(&i).cloned().unwrap_or(Value::Null),
                PathElement::Object(i),
            ))
        }
    }
}

pub(crate) fn slice(
    value: Value,
    start: Option<Value>,
    end: Option<Value>,
) -> Result<(Value, PathElement)> {
    let mut array = if let Value::Array(array) = value {
        array
    } else {
        return Err(QueryExecutionError::SliceOnNonArray(value));
    };
    let (end, end_path_elem) = if let Some(end) = end {
        let (i, end_path_elem) =
            get_array_index(array.len(), end, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = i {
            if i == 0 {
                (None, Some(end_path_elem))
            } else if i < array.len() {
                (Some(Bound::Excluded(i)), Some(end_path_elem))
            } else {
                (Some(Bound::Unbounded), Some(end_path_elem))
            }
        } else if end_path_elem < 0 {
            (Some(Bound::Unbounded), Some(end_path_elem))
        } else {
            (None, Some(end_path_elem))
        }
    } else {
        (Some(Bound::Unbounded), None)
    };
    let (start, start_path_elem) = if let Some(start) = start {
        let (i, start_path_elem) =
            get_array_index(array.len(), start, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = i {
            if i < array.len() {
                (Some(Bound::Included(i)), Some(start_path_elem))
            } else {
                (None, Some(start_path_elem))
            }
        } else if start_path_elem < 0 {
            (None, Some(start_path_elem))
        } else {
            (Some(Bound::Unbounded), Some(start_path_elem))
        }
    } else {
        (Some(Bound::Unbounded), None)
    };
    let path_element = PathElement::Slice(start_path_elem, end_path_elem);
    match (start, end) {
        (Some(start), Some(end)) => Ok((Value::Array(array.slice((start, end))), path_element)),
        _ => Ok((Value::Array(Default::default()), path_element)),
    }
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
