use crate::{
    vm::{machine::PathElement, QueryExecutionError},
    IntOrReal, Number, Value,
};
use num::ToPrimitive;
use std::{
    ops::{Bound, RangeBounds},
    rc::Rc,
};

fn try_into_isize(n: &Number) -> Result<isize, QueryExecutionError> {
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
) -> Result<(Option<usize>, isize), QueryExecutionError> {
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

pub(crate) fn index(
    value: Value,
    index: Value,
) -> Result<(Value, PathElement), QueryExecutionError> {
    match value {
        value @ (Value::Null | Value::True | Value::False | Value::Number(_)) => {
            Err(QueryExecutionError::IndexOnNonIndexable(value))
        }
        Value::String(s) => {
            let len = s.chars().count();
            let (idx, path_idx) =
                get_array_index(len, index, QueryExecutionError::ArrayIndexByNonInt)?;
            Ok((
                idx.and_then(|i| s.chars().nth(i))
                    .map(|c| Value::String(Rc::new(String::from(c))))
                    .unwrap_or_else(|| Value::String(Rc::new("".to_string()))),
                PathElement::Array(path_idx),
            ))
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
) -> Result<(Value, PathElement), QueryExecutionError> {
    let length = match &value {
        Value::Null | Value::True | Value::False | Value::Number(_) | Value::Object(_) => {
            return Err(QueryExecutionError::SliceOnNonArrayNorString(value));
        }
        Value::String(s) => s.chars().count(),
        Value::Array(v) => v.len(),
    };
    let (end, end_path_elem) = if let Some(end) = end {
        let (i, end_path_elem) = get_array_index(length, end, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = i {
            if i == 0 {
                (None, Some(end_path_elem))
            } else if i < length {
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
            get_array_index(length, start, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = i {
            if i < length {
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
    match value {
        Value::String(s) => match (start, end) {
            (Some(start), Some(end)) => Ok((
                Value::String(Rc::new(
                    s.chars()
                        .enumerate()
                        .filter(|(i, _)| (start, end).contains(i))
                        .map(|(_, c)| c)
                        .collect::<String>(),
                )),
                path_element,
            )),
            _ => Ok((Value::Array(Default::default()), path_element)),
        },
        Value::Array(mut array) => match (start, end) {
            (Some(start), Some(end)) => Ok((Value::Array(array.slice((start, end))), path_element)),
            _ => Ok((Value::Array(Default::default()), path_element)),
        },
        _ => unreachable!(),
    }
}
