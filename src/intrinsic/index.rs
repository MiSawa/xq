use crate::{
    vm::{error::Result, machine::PathElement, QueryExecutionError},
    Array, Number, Object, Value,
};
use num::{ToPrimitive, Zero};
use std::{
    ops::{Bound, Range},
    rc::Rc,
};

fn number_to_isize(n: Number) -> isize {
    n.to_isize().unwrap_or(if n > Zero::zero() {
        isize::MAX
    } else {
        isize::MIN
    })
}

fn parse_and_shift_index<F: Fn(Value) -> QueryExecutionError>(
    array_length: usize,
    index: &Value,
    err: F,
) -> Result<Option<usize>> {
    let i = match index {
        Value::Number(i) => *i,
        value => return Err(err(value.clone())),
    };
    let i = number_to_isize(i);
    let idx = if i < 0 {
        let shifted = i + (array_length as isize);
        if shifted < 0 {
            return Ok(None);
        } else {
            shifted as usize
        }
    } else {
        i as usize
    };
    Ok(Some(idx))
}

pub(crate) fn index(value: Value, index: Value) -> Result<(Value, PathElement)> {
    match value {
        Value::Null
            if matches!(
                index,
                Value::String(_) | Value::Number(_) | Value::Object(_)
            ) =>
        {
            Ok((Value::Null, PathElement::Any(index)))
        }
        value @ (Value::Null | Value::Boolean(_) | Value::Number(_)) => {
            Err(QueryExecutionError::IndexOnNonIndexable(value))
        }
        Value::String(s) => {
            let len = s.chars().count();
            let idx = parse_and_shift_index(len, &index, QueryExecutionError::ArrayIndexByNonInt)?;
            Ok((
                idx.and_then(|i| s.chars().nth(i))
                    .map(|c| Value::string(String::from(c)))
                    .unwrap_or_else(|| Value::string("".to_string())),
                PathElement::Any(index),
            ))
        }
        Value::Array(array) => {
            let idx = parse_and_shift_index(
                array.len(),
                &index,
                QueryExecutionError::ArrayIndexByNonInt,
            )?;
            Ok((
                idx.and_then(|i| array.get(i).cloned())
                    .unwrap_or(Value::Null),
                PathElement::Any(index),
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

pub(crate) fn calculate_slice_index(
    length: usize,
    start: Option<&Value>,
    end: Option<&Value>,
) -> Result<Range<usize>> {
    if start.is_none() && end.is_none() {
        return Err(QueryExecutionError::UnboundedRange);
    }
    let end = if let Some(end) = end {
        let shifted = parse_and_shift_index(length, end, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = shifted {
            if i < length {
                Bound::Excluded(i)
            } else {
                // e.g. [:99999]
                Bound::Unbounded
            }
        } else {
            Bound::Excluded(0) // e.g. [:-99999]
        }
    } else {
        Bound::Unbounded
    };
    let start = if let Some(start) = start {
        let shifted = parse_and_shift_index(length, start, QueryExecutionError::SliceByNonInt)?;
        if let Some(i) = shifted {
            if i < length {
                Bound::Included(i)
            } else {
                // e.g. [99999:]
                Bound::Included(length)
            }
        } else {
            // e.g. [-99999:]
            Bound::Unbounded
        }
    } else {
        Bound::Unbounded
    };
    let start = match start {
        Bound::Included(i) => i,
        Bound::Excluded(i) => i + 1,
        Bound::Unbounded => 0,
    };
    let end = match end {
        Bound::Included(i) => i + 1,
        Bound::Excluded(i) => i,
        Bound::Unbounded => length,
    };
    Ok(if start <= end {
        start..end
    } else {
        start..start
    })
}

pub(crate) fn slice(
    value: Value,
    start: Option<Value>,
    end: Option<Value>,
) -> Result<(Value, PathElement)> {
    let path_element = {
        let mut obj = Object::new();
        if let Some(start) = &start {
            obj.insert(Rc::new("start".to_string()), start.clone());
        }
        if let Some(end) = &end {
            obj.insert(Rc::new("end".to_string()), end.clone());
        }
        PathElement::Any(obj.into())
    };
    let length = match &value {
        Value::Null => {
            return Ok((Value::Null, path_element)); // Why jq doesn't check `start` and `end` type...
        }
        Value::Boolean(_) | Value::Number(_) | Value::Object(_) => {
            return Err(QueryExecutionError::SliceOnNonArrayNorString(value));
        }
        Value::String(s) => s.chars().count(),
        Value::Array(v) => v.len(),
    };
    let range = calculate_slice_index(length, start.as_ref(), end.as_ref())?;
    match value {
        Value::String(s) => Ok((
            Value::string(
                s.chars()
                    .enumerate()
                    .filter(|(i, _)| range.contains(i))
                    .map(|(_, c)| c)
                    .collect::<String>(),
            ),
            path_element,
        )),
        Value::Array(array) => Ok((
            (&array[range]).iter().cloned().collect::<Array>().into(),
            path_element,
        )),
        _ => unreachable!(),
    }
}
