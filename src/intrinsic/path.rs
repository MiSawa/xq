use crate::{
    data_structure::{PHashMap, PVector},
    vm::{error::Result, QueryExecutionError},
    Value,
};
use itertools::repeat_n;
use num::ToPrimitive;
use std::{ops::Range, rc::Rc};

pub(crate) fn set_path(context: Value, path: Value, value: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => set_path_rec(context, path_entries, value),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}

pub(crate) fn del_path(context: Value, path: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => del_path_rec(context, path_entries),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}

fn map_to_slice_range(length: usize, index: &PHashMap<Rc<String>, Value>) -> Result<Range<usize>> {
    let start = index.get(&"start".to_string());
    let end = index.get(&"end".to_string());
    super::index::calculate_slice_index(length, start, end)
}

fn set_path_rec(context: Value, mut path: PVector<Value>, replacement: Value) -> Result<Value> {
    let ret = match path.pop_front() {
        None => replacement,
        Some(index) => match (context, index) {
            (context @ (Value::True | Value::False | Value::Number(_) | Value::String(_)), _) => {
                return Err(QueryExecutionError::IndexOnNonIndexable(context))
            }
            (Value::Null, Value::Number(n)) => {
                let index = n
                    .to_usize()
                    .ok_or(QueryExecutionError::InvalidIndex(Value::Number(n)))?;
                let mut arr: PVector<Value> = repeat_n(Value::Null, index).collect();
                arr.push_back(set_path_rec(Value::Null, path, replacement)?);
                Value::Array(arr)
            }
            (Value::Null, Value::String(key)) => {
                let mut map = PHashMap::new();
                map.insert(key, set_path_rec(Value::Null, path, replacement)?);
                Value::Object(map)
            }
            (Value::Null, Value::Object(map)) => {
                map_to_slice_range(0, &map)?; // verify the slicing
                let replacement = set_path_rec(Value::Null, path, replacement)?;
                // NOTE: Why??? Why `null | setpath([{start: 1, end: 10}]; 0)` doesn't return [null, 0]? Anyway that's what jq is...
                if matches!(replacement, Value::Array(_)) {
                    replacement
                } else {
                    return Err(QueryExecutionError::ExpectedAnArray(replacement));
                }
            }
            (Value::Array(mut arr), Value::Number(n)) => {
                let index = n
                    .to_isize()
                    .ok_or(QueryExecutionError::InvalidIndex(Value::Number(n)))?;
                if index + (arr.len() as isize) < 0 {
                    return Err(QueryExecutionError::InvalidIndex(Value::Number(n)));
                }
                let index = if index < 0 {
                    (index + arr.len() as isize) as usize
                } else {
                    index as usize
                };
                if index >= arr.len() {
                    arr.extend(repeat_n(Value::Null, index - arr.len() + 1));
                }
                let v = arr.set(index, Value::Null);
                arr[index] = set_path_rec(v, path, replacement)?;
                Value::Array(arr)
            }
            (Value::Array(mut arr), Value::Object(map)) => {
                let range = map_to_slice_range(arr.len(), &map)?;
                if range.is_empty() {
                    // FIXME: JQ seems to handle this differently...
                    return Ok(Value::Array(arr));
                }
                let replacement = set_path_rec(Value::Null, path, replacement)?;
                if let Value::Array(replacement) = replacement {
                    let after = arr.split_off(range.end);
                    let _middle = arr.split_off(range.start + 1);
                    Value::Array(arr + replacement + after)
                } else {
                    return Err(QueryExecutionError::ExpectedAnArray(replacement));
                }
            }
            (Value::Object(mut map), Value::String(key)) => {
                let v = map.remove(&key).unwrap_or(Value::Null);
                map.insert(key, set_path_rec(v, path, replacement)?);
                Value::Object(map)
            }
            (value, index) => return Err(QueryExecutionError::InvalidIndexing(value, index)),
        },
    };
    Ok(ret)
}

fn del_path_rec(context: Value, mut path: PVector<Value>) -> Result<Value> {
    let ret = match path.pop_front() {
        None => Value::Null,
        Some(index) => match (context, index) {
            (context @ (Value::True | Value::False | Value::Number(_) | Value::String(_)), _) => {
                return Err(QueryExecutionError::IndexOnNonIndexable(context))
            }
            (Value::Null, _) => Value::Null,
            (Value::Array(mut arr), Value::Number(n)) => {
                let index = n
                    .to_isize()
                    .ok_or(QueryExecutionError::InvalidIndex(Value::Number(n)))?;
                if index + (arr.len() as isize) < 0 {
                    return Err(QueryExecutionError::InvalidIndex(Value::Number(n)));
                }
                let index = if index < 0 {
                    (index + arr.len() as isize) as usize
                } else {
                    index as usize
                };
                if index >= arr.len() {
                    // nop
                } else if path.is_empty() {
                    arr.remove(index);
                } else {
                    let prev = arr.set(index, Value::Null);
                    arr[index] = del_path_rec(prev, path)?;
                }
                Value::Array(arr)
            }
            (Value::Array(mut arr), Value::Object(map)) => {
                let range = map_to_slice_range(arr.len(), &map)?;
                if range.is_empty() {
                    return Ok(Value::Array(arr));
                }
                let after = arr.split_off(range.end);
                let middle = arr.split_off(range.start + 1);
                let before = arr;
                if path.is_empty() {
                    Value::Array(before + after)
                } else {
                    let middle = del_path_rec(Value::Array(middle), path)?;
                    if let Value::Array(middle) = middle {
                        Value::Array(before + middle + after)
                    } else {
                        return Err(QueryExecutionError::ExpectedAnArray(middle));
                    }
                }
            }
            (Value::Object(mut map), Value::String(key)) => {
                if path.is_empty() {
                    map.remove(&key);
                } else {
                    let v = map.remove(&key).unwrap_or(Value::Null);
                    map.insert(key, del_path_rec(v, path)?);
                }
                Value::Object(map)
            }
            (value, index) => return Err(QueryExecutionError::InvalidIndexing(value, index)),
        },
    };
    Ok(ret)
}
