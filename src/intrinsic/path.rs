use crate::{
    util::make_owned,
    vm::{error::Result, QueryExecutionError},
    Array, Object, Value,
};
use itertools::{repeat_n, Itertools};
use num::ToPrimitive;
use std::ops::Range;

pub(crate) fn get_path(context: Value, path: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => get_path_rec(context, &path_entries[..]),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}

pub(crate) fn set_path(context: Value, path: Value, value: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => set_path_rec(context, &path_entries[..], value),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}

pub(crate) fn del_path(context: Value, path: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => del_path_rec(context, &path_entries[..]),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}

fn map_to_slice_range(length: usize, index: &Object) -> Result<Range<usize>> {
    let start = index.get(&"start".to_string());
    let end = index.get(&"end".to_string());
    super::index::calculate_slice_index(length, start, end)
}

fn get_path_rec(context: Value, path: &[Value]) -> Result<Value> {
    match path.split_first() {
        None => Ok(context),
        Some((index, path)) => match (context, index) {
            (context @ (Value::Boolean(_) | Value::Number(_) | Value::String(_)), _) => {
                Err(QueryExecutionError::IndexOnNonIndexable(context))
            }
            (Value::Null, Value::Number(_)) => get_path_rec(Value::Null, path),
            (Value::Null, Value::String(_)) => get_path_rec(Value::Null, path),
            (Value::Null, Value::Object(map)) => {
                map_to_slice_range(0, map.as_ref())?; // verify the slicing
                get_path_rec(Value::Null, path)
            }
            (Value::Array(arr), Value::Number(n)) => {
                let i = n
                    .to_isize()
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(index.clone()))?;
                if i + (arr.len() as isize) < 0 {
                    Err(QueryExecutionError::InvalidIndex(index.clone()))
                } else {
                    let i = if i < 0 {
                        (i + arr.len() as isize) as usize
                    } else {
                        i as usize
                    };
                    if i >= arr.len() {
                        get_path_rec(Value::Null, path)
                    } else {
                        get_path_rec(arr[i].clone(), path)
                    }
                }
            }
            (Value::Array(arr), Value::Object(map)) => {
                let range = map_to_slice_range(arr.len(), map)?;
                if range.is_empty() {
                    get_path_rec(Array::new().into(), path)
                } else {
                    let temp = arr[range].iter().cloned().collect_vec();
                    get_path_rec(Array::from_vec(temp).into(), path)
                }
            }
            (Value::Object(map), Value::String(key)) => {
                let value = map.get(key).cloned().unwrap_or(Value::Null);
                get_path_rec(value, path)
            }
            (value, index) => Err(QueryExecutionError::InvalidIndexing(value, index.clone())),
        },
    }
}

fn set_path_rec(context: Value, path: &[Value], replacement: Value) -> Result<Value> {
    let ret = match path.split_first() {
        None => replacement,
        Some((index, path)) => match (context, index) {
            (context @ (Value::Boolean(_) | Value::Number(_) | Value::String(_)), _) => {
                return Err(QueryExecutionError::IndexOnNonIndexable(context))
            }
            (Value::Null, Value::Number(n)) => {
                let index = n
                    .to_usize()
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(index.clone()))?;
                let mut arr: Array = repeat_n(Value::Null, index).collect();
                arr.push(set_path_rec(Value::Null, path, replacement)?);
                arr.into()
            }
            (Value::Null, Value::String(key)) => {
                let mut obj = Object::new();
                obj.insert(key.clone(), set_path_rec(Value::Null, path, replacement)?);
                obj.into()
            }
            (Value::Null, Value::Object(map)) => {
                map_to_slice_range(0, map.as_ref())?; // verify the slicing
                let replacement = set_path_rec(Value::Null, path, replacement)?;
                // NOTE: Why??? Why `null | setpath([{start: 1, end: 10}]; 0)` doesn't return [null, 0]? Anyway that's what jq is...
                if matches!(replacement, Value::Array(_)) {
                    replacement
                } else {
                    return Err(QueryExecutionError::ExpectedAnArray(replacement));
                }
            }
            (Value::Array(arr), Value::Number(n)) => {
                let i = n
                    .to_isize()
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(index.clone()))?;
                if i + (arr.len() as isize) < 0 {
                    return Err(QueryExecutionError::InvalidIndex(index.clone()));
                }
                let i = if i < 0 {
                    (i + arr.len() as isize) as usize
                } else {
                    i as usize
                };
                let mut arr = (*arr).clone();
                if i >= arr.len() {
                    arr.extend(repeat_n(Value::Null, i - arr.len() + 1));
                }
                let v = std::mem::replace(&mut arr[i], Value::Null);
                arr[i] = set_path_rec(v, path, replacement)?;
                arr.into()
            }
            (Value::Array(arr), Value::Object(map)) => {
                let range = map_to_slice_range(arr.len(), map)?;
                if range.is_empty() {
                    // FIXME: JQ seems to handle this differently...
                    return Ok(Value::Array(arr));
                }
                let replacement = set_path_rec(Value::Null, path, replacement)?;
                if let Value::Array(replacement) = replacement {
                    // TODO: test this
                    let before = &arr[..range.start];
                    let after = &arr[range.end..];
                    before
                        .iter()
                        .chain(replacement.as_ref())
                        .chain(after)
                        .cloned()
                        .collect::<Array>()
                        .into()
                } else {
                    return Err(QueryExecutionError::ExpectedAnArray(replacement));
                }
            }
            (Value::Object(map), Value::String(key)) => {
                let mut map = (*map).clone();
                let value = map.entry(key.clone()).or_insert(Value::Null);
                let tmp = std::mem::replace(value, Value::Null);
                *value = set_path_rec(tmp, path, replacement)?;
                map.into()
            }
            (value, index) => {
                return Err(QueryExecutionError::InvalidIndexing(value, index.clone()))
            }
        },
    };
    Ok(ret)
}

fn del_path_rec(context: Value, path: &[Value]) -> Result<Value> {
    let ret = match path.split_first() {
        None => Value::Null,
        Some((index, path)) => match (context, index) {
            (context @ (Value::Boolean(_) | Value::Number(_) | Value::String(_)), _) => {
                return Err(QueryExecutionError::IndexOnNonIndexable(context))
            }
            (Value::Null, _) => Value::Null,
            (Value::Array(arr), Value::Number(n)) => {
                let i = n
                    .to_isize()
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(index.clone()))?;
                if i + (arr.len() as isize) < 0 {
                    return Err(QueryExecutionError::InvalidIndex(index.clone()));
                }
                let i = if i < 0 {
                    (i + arr.len() as isize) as usize
                } else {
                    i as usize
                };
                if i >= arr.len() {
                    // nop
                    arr.into()
                } else if path.is_empty() {
                    let mut arr = (*arr).clone();
                    arr.remove(i);
                    arr.into()
                } else {
                    let mut arr = (*arr).clone();
                    let prev = std::mem::replace(&mut arr[i], Value::Null);
                    arr[i] = del_path_rec(prev, path)?;
                    arr.into()
                }
            }
            (Value::Array(arr), Value::Object(map)) => {
                let range = map_to_slice_range(arr.len(), map)?;
                if range.is_empty() {
                    return Ok(Value::Array(arr));
                }
                // TODO: Test this
                let before = &arr[0..range.start];
                let middle = &arr[range.clone()];
                let after = &arr[range.end..];
                if path.is_empty() {
                    before
                        .iter()
                        .chain(after)
                        .cloned()
                        .collect::<Array>()
                        .into()
                } else {
                    let middle_array = middle.iter().cloned().collect::<Array>().into();
                    let middle = del_path_rec(middle_array, path)?;
                    if let Value::Array(middle) = middle {
                        before
                            .iter()
                            .cloned()
                            .chain(make_owned(middle).into_iter())
                            .chain(after.iter().cloned())
                            .collect::<Array>()
                            .into()
                    } else {
                        return Err(QueryExecutionError::ExpectedAnArray(middle));
                    }
                }
            }
            (Value::Object(map), Value::String(key)) => {
                let mut map = make_owned(map);
                if path.is_empty() {
                    map.remove(key);
                } else if let Some(v) = map.get_mut(key) {
                    let tmp = std::mem::replace(v, Value::Null);
                    *v = del_path_rec(tmp, path)?;
                }
                map.into()
            }
            (value, index) => {
                return Err(QueryExecutionError::InvalidIndexing(value, index.clone()))
            }
        },
    };
    Ok(ret)
}
