use crate::{
    util::make_owned,
    value::RcString,
    vm::{error::Result, QueryExecutionError},
    Array, Object, Value,
};
use itertools::{repeat_n, Itertools};
use num::ToPrimitive;
use std::{ops::Range, rc::Rc};

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

pub(crate) fn del_paths(context: Value, paths: Value) -> Result<Value> {
    match paths {
        Value::Array(rc_paths) => {
            let placeholder = Rc::new("placeholder".to_string());
            let mut paths = vec![];
            for path in rc_paths.iter() {
                match path {
                    Value::Array(path) => paths.push(path),
                    v => return Err(QueryExecutionError::PathNotArray(v.clone())),
                }
            }
            paths.sort_by_key(|p| p.len());
            let mut value = context.clone();
            for path in paths {
                value =
                    replace_tombstone_rec(value, &path[..], Value::String(placeholder.clone()))?;
            }
            value = del_tombstone_rec(&context, value, &placeholder)?.unwrap_or(Value::Null);
            Ok(value)
        }
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

fn replace_tombstone_rec(context: Value, path: &[Value], placeholder: Value) -> Result<Value> {
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
                    arr[i] = placeholder;
                    arr.into()
                } else {
                    let mut arr = (*arr).clone();
                    let prev = std::mem::replace(&mut arr[i], Value::Null);
                    arr[i] = replace_tombstone_rec(prev, path, placeholder)?;
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
                        .cloned()
                        .chain(std::iter::repeat_with(|| placeholder.clone()).take(middle.len()))
                        .chain(after.iter().cloned())
                        .collect::<Array>()
                        .into()
                } else {
                    let middle_array = middle.iter().cloned().collect::<Array>().into();
                    let middle = replace_tombstone_rec(middle_array, path, placeholder)?;
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
                    map.entry(key.clone()).and_modify(|v| *v = placeholder);
                } else if let Some(v) = map.get_mut(key) {
                    let tmp = std::mem::replace(v, Value::Null);
                    *v = replace_tombstone_rec(tmp, path, placeholder)?;
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

fn del_tombstone_rec(
    original: &Value,
    tomb_stoned: Value,
    placeholder: &RcString,
) -> Result<Option<Value>> {
    let ret = match (original, tomb_stoned) {
        (_, Value::String(s)) if Rc::ptr_eq(&s, placeholder) => return Ok(None),
        (Value::Array(original), Value::Array(tomb_stoned)) => {
            if Rc::ptr_eq(original, &tomb_stoned) {
                Value::Array(tomb_stoned)
            } else {
                let tomb_stoned = make_owned(tomb_stoned);
                println!("{:?}, {:?}", original, tomb_stoned);
                let arr = original
                    .iter()
                    .zip_eq(tomb_stoned.into_iter())
                    .flat_map(|(orig, tomb)| del_tombstone_rec(orig, tomb, placeholder).transpose())
                    .collect::<Result<Vec<_>>>()?;
                Array::from_vec(arr).into()
            }
        }
        (Value::Object(original), Value::Object(tomb_stoned)) => {
            if Rc::ptr_eq(original, &tomb_stoned) {
                Value::Object(tomb_stoned)
            } else {
                assert_eq!(original.len(), tomb_stoned.len());
                let mut obj = make_owned(tomb_stoned);
                for (key, orig) in original.iter() {
                    let v = obj.get_mut(key).expect("Shouldn't be deleted yet");
                    let tmp = std::mem::replace(v, Value::Null);
                    match del_tombstone_rec(orig, tmp, placeholder)? {
                        Some(tmp) => {
                            *v = tmp;
                        }
                        None => {
                            obj.remove(key);
                        }
                    }
                }
                obj.into()
            }
        }
        (_, v) => v,
    };
    Ok(Some(ret))
}
