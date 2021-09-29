use crate::{
    data_structure::{PHashMap, PVector},
    vm::{error::Result, QueryExecutionError},
    Value,
};
use itertools::repeat_n;
use num::ToPrimitive;

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
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::Number(n.clone())))?;
                let mut arr: PVector<Value> = repeat_n(Value::Null, index).collect();
                arr.push_back(set_path_rec(Value::Null, path, replacement)?);
                Value::Array(arr)
            }
            (Value::Null, Value::String(key)) => {
                let mut map = PHashMap::new();
                map.insert(key, set_path_rec(Value::Null, path, replacement)?);
                Value::Object(map)
            }
            (Value::Array(mut arr), Value::Number(n)) => {
                let index = n
                    .to_isize()
                    .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::Number(n.clone())))?;
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
            (Value::Array(_arr), Value::Object(_map)) => {
                todo!("slice update")
            }
            (Value::Object(mut map), Value::String(key)) => {
                let v = map.remove(&key).unwrap_or(Value::Null);
                map.insert(key, set_path_rec(v, path, replacement)?);
                Value::Object(map)
            }
            (_, _) => {
                todo!()
            }
        },
    };
    Ok(ret)
}

pub(crate) fn set_path(context: Value, path: Value, value: Value) -> Result<Value> {
    match path {
        Value::Array(path_entries) => set_path_rec(context, path_entries, value),
        v => Err(QueryExecutionError::PathNotArray(v)),
    }
}
