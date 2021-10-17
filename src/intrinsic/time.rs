use crate::{
    number::PrimitiveReal,
    value::RcString,
    vm::{QueryExecutionError, Result},
    Array, Value,
};
use chrono::{DateTime, Datelike, Local, TimeZone, Timelike, Utc};
use std::rc::Rc;

fn try_unwrap_number(func: &'static str, value: &Value) -> Result<PrimitiveReal> {
    match value {
        Value::Number(n) => Ok(n.to_primitive_real()),
        _ => Err(QueryExecutionError::InvalidArgType(func, value.clone())),
    }
}
fn try_unwrap_string(func: &'static str, value: &Value) -> Result<RcString> {
    match value {
        Value::String(s) => Ok(s.clone()),
        _ => Err(QueryExecutionError::InvalidArgType(func, value.clone())),
    }
}
fn try_unwrap_array(func: &'static str, value: &Value) -> Result<Rc<Array>> {
    match value {
        Value::Array(arr) => Ok(arr.clone()),
        _ => Err(QueryExecutionError::InvalidArgType(func, value.clone())),
    }
}
fn timestamp_to_time<TZ: TimeZone>(tz: &TZ, timestamp: PrimitiveReal) -> DateTime<TZ> {
    let seconds = timestamp.div_euclid(1.0) as i64;
    let nanos = (timestamp.rem_euclid(1.0) * 1e9) as u32;
    tz.timestamp(seconds, nanos)
}
fn time_to_timestamp<TZ: TimeZone>(dt: &DateTime<TZ>) -> Value {
    let nanos = dt.timestamp_nanos();
    Value::number((nanos as PrimitiveReal) / 1e9)
}
fn break_down_time<DT: Datelike + Timelike>(dt: &DT) -> Value {
    let v = vec![
        Value::number(dt.year()),
        Value::number(dt.month0()),
        Value::number(dt.day()),
        Value::number(dt.hour()),
        Value::number(dt.minute()),
        Value::number(dt.second() as PrimitiveReal + (dt.nanosecond() as PrimitiveReal) / 1e9),
        Value::number(dt.weekday().num_days_from_sunday()),
        Value::number(dt.ordinal0()),
    ];
    Array::from_vec(v).into()
}

pub(crate) fn format_time(context: Value, format: Value) -> Result<Value> {
    let timestamp = try_unwrap_number("strftime", &context)?;
    let format = try_unwrap_string("strftime", &format)?;
    let dt = timestamp_to_time(&Utc, timestamp);
    let s = format!("{}", dt.format(format.as_ref()));
    Ok(Value::string(s))
}

pub(crate) fn format_time_local(context: Value, format: Value) -> Result<Value> {
    let timestamp = try_unwrap_number("strflocaltime", &context)?;
    let format = try_unwrap_string("strflocaltime", &format)?;
    let dt = timestamp_to_time(&Local, timestamp);
    let s = format!("{}", dt.format(format.as_ref()));
    Ok(Value::string(s))
}

pub(crate) fn parse_time(context: Value, format: Value) -> Result<Value> {
    let time = try_unwrap_string("strptime", &context)?;
    let format = try_unwrap_string("strptime", &format)?;
    let dt = chrono::NaiveDateTime::parse_from_str(time.as_ref(), format.as_ref())?;
    Ok(break_down_time(&dt))
}

pub(crate) fn gm_time(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number("gmtime", &context)?;
    let dt = timestamp_to_time(&Utc, timestamp);
    Ok(break_down_time(&dt))
}

pub(crate) fn gm_time_local(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number("localtime", &context)?;
    let dt = timestamp_to_time(&Local, timestamp);
    Ok(break_down_time(&dt))
}

pub(crate) fn mk_time(context: Value) -> Result<Value> {
    let broken = try_unwrap_array("mktime", &context)?;
    let year = try_unwrap_number(
        "mktime",
        broken
            .get(0)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(0)))?,
    )?;
    let month0 = try_unwrap_number(
        "mktime",
        broken
            .get(1)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(1)))?,
    )?;
    let day = try_unwrap_number(
        "mktime",
        broken
            .get(2)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(2)))?,
    )?;
    let hour = try_unwrap_number(
        "mktime",
        broken
            .get(3)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(3)))?,
    )?;
    let minute = try_unwrap_number(
        "mktime",
        broken
            .get(4)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(4)))?,
    )?;
    let second = try_unwrap_number(
        "mktime",
        broken
            .get(5)
            .ok_or_else(|| QueryExecutionError::InvalidIndex(Value::number(5)))?,
    )?;
    let dt = Utc
        .ymd(year as i32, month0 as u32 + 1, day as u32)
        .and_hms_nano(
            hour as u32,
            minute as u32,
            second.div_euclid(1.0) as u32,
            (second.rem_euclid(1.0) * 1e9) as u32,
        );
    Ok(time_to_timestamp(&dt))
}

pub(crate) fn now(_context: Value) -> Result<Value> {
    let now = Utc::now();
    Ok(time_to_timestamp(&now))
}
