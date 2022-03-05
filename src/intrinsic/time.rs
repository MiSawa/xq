use crate::{
    number::PrimitiveReal,
    value::RcString,
    vm::{QueryExecutionError, Result},
    Array, Value,
};
use chrono::{DateTime, Datelike, Local, TimeZone, Timelike, Utc};
use std::rc::Rc;

fn try_unwrap_number(value: &Value) -> Option<PrimitiveReal> {
    match value {
        Value::Number(n) => Some(n.to_primitive_real()),
        _ => None,
    }
}

fn try_unwrap_string(value: &Value) -> Option<RcString> {
    match value {
        Value::String(s) => Some(s.clone()),
        _ => None,
    }
}

fn try_unwrap_array(value: &Value) -> Option<Rc<Array>> {
    match value {
        Value::Array(arr) => Some(arr.clone()),
        _ => None,
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

fn time_to_array<DT: Datelike + Timelike>(dt: &DT) -> Value {
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

fn try_array_to_time<TZ: TimeZone>(tz: &TZ, value: &Value) -> Option<DateTime<TZ>> {
    let arr = try_unwrap_array(value)?;
    let year = try_unwrap_number(arr.get(0)?)?;
    let month0 = try_unwrap_number(arr.get(1)?)?;
    let day = try_unwrap_number(arr.get(2)?)?;
    let hour = try_unwrap_number(arr.get(3)?)?;
    let minute = try_unwrap_number(arr.get(4)?)?;
    let second = try_unwrap_number(arr.get(5)?)?;
    Some(
        tz.ymd(year as i32, month0 as u32 + 1, day as u32)
            .and_hms_nano(
                hour as u32,
                minute as u32,
                second.div_euclid(1.0) as u32,
                (second.rem_euclid(1.0) * 1e9) as u32,
            ),
    )
}

pub(crate) fn format_time(context: Value, format: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context)
        .ok_or(QueryExecutionError::InvalidArgType("strftime", context))?;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strftime", format))?;
    let dt = timestamp_to_time(&Utc, timestamp);
    let s = format!("{}", dt.format(format.as_ref()));
    Ok(Value::string(s))
}

pub(crate) fn format_time_local(context: Value, format: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context).ok_or(QueryExecutionError::InvalidArgType(
        "strflocaltime",
        context,
    ))?;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strflocaltime", format))?;
    let dt = timestamp_to_time(&Local, timestamp);
    let s = format!("{}", dt.format(format.as_ref()));
    Ok(Value::string(s))
}

pub(crate) fn parse_time(context: Value, format: Value) -> Result<Value> {
    let time = try_unwrap_string(&context)
        .ok_or(QueryExecutionError::InvalidArgType("strptime", context))?;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strptime", format))?;
    let dt = chrono::NaiveDateTime::parse_from_str(time.as_ref(), format.as_ref())?;
    Ok(time_to_array(&dt))
}

pub(crate) fn gm_time(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context)
        .ok_or(QueryExecutionError::InvalidArgType("gmtime", context))?;
    let dt = timestamp_to_time(&Utc, timestamp);
    Ok(time_to_array(&dt))
}

pub(crate) fn gm_time_local(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context)
        .ok_or(QueryExecutionError::InvalidArgType("localtime", context))?;
    let dt = timestamp_to_time(&Local, timestamp);
    Ok(time_to_array(&dt))
}

pub(crate) fn mk_time(context: Value) -> Result<Value> {
    let dt = try_array_to_time(&Utc, &context)
        .ok_or(QueryExecutionError::InvalidArgType("mktime", context))?;
    Ok(time_to_timestamp(&dt))
}

pub(crate) fn now(_: Value) -> Result<Value> {
    let now = Utc::now();
    Ok(time_to_timestamp(&now))
}
