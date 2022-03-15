use crate::{
    number::PrimitiveReal,
    value::RcString,
    vm::{QueryExecutionError, Result},
    Array, Value,
};
use std::rc::Rc;
use time::{Date, Month, OffsetDateTime, PrimitiveDateTime, Time, UtcOffset};
use time_fmt::{format::format_zoned_offset_date_time, parse::parse_date_time_maybe_with_zone};
use time_tz::{system::get_timezone, Offset, TimeZone};

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

fn timestamp_to_time(timestamp: PrimitiveReal) -> Result<OffsetDateTime> {
    let seconds = timestamp.div_euclid(1.0) as i128;
    let nanos = (timestamp.rem_euclid(1.0) * 1e9) as i128;
    Ok(OffsetDateTime::from_unix_timestamp_nanos(
        seconds.saturating_mul(1e9 as i128).saturating_add(nanos)
    )?)
}

fn time_to_timestamp(dt: &OffsetDateTime) -> Value {
    let nanos = dt.unix_timestamp_nanos();
    Value::number((nanos as PrimitiveReal) / 1e9)
}

fn time_to_array(dt: &OffsetDateTime) -> Value {
    let v = vec![
        Value::number(dt.year()),
        Value::number(u8::from(dt.month()) - 1),
        Value::number(dt.day()),
        Value::number(dt.hour()),
        Value::number(dt.minute()),
        Value::number(dt.second() as PrimitiveReal + (dt.nanosecond() as PrimitiveReal) / 1e9),
        Value::number(dt.weekday().number_days_from_sunday()),
        Value::number(dt.ordinal() - 1),
    ];
    Array::from_vec(v).into()
}

fn try_array_to_time(value: &Value) -> Option<Result<PrimitiveDateTime>> {
    let arr = try_unwrap_array(value)?;
    let year = try_unwrap_number(arr.get(0)?)?;
    let month0 = try_unwrap_number(arr.get(1)?)?;
    let day = try_unwrap_number(arr.get(2)?)?;
    let hour = try_unwrap_number(arr.get(3)?)?;
    let minute = try_unwrap_number(arr.get(4)?)?;
    let second = try_unwrap_number(arr.get(5)?)?;
    Some((|| -> Result<PrimitiveDateTime> {
        Ok(PrimitiveDateTime::new(
            Date::from_calendar_date(year as i32, Month::try_from(month0 as u8 + 1)?, day as u8)?,
            Time::from_hms_nano(
                hour as u8,
                minute as u8,
                second.div_euclid(1.0) as u8,
                (second.rem_euclid(1.0) * 1e9) as u32,
            )?,
        ))
    })())
}

pub(crate) fn format_time(context: Value, format: Value) -> Result<Value> {
    let dt = try_unwrap_number(&context)
        .map(timestamp_to_time)
        .or_else(|| try_array_to_time(&context).map(|r| r.map(PrimitiveDateTime::assume_utc)))
        .ok_or(QueryExecutionError::InvalidArgType("strftime", context))??;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strftime", format))?;
    let s = format_zoned_offset_date_time(format.as_ref(), dt, "UTC")?;
    Ok(Value::string(s))
}

pub(crate) fn format_time_local(context: Value, format: Value) -> Result<Value> {
    let dt = try_unwrap_number(&context)
        .map(timestamp_to_time)
        .or_else(|| {
            try_array_to_time(&context).map(|r| {
                r.and_then(|dt| {
                    // This is not right. e.g. daylight saving time...
                    // But there's no way to do this right actually.
                    let offset = UtcOffset::local_offset_at(dt.assume_utc())?;
                    Ok(dt.assume_offset(offset))
                })
            })
        })
        .ok_or(QueryExecutionError::InvalidArgType(
            "strflocaltime",
            context,
        ))??;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strflocaltime", format))?;
    let s = if let Ok(s) = std::env::var("TZ") {
        let posix_tz = time_tz::posix_tz::PosixTz::parse(&s).map_err(|_| {
            QueryExecutionError::TimeZoneLookupFailure(Rc::new(
                "Invalid TZ environment variable".to_string(),
            ))
        })?;
        let zone = posix_tz.get_offset(&dt).map_err(|_| {
            QueryExecutionError::TimeZoneLookupFailure(Rc::new(
                "Unable to apply time zone got from TZ".to_string(),
            ))
        })?;
        format_zoned_offset_date_time(format.as_ref(), dt.to_offset(zone.to_utc()), zone.name())?
    } else {
        let zone = get_timezone()?.get_offset_utc(&dt);
        format_zoned_offset_date_time(format.as_ref(), dt.to_offset(zone.to_utc()), zone.name())?
    };
    Ok(Value::string(s))
}

pub(crate) fn parse_time(context: Value, format: Value) -> Result<Value> {
    let time = try_unwrap_string(&context)
        .ok_or(QueryExecutionError::InvalidArgType("strptime", context))?;
    let format = try_unwrap_string(&format)
        .ok_or(QueryExecutionError::InvalidArgType("strptime", format))?;
    let dt = parse_date_time_maybe_with_zone(format.as_ref(), time.as_ref())?
        .0
        .assume_utc();
    Ok(time_to_array(&dt))
}

pub(crate) fn fromdateiso8601(context: Value) -> Result<Value> {
    let time = try_unwrap_string(&context).ok_or(QueryExecutionError::InvalidArgType(
        "fromdateiso8601",
        context,
    ))?;
    let dt = OffsetDateTime::parse(
        time.as_ref(),
        &time::format_description::well_known::Rfc3339,
    )?;
    Ok(time_to_timestamp(&dt))
}

pub(crate) fn gm_time(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context)
        .ok_or(QueryExecutionError::InvalidArgType("gmtime", context))?;
    let dt = timestamp_to_time(timestamp)?;
    Ok(time_to_array(&dt))
}

pub(crate) fn gm_time_local(context: Value) -> Result<Value> {
    let timestamp = try_unwrap_number(&context)
        .ok_or(QueryExecutionError::InvalidArgType("localtime", context))?;
    let dt = timestamp_to_time(timestamp)?;
    let dt = dt.to_offset(UtcOffset::local_offset_at(dt)?);
    Ok(time_to_array(&dt))
}

pub(crate) fn mk_time(context: Value) -> Result<Value> {
    let dt = try_array_to_time(&context)
        .ok_or(QueryExecutionError::InvalidArgType("mktime", context))??
        .assume_utc();
    Ok(time_to_timestamp(&dt))
}

pub(crate) fn now(_: Value) -> Result<Value> {
    let now = OffsetDateTime::now_utc();
    Ok(time_to_timestamp(&now))
}
