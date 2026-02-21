use std::sync::Arc;

use chrono::{DateTime, TimeZone, Utc};

use super::{NValue, NativeError};

pub(super) fn native_datetime_now(_args: &[NValue]) -> Result<NValue, NativeError> {
    let ms = Utc::now().timestamp_millis();
    Ok(NValue::int(ms))
}

pub(super) fn native_datetime_parse(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => match DateTime::parse_from_rfc3339(s) {
            Ok(dt) => Ok(NValue::enum_val(
                "Ok".into(),
                NValue::int(dt.timestamp_millis()),
            )),
            Err(e) => Ok(NValue::enum_val(
                "Err".into(),
                NValue::string(Arc::from(e.to_string().as_str())),
            )),
        },
        None => Err(NativeError("DateTime.parse: expected String".into())),
    }
}

pub(super) fn native_datetime_to_string(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_any_int() {
        return Err(NativeError(format!(
            "DateTime.to_string: expected Int (epoch ms), got {}",
            args[0]
        )));
    }
    let ms = args[0].as_any_int();
    let secs = ms / 1000;
    let nsecs = ((ms % 1000) * 1_000_000) as u32;
    match Utc.timestamp_opt(secs, nsecs) {
        chrono::LocalResult::Single(dt) => Ok(NValue::string(Arc::from(dt.to_rfc3339().as_str()))),
        _ => Err(NativeError(format!(
            "DateTime.to_string: invalid epoch ms {}",
            ms
        ))),
    }
}

pub(super) fn native_datetime_add(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 || !args[0].is_any_int() || !args[1].is_any_int() {
        return Err(NativeError("DateTime.add: expected (Int, Int)".into()));
    }
    let dt_ms = args[0].as_any_int();
    let seconds = args[1].as_any_int();
    Ok(NValue::int(dt_ms + seconds * 1000))
}

pub(super) fn native_datetime_diff(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 || !args[0].is_any_int() || !args[1].is_any_int() {
        return Err(NativeError("DateTime.diff: expected (Int, Int)".into()));
    }
    let a_ms = args[0].as_any_int();
    let b_ms = args[1].as_any_int();
    Ok(NValue::int((a_ms - b_ms) / 1000))
}
