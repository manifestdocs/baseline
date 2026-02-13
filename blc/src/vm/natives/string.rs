use super::{NValue, NativeError};

pub(super) fn native_string_length(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::int(s.len() as i64)),
        None => Err(NativeError(format!(
            "String.length: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_to_upper(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.to_uppercase().into())),
        None => Err(NativeError(format!(
            "String.to_upper: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_to_lower(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.to_lowercase().into())),
        None => Err(NativeError(format!(
            "String.to_lower: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_trim(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.trim().into())),
        None => Err(NativeError(format!(
            "String.trim: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(sub)) => Ok(NValue::bool(s.contains(&**sub))),
        _ => Err(NativeError(
            "String.contains: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_starts_with(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(prefix)) => Ok(NValue::bool(s.starts_with(&**prefix))),
        _ => Err(NativeError(
            "String.starts_with: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_split(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(delim)) => {
            let parts: Vec<NValue> = s
                .split(&**delim)
                .map(|p| NValue::string(p.into()))
                .collect();
            Ok(NValue::list(parts))
        }
        _ => Err(NativeError(
            "String.split: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_join(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_list(), args[1].as_string()) {
        (Some(items), Some(sep)) => {
            let strs: Vec<String> = items.iter().map(|v| v.to_string()).collect();
            Ok(NValue::string(strs.join(&**sep).into()))
        }
        _ => Err(NativeError("String.join: expected (List, String)".into())),
    }
}

pub(super) fn native_string_slice(args: &[NValue]) -> Result<NValue, NativeError> {
    match (
        args[0].as_string(),
        args[1].is_any_int(),
        args[2].is_any_int(),
    ) {
        (Some(s), true, true) => {
            let start = args[1].as_any_int() as usize;
            let len = args[2].as_any_int() as usize;
            let result: String = s.chars().skip(start).take(len).collect();
            Ok(NValue::string(result.into()))
        }
        _ => Err(NativeError(
            "String.slice: expected (String, Int, Int)".into(),
        )),
    }
}

pub(super) fn native_string_ends_with(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(suffix)) => Ok(NValue::bool(s.ends_with(&**suffix))),
        _ => Err(NativeError(
            "String.ends_with: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_chars(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => {
            let chars: Vec<NValue> = s
                .chars()
                .map(|c| NValue::string(c.to_string().into()))
                .collect();
            Ok(NValue::list(chars))
        }
        None => Err(NativeError(format!(
            "String.chars: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_char_at(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].is_any_int()) {
        (Some(s), true) => {
            let idx = args[1].as_any_int() as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(NValue::enum_val("Some".into(), NValue::string(c.to_string().into()))),
                None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            }
        }
        _ => Err(NativeError(
            "String.char_at: expected (String, Int)".into(),
        )),
    }
}

pub(super) fn native_string_index_of(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(needle)) => match s.find(&**needle) {
            Some(idx) => Ok(NValue::enum_val("Some".into(), NValue::int(idx as i64))),
            None => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        _ => Err(NativeError(
            "String.index_of: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_to_int(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => match s.trim().parse::<i64>() {
            Ok(n) => Ok(NValue::enum_val("Some".into(), NValue::int(n))),
            Err(_) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        None => Err(NativeError(format!(
            "String.to_int: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_from_char_code(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() {
        let code = args[0].as_any_int() as u32;
        match char::from_u32(code) {
            Some(c) => Ok(NValue::string(c.to_string().into())),
            None => Err(NativeError(format!(
                "String.from_char_code: invalid code point {}",
                code
            ))),
        }
    } else {
        Err(NativeError(format!(
            "String.from_char_code: expected Int, got {}",
            args[0]
        )))
    }
}

pub(super) fn native_string_char_code(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => match s.chars().next() {
            Some(c) => Ok(NValue::int(c as i64)),
            None => Err(NativeError(
                "String.char_code: empty string".into(),
            )),
        },
        None => Err(NativeError(format!(
            "String.char_code: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_string_replace(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string(), args[2].as_string()) {
        (Some(s), Some(old), Some(new)) => {
            Ok(NValue::string(s.replace(&**old, new).into()))
        }
        _ => Err(NativeError(
            "String.replace: expected (String, String, String)".into(),
        )),
    }
}
