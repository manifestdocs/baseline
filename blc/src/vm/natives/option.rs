use super::{NValue, NativeError};

pub(super) fn native_option_unwrap(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "None" => {
            Err(NativeError("Option.unwrap: called on None".into()))
        }
        _ => Err(NativeError(format!(
            "Option.unwrap: expected Option, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_option_unwrap_or(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "None" => Ok(args[1].clone()),
        _ => Err(NativeError(format!(
            "Option.unwrap_or: expected Option, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_option_is_some(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Some")),
        None => Err(NativeError(format!(
            "Option.is_some: expected Option, got {}",
            args[0]
        ))),
    }
}

/// Option.ok_or(opt, err) — Some(x) -> Ok(x), None -> Err(err)
pub(super) fn native_option_ok_or(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError("Option.ok_or: expected 2 arguments".into()));
    }
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => {
            Ok(NValue::enum_val("Ok".into(), payload.clone()))
        }
        Some((tag, _)) if &**tag == "None" => Ok(NValue::enum_val("Err".into(), args[1].clone())),
        _ => Err(NativeError(format!(
            "Option.ok_or: expected Option, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_option_is_none(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "None")),
        None => Err(NativeError(format!(
            "Option.is_none: expected Option, got {}",
            args[0]
        ))),
    }
}
