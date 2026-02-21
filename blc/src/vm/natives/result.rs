use super::{NValue, NativeError};

pub(super) fn native_result_unwrap(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => Ok(payload.clone()),
        Some((tag, payload)) if &**tag == "Err" => Err(NativeError(format!(
            "Result.unwrap: called on Err({})",
            payload
        ))),
        _ => Err(NativeError(format!(
            "Result.unwrap: expected Result, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_result_unwrap_or(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "Err" => Ok(args[1].clone()),
        _ => Err(NativeError(format!(
            "Result.unwrap_or: expected Result, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_result_is_ok(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Ok")),
        None => Err(NativeError(format!(
            "Result.is_ok: expected Result, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_result_is_err(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Err")),
        None => Err(NativeError(format!(
            "Result.is_err: expected Result, got {}",
            args[0]
        ))),
    }
}

/// Result.ensure(condition, err) — if condition is true, Ok(()), else Err(err)
pub(super) fn native_result_ensure(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError("Result.ensure: expected 2 arguments".into()));
    }
    if args[0].is_truthy() {
        Ok(NValue::enum_val("Ok".into(), NValue::unit()))
    } else {
        Ok(NValue::enum_val("Err".into(), args[1].clone()))
    }
}

/// Result.context(res, msg) — if Err(e), wraps as Err({ error: e, context: msg })
pub(super) fn native_result_context(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError("Result.context: expected 2 arguments".into()));
    }
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => {
            Ok(NValue::enum_val("Ok".into(), payload.clone()))
        }
        Some((tag, payload)) if &**tag == "Err" => {
            let fields = vec![
                ("error".into(), payload.clone()),
                ("context".into(), args[1].clone()),
            ];
            Ok(NValue::enum_val("Err".into(), NValue::record(fields)))
        }
        _ => Err(NativeError(format!(
            "Result.context: expected Result, got {}",
            args[0]
        ))),
    }
}
