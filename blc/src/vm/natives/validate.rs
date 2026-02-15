use super::{NValue, NativeError, RcStr};

fn validation_err(field: &str, message: &str) -> NValue {
    let msg: RcStr = format!("{}: {}", field, message).into();
    NValue::enum_val(
        "Err".into(),
        NValue::enum_val("Unprocessable".into(), NValue::string(msg)),
    )
}

/// Validate.required(val, field) -> Result<Unknown, HttpError>
/// Returns Err if value is None enum variant.
pub(super) fn native_validate_required(args: &[NValue]) -> Result<NValue, NativeError> {
    let field = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Validate.required: field name must be String".into()))?;
    // Check if the value is a None enum variant
    if let Some((tag, _)) = args[0].as_enum() {
        if tag.as_ref() == "None" {
            return Ok(validation_err(field, "is required"));
        }
    }
    Ok(NValue::enum_val("Ok".into(), args[0].clone()))
}

/// Validate.string(val, field) -> Result<String, HttpError>
pub(super) fn native_validate_string(args: &[NValue]) -> Result<NValue, NativeError> {
    let field = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Validate.string: field name must be String".into()))?;
    match args[0].as_string() {
        Some(_) => Ok(NValue::enum_val("Ok".into(), args[0].clone())),
        None => Ok(validation_err(field, "must be a string")),
    }
}

/// Validate.int(val, field) -> Result<Int, HttpError>
pub(super) fn native_validate_int(args: &[NValue]) -> Result<NValue, NativeError> {
    let field = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Validate.int: field name must be String".into()))?;
    if args[0].is_any_int() {
        return Ok(NValue::enum_val("Ok".into(), args[0].clone()));
    }
    // Try parsing string as int
    if let Some(s) = args[0].as_string() {
        if let Ok(n) = s.parse::<i64>() {
            return Ok(NValue::enum_val("Ok".into(), NValue::int(n)));
        }
    }
    Ok(validation_err(field, "must be an integer"))
}

/// Validate.boolean(val, field) -> Result<Boolean, HttpError>
pub(super) fn native_validate_boolean(args: &[NValue]) -> Result<NValue, NativeError> {
    let field = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Validate.boolean: field name must be String".into()))?;
    if args[0].is_bool() {
        return Ok(NValue::enum_val("Ok".into(), args[0].clone()));
    }
    Ok(validation_err(field, "must be a boolean"))
}

/// Validate.min_length(str, min, field) -> Result<String, HttpError>
pub(super) fn native_validate_min_length(args: &[NValue]) -> Result<NValue, NativeError> {
    let s = args[0]
        .as_string()
        .ok_or_else(|| NativeError("Validate.min_length: first arg must be String".into()))?;
    let min = args[1].as_any_int();
    let field = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Validate.min_length: field name must be String".into()))?;
    if (s.len() as i64) < min {
        Ok(validation_err(
            field,
            &format!("must be at least {} characters", min),
        ))
    } else {
        Ok(NValue::enum_val("Ok".into(), args[0].clone()))
    }
}

/// Validate.max_length(str, max, field) -> Result<String, HttpError>
pub(super) fn native_validate_max_length(args: &[NValue]) -> Result<NValue, NativeError> {
    let s = args[0]
        .as_string()
        .ok_or_else(|| NativeError("Validate.max_length: first arg must be String".into()))?;
    let max = args[1].as_any_int();
    let field = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Validate.max_length: field name must be String".into()))?;
    if (s.len() as i64) > max {
        Ok(validation_err(
            field,
            &format!("must be at most {} characters", max),
        ))
    } else {
        Ok(NValue::enum_val("Ok".into(), args[0].clone()))
    }
}

/// Validate.min(num, min, field) -> Result<Int, HttpError>
pub(super) fn native_validate_min(args: &[NValue]) -> Result<NValue, NativeError> {
    let val = args[0].as_any_int();
    let min = args[1].as_any_int();
    let field = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Validate.min: field name must be String".into()))?;
    if val < min {
        Ok(validation_err(
            field,
            &format!("must be at least {}", min),
        ))
    } else {
        Ok(NValue::enum_val("Ok".into(), args[0].clone()))
    }
}

/// Validate.max(num, max, field) -> Result<Int, HttpError>
pub(super) fn native_validate_max(args: &[NValue]) -> Result<NValue, NativeError> {
    let val = args[0].as_any_int();
    let max = args[1].as_any_int();
    let field = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Validate.max: field name must be String".into()))?;
    if val > max {
        Ok(validation_err(
            field,
            &format!("must be at most {}", max),
        ))
    } else {
        Ok(NValue::enum_val("Ok".into(), args[0].clone()))
    }
}

/// Validate.one_of(val, options, field) -> Result<String, HttpError>
pub(super) fn native_validate_one_of(args: &[NValue]) -> Result<NValue, NativeError> {
    let val = args[0]
        .as_string()
        .ok_or_else(|| NativeError("Validate.one_of: first arg must be String".into()))?;
    let options = args[1]
        .as_list()
        .ok_or_else(|| NativeError("Validate.one_of: second arg must be List".into()))?;
    let field = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Validate.one_of: field name must be String".into()))?;

    for opt in options {
        if let Some(s) = opt.as_string() {
            if s.as_ref() == val.as_ref() {
                return Ok(NValue::enum_val("Ok".into(), args[0].clone()));
            }
        }
    }

    let opt_strs: Vec<String> = options
        .iter()
        .filter_map(|o| o.as_string().map(|s| s.to_string()))
        .collect();
    Ok(validation_err(
        field,
        &format!("must be one of: {}", opt_strs.join(", ")),
    ))
}
