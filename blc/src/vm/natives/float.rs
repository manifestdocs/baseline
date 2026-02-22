use super::{NValue, NativeError};

pub(super) fn native_float_from_int(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() {
        Ok(NValue::float(args[0].as_any_int() as f64))
    } else {
        Err(NativeError(format!(
            "Float.from_int: expected Int, got {}",
            args[0]
        )))
    }
}

pub(super) fn native_int_from_float(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_float() {
        Ok(NValue::int(args[0].as_float() as i64))
    } else if args[0].is_any_int() {
        // Already an int, return as-is
        Ok(NValue::int(args[0].as_any_int()))
    } else {
        Err(NativeError(format!(
            "Int.from_float: expected Float, got {}",
            args[0]
        )))
    }
}

pub(super) fn native_float_format(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_number() {
        return Err(NativeError(format!(
            "Float.format: expected number as first argument, got {}",
            args[0]
        )));
    }
    if !args[1].is_any_int() {
        return Err(NativeError(format!(
            "Float.format: expected Int as second argument, got {}",
            args[1]
        )));
    }
    let value = args[0].as_f64();
    let decimals = args[1].as_any_int() as usize;
    Ok(NValue::string(format!("{:.prec$}", value, prec = decimals).into()))
}
