use super::{NValue, NativeError};

pub(super) fn native_math_abs(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() {
        Ok(NValue::int(args[0].as_any_int().abs()))
    } else if args[0].is_float() {
        Ok(NValue::float(args[0].as_float().abs()))
    } else {
        Err(NativeError(format!(
            "Math.abs: expected number, got {}",
            args[0]
        )))
    }
}

pub(super) fn native_math_min(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() && args[1].is_any_int() {
        Ok(NValue::int(args[0].as_any_int().min(args[1].as_any_int())))
    } else if args[0].is_number() && args[1].is_number() {
        Ok(NValue::float(args[0].as_f64().min(args[1].as_f64())))
    } else {
        Err(NativeError("Math.min: expected two numbers".into()))
    }
}

pub(super) fn native_math_max(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() && args[1].is_any_int() {
        Ok(NValue::int(args[0].as_any_int().max(args[1].as_any_int())))
    } else if args[0].is_number() && args[1].is_number() {
        Ok(NValue::float(args[0].as_f64().max(args[1].as_f64())))
    } else {
        Err(NativeError("Math.max: expected two numbers".into()))
    }
}

pub(super) fn native_math_clamp(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() && args[1].is_any_int() && args[2].is_any_int() {
        let x = args[0].as_any_int();
        let lo = args[1].as_any_int();
        let hi = args[2].as_any_int();
        Ok(NValue::int(x.max(lo).min(hi)))
    } else if args[0].is_number() && args[1].is_number() && args[2].is_number() {
        let x = args[0].as_f64();
        let lo = args[1].as_f64();
        let hi = args[2].as_f64();
        Ok(NValue::float(x.max(lo).min(hi)))
    } else {
        Err(NativeError("Math.clamp: expected three numbers".into()))
    }
}

pub(super) fn native_math_pow(args: &[NValue]) -> Result<NValue, NativeError> {
    let a = &args[0];
    let b = &args[1];
    if a.is_any_int() && b.is_any_int() {
        Ok(NValue::int(
            (a.as_any_int() as f64).powi(b.as_any_int() as i32) as i64,
        ))
    } else if a.is_number() && b.is_number() {
        Ok(NValue::float(a.as_f64().powf(b.as_f64())))
    } else {
        Err(NativeError("Math.pow: expected two numbers".into()))
    }
}
