use super::{NValue, NativeError};

pub(super) fn native_log_info(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[INFO] {}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_log_warn(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[WARN] {}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_log_error(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[ERROR] {}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_log_debug(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[DEBUG] {}", args[0]);
    }
    Ok(NValue::unit())
}
