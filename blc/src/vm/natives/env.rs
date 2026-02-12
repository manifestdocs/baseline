use super::{NValue, NativeError};

pub(super) fn native_env_get(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(key) => match std::env::var(&**key) {
            Ok(val) => Ok(NValue::enum_val("Some".into(), NValue::string(val.into()))),
            Err(_) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        None => Err(NativeError(format!(
            "Env.get!: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_env_set(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(key), Some(val)) => {
            // SAFETY: We control the single-threaded VM execution context.
            unsafe { std::env::set_var(&**key, &**val) };
            Ok(NValue::unit())
        }
        _ => Err(NativeError(
            "Env.set!: expected (String, String)".into(),
        )),
    }
}
