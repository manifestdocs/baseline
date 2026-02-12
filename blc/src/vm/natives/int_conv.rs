use super::{NValue, NativeError};

pub(super) fn native_int_to_string(args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::string(args[0].to_string().into()))
}

pub(super) fn native_int_parse(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => match s.parse::<i64>() {
            Ok(n) => Ok(NValue::enum_val("Ok".into(), NValue::int(n))),
            Err(_) => Ok(NValue::enum_val(
                "Err".into(),
                NValue::string(format!("Cannot parse '{}' as Int", s).into()),
            )),
        },
        None => Err(NativeError(format!(
            "Int.parse: expected String, got {}",
            args[0]
        ))),
    }
}
