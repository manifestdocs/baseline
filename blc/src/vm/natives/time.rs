use super::{NValue, NativeError};

pub(super) fn native_time_now(_args: &[NValue]) -> Result<NValue, NativeError> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    Ok(NValue::int(ms))
}

pub(super) fn native_time_sleep(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_any_int() {
        return Err(NativeError(format!(
            "Time.sleep!: expected Int (milliseconds), got {}",
            args[0]
        )));
    }
    let ms = args[0].as_any_int().max(0) as u64;
    std::thread::sleep(std::time::Duration::from_millis(ms));
    Ok(NValue::unit())
}
