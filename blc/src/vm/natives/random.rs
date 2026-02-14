use std::sync::Arc;

use super::{NValue, NativeError};

pub(super) fn native_random_int(args: &[NValue]) -> Result<NValue, NativeError> {
    use rand::Rng;
    if args.len() != 2 || !args[0].is_any_int() || !args[1].is_any_int() {
        return Err(NativeError(
            "Random.int!: expected (Int, Int)".into(),
        ));
    }
    let lo = args[0].as_any_int();
    let hi = args[1].as_any_int();
    if lo > hi {
        return Err(NativeError(format!(
            "Random.int!: min ({}) > max ({})",
            lo, hi
        )));
    }
    let n = rand::thread_rng().gen_range(lo..=hi);
    Ok(NValue::int(n))
}

pub(super) fn native_random_bool(_args: &[NValue]) -> Result<NValue, NativeError> {
    use rand::Rng;
    Ok(NValue::bool(rand::thread_rng().gen_bool(0.5)))
}

pub(super) fn native_random_uuid(_args: &[NValue]) -> Result<NValue, NativeError> {
    use rand::Rng;
    let mut bytes = [0u8; 16];
    rand::thread_rng().fill(&mut bytes);
    // Set version 4 (bits 12-15 of time_hi_and_version)
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    // Set variant 1 (bits 6-7 of clock_seq_hi_and_reserved)
    bytes[8] = (bytes[8] & 0x3f) | 0x80;
    let uuid = format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        bytes[0], bytes[1], bytes[2], bytes[3],
        bytes[4], bytes[5],
        bytes[6], bytes[7],
        bytes[8], bytes[9],
        bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
    );
    Ok(NValue::string(Arc::from(uuid.as_str())))
}
