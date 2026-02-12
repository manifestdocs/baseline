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
