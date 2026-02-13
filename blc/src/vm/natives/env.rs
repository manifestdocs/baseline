use std::cell::RefCell;

use super::{NValue, NativeError};

// ---------------------------------------------------------------------------
// Program arguments -- set from CLI before VM execution
// ---------------------------------------------------------------------------

thread_local! {
    static PROGRAM_ARGS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

/// Set the program arguments that `Env.args!` will return.
/// Call this from the CLI before launching the VM.
pub fn set_program_args(args: Vec<String>) {
    PROGRAM_ARGS.with(|cell| {
        *cell.borrow_mut() = args;
    });
}

/// Native: `Env.args!() -> List<String>`
///
/// Returns the command-line arguments passed after `--` to `blc run`.
pub(super) fn native_env_args(_args: &[NValue]) -> Result<NValue, NativeError> {
    let items = PROGRAM_ARGS.with(|cell| {
        cell.borrow()
            .iter()
            .map(|s| NValue::string(s.clone().into()))
            .collect::<Vec<_>>()
    });
    Ok(NValue::list(items))
}

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
