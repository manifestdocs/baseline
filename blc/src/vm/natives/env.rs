use std::cell::RefCell;
use std::collections::HashMap;

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

// ---------------------------------------------------------------------------
// Thread-local environment override map
// ---------------------------------------------------------------------------
//
// `Env.set!` writes here instead of calling `std::env::set_var` (which is
// unsafe in multi-threaded contexts). `Env.get!` checks this map first,
// falling back to `std::env::var` for keys not overridden.

thread_local! {
    static ENV_OVERRIDES: RefCell<HashMap<String, String>> = RefCell::new(HashMap::new());
}

pub(super) fn native_env_get(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(key) => {
            // Check thread-local overrides first
            let override_val = ENV_OVERRIDES.with(|cell| cell.borrow().get(&**key).cloned());
            let val = override_val.or_else(|| std::env::var(&**key).ok());
            match val {
                Some(v) => Ok(NValue::enum_val("Some".into(), NValue::string(v.into()))),
                None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            }
        }
        None => Err(NativeError(format!(
            "Env.get!: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_env_set(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(key), Some(val)) => {
            ENV_OVERRIDES.with(|cell| {
                cell.borrow_mut().insert(key.to_string(), val.to_string());
            });
            Ok(NValue::unit())
        }
        _ => Err(NativeError("Env.set!: expected (String, String)".into())),
    }
}
