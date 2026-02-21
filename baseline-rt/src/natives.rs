//! Native function implementations for AOT-compiled Baseline programs.
//!
//! Each function uses `extern "C"` with uniform signature:
//!   `(args: *const u64, count: u64) -> u64`
//!
//! Args are NaN-boxed NValue bits. Return value is NaN-boxed NValue bits.
//! On error, calls `jit_set_error()` and returns NV_UNIT.

use crate::helpers::{NV_UNIT, jit_arena_push, jit_set_error};
use crate::nvalue::{HeapObject, NValue};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Read args as NValue slice from raw pointer + count.
/// Uses `borrow_from_raw` to bump Arc refcounts so dropping the returned Vec
/// does not free the caller's values.
fn args_from_raw(args: *const u64, count: u64) -> Vec<NValue> {
    let slice = unsafe { std::slice::from_raw_parts(args, count as usize) };
    slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect()
}

/// Push an NValue to the arena and return its raw bits.
fn push_result(val: NValue) -> u64 {
    let bits = val.raw();
    jit_arena_push(val);
    bits
}

/// Return an error: set the error message and return Unit.
fn native_error(msg: String) -> u64 {
    jit_set_error(msg);
    NV_UNIT
}

// ---------------------------------------------------------------------------
// Console
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_console_println(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals.is_empty() {
        println!();
    } else {
        println!("{}", vals[0]);
    }
    NV_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_console_print(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        print!("{}", vals[0]);
    }
    NV_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_console_error(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        eprintln!("{}", vals[0]);
    }
    NV_UNIT
}

// ---------------------------------------------------------------------------
// Log
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_log_info(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        eprintln!("[INFO] {}", vals[0]);
    }
    NV_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_log_warn(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        eprintln!("[WARN] {}", vals[0]);
    }
    NV_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_log_error(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        eprintln!("[ERROR] {}", vals[0]);
    }
    NV_UNIT
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_log_debug(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if !vals.is_empty() {
        eprintln!("[DEBUG] {}", vals[0]);
    }
    NV_UNIT
}

// ---------------------------------------------------------------------------
// Math
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_math_abs(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals[0].is_any_int() {
        NValue::int(vals[0].as_any_int().abs()).raw()
    } else if vals[0].is_float() {
        NValue::float(vals[0].as_float().abs()).raw()
    } else {
        native_error(format!("Math.abs: expected number, got {}", vals[0]))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_math_min(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals[0].is_any_int() && vals[1].is_any_int() {
        NValue::int(vals[0].as_any_int().min(vals[1].as_any_int())).raw()
    } else if vals[0].is_number() && vals[1].is_number() {
        NValue::float(vals[0].as_f64().min(vals[1].as_f64())).raw()
    } else {
        native_error("Math.min: expected two numbers".into())
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_math_max(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals[0].is_any_int() && vals[1].is_any_int() {
        NValue::int(vals[0].as_any_int().max(vals[1].as_any_int())).raw()
    } else if vals[0].is_number() && vals[1].is_number() {
        NValue::float(vals[0].as_f64().max(vals[1].as_f64())).raw()
    } else {
        native_error("Math.max: expected two numbers".into())
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_math_clamp(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals[0].is_any_int() && vals[1].is_any_int() && vals[2].is_any_int() {
        let x = vals[0].as_any_int();
        let lo = vals[1].as_any_int();
        let hi = vals[2].as_any_int();
        NValue::int(x.max(lo).min(hi)).raw()
    } else if vals[0].is_number() && vals[1].is_number() && vals[2].is_number() {
        let x = vals[0].as_f64();
        let lo = vals[1].as_f64();
        let hi = vals[2].as_f64();
        NValue::float(x.max(lo).min(hi)).raw()
    } else {
        native_error("Math.clamp: expected three numbers".into())
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_math_pow(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let (a, b) = (&vals[0], &vals[1]);
    if a.is_any_int() && b.is_any_int() {
        NValue::int((a.as_any_int() as f64).powi(b.as_any_int() as i32) as i64).raw()
    } else if a.is_number() && b.is_number() {
        NValue::float(a.as_f64().powf(b.as_f64())).raw()
    } else {
        native_error("Math.pow: expected two numbers".into())
    }
}

// ---------------------------------------------------------------------------
// String
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_length(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => NValue::int(s.len() as i64).raw(),
        None => native_error(format!("String.length: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_to_upper(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => push_result(NValue::string(s.to_uppercase().into())),
        None => native_error(format!("String.to_upper: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_to_lower(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => push_result(NValue::string(s.to_lowercase().into())),
        None => native_error(format!("String.to_lower: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_trim(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => push_result(NValue::string(s.trim().into())),
        None => native_error(format!("String.trim: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_contains(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(s), Some(sub)) => NValue::bool(s.contains(&**sub)).raw(),
        _ => native_error("String.contains: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_starts_with(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(s), Some(prefix)) => NValue::bool(s.starts_with(&**prefix)).raw(),
        _ => native_error("String.starts_with: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_ends_with(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(s), Some(suffix)) => NValue::bool(s.ends_with(&**suffix)).raw(),
        _ => native_error("String.ends_with: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_split(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(s), Some(delim)) => {
            let parts: Vec<NValue> = s
                .split(&**delim)
                .map(|p| NValue::string(p.into()))
                .collect();
            push_result(NValue::list(parts))
        }
        _ => native_error("String.split: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_join(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_list(), vals[1].as_string()) {
        (Some(items), Some(sep)) => {
            let strs: Vec<String> = items.iter().map(|v| v.to_string()).collect();
            push_result(NValue::string(strs.join(&**sep).into()))
        }
        _ => native_error("String.join: expected (List, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_slice(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (
        vals[0].as_string(),
        vals[1].is_any_int(),
        vals[2].is_any_int(),
    ) {
        (Some(s), true, true) => {
            let start = vals[1].as_any_int() as usize;
            let len = vals[2].as_any_int() as usize;
            let result: String = s.chars().skip(start).take(len).collect();
            push_result(NValue::string(result.into()))
        }
        _ => native_error("String.slice: expected (String, Int, Int)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_chars(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => {
            let chars: Vec<NValue> = s
                .chars()
                .map(|c| NValue::string(c.to_string().into()))
                .collect();
            push_result(NValue::list(chars))
        }
        None => native_error(format!("String.chars: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_char_at(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].is_any_int()) {
        (Some(s), true) => {
            let idx = vals[1].as_any_int() as usize;
            let result = match s.chars().nth(idx) {
                Some(c) => NValue::enum_val("Some".into(), NValue::string(c.to_string().into())),
                None => NValue::enum_val("None".into(), NValue::unit()),
            };
            push_result(result)
        }
        _ => native_error("String.char_at: expected (String, Int)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_index_of(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(s), Some(needle)) => {
            let result = match s.find(&**needle) {
                Some(idx) => NValue::enum_val("Some".into(), NValue::int(idx as i64)),
                None => NValue::enum_val("None".into(), NValue::unit()),
            };
            push_result(result)
        }
        _ => native_error("String.index_of: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_to_int(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => {
            let result = match s.trim().parse::<i64>() {
                Ok(n) => NValue::enum_val("Some".into(), NValue::int(n)),
                Err(_) => NValue::enum_val("None".into(), NValue::unit()),
            };
            push_result(result)
        }
        None => native_error(format!("String.to_int: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_from_char_code(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    if vals[0].is_any_int() {
        let code = vals[0].as_any_int() as u32;
        match char::from_u32(code) {
            Some(c) => push_result(NValue::string(c.to_string().into())),
            None => native_error(format!(
                "String.from_char_code: invalid code point {}",
                code
            )),
        }
    } else {
        native_error(format!(
            "String.from_char_code: expected Int, got {}",
            vals[0]
        ))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_string_char_code(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => match s.chars().next() {
            Some(c) => NValue::int(c as i64).raw(),
            None => native_error("String.char_code: empty string".into()),
        },
        None => native_error(format!(
            "String.char_code: expected String, got {}",
            vals[0]
        )),
    }
}

// ---------------------------------------------------------------------------
// Int
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_int_to_string(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    push_result(NValue::string(vals[0].to_string().into()))
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_int_parse(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(s) => {
            let result = match s.parse::<i64>() {
                Ok(n) => NValue::enum_val("Ok".into(), NValue::int(n)),
                Err(_) => NValue::enum_val(
                    "Err".into(),
                    NValue::string(format!("Cannot parse '{}' as Int", s).into()),
                ),
            };
            push_result(result)
        }
        None => native_error(format!("Int.parse: expected String, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// List (non-HOF)
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_length(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => NValue::int(items.len() as i64).raw(),
        None => native_error(format!("List.length: expected List, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_head(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let result = if let Some(first) = items.first() {
                NValue::enum_val("Some".into(), first.clone())
            } else {
                NValue::enum_val("None".into(), NValue::unit())
            };
            push_result(result)
        }
        None => native_error(format!("List.head: expected List, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_tail(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let result = if items.is_empty() {
                NValue::list(Vec::new())
            } else {
                NValue::list(items[1..].to_vec())
            };
            push_result(result)
        }
        None => native_error(format!("List.tail: expected List, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_reverse(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let mut reversed = items.clone();
            reversed.reverse();
            push_result(NValue::list(reversed))
        }
        None => native_error(format!("List.reverse: expected List, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_sort(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let mut sorted = items.clone();
            sorted.sort_by(|a, b| {
                if a.is_any_int() && b.is_any_int() {
                    a.as_any_int().cmp(&b.as_any_int())
                } else if a.is_float() && b.is_float() {
                    a.as_float()
                        .partial_cmp(&b.as_float())
                        .unwrap_or(std::cmp::Ordering::Equal)
                } else if let (Some(x), Some(y)) = (a.as_string(), b.as_string()) {
                    x.cmp(y)
                } else {
                    std::cmp::Ordering::Equal
                }
            });
            push_result(NValue::list(sorted))
        }
        None => native_error(format!("List.sort: expected List, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_concat_native(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_list(), vals[1].as_list()) {
        (Some(a), Some(b)) => {
            let mut result = a.clone();
            result.extend(b.iter().cloned());
            push_result(NValue::list(result))
        }
        _ => native_error("List.concat: expected (List, List)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_list_contains(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => NValue::bool(items.contains(&vals[1])).raw(),
        None => native_error(format!("List.contains: expected List, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// Option
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_option_unwrap(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => {
            let bits = payload.raw();
            jit_arena_push(payload.clone());
            bits
        }
        Some((tag, _)) if &**tag == "None" => native_error("Option.unwrap: called on None".into()),
        _ => native_error(format!("Option.unwrap: expected Option, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_option_unwrap_or(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => {
            let bits = payload.raw();
            jit_arena_push(payload.clone());
            bits
        }
        Some((tag, _)) if &**tag == "None" => vals[1].raw(),
        _ => native_error(format!(
            "Option.unwrap_or: expected Option, got {}",
            vals[0]
        )),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_option_is_some(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, _)) => NValue::bool(&**tag == "Some").raw(),
        None => native_error(format!("Option.is_some: expected Option, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_option_is_none(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, _)) => NValue::bool(&**tag == "None").raw(),
        None => native_error(format!("Option.is_none: expected Option, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// Result
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_result_unwrap(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => {
            let bits = payload.raw();
            jit_arena_push(payload.clone());
            bits
        }
        Some((tag, payload)) if &**tag == "Err" => {
            native_error(format!("Result.unwrap: called on Err({})", payload))
        }
        _ => native_error(format!("Result.unwrap: expected Result, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_result_unwrap_or(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => {
            let bits = payload.raw();
            jit_arena_push(payload.clone());
            bits
        }
        Some((tag, _)) if &**tag == "Err" => vals[1].raw(),
        _ => native_error(format!(
            "Result.unwrap_or: expected Result, got {}",
            vals[0]
        )),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_result_is_ok(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, _)) => NValue::bool(&**tag == "Ok").raw(),
        None => native_error(format!("Result.is_ok: expected Result, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_result_is_err(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_enum() {
        Some((tag, _)) => NValue::bool(&**tag == "Err").raw(),
        None => native_error(format!("Result.is_err: expected Result, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// Time
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_time_now(_args: *const u64, _count: u64) -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    NValue::int(ms).raw()
}

// ---------------------------------------------------------------------------
// Fs
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_fs_read_file(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(path) => match std::fs::read_to_string(&**path) {
            Ok(content) => push_result(NValue::string(content.into())),
            Err(e) => native_error(format!("Fs.read_file!: failed for \"{}\": {}", path, e)),
        },
        None => native_error(format!("Fs.read_file!: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_fs_write_file(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_string(), vals[1].as_string()) {
        (Some(path), Some(content)) => match std::fs::write(&**path, &**content) {
            Ok(()) => NV_UNIT,
            Err(e) => native_error(format!("Fs.write_file!: failed for \"{}\": {}", path, e)),
        },
        _ => native_error("Fs.write_file!: expected (String, String)".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_fs_exists(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(path) => NValue::bool(std::path::Path::new(&**path).exists()).raw(),
        None => native_error(format!("Fs.exists!: expected String, got {}", vals[0])),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_fs_list_dir(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_string() {
        Some(path) => match std::fs::read_dir(&**path) {
            Ok(entries) => {
                let mut names: Vec<NValue> = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(e) => {
                            let name = e.file_name().to_string_lossy().to_string();
                            names.push(NValue::string(name.into()));
                        }
                        Err(e) => {
                            return native_error(format!(
                                "Fs.list_dir!: error reading entry: {}",
                                e
                            ));
                        }
                    }
                }
                push_result(NValue::list(names))
            }
            Err(e) => native_error(format!("Fs.list_dir!: failed for \"{}\": {}", path, e)),
        },
        None => native_error(format!("Fs.list_dir!: expected String, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// Map
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_empty(_args: *const u64, _count: u64) -> u64 {
    push_result(NValue::map_from_hashmap(std::collections::HashMap::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_insert(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let key = vals[1].clone();
    let val = vals[2].clone();
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => {
            let mut new_map = entries.clone();
            new_map.insert(key, val);
            push_result(NValue::map_from_hashmap(new_map))
        }
        _ => native_error("Map.insert: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_get(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let key = &vals[1];
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => match entries.get(key) {
            Some(v) => push_result(NValue::enum_val("Some".into(), v.clone())),
            None => push_result(NValue::enum_val("None".into(), NValue::unit())),
        },
        _ => native_error("Map.get: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_remove(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let key = &vals[1];
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => {
            let mut new_map = entries.clone();
            new_map.remove(key);
            push_result(NValue::map_from_hashmap(new_map))
        }
        _ => native_error("Map.remove: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_contains(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let key = &vals[1];
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => NValue::bool(entries.contains_key(key)).raw(),
        _ => native_error("Map.contains: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_keys(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => {
            let keys: Vec<_> = entries.keys().cloned().collect();
            push_result(NValue::list(keys))
        }
        _ => native_error("Map.keys: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_values(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => {
            let vals: Vec<_> = entries.values().cloned().collect();
            push_result(NValue::list(vals))
        }
        _ => native_error("Map.values: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_len(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_heap_ref() {
        HeapObject::Map(entries) => NValue::int(entries.len() as i64).raw(),
        _ => native_error("Map.len: expected Map".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_map_from_list(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let mut entries = std::collections::HashMap::new();
            for item in items {
                if let Some(tuple) = item.as_tuple() {
                    if tuple.len() >= 2 {
                        entries.insert(tuple[0].clone(), tuple[1].clone());
                    } else {
                        return native_error(
                            "Map.from_list: each element must be a (key, value) pair".into(),
                        );
                    }
                } else {
                    return native_error(
                        "Map.from_list: each element must be a (key, value) tuple".into(),
                    );
                }
            }
            push_result(NValue::map_from_hashmap(entries))
        }
        None => native_error(format!("Map.from_list: expected List, got {}", vals[0])),
    }
}

// ---------------------------------------------------------------------------
// Set
// ---------------------------------------------------------------------------

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_empty(_args: *const u64, _count: u64) -> u64 {
    push_result(NValue::set(Vec::new()))
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_insert(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let elem = vals[1].clone();
    match vals[0].as_heap_ref() {
        HeapObject::Set(elems) => {
            if elems.contains(&elem) {
                push_result(NValue::set(elems.clone()))
            } else {
                let mut new_elems = elems.clone();
                new_elems.push(elem);
                push_result(NValue::set(new_elems))
            }
        }
        _ => native_error("Set.insert: expected Set".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_remove(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let elem = &vals[1];
    match vals[0].as_heap_ref() {
        HeapObject::Set(elems) => {
            let new_elems: Vec<_> = elems.iter().filter(|e| *e != elem).cloned().collect();
            push_result(NValue::set(new_elems))
        }
        _ => native_error("Set.remove: expected Set".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_contains(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    let elem = &vals[1];
    match vals[0].as_heap_ref() {
        HeapObject::Set(elems) => NValue::bool(elems.contains(elem)).raw(),
        _ => native_error("Set.contains: expected Set".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_union(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_heap_ref(), vals[1].as_heap_ref()) {
        (HeapObject::Set(a), HeapObject::Set(b)) => {
            let mut result = a.clone();
            for elem in b {
                if !result.contains(elem) {
                    result.push(elem.clone());
                }
            }
            push_result(NValue::set(result))
        }
        _ => native_error("Set.union: expected two Sets".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_intersection(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match (vals[0].as_heap_ref(), vals[1].as_heap_ref()) {
        (HeapObject::Set(a), HeapObject::Set(b)) => {
            let result: Vec<_> = a.iter().filter(|e| b.contains(e)).cloned().collect();
            push_result(NValue::set(result))
        }
        _ => native_error("Set.intersection: expected two Sets".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_len(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_heap_ref() {
        HeapObject::Set(elems) => NValue::int(elems.len() as i64).raw(),
        _ => native_error("Set.len: expected Set".into()),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn bl_set_from_list(args: *const u64, count: u64) -> u64 {
    let vals = args_from_raw(args, count);
    match vals[0].as_list() {
        Some(items) => {
            let mut result = Vec::new();
            for item in items {
                if !result.contains(item) {
                    result.push(item.clone());
                }
            }
            push_result(NValue::set(result))
        }
        None => native_error(format!("Set.from_list: expected List, got {}", vals[0])),
    }
}
