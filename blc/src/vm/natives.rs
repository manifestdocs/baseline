use std::collections::HashMap;

use super::nvalue::{HeapObject, NValue};
use super::value::RcStr;

// ---------------------------------------------------------------------------
// Native Function Registry
// ---------------------------------------------------------------------------

/// Error from a native function call.
#[derive(Debug, Clone)]
pub struct NativeError(pub String);

/// A native function: takes NValue args, returns an NValue.
type SimpleFn = fn(&[NValue]) -> Result<NValue, NativeError>;

/// A native function entry.
struct NativeEntry {
    pub name: &'static str,
    pub func: SimpleFn,
    /// Expected argument count (None = variadic/unchecked).
    pub arity: Option<u8>,
}

/// Registry of native functions callable from bytecode via CallNative.
pub struct NativeRegistry {
    entries: Vec<NativeEntry>,
    /// Name → index for O(1) lookup during compilation.
    name_index: HashMap<&'static str, u16>,
}

impl Default for NativeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl NativeRegistry {
    pub fn new() -> Self {
        let mut registry = NativeRegistry {
            entries: Vec::new(),
            name_index: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    /// Look up a native function ID by qualified name (e.g., "Console.println!").
    pub fn lookup(&self, name: &str) -> Option<u16> {
        self.name_index.get(name).copied()
    }

    /// Call a native function by ID.
    pub fn call(&self, id: u16, args: &[NValue]) -> Result<NValue, NativeError> {
        let entry = self.entries.get(id as usize).ok_or_else(|| {
            NativeError(format!("Invalid native function id: {}", id))
        })?;
        if let Some(arity) = entry.arity
            && args.len() != arity as usize
        {
            return Err(NativeError(format!(
                "{}: expected {} argument(s), got {}",
                entry.name, arity, args.len()
            )));
        }
        (entry.func)(args)
    }

    /// Check if a function is a HOF (returns true for List.map, List.filter, etc.).
    pub fn is_hof(&self, id: u16) -> bool {
        self.entries
            .get(id as usize)
            .map(|e| {
                matches!(
                    e.name,
                    "List.map"
                        | "List.filter"
                        | "List.fold"
                        | "List.find"
                        | "Option.map"
                        | "Result.map"
                )
            })
            .unwrap_or(false)
    }

    /// Check if a function requires VM re-entrancy (server dispatch).
    pub fn is_server_listen(&self, id: u16) -> bool {
        self.entries
            .get(id as usize)
            .map(|e| matches!(e.name, "Server.listen!" | "Server.listen"))
            .unwrap_or(false)
    }

    /// Get the function name by ID (for error messages).
    pub fn name(&self, id: u16) -> &str {
        self.entries
            .get(id as usize)
            .map(|e| e.name)
            .unwrap_or("<unknown>")
    }

    fn register(&mut self, name: &'static str, func: SimpleFn) {
        let idx = self.entries.len() as u16;
        self.entries.push(NativeEntry {
            name,
            func,
            arity: None,
        });
        self.name_index.insert(name, idx);
    }

    #[allow(dead_code)]
    fn register_with_arity(&mut self, name: &'static str, func: SimpleFn, arity: u8) {
        let idx = self.entries.len() as u16;
        self.entries.push(NativeEntry {
            name,
            func,
            arity: Some(arity),
        });
        self.name_index.insert(name, idx);
    }

    fn register_all(&mut self) {
        // -- Console --
        // Register both with and without `!` since grammar may parse either way
        self.register("Console.println!", native_console_println);
        self.register("Console.println", native_console_println);
        self.register("Console.print!", native_console_print);
        self.register("Console.print", native_console_print);
        self.register("Console.error!", native_console_error);
        self.register("Console.error", native_console_error);

        // -- Log --
        self.register("Log.info!", native_log_info);
        self.register("Log.info", native_log_info);
        self.register("Log.warn!", native_log_warn);
        self.register("Log.warn", native_log_warn);
        self.register("Log.error!", native_log_error);
        self.register("Log.error", native_log_error);
        self.register("Log.debug!", native_log_debug);
        self.register("Log.debug", native_log_debug);

        // -- Math --
        self.register("Math.abs", native_math_abs);
        self.register("Math.min", native_math_min);
        self.register("Math.max", native_math_max);
        self.register("Math.clamp", native_math_clamp);
        self.register("Math.pow", native_math_pow);

        // -- String --
        self.register("String.length", native_string_length);
        self.register("String.to_upper", native_string_to_upper);
        self.register("String.to_lower", native_string_to_lower);
        self.register("String.trim", native_string_trim);
        self.register("String.contains", native_string_contains);
        self.register("String.starts_with", native_string_starts_with);
        self.register("String.split", native_string_split);
        self.register("String.join", native_string_join);
        self.register("String.slice", native_string_slice);

        // -- List (non-HOF) --
        self.register("List.length", native_list_length);
        self.register("List.head", native_list_head);
        self.register("List.tail", native_list_tail);
        self.register("List.reverse", native_list_reverse);
        self.register("List.sort", native_list_sort);
        self.register("List.concat", native_list_concat);

        // -- List (HOF) — these are placeholders; actual execution handled by VM --
        self.register("List.map", native_hof_placeholder);
        self.register("List.filter", native_hof_placeholder);
        self.register("List.fold", native_hof_placeholder);
        self.register("List.find", native_hof_placeholder);

        // -- Option --
        self.register("Option.unwrap", native_option_unwrap);
        self.register("Option.unwrap_or", native_option_unwrap_or);
        self.register("Option.is_some", native_option_is_some);
        self.register("Option.is_none", native_option_is_none);
        self.register("Option.map", native_hof_placeholder);

        // -- Result --
        self.register("Result.unwrap", native_result_unwrap);
        self.register("Result.unwrap_or", native_result_unwrap_or);
        self.register("Result.is_ok", native_result_is_ok);
        self.register("Result.is_err", native_result_is_err);
        self.register("Result.map", native_hof_placeholder);

        // -- Time --
        self.register("Time.now!", native_time_now);
        self.register("Time.now", native_time_now);

        // -- Int/String conversion --
        self.register("Int.to_string", native_int_to_string);
        self.register("Int.parse", native_int_parse);

        // -- Json --
        self.register("Json.parse", native_json_parse);
        self.register("Json.to_string", native_json_to_string);
        self.register("Json.to_string_pretty", native_json_to_string_pretty);

        // -- Response --
        self.register("Response.ok", native_response_ok);
        self.register("Response.json", native_response_json);
        self.register("Response.created", native_response_created);
        self.register("Response.no_content", native_response_no_content);
        self.register("Response.bad_request", native_response_bad_request);
        self.register("Response.not_found", native_response_not_found);
        self.register("Response.error", native_response_error);
        self.register("Response.status", native_response_status);
        self.register("Response.with_header", native_response_with_header);
        self.register("Response.with_headers", native_response_with_headers);
        self.register("Response.redirect", native_response_redirect);
        self.register("Response.redirect_permanent", native_response_redirect_permanent);

        // -- Router --
        self.register("Router.new", native_router_new);
        self.register("Router.routes", native_router_routes);
        self.register("Router.get", native_router_get);
        self.register("Router.post", native_router_post);
        self.register("Router.put", native_router_put);
        self.register("Router.delete", native_router_delete);
        self.register("Router.patch", native_router_patch);
        self.register("Router.options", native_router_options);
        self.register("Router.head", native_router_head);
        self.register("Router.any", native_router_any);
        self.register("Router.use", native_router_use);
        self.register("Router.group", native_router_group);

        // -- Request --
        self.register("Request.header", native_request_header);
        self.register("Request.method", native_request_method);
        self.register("Request.body_json", native_request_body_json);
        self.register("Request.with_state", native_request_with_state);
        self.register("Request.state", native_request_state);

        // -- Server (VM-reentrant, dispatched specially in vm.rs) --
        self.register("Server.listen!", native_server_listen_placeholder);
        self.register("Server.listen", native_server_listen_placeholder);
    }
}

// ---------------------------------------------------------------------------
// HOF placeholder — actual execution is in the VM
// ---------------------------------------------------------------------------

fn native_hof_placeholder(_args: &[NValue]) -> Result<NValue, NativeError> {
    Err(NativeError(
        "HOF must be executed by VM, not called directly".into(),
    ))
}

// ---------------------------------------------------------------------------
// Console
// ---------------------------------------------------------------------------

fn native_console_println(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        println!();
    } else {
        println!("{}", args[0]);
    }
    Ok(NValue::unit())
}

fn native_console_print(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        print!("{}", args[0]);
    }
    Ok(NValue::unit())
}

fn native_console_error(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("{}", args[0]);
    }
    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Log
// ---------------------------------------------------------------------------

fn native_log_info(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[INFO] {}", args[0]);
    }
    Ok(NValue::unit())
}

fn native_log_warn(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[WARN] {}", args[0]);
    }
    Ok(NValue::unit())
}

fn native_log_error(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[ERROR] {}", args[0]);
    }
    Ok(NValue::unit())
}

fn native_log_debug(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("[DEBUG] {}", args[0]);
    }
    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Math
// ---------------------------------------------------------------------------

fn native_math_abs(args: &[NValue]) -> Result<NValue, NativeError> {
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

fn native_math_min(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() && args[1].is_any_int() {
        Ok(NValue::int(args[0].as_any_int().min(args[1].as_any_int())))
    } else if args[0].is_number() && args[1].is_number() {
        Ok(NValue::float(args[0].as_f64().min(args[1].as_f64())))
    } else {
        Err(NativeError("Math.min: expected two numbers".into()))
    }
}

fn native_math_max(args: &[NValue]) -> Result<NValue, NativeError> {
    if args[0].is_any_int() && args[1].is_any_int() {
        Ok(NValue::int(args[0].as_any_int().max(args[1].as_any_int())))
    } else if args[0].is_number() && args[1].is_number() {
        Ok(NValue::float(args[0].as_f64().max(args[1].as_f64())))
    } else {
        Err(NativeError("Math.max: expected two numbers".into()))
    }
}

fn native_math_clamp(args: &[NValue]) -> Result<NValue, NativeError> {
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

fn native_math_pow(args: &[NValue]) -> Result<NValue, NativeError> {
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

// ---------------------------------------------------------------------------
// String
// ---------------------------------------------------------------------------

fn native_string_length(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::int(s.len() as i64)),
        None => Err(NativeError(format!(
            "String.length: expected String, got {}",
            args[0]
        ))),
    }
}

fn native_string_to_upper(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.to_uppercase().into())),
        None => Err(NativeError(format!(
            "String.to_upper: expected String, got {}",
            args[0]
        ))),
    }
}

fn native_string_to_lower(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.to_lowercase().into())),
        None => Err(NativeError(format!(
            "String.to_lower: expected String, got {}",
            args[0]
        ))),
    }
}

fn native_string_trim(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => Ok(NValue::string(s.trim().into())),
        None => Err(NativeError(format!(
            "String.trim: expected String, got {}",
            args[0]
        ))),
    }
}

fn native_string_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(sub)) => Ok(NValue::bool(s.contains(&**sub))),
        _ => Err(NativeError(
            "String.contains: expected (String, String)".into(),
        )),
    }
}

fn native_string_starts_with(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(prefix)) => Ok(NValue::bool(s.starts_with(&**prefix))),
        _ => Err(NativeError(
            "String.starts_with: expected (String, String)".into(),
        )),
    }
}

fn native_string_split(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(delim)) => {
            let parts: Vec<NValue> = s
                .split(&**delim)
                .map(|p| NValue::string(p.into()))
                .collect();
            Ok(NValue::list(parts))
        }
        _ => Err(NativeError(
            "String.split: expected (String, String)".into(),
        )),
    }
}

fn native_string_join(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_list(), args[1].as_string()) {
        (Some(items), Some(sep)) => {
            let strs: Vec<String> = items.iter().map(|v| v.to_string()).collect();
            Ok(NValue::string(strs.join(&**sep).into()))
        }
        _ => Err(NativeError("String.join: expected (List, String)".into())),
    }
}

fn native_string_slice(args: &[NValue]) -> Result<NValue, NativeError> {
    match (
        args[0].as_string(),
        args[1].is_any_int(),
        args[2].is_any_int(),
    ) {
        (Some(s), true, true) => {
            let start = args[1].as_any_int() as usize;
            let len = args[2].as_any_int() as usize;
            let result: String = s.chars().skip(start).take(len).collect();
            Ok(NValue::string(result.into()))
        }
        _ => Err(NativeError(
            "String.slice: expected (String, Int, Int)".into(),
        )),
    }
}

// ---------------------------------------------------------------------------
// List (non-HOF)
// ---------------------------------------------------------------------------

fn native_list_length(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => Ok(NValue::int(items.len() as i64)),
        None => Err(NativeError(format!(
            "List.length: expected List, got {}",
            args[0]
        ))),
    }
}

fn native_list_head(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            if let Some(first) = items.first() {
                Ok(NValue::enum_val("Some".into(), first.clone()))
            } else {
                Ok(NValue::enum_val("None".into(), NValue::unit()))
            }
        }
        None => Err(NativeError(format!(
            "List.head: expected List, got {}",
            args[0]
        ))),
    }
}

fn native_list_tail(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            if items.is_empty() {
                Ok(NValue::list(Vec::new()))
            } else {
                Ok(NValue::list(items[1..].to_vec()))
            }
        }
        None => Err(NativeError(format!(
            "List.tail: expected List, got {}",
            args[0]
        ))),
    }
}

fn native_list_reverse(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            let mut reversed = items.clone();
            reversed.reverse();
            Ok(NValue::list(reversed))
        }
        None => Err(NativeError(format!(
            "List.reverse: expected List, got {}",
            args[0]
        ))),
    }
}

fn native_list_sort(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
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
            Ok(NValue::list(sorted))
        }
        None => Err(NativeError(format!(
            "List.sort: expected List, got {}",
            args[0]
        ))),
    }
}

fn native_list_concat(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_list(), args[1].as_list()) {
        (Some(a), Some(b)) => {
            let mut result = a.clone();
            result.extend(b.iter().cloned());
            Ok(NValue::list(result))
        }
        _ => Err(NativeError("List.concat: expected (List, List)".into())),
    }
}

// ---------------------------------------------------------------------------
// Option
// ---------------------------------------------------------------------------

fn native_option_unwrap(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "None" => {
            Err(NativeError("Option.unwrap: called on None".into()))
        }
        _ => Err(NativeError(format!(
            "Option.unwrap: expected Option, got {}",
            args[0]
        ))),
    }
}

fn native_option_unwrap_or(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Some" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "None" => Ok(args[1].clone()),
        _ => Err(NativeError(format!(
            "Option.unwrap_or: expected Option, got {}",
            args[0]
        ))),
    }
}

fn native_option_is_some(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Some")),
        None => Err(NativeError(format!(
            "Option.is_some: expected Option, got {}",
            args[0]
        ))),
    }
}

fn native_option_is_none(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "None")),
        None => Err(NativeError(format!(
            "Option.is_none: expected Option, got {}",
            args[0]
        ))),
    }
}

// ---------------------------------------------------------------------------
// Result
// ---------------------------------------------------------------------------

fn native_result_unwrap(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => Ok(payload.clone()),
        Some((tag, payload)) if &**tag == "Err" => Err(NativeError(format!(
            "Result.unwrap: called on Err({})",
            payload
        ))),
        _ => Err(NativeError(format!(
            "Result.unwrap: expected Result, got {}",
            args[0]
        ))),
    }
}

fn native_result_unwrap_or(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, payload)) if &**tag == "Ok" => Ok(payload.clone()),
        Some((tag, _)) if &**tag == "Err" => Ok(args[1].clone()),
        _ => Err(NativeError(format!(
            "Result.unwrap_or: expected Result, got {}",
            args[0]
        ))),
    }
}

fn native_result_is_ok(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Ok")),
        None => Err(NativeError(format!(
            "Result.is_ok: expected Result, got {}",
            args[0]
        ))),
    }
}

fn native_result_is_err(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_enum() {
        Some((tag, _)) => Ok(NValue::bool(&**tag == "Err")),
        None => Err(NativeError(format!(
            "Result.is_err: expected Result, got {}",
            args[0]
        ))),
    }
}

// ---------------------------------------------------------------------------
// Time
// ---------------------------------------------------------------------------

fn native_time_now(_args: &[NValue]) -> Result<NValue, NativeError> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    Ok(NValue::int(ms))
}

// ---------------------------------------------------------------------------
// Int
// ---------------------------------------------------------------------------

fn native_int_to_string(args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::string(args[0].to_string().into()))
}

fn native_int_parse(args: &[NValue]) -> Result<NValue, NativeError> {
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

// ---------------------------------------------------------------------------
// Json
// ---------------------------------------------------------------------------

pub(crate) fn serde_to_nvalue(value: serde_json::Value) -> NValue {
    match value {
        serde_json::Value::Null => NValue::enum_val("Null".into(), NValue::unit()),
        serde_json::Value::Bool(b) => NValue::bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                NValue::int(i)
            } else if let Some(f) = n.as_f64() {
                NValue::float(f)
            } else {
                NValue::float(f64::NAN)
            }
        }
        serde_json::Value::String(s) => NValue::string(s.into()),
        serde_json::Value::Array(arr) => {
            NValue::list(arr.into_iter().map(serde_to_nvalue).collect())
        }
        serde_json::Value::Object(obj) => {
            let fields: Vec<(RcStr, NValue)> = obj
                .into_iter()
                .map(|(k, v)| (RcStr::from(k.as_str()), serde_to_nvalue(v)))
                .collect();
            NValue::record(fields)
        }
    }
}

pub(crate) fn nvalue_to_serde(value: &NValue) -> Result<serde_json::Value, NativeError> {
    if value.is_any_int() {
        return Ok(serde_json::json!(value.as_any_int()));
    }
    if value.is_float() {
        return serde_json::Number::from_f64(value.as_float())
            .map(serde_json::Value::Number)
            .ok_or_else(|| {
                NativeError(format!(
                    "Json.to_string: cannot serialize float {}",
                    value.as_float()
                ))
            });
    }
    if value.is_bool() {
        return Ok(serde_json::Value::Bool(value.as_bool()));
    }
    if value.is_unit() {
        return Ok(serde_json::Value::Null);
    }
    if value.is_heap() {
        match value.as_heap_ref() {
            HeapObject::String(s) => return Ok(serde_json::Value::String(s.to_string())),
            HeapObject::List(items) => {
                let arr: Result<Vec<_>, _> = items.iter().map(nvalue_to_serde).collect();
                return Ok(serde_json::Value::Array(arr?));
            }
            HeapObject::Record(fields) => {
                let mut map = serde_json::Map::new();
                for (k, v) in fields.iter() {
                    map.insert(k.to_string(), nvalue_to_serde(v)?);
                }
                return Ok(serde_json::Value::Object(map));
            }
            HeapObject::Struct { name, fields } => {
                let mut map = serde_json::Map::new();
                map.insert(
                    "_type".to_string(),
                    serde_json::Value::String(name.to_string()),
                );
                for (k, v) in fields.iter() {
                    map.insert(k.to_string(), nvalue_to_serde(v)?);
                }
                return Ok(serde_json::Value::Object(map));
            }
            HeapObject::Tuple(items) => {
                let arr: Result<Vec<_>, _> = items.iter().map(nvalue_to_serde).collect();
                return Ok(serde_json::Value::Array(arr?));
            }
            HeapObject::Enum { tag, payload } if &**tag == "Null" && payload.is_unit() => {
                return Ok(serde_json::Value::Null);
            }
            HeapObject::Enum { tag, .. } if &**tag == "None" => {
                return Ok(serde_json::Value::Null);
            }
            HeapObject::Enum { tag, payload } if &**tag == "Some" => {
                return nvalue_to_serde(payload);
            }
            HeapObject::Enum { tag, payload } if &**tag == "Ok" => {
                return nvalue_to_serde(payload);
            }
            _ => {}
        }
    }
    Err(NativeError(format!(
        "Json.to_string: cannot serialize {}",
        value
    )))
}

fn native_json_parse(args: &[NValue]) -> Result<NValue, NativeError> {
    let s = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Json.parse expects String, got {}",
                args[0]
            )));
        }
    };
    let value: serde_json::Value =
        serde_json::from_str(s).map_err(|e| NativeError(format!("Json.parse: {}", e)))?;
    Ok(serde_to_nvalue(value))
}

fn native_json_to_string(args: &[NValue]) -> Result<NValue, NativeError> {
    let serde_val = nvalue_to_serde(&args[0])?;
    let json_str = serde_json::to_string(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string: {}", e)))?;
    Ok(NValue::string(json_str.into()))
}

fn native_json_to_string_pretty(args: &[NValue]) -> Result<NValue, NativeError> {
    let serde_val = nvalue_to_serde(&args[0])?;
    let json_str = serde_json::to_string_pretty(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string_pretty: {}", e)))?;
    Ok(NValue::string(json_str.into()))
}

// ---------------------------------------------------------------------------
// Response
// ---------------------------------------------------------------------------

fn make_response(status: i64, headers: Vec<NValue>, body: &str) -> NValue {
    NValue::record(vec![
        ("body".into(), NValue::string(body.into())),
        ("headers".into(), NValue::list(headers)),
        ("status".into(), NValue::int(status)),
    ])
}

fn native_response_ok(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.ok expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(200, Vec::new(), body))
}

fn native_response_json(args: &[NValue]) -> Result<NValue, NativeError> {
    // Accept String as-is, or auto-serialize any other value to JSON
    let body_str: RcStr = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            let serde_val = nvalue_to_serde(&args[0])?;
            let json = serde_json::to_string(&serde_val)
                .map_err(|e| NativeError(format!("Response.json: {}", e)))?;
            RcStr::from(json.as_str())
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Content-Type".into()),
        NValue::string("application/json".into()),
    ])];
    Ok(make_response(200, headers, &body_str))
}

fn native_response_created(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.created expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(201, Vec::new(), body))
}

fn native_response_no_content(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(make_response(204, Vec::new(), ""))
}

fn native_response_bad_request(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.bad_request expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(400, Vec::new(), body))
}

fn native_response_not_found(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.not_found expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(404, Vec::new(), body))
}

fn native_response_error(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.error expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(500, Vec::new(), body))
}

fn native_response_status(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_any_int() {
        return Err(NativeError(format!(
            "Response.status expects Int, got {}",
            args[0]
        )));
    }
    let code = args[0].as_any_int();
    let body = match args[1].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.status expects String body, got {}",
                args[1]
            )));
        }
    };
    Ok(make_response(code, Vec::new(), body))
}

fn native_response_with_header(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects Response record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects String header name, got {}",
                args[1]
            )));
        }
    };
    let val = match args[2].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects String header value, got {}",
                args[2]
            )));
        }
    };

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            headers.push(NValue::tuple(vec![
                NValue::string(name),
                NValue::string(val),
            ]));
            *v = NValue::list(headers);
            return Ok(NValue::record(new_fields));
        }
    }
    // No headers field; add one
    new_fields.push((
        "headers".into(),
        NValue::list(vec![NValue::tuple(vec![
            NValue::string(name),
            NValue::string(val),
        ])]),
    ));
    Ok(NValue::record(new_fields))
}

/// Response.with_headers(resp, headers_list) -> Response
/// Bulk-add headers from a list of (name, value) tuples.
fn native_response_with_headers(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Response.with_headers: expected Response record, got {}",
                args[0]
            )));
        }
    };
    let new_headers = match args[1].as_list() {
        Some(l) => l.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_headers: expected List of headers, got {}",
                args[1]
            )));
        }
    };

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            headers.extend(new_headers);
            *v = NValue::list(headers);
            return Ok(NValue::record(new_fields));
        }
    }
    new_fields.push(("headers".into(), NValue::list(new_headers)));
    Ok(NValue::record(new_fields))
}

/// Response.redirect(url) -> Response with 302 and Location header.
fn native_response_redirect(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.redirect: expected String URL, got {}",
                args[0]
            )));
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Location".into()),
        NValue::string(url),
    ])];
    Ok(make_response(302, headers, ""))
}

/// Response.redirect_permanent(url) -> Response with 301 and Location header.
fn native_response_redirect_permanent(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.redirect_permanent: expected String URL, got {}",
                args[0]
            )));
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Location".into()),
        NValue::string(url),
    ])];
    Ok(make_response(301, headers, ""))
}

// ---------------------------------------------------------------------------
// Router
// ---------------------------------------------------------------------------

fn native_router_new(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::record(vec![
        ("middleware".into(), NValue::list(Vec::new())),
        ("routes".into(), NValue::list(Vec::new())),
    ]))
}

fn native_router_routes(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_record() {
        Some(fields) => {
            for (k, v) in fields.iter() {
                if &**k == "routes" {
                    return Ok(v.clone());
                }
            }
            Err(NativeError("Router.routes: no routes field".into()))
        }
        None => Err(NativeError(format!(
            "Router.routes: expected Router, got {}",
            args[0]
        ))),
    }
}

fn router_add_route(method: &str, args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.{}: expected Router, got {}",
                method.to_lowercase(),
                args[0]
            )));
        }
    };
    if args[1].as_string().is_none() {
        return Err(NativeError(format!(
            "Router.{}: expected String path, got {}",
            method.to_lowercase(),
            args[1]
        )));
    }

    let route = NValue::record(vec![
        ("handler".into(), args[2].clone()),
        ("method".into(), NValue::string(method.into())),
        ("path".into(), args[1].clone()),
    ]);

    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "routes" {
            let mut routes = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            routes.push(route.clone());
            new_fields.push((k.clone(), NValue::list(routes)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

fn native_router_get(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("GET", args)
}

fn native_router_post(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("POST", args)
}

fn native_router_put(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("PUT", args)
}

fn native_router_delete(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("DELETE", args)
}

fn native_router_patch(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("PATCH", args)
}

fn native_router_options(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("OPTIONS", args)
}

fn native_router_head(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("HEAD", args)
}

/// Router.any(router, path, handler) — registers handler for ALL methods.
fn native_router_any(args: &[NValue]) -> Result<NValue, NativeError> {
    let methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD"];
    let mut result = args[0].clone();
    for method in &methods {
        let fields = match result.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(NativeError(format!(
                    "Router.any: expected Router, got {}",
                    result
                )));
            }
        };
        if args[1].as_string().is_none() {
            return Err(NativeError(format!(
                "Router.any: expected String path, got {}",
                args[1]
            )));
        }
        let route = NValue::record(vec![
            ("handler".into(), args[2].clone()),
            ("method".into(), NValue::string(RcStr::from(*method))),
            ("path".into(), args[1].clone()),
        ]);
        let mut new_fields = Vec::new();
        for (k, v) in fields.iter() {
            if &**k == "routes" {
                let mut routes = match v.as_list() {
                    Some(list) => list.clone(),
                    None => Vec::new(),
                };
                routes.push(route.clone());
                new_fields.push((k.clone(), NValue::list(routes)));
            } else {
                new_fields.push((k.clone(), v.clone()));
            }
        }
        result = NValue::record(new_fields);
    }
    Ok(result)
}

fn native_router_use(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.use: expected Router, got {}",
                args[0]
            )));
        }
    };
    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "middleware" {
            let mut mw = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            mw.push(args[1].clone());
            new_fields.push((k.clone(), NValue::list(mw)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

// ---------------------------------------------------------------------------
// Request
// ---------------------------------------------------------------------------

/// Request.header(req, name) -> Option<String>
/// Case-insensitive header lookup from request record's headers list.
fn native_request_header(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.header expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.header: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.to_string().to_lowercase(),
        None => {
            return Err(NativeError(format!(
                "Request.header: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let headers = fields
        .iter()
        .find(|(k, _)| &**k == "headers")
        .and_then(|(_, v)| v.as_list());

    if let Some(headers) = headers {
        for item in headers {
            if item.is_heap() {
                if let HeapObject::Tuple(pair) = item.as_heap_ref() {
                    if pair.len() == 2 {
                        if let Some(k) = pair[0].as_string() {
                            if k.to_lowercase() == name {
                                return Ok(NValue::enum_val("Some".into(), pair[1].clone()));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(NValue::enum_val("None".into(), NValue::unit()))
}

/// Request.method(req) -> String
/// Extract the HTTP method from a request record.
fn native_request_method(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.method: expected Request record, got {}",
                args[0]
            )));
        }
    };
    for (k, v) in fields.iter() {
        if &**k == "method" {
            return Ok(v.clone());
        }
    }
    Ok(NValue::string("GET".into()))
}

/// Request.body_json(req) -> parsed JSON value (sugar for Json.parse(req.body)).
fn native_request_body_json(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.body_json: expected Request record, got {}",
                args[0]
            )));
        }
    };
    let body = fields
        .iter()
        .find(|(k, _)| &**k == "body")
        .map(|(_, v)| v.clone())
        .unwrap_or_else(|| NValue::string("".into()));

    let body_str = match body.as_string() {
        Some(s) => s.to_string(),
        None => return Ok(NValue::enum_val("Err".into(), NValue::string("Request body is not a string".into()))),
    };

    if body_str.is_empty() {
        return Ok(NValue::enum_val("Err".into(), NValue::string("Request body is empty".into())));
    }

    match serde_json::from_str::<serde_json::Value>(&body_str) {
        Ok(value) => Ok(NValue::enum_val("Ok".into(), serde_to_nvalue(value))),
        Err(e) => Ok(NValue::enum_val("Err".into(), NValue::string(format!("JSON parse error: {}", e).into()))),
    }
}

/// Request.with_state(req, key, value) -> Request
/// Add/update a field in the request's `state` sub-record.
fn native_request_with_state(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Request.with_state expects 3 arguments (req, key, value), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.with_state: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let key = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.with_state: key must be String, got {}",
                args[1]
            )));
        }
    };

    let mut new_fields = fields;
    // Find or create state sub-record
    let mut state_fields: Vec<(RcStr, NValue)> = new_fields
        .iter()
        .find(|(k, _)| &**k == "state")
        .and_then(|(_, v)| v.as_record())
        .cloned()
        .unwrap_or_default();

    // Update or insert the key
    let mut found = false;
    for (k, v) in &mut state_fields {
        if *k == key {
            *v = args[2].clone();
            found = true;
            break;
        }
    }
    if !found {
        state_fields.push((key, args[2].clone()));
    }

    // Update the state field on the request
    let mut state_updated = false;
    for (k, v) in &mut new_fields {
        if &**k == "state" {
            *v = NValue::record(state_fields.clone());
            state_updated = true;
            break;
        }
    }
    if !state_updated {
        new_fields.push(("state".into(), NValue::record(state_fields)));
    }

    Ok(NValue::record(new_fields))
}

/// Request.state(req, key) -> Option<String>
/// Read a value from the request's `state` sub-record.
fn native_request_state(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.state expects 2 arguments (req, key), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.state: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let key = match args[1].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Request.state: key must be String, got {}",
                args[1]
            )));
        }
    };

    let state = fields
        .iter()
        .find(|(k, _)| &**k == "state")
        .and_then(|(_, v)| v.as_record());

    if let Some(state_fields) = state {
        for (k, v) in state_fields {
            if &**k == &**key {
                return Ok(NValue::enum_val("Some".into(), v.clone()));
            }
        }
    }
    Ok(NValue::enum_val("None".into(), NValue::unit()))
}

fn native_router_group(args: &[NValue]) -> Result<NValue, NativeError> {
    // Router.group(router, prefix, sub_router)
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Router.group expects 3 arguments (router, prefix, sub_router), got {}",
            args.len()
        )));
    }
    let parent_fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.group: first arg must be Router, got {}",
                args[0]
            )));
        }
    };
    let prefix = match args[1].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Router.group: second arg must be String prefix, got {}",
                args[1]
            )));
        }
    };
    let sub_fields = match args[2].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.group: third arg must be Router, got {}",
                args[2]
            )));
        }
    };

    // Extract sub-router routes and prepend prefix to each path
    let sub_routes: Vec<NValue> = sub_fields
        .iter()
        .find(|(k, _)| &**k == "routes")
        .and_then(|(_, v)| v.as_list())
        .cloned()
        .unwrap_or_default();

    let prefixed_routes: Vec<NValue> = sub_routes
        .iter()
        .filter_map(|route| {
            let route_fields = route.as_record()?;
            let mut new_fields: Vec<(RcStr, NValue)> = Vec::new();
            for (k, v) in route_fields.iter() {
                if &**k == "path" {
                    if let Some(path) = v.as_string() {
                        let prefixed = format!(
                            "{}{}",
                            prefix.trim_end_matches('/'),
                            if path.starts_with('/') {
                                path.to_string()
                            } else {
                                format!("/{}", path)
                            }
                        );
                        new_fields.push((k.clone(), NValue::string(prefixed.into())));
                    } else {
                        new_fields.push((k.clone(), v.clone()));
                    }
                } else {
                    new_fields.push((k.clone(), v.clone()));
                }
            }
            Some(NValue::record(new_fields))
        })
        .collect();

    // Merge into parent router's routes
    let mut new_fields: Vec<(RcStr, NValue)> = Vec::new();
    for (k, v) in parent_fields.iter() {
        if &**k == "routes" {
            let mut routes = v.as_list().cloned().unwrap_or_default();
            routes.extend(prefixed_routes.clone());
            new_fields.push((k.clone(), NValue::list(routes)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

fn native_server_listen_placeholder(_args: &[NValue]) -> Result<NValue, NativeError> {
    Err(NativeError(
        "Server.listen! must be executed by VM, not called directly".into(),
    ))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_lookup() {
        let reg = NativeRegistry::new();
        assert!(reg.lookup("Console.println!").is_some());
        assert!(reg.lookup("Math.abs").is_some());
        assert!(reg.lookup("NonExistent.foo").is_none());
    }

    #[test]
    fn console_println_returns_unit() {
        let result = native_console_println(&[NValue::string("test".into())]).unwrap();
        assert!(result.is_unit());
    }

    #[test]
    fn math_abs_int() {
        let result = native_math_abs(&[NValue::int(-5)]).unwrap();
        assert!(result.is_any_int());
        assert_eq!(result.as_any_int(), 5);
    }

    #[test]
    fn math_min_max() {
        let min = native_math_min(&[NValue::int(3), NValue::int(7)]).unwrap();
        assert_eq!(min.as_any_int(), 3);
        let max = native_math_max(&[NValue::int(3), NValue::int(7)]).unwrap();
        assert_eq!(max.as_any_int(), 7);
    }

    #[test]
    fn string_length() {
        let result = native_string_length(&[NValue::string("hello".into())]).unwrap();
        assert_eq!(result.as_any_int(), 5);
    }

    #[test]
    fn string_contains() {
        let result = native_string_contains(&[
            NValue::string("hello world".into()),
            NValue::string("world".into()),
        ])
        .unwrap();
        assert!(result.is_bool());
        assert!(result.as_bool());
    }

    #[test]
    fn string_split() {
        let result =
            native_string_split(&[NValue::string("a,b,c".into()), NValue::string(",".into())])
                .unwrap();
        let items = result.as_list().unwrap();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].as_string().unwrap().as_ref(), "a");
        assert_eq!(items[1].as_string().unwrap().as_ref(), "b");
        assert_eq!(items[2].as_string().unwrap().as_ref(), "c");
    }

    #[test]
    fn list_head_some() {
        let list = NValue::list(vec![NValue::int(1), NValue::int(2)]);
        let result = native_list_head(&[list]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
        assert_eq!(payload.as_any_int(), 1);
    }

    #[test]
    fn list_head_empty() {
        let list = NValue::list(vec![]);
        let result = native_list_head(&[list]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    #[test]
    fn option_unwrap_some() {
        let some = NValue::enum_val("Some".into(), NValue::int(42));
        let result = native_option_unwrap(&[some]).unwrap();
        assert_eq!(result.as_any_int(), 42);
    }

    #[test]
    fn option_unwrap_none_fails() {
        let none = NValue::enum_val("None".into(), NValue::unit());
        assert!(native_option_unwrap(&[none]).is_err());
    }

    #[test]
    fn result_is_ok() {
        let ok = NValue::enum_val("Ok".into(), NValue::int(1));
        let err = NValue::enum_val("Err".into(), NValue::string("bad".into()));
        assert!(native_result_is_ok(&[ok]).unwrap().as_bool());
        assert!(!native_result_is_ok(&[err]).unwrap().as_bool());
    }
}
