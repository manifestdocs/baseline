use std::rc::Rc;

use super::value::{RcStr, Value};

// ---------------------------------------------------------------------------
// Native Function Registry
// ---------------------------------------------------------------------------

/// Error from a native function call.
#[derive(Debug, Clone)]
pub struct NativeError(pub String);

/// A native function: takes args, returns a value.
type SimpleFn = fn(&[Value]) -> Result<Value, NativeError>;

/// A native function entry.
struct NativeEntry {
    pub name: &'static str,
    pub func: SimpleFn,
}

/// Registry of native functions callable from bytecode via CallNative.
pub struct NativeRegistry {
    entries: Vec<NativeEntry>,
}

impl NativeRegistry {
    pub fn new() -> Self {
        let mut registry = NativeRegistry {
            entries: Vec::new(),
        };
        registry.register_all();
        registry
    }

    /// Look up a native function ID by qualified name (e.g., "Console.println!").
    pub fn lookup(&self, name: &str) -> Option<u16> {
        self.entries.iter().position(|e| e.name == name).map(|i| i as u16)
    }

    /// Call a native function by ID.
    pub fn call(&self, id: u16, args: &[Value]) -> Result<Value, NativeError> {
        let entry = &self.entries[id as usize];
        (entry.func)(args)
    }

    /// Check if a function is a HOF (returns true for List.map, List.filter, etc.).
    pub fn is_hof(&self, id: u16) -> bool {
        let name = self.entries[id as usize].name;
        matches!(name,
            "List.map" | "List.filter" | "List.fold" | "List.find"
            | "Option.map" | "Result.map"
        )
    }

    /// Get the function name by ID (for error messages).
    pub fn name(&self, id: u16) -> &str {
        self.entries[id as usize].name
    }

    fn register(&mut self, name: &'static str, func: SimpleFn) {
        self.entries.push(NativeEntry { name, func });
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

        // -- Router --
        self.register("Router.new", native_router_new);
        self.register("Router.routes", native_router_routes);
        self.register("Router.get", native_router_get);
        self.register("Router.post", native_router_post);
        self.register("Router.put", native_router_put);
        self.register("Router.delete", native_router_delete);
        self.register("Router.use", native_router_use);
    }
}

// ---------------------------------------------------------------------------
// HOF placeholder — actual execution is in the VM
// ---------------------------------------------------------------------------

fn native_hof_placeholder(_args: &[Value]) -> Result<Value, NativeError> {
    Err(NativeError("HOF must be executed by VM, not called directly".into()))
}

// ---------------------------------------------------------------------------
// Console
// ---------------------------------------------------------------------------

fn native_console_println(args: &[Value]) -> Result<Value, NativeError> {
    if args.is_empty() {
        println!();
    } else {
        println!("{}", args[0]);
    }
    Ok(Value::Unit)
}

fn native_console_print(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() {
        print!("{}", args[0]);
    }
    Ok(Value::Unit)
}

fn native_console_error(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() {
        eprintln!("{}", args[0]);
    }
    Ok(Value::Unit)
}

// ---------------------------------------------------------------------------
// Log
// ---------------------------------------------------------------------------

fn native_log_info(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() { eprintln!("[INFO] {}", args[0]); }
    Ok(Value::Unit)
}

fn native_log_warn(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() { eprintln!("[WARN] {}", args[0]); }
    Ok(Value::Unit)
}

fn native_log_error(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() { eprintln!("[ERROR] {}", args[0]); }
    Ok(Value::Unit)
}

fn native_log_debug(args: &[Value]) -> Result<Value, NativeError> {
    if !args.is_empty() { eprintln!("[DEBUG] {}", args[0]); }
    Ok(Value::Unit)
}

// ---------------------------------------------------------------------------
// Math
// ---------------------------------------------------------------------------

fn native_math_abs(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(n) => Ok(Value::Float(n.abs())),
        v => Err(NativeError(format!("Math.abs: expected number, got {}", v))),
    }
}

fn native_math_min(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
        _ => Err(NativeError("Math.min: expected two numbers".into())),
    }
}

fn native_math_max(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
        _ => Err(NativeError("Math.max: expected two numbers".into())),
    }
}

fn native_math_clamp(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Int(x), Value::Int(lo), Value::Int(hi)) => Ok(Value::Int(*x.max(lo).min(hi))),
        (Value::Float(x), Value::Float(lo), Value::Float(hi)) => Ok(Value::Float(x.max(*lo).min(*hi))),
        _ => Err(NativeError("Math.clamp: expected three numbers".into())),
    }
}

fn native_math_pow(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::Int(base), Value::Int(exp)) => {
            Ok(Value::Int((*base as f64).powi(*exp as i32) as i64))
        }
        (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
        (Value::Int(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
        (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powi(*exp as i32))),
        _ => Err(NativeError("Math.pow: expected two numbers".into())),
    }
}

// ---------------------------------------------------------------------------
// String
// ---------------------------------------------------------------------------

fn native_string_length(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        v => Err(NativeError(format!("String.length: expected String, got {}", v))),
    }
}

fn native_string_to_upper(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_uppercase().into())),
        v => Err(NativeError(format!("String.to_upper: expected String, got {}", v))),
    }
}

fn native_string_to_lower(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_lowercase().into())),
        v => Err(NativeError(format!("String.to_lower: expected String, got {}", v))),
    }
}

fn native_string_trim(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.trim().into())),
        v => Err(NativeError(format!("String.trim: expected String, got {}", v))),
    }
}

fn native_string_contains(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(sub)) => Ok(Value::Bool(s.contains(&**sub))),
        _ => Err(NativeError("String.contains: expected (String, String)".into())),
    }
}

fn native_string_starts_with(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(prefix)) => Ok(Value::Bool(s.starts_with(&**prefix))),
        _ => Err(NativeError("String.starts_with: expected (String, String)".into())),
    }
}

fn native_string_split(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::String(s), Value::String(delim)) => {
            let parts: Vec<Value> = s.split(&**delim)
                .map(|p| Value::String(p.into()))
                .collect();
            Ok(Value::List(Rc::new(parts)))
        }
        _ => Err(NativeError("String.split: expected (String, String)".into())),
    }
}

fn native_string_join(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::List(items), Value::String(sep)) => {
            let strs: Vec<String> = items.iter().map(|v| v.to_string()).collect();
            Ok(Value::String(strs.join(&**sep).into()))
        }
        _ => Err(NativeError("String.join: expected (List, String)".into())),
    }
}

fn native_string_slice(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Int(start), Value::Int(len)) => {
            let start = *start as usize;
            let len = *len as usize;
            let result: String = s.chars().skip(start).take(len).collect();
            Ok(Value::String(result.into()))
        }
        _ => Err(NativeError("String.slice: expected (String, Int, Int)".into())),
    }
}

// ---------------------------------------------------------------------------
// List (non-HOF)
// ---------------------------------------------------------------------------

fn native_list_length(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        v => Err(NativeError(format!("List.length: expected List, got {}", v))),
    }
}

fn native_list_head(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::List(items) => {
            if let Some(first) = items.first() {
                Ok(Value::Enum("Some".into(), Rc::new(first.clone())))
            } else {
                Ok(Value::Enum("None".into(), Rc::new(Value::Unit)))
            }
        }
        v => Err(NativeError(format!("List.head: expected List, got {}", v))),
    }
}

fn native_list_tail(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                Ok(Value::List(Rc::new(Vec::new())))
            } else {
                Ok(Value::List(Rc::new(items[1..].to_vec())))
            }
        }
        v => Err(NativeError(format!("List.tail: expected List, got {}", v))),
    }
}

fn native_list_reverse(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::List(items) => {
            let mut reversed = (**items).clone();
            reversed.reverse();
            Ok(Value::List(Rc::new(reversed)))
        }
        v => Err(NativeError(format!("List.reverse: expected List, got {}", v))),
    }
}

fn native_list_sort(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::List(items) => {
            let mut sorted = (**items).clone();
            sorted.sort_by(|a, b| {
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::String(x), Value::String(y)) => x.cmp(y),
                    _ => std::cmp::Ordering::Equal,
                }
            });
            Ok(Value::List(Rc::new(sorted)))
        }
        v => Err(NativeError(format!("List.sort: expected List, got {}", v))),
    }
}

fn native_list_concat(args: &[Value]) -> Result<Value, NativeError> {
    match (&args[0], &args[1]) {
        (Value::List(a), Value::List(b)) => {
            let mut result = (**a).clone();
            result.extend(b.iter().cloned());
            Ok(Value::List(Rc::new(result)))
        }
        _ => Err(NativeError("List.concat: expected (List, List)".into())),
    }
}

// ---------------------------------------------------------------------------
// Option
// ---------------------------------------------------------------------------

fn native_option_unwrap(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, payload) if **tag == *"Some" => Ok((**payload).clone()),
        Value::Enum(tag, _) if **tag == *"None" => {
            Err(NativeError("Option.unwrap: called on None".into()))
        }
        v => Err(NativeError(format!("Option.unwrap: expected Option, got {}", v))),
    }
}

fn native_option_unwrap_or(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, payload) if **tag == *"Some" => Ok((**payload).clone()),
        Value::Enum(tag, _) if **tag == *"None" => Ok(args[1].clone()),
        v => Err(NativeError(format!("Option.unwrap_or: expected Option, got {}", v))),
    }
}

fn native_option_is_some(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, _) => Ok(Value::Bool(**tag == *"Some")),
        v => Err(NativeError(format!("Option.is_some: expected Option, got {}", v))),
    }
}

fn native_option_is_none(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, _) => Ok(Value::Bool(**tag == *"None")),
        v => Err(NativeError(format!("Option.is_none: expected Option, got {}", v))),
    }
}

// ---------------------------------------------------------------------------
// Result
// ---------------------------------------------------------------------------

fn native_result_unwrap(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, payload) if **tag == *"Ok" => Ok((**payload).clone()),
        Value::Enum(tag, payload) if **tag == *"Err" => {
            Err(NativeError(format!("Result.unwrap: called on Err({})", payload)))
        }
        v => Err(NativeError(format!("Result.unwrap: expected Result, got {}", v))),
    }
}

fn native_result_unwrap_or(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, payload) if **tag == *"Ok" => Ok((**payload).clone()),
        Value::Enum(tag, _) if **tag == *"Err" => Ok(args[1].clone()),
        v => Err(NativeError(format!("Result.unwrap_or: expected Result, got {}", v))),
    }
}

fn native_result_is_ok(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, _) => Ok(Value::Bool(**tag == *"Ok")),
        v => Err(NativeError(format!("Result.is_ok: expected Result, got {}", v))),
    }
}

fn native_result_is_err(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Enum(tag, _) => Ok(Value::Bool(**tag == *"Err")),
        v => Err(NativeError(format!("Result.is_err: expected Result, got {}", v))),
    }
}

// ---------------------------------------------------------------------------
// Time
// ---------------------------------------------------------------------------

fn native_time_now(_args: &[Value]) -> Result<Value, NativeError> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    Ok(Value::Int(ms))
}

// ---------------------------------------------------------------------------
// Int
// ---------------------------------------------------------------------------

fn native_int_to_string(args: &[Value]) -> Result<Value, NativeError> {
    Ok(Value::String(args[0].to_string().into()))
}

fn native_int_parse(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::String(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Enum("Ok".into(), Rc::new(Value::Int(n)))),
            Err(_) => Ok(Value::Enum("Err".into(), Rc::new(Value::String(format!("Cannot parse '{}' as Int", s).into())))),
        },
        v => Err(NativeError(format!("Int.parse: expected String, got {}", v))),
    }
}

// ---------------------------------------------------------------------------
// Json
// ---------------------------------------------------------------------------

fn serde_to_vm(value: serde_json::Value) -> Value {
    match value {
        serde_json::Value::Null => Value::Enum("Null".into(), Rc::new(Value::Unit)),
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Float(f64::NAN)
            }
        }
        serde_json::Value::String(s) => Value::String(s.into()),
        serde_json::Value::Array(arr) => {
            Value::List(Rc::new(arr.into_iter().map(serde_to_vm).collect()))
        }
        serde_json::Value::Object(obj) => {
            let fields: Vec<(RcStr, Value)> = obj
                .into_iter()
                .map(|(k, v)| (RcStr::from(k.as_str()), serde_to_vm(v)))
                .collect();
            Value::Record(Rc::new(fields))
        }
    }
}

fn vm_to_serde(value: &Value) -> Result<serde_json::Value, NativeError> {
    match value {
        Value::Enum(tag, payload) if &**tag == "Null" && **payload == Value::Unit => {
            Ok(serde_json::Value::Null)
        }
        Value::Enum(tag, _) if &**tag == "None" => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(i) => Ok(serde_json::json!(*i)),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| NativeError(format!("Json.to_string: cannot serialize float {}", f))),
        Value::String(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::List(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(vm_to_serde).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        Value::Record(fields) => {
            let mut map = serde_json::Map::new();
            for (k, v) in fields.iter() {
                map.insert(k.to_string(), vm_to_serde(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        Value::Struct(name, fields) => {
            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String(name.to_string()));
            for (k, v) in fields.iter() {
                map.insert(k.to_string(), vm_to_serde(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        Value::Tuple(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(vm_to_serde).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        Value::Unit => Ok(serde_json::Value::Null),
        Value::Enum(tag, payload) if &**tag == "Some" => vm_to_serde(payload),
        Value::Enum(tag, payload) if &**tag == "Ok" => vm_to_serde(payload),
        other => Err(NativeError(format!("Json.to_string: cannot serialize {}", other))),
    }
}

fn native_json_parse(args: &[Value]) -> Result<Value, NativeError> {
    let s = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Json.parse expects String, got {}", other))),
    };
    let value: serde_json::Value = serde_json::from_str(s)
        .map_err(|e| NativeError(format!("Json.parse: {}", e)))?;
    Ok(serde_to_vm(value))
}

fn native_json_to_string(args: &[Value]) -> Result<Value, NativeError> {
    let serde_val = vm_to_serde(&args[0])?;
    let json_str = serde_json::to_string(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string: {}", e)))?;
    Ok(Value::String(json_str.into()))
}

fn native_json_to_string_pretty(args: &[Value]) -> Result<Value, NativeError> {
    let serde_val = vm_to_serde(&args[0])?;
    let json_str = serde_json::to_string_pretty(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string_pretty: {}", e)))?;
    Ok(Value::String(json_str.into()))
}

// ---------------------------------------------------------------------------
// Response
// ---------------------------------------------------------------------------

fn make_response(status: i64, headers: Vec<Value>, body: &str) -> Value {
    Value::Record(Rc::new(vec![
        ("body".into(), Value::String(body.into())),
        ("headers".into(), Value::List(Rc::new(headers))),
        ("status".into(), Value::Int(status)),
    ]))
}

fn native_response_ok(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.ok expects String body, got {}", other))),
    };
    Ok(make_response(200, Vec::new(), body))
}

fn native_response_json(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.json expects String body, got {}", other))),
    };
    let headers = vec![Value::Tuple(Rc::new(vec![
        Value::String("Content-Type".into()),
        Value::String("application/json".into()),
    ]))];
    Ok(make_response(200, headers, body))
}

fn native_response_created(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.created expects String body, got {}", other))),
    };
    Ok(make_response(201, Vec::new(), body))
}

fn native_response_no_content(_args: &[Value]) -> Result<Value, NativeError> {
    Ok(make_response(204, Vec::new(), ""))
}

fn native_response_bad_request(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.bad_request expects String body, got {}", other))),
    };
    Ok(make_response(400, Vec::new(), body))
}

fn native_response_not_found(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.not_found expects String body, got {}", other))),
    };
    Ok(make_response(404, Vec::new(), body))
}

fn native_response_error(args: &[Value]) -> Result<Value, NativeError> {
    let body = match &args[0] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.error expects String body, got {}", other))),
    };
    Ok(make_response(500, Vec::new(), body))
}

fn native_response_status(args: &[Value]) -> Result<Value, NativeError> {
    let code = match &args[0] {
        Value::Int(i) => *i,
        other => return Err(NativeError(format!("Response.status expects Int, got {}", other))),
    };
    let body = match &args[1] {
        Value::String(s) => s,
        other => return Err(NativeError(format!("Response.status expects String body, got {}", other))),
    };
    Ok(make_response(code, Vec::new(), body))
}

fn native_response_with_header(args: &[Value]) -> Result<Value, NativeError> {
    let fields = match &args[0] {
        Value::Record(f) => f,
        other => return Err(NativeError(format!("Response.with_header expects Response record, got {}", other))),
    };
    let name = match &args[1] {
        Value::String(s) => s.clone(),
        other => return Err(NativeError(format!("Response.with_header expects String header name, got {}", other))),
    };
    let val = match &args[2] {
        Value::String(s) => s.clone(),
        other => return Err(NativeError(format!("Response.with_header expects String header value, got {}", other))),
    };

    let mut new_fields: Vec<(RcStr, Value)> = (**fields).clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v {
                Value::List(list) => (**list).clone(),
                _ => Vec::new(),
            };
            headers.push(Value::Tuple(Rc::new(vec![
                Value::String(name),
                Value::String(val),
            ])));
            *v = Value::List(Rc::new(headers));
            return Ok(Value::Record(Rc::new(new_fields)));
        }
    }
    // No headers field; add one
    new_fields.push(("headers".into(), Value::List(Rc::new(vec![
        Value::Tuple(Rc::new(vec![Value::String(name), Value::String(val)])),
    ]))));
    Ok(Value::Record(Rc::new(new_fields)))
}

// ---------------------------------------------------------------------------
// Router
// ---------------------------------------------------------------------------

fn native_router_new(_args: &[Value]) -> Result<Value, NativeError> {
    Ok(Value::Record(Rc::new(vec![
        ("middleware".into(), Value::List(Rc::new(Vec::new()))),
        ("routes".into(), Value::List(Rc::new(Vec::new()))),
    ])))
}

fn native_router_routes(args: &[Value]) -> Result<Value, NativeError> {
    match &args[0] {
        Value::Record(fields) => {
            for (k, v) in fields.iter() {
                if &**k == "routes" {
                    return Ok(v.clone());
                }
            }
            Err(NativeError("Router.routes: no routes field".into()))
        }
        other => Err(NativeError(format!("Router.routes: expected Router, got {}", other))),
    }
}

fn router_add_route(method: &str, args: &[Value]) -> Result<Value, NativeError> {
    let fields = match &args[0] {
        Value::Record(f) => f,
        other => return Err(NativeError(format!("Router.{}: expected Router, got {}", method.to_lowercase(), other))),
    };
    match &args[1] {
        Value::String(_) => {}
        other => return Err(NativeError(format!("Router.{}: expected String path, got {}", method.to_lowercase(), other))),
    };

    let route = Value::Record(Rc::new(vec![
        ("handler".into(), args[2].clone()),
        ("method".into(), Value::String(method.into())),
        ("path".into(), args[1].clone()),
    ]));

    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "routes" {
            let mut routes = match v {
                Value::List(list) => (**list).clone(),
                _ => Vec::new(),
            };
            routes.push(route.clone());
            new_fields.push((k.clone(), Value::List(Rc::new(routes))));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(Value::Record(Rc::new(new_fields)))
}

fn native_router_get(args: &[Value]) -> Result<Value, NativeError> {
    router_add_route("GET", args)
}

fn native_router_post(args: &[Value]) -> Result<Value, NativeError> {
    router_add_route("POST", args)
}

fn native_router_put(args: &[Value]) -> Result<Value, NativeError> {
    router_add_route("PUT", args)
}

fn native_router_delete(args: &[Value]) -> Result<Value, NativeError> {
    router_add_route("DELETE", args)
}

fn native_router_use(args: &[Value]) -> Result<Value, NativeError> {
    let fields = match &args[0] {
        Value::Record(f) => f,
        other => return Err(NativeError(format!("Router.use: expected Router, got {}", other))),
    };
    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "middleware" {
            let mut mw = match v {
                Value::List(list) => (**list).clone(),
                _ => Vec::new(),
            };
            mw.push(args[1].clone());
            new_fields.push((k.clone(), Value::List(Rc::new(mw))));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(Value::Record(Rc::new(new_fields)))
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
        let result = native_console_println(&[Value::String("test".into())]).unwrap();
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn math_abs_int() {
        assert_eq!(native_math_abs(&[Value::Int(-5)]).unwrap(), Value::Int(5));
    }

    #[test]
    fn math_min_max() {
        assert_eq!(native_math_min(&[Value::Int(3), Value::Int(7)]).unwrap(), Value::Int(3));
        assert_eq!(native_math_max(&[Value::Int(3), Value::Int(7)]).unwrap(), Value::Int(7));
    }

    #[test]
    fn string_length() {
        assert_eq!(
            native_string_length(&[Value::String("hello".into())]).unwrap(),
            Value::Int(5)
        );
    }

    #[test]
    fn string_contains() {
        assert_eq!(
            native_string_contains(&[Value::String("hello world".into()), Value::String("world".into())]).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn string_split() {
        assert_eq!(
            native_string_split(&[Value::String("a,b,c".into()), Value::String(",".into())]).unwrap(),
            Value::List(Rc::new(vec![
                Value::String("a".into()),
                Value::String("b".into()),
                Value::String("c".into()),
            ]))
        );
    }

    #[test]
    fn list_head_some() {
        assert_eq!(
            native_list_head(&[Value::List(Rc::new(vec![Value::Int(1), Value::Int(2)]))]).unwrap(),
            Value::Enum("Some".into(), Rc::new(Value::Int(1)))
        );
    }

    #[test]
    fn list_head_empty() {
        assert_eq!(
            native_list_head(&[Value::List(Rc::new(vec![]))]).unwrap(),
            Value::Enum("None".into(), Rc::new(Value::Unit))
        );
    }

    #[test]
    fn option_unwrap_some() {
        let some = Value::Enum("Some".into(), Rc::new(Value::Int(42)));
        assert_eq!(native_option_unwrap(&[some]).unwrap(), Value::Int(42));
    }

    #[test]
    fn option_unwrap_none_fails() {
        let none = Value::Enum("None".into(), Rc::new(Value::Unit));
        assert!(native_option_unwrap(&[none]).is_err());
    }

    #[test]
    fn result_is_ok() {
        let ok = Value::Enum("Ok".into(), Rc::new(Value::Int(1)));
        let err = Value::Enum("Err".into(), Rc::new(Value::String("bad".into())));
        assert_eq!(native_result_is_ok(&[ok]).unwrap(), Value::Bool(true));
        assert_eq!(native_result_is_ok(&[err]).unwrap(), Value::Bool(false));
    }
}
