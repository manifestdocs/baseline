use std::collections::HashMap;
use tree_sitter::Node;

use crate::builtins::BuiltinRegistry;
use crate::prelude::Prelude;
use crate::stdlib::NativeRegistry;
use crate::stdlib::router;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue<'a> {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
    Function(Vec<String>, Node<'a>),
    /// Closure: params, body, captured environment
    Closure(Vec<String>, Node<'a>, HashMap<String, RuntimeValue<'a>>),
    Tuple(Vec<RuntimeValue<'a>>),
    Struct(String, HashMap<String, RuntimeValue<'a>>),
    Record(HashMap<String, RuntimeValue<'a>>),
    List(Vec<RuntimeValue<'a>>),
    Enum(String, Vec<RuntimeValue<'a>>), // VariantName, PayloadValues
    /// Sentinel for `?` operator early-return propagation.
    EarlyReturn(Box<RuntimeValue<'a>>),
    /// Native closure: qualified name + captured args for partial application.
    /// Used by Router.get/post/etc. for pipe-based composition.
    NativeClosure(String, Vec<RuntimeValue<'a>>),
}

impl<'a> std::fmt::Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Int(i) => write!(f, "{}", i),
            RuntimeValue::Float(fl) => write!(f, "{}", fl),
            RuntimeValue::String(s) => write!(f, "{}", s), // TODO: quote strings?
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Unit => write!(f, "()"),
            RuntimeValue::Function(args, _) => write!(f, "|{}| ...", args.join(", ")),
            RuntimeValue::Closure(args, _, _) => write!(f, "|{}| <closure>", args.join(", ")),
            RuntimeValue::Tuple(vals) => {
                let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({})", s)
            }
            RuntimeValue::Struct(name, _fields) => {
                 write!(f, "{} {{ ... }}", name)
            }
            RuntimeValue::Record(_) => write!(f, "{{ ... }}"),
            RuntimeValue::List(vals) => {
                 let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                 write!(f, "[{}]", s)
            }
            RuntimeValue::Enum(name, payload) => {
                if payload.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let s = payload.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                    write!(f, "{}({})", name, s)
                }
            }
            RuntimeValue::EarlyReturn(inner) => write!(f, "<early_return: {}>", inner),
            RuntimeValue::NativeClosure(name, _) => write!(f, "<native_closure: {}>", name),
        }
    }
}

// ---------------------------------------------------------------------------
// RuntimeError — carries source location and call stack
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub file: String,
    pub location: Option<(usize, usize)>, // (line, col), 1-indexed
    pub stack: Vec<StackFrame>,
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub name: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((line, col)) = self.location {
            write!(f, "{}:{}:{}: {}", self.file, line, col, self.message)?;
        } else {
            write!(f, "{}: {}", self.file, self.message)?;
        }
        for frame in &self.stack {
            write!(f, "\n  at {} ({}:{}:{})", frame.name, self.file, frame.line, frame.col)?;
        }
        Ok(())
    }
}

impl From<String> for RuntimeError {
    fn from(message: String) -> Self {
        Self {
            message,
            file: "<unknown>".to_string(),
            location: None,
            stack: Vec::new(),
        }
    }
}

/// Create a RuntimeError with position from a tree-sitter node and the current call stack.
fn err_at(msg: impl Into<String>, node: &Node, ctx: &Context) -> RuntimeError {
    let pos = node.start_position();
    RuntimeError {
        message: msg.into(),
        file: ctx.file.clone(),
        location: Some((pos.row + 1, pos.column + 1)),
        stack: ctx.call_stack.clone(),
    }
}

/// All known module names in the language (for error messages).
const ALL_MODULES: &[&str] = &[
    "Option", "Result", "String", "List", "Json", "Math",
    "Console", "Log", "Time", "Random", "Env", "Fs", "Http", "Response",
    "Router", "Server", "Db", "Metrics",
];

pub struct Context<'a> {
    scopes: Vec<HashMap<String, RuntimeValue<'a>>>,
    builtins: BuiltinRegistry,
    natives: NativeRegistry,
    prelude: Prelude,
    pub file: String,
    call_stack: Vec<StackFrame>,
}

impl<'a> Context<'a> {
    /// Create a context with ALL modules (backwards compat for tests).
    pub fn new() -> Self {
        Self::with_prelude(Prelude::Script)
    }

    /// Create a context gated by the given prelude.
    pub fn with_prelude(prelude: Prelude) -> Self {
        Self::with_prelude_and_file(prelude, "<unknown>".to_string())
    }

    /// Create a context with a specific prelude and source file path.
    pub fn with_prelude_and_file(prelude: Prelude, file: String) -> Self {
        let mut ctx = Self {
            scopes: vec![HashMap::new()],
            builtins: BuiltinRegistry::with_prelude(prelude),
            natives: NativeRegistry::with_prelude(prelude),
            prelude,
            file,
            call_stack: Vec::new(),
        };
        // Option/Result constructors are language primitives — always available.
        ctx.set("None".to_string(), RuntimeValue::Enum("None".to_string(), Vec::new()));
        ctx.set("Some".to_string(), RuntimeValue::Enum("Some".to_string(), Vec::new()));
        ctx.set("Ok".to_string(), RuntimeValue::Enum("Ok".to_string(), Vec::new()));
        ctx.set("Err".to_string(), RuntimeValue::Enum("Err".to_string(), Vec::new()));
        ctx
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn set(&mut self, name: String, val: RuntimeValue<'a>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, val);
        }
    }

    pub fn get(&self, name: &str) -> Option<&RuntimeValue<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    /// Check if a name is a known module. If known but not in scope, return
    /// an error message. If unknown, return None (not a module reference).
    pub fn check_module_scope(&self, name: &str) -> Option<String> {
        let in_scope: Vec<&str> = self.prelude.type_modules().to_vec();
        if in_scope.contains(&name) {
            None // in scope, no error
        } else if ALL_MODULES.contains(&name) {
            Some(format!(
                "Module `{}` is not available with the current prelude. \
                 Try using @prelude(script) or @prelude(core).",
                name
            ))
        } else {
            None // not a known module
        }
    }

    /// Snapshot the entire environment (all scopes) into a flat HashMap.
    /// Used for closure capture.
    pub fn snapshot(&self) -> HashMap<String, RuntimeValue<'a>> {
        let mut env = HashMap::new();
        // Iterate from outermost to innermost so inner scopes shadow outer
        for scope in &self.scopes {
            env.extend(scope.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        env
    }

    fn push_frame(&mut self, name: String, node: &Node) {
        let pos = node.start_position();
        self.call_stack.push(StackFrame {
            name,
            line: pos.row + 1,
            col: pos.column + 1,
        });
    }

    fn pop_frame(&mut self) {
        self.call_stack.pop();
    }
}

/// If the result contains an EarlyReturn sentinel, unwrap it to the inner value.
/// Called at function call boundaries so `?` propagation stops at the enclosing function.
fn unwrap_early_return<'a>(
    result: Result<RuntimeValue<'a>, RuntimeError>,
) -> Result<RuntimeValue<'a>, RuntimeError> {
    match result {
        Ok(RuntimeValue::EarlyReturn(inner)) => Ok(*inner),
        other => other,
    }
}

/// Propagate EarlyReturn from a subexpression: if the value is EarlyReturn,
/// return it immediately without evaluating further operands.
macro_rules! propagate {
    ($val:expr) => {
        if matches!($val, RuntimeValue::EarlyReturn(_)) {
            return Ok($val);
        }
    };
}

/// Apply a Function or Closure value to a list of arguments.
/// Extracted for reuse by Option.map and future HOFs like List.map.
fn apply_function<'a>(
    func: &RuntimeValue<'a>,
    args: &[RuntimeValue<'a>],
    source: &str,
    context: &mut Context<'a>,
    call_node: Option<&Node>,
) -> Result<RuntimeValue<'a>, RuntimeError> {
    match func {
        RuntimeValue::Function(param_names, body_node) => {
            if args.len() != param_names.len() {
                let msg = format!("Arg count mismatch: expected {}, got {}", param_names.len(), args.len());
                return match call_node {
                    Some(n) => Err(err_at(msg, n, context)),
                    None => Err(RuntimeError::from(msg)),
                };
            }
            if let Some(n) = call_node {
                context.push_frame("<closure>".to_string(), n);
            }
            context.enter_scope();
            for (name, val) in param_names.iter().zip(args.iter()) {
                context.set(name.clone(), val.clone());
            }
            let result = eval(body_node, source, context);
            context.exit_scope();
            if call_node.is_some() {
                context.pop_frame();
            }
            unwrap_early_return(result)
        }
        RuntimeValue::Closure(param_names, body_node, captured_env) => {
            if args.len() != param_names.len() {
                let msg = format!("Arg count mismatch: expected {}, got {}", param_names.len(), args.len());
                return match call_node {
                    Some(n) => Err(err_at(msg, n, context)),
                    None => Err(RuntimeError::from(msg)),
                };
            }
            if let Some(n) = call_node {
                context.push_frame("<closure>".to_string(), n);
            }
            context.enter_scope();
            for (k, v) in captured_env {
                context.set(k.clone(), v.clone());
            }
            for (name, val) in param_names.iter().zip(args.iter()) {
                context.set(name.clone(), val.clone());
            }
            let result = eval(body_node, source, context);
            context.exit_scope();
            if call_node.is_some() {
                context.pop_frame();
            }
            unwrap_early_return(result)
        }
        _ => {
            let msg = format!("Not a function: {}", func);
            match call_node {
                Some(n) => Err(err_at(msg, n, context)),
                None => Err(RuntimeError::from(msg)),
            }
        }
    }
}

/// Dispatch higher-order functions that need closure invocation (eval access).
/// Returns Ok(Some(value)) if handled, Ok(None) if not a HOF.
fn dispatch_hof<'a>(
    qualified: &str,
    node: &Node<'a>,
    source: &str,
    context: &mut Context<'a>,
) -> Result<Option<RuntimeValue<'a>>, RuntimeError> {
    // Collect all call arguments
    let total_children = node.named_child_count();
    let mut arg_vals = Vec::new();
    for i in 1..total_children {
        let arg_node = node.named_child(i).unwrap();
        arg_vals.push(eval(&arg_node, source, context)?);
    }

    match qualified {
        // Option.map(opt, fn) -> Option
        "Option.map" => {
            if arg_vals.len() != 2 {
                return Err(err_at(format!("Option.map expects 2 arguments, got {}", arg_vals.len()), node, context));
            }
            let opt_val = &arg_vals[0];
            let map_fn = &arg_vals[1];
            match opt_val {
                RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
                    let result = apply_function(map_fn, &[payload[0].clone()], source, context, Some(node))?;
                    Ok(Some(RuntimeValue::Enum("Some".to_string(), vec![result])))
                }
                RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
                    Ok(Some(RuntimeValue::Enum("None".to_string(), Vec::new())))
                }
                other => Err(err_at(format!("Option.map expects Option as first argument, got {}", other), node, context)),
            }
        }
        // Result.map(res, fn) -> Result
        "Result.map" => {
            if arg_vals.len() != 2 {
                return Err(err_at(format!("Result.map expects 2 arguments, got {}", arg_vals.len()), node, context));
            }
            let res_val = &arg_vals[0];
            let map_fn = &arg_vals[1];
            match res_val {
                RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
                    let result = apply_function(map_fn, &[payload[0].clone()], source, context, Some(node))?;
                    Ok(Some(RuntimeValue::Enum("Ok".to_string(), vec![result])))
                }
                RuntimeValue::Enum(name, _payload) if name == "Err" => {
                    Ok(Some(arg_vals[0].clone()))
                }
                other => Err(err_at(format!("Result.map expects Result as first argument, got {}", other), node, context)),
            }
        }
        // List.map(list, fn) -> List
        "List.map" => {
            if arg_vals.len() != 2 {
                return Err(err_at(format!("List.map expects 2 arguments, got {}", arg_vals.len()), node, context));
            }
            let items = match &arg_vals[0] {
                RuntimeValue::List(items) => items.clone(),
                other => return Err(err_at(format!("List.map expects List as first argument, got {}", other), node, context)),
            };
            let map_fn = &arg_vals[1];
            let mut results = Vec::with_capacity(items.len());
            for item in items {
                results.push(apply_function(map_fn, &[item], source, context, Some(node))?);
            }
            Ok(Some(RuntimeValue::List(results)))
        }
        // List.filter(list, fn) -> List
        "List.filter" => {
            if arg_vals.len() != 2 {
                return Err(err_at(format!("List.filter expects 2 arguments, got {}", arg_vals.len()), node, context));
            }
            let items = match &arg_vals[0] {
                RuntimeValue::List(items) => items.clone(),
                other => return Err(err_at(format!("List.filter expects List as first argument, got {}", other), node, context)),
            };
            let pred_fn = &arg_vals[1];
            let mut results = Vec::new();
            for item in items {
                let keep = apply_function(pred_fn, &[item.clone()], source, context, Some(node))?;
                if let RuntimeValue::Bool(true) = keep {
                    results.push(item);
                }
            }
            Ok(Some(RuntimeValue::List(results)))
        }
        // List.fold(list, init, fn) -> T
        "List.fold" => {
            if arg_vals.len() != 3 {
                return Err(err_at(format!("List.fold expects 3 arguments, got {}", arg_vals.len()), node, context));
            }
            let items = match &arg_vals[0] {
                RuntimeValue::List(items) => items.clone(),
                other => return Err(err_at(format!("List.fold expects List as first argument, got {}", other), node, context)),
            };
            let mut acc = arg_vals[1].clone();
            let fold_fn = &arg_vals[2];
            for item in items {
                acc = apply_function(fold_fn, &[acc, item], source, context, Some(node))?;
            }
            Ok(Some(acc))
        }
        // List.find(list, fn) -> Option
        "List.find" => {
            if arg_vals.len() != 2 {
                return Err(err_at(format!("List.find expects 2 arguments, got {}", arg_vals.len()), node, context));
            }
            let items = match &arg_vals[0] {
                RuntimeValue::List(items) => items.clone(),
                other => return Err(err_at(format!("List.find expects List as first argument, got {}", other), node, context)),
            };
            let pred_fn = &arg_vals[1];
            for item in items {
                let found = apply_function(pred_fn, &[item.clone()], source, context, Some(node))?;
                if let RuntimeValue::Bool(true) = found {
                    return Ok(Some(RuntimeValue::Enum("Some".to_string(), vec![item])));
                }
            }
            Ok(Some(RuntimeValue::Enum("None".to_string(), Vec::new())))
        }
        // Router.get/post/put/delete — 3 args: direct, 2 args: partial for pipes
        "Router.get" | "Router.post" | "Router.put" | "Router.delete" => {
            let method = qualified.strip_prefix("Router.").unwrap().to_uppercase();
            match arg_vals.len() {
                3 => {
                    let result = router::add_route(&method, &arg_vals[0], &arg_vals[1], &arg_vals[2])
                        .map_err(|msg| err_at(msg, node, context))?;
                    Ok(Some(result))
                }
                2 => {
                    // Partial application: return NativeClosure for pipe composition
                    Ok(Some(RuntimeValue::NativeClosure(qualified.to_string(), arg_vals)))
                }
                n => Err(err_at(
                    format!("{} expects 2 or 3 arguments, got {}", qualified, n),
                    node, context,
                )),
            }
        }
        // Server.listen!(port, router) — start HTTP server
        "Server.listen!" => {
            if arg_vals.len() != 2 {
                return Err(err_at(
                    format!("Server.listen! expects 2 arguments (port, router), got {}", arg_vals.len()),
                    node, context,
                ));
            }
            let port = match &arg_vals[0] {
                RuntimeValue::Int(p) => *p as u16,
                other => return Err(err_at(
                    format!("Server.listen! port must be Int, got {}", other),
                    node, context,
                )),
            };
            let router_val = arg_vals[1].clone();
            server_listen(port, &router_val, node, source, context)?;
            Ok(Some(RuntimeValue::Unit))
        }
        _ => Ok(None),
    }
}

/// Start an HTTP server on the given port using the router's route table.
fn server_listen<'a>(
    port: u16,
    router_val: &RuntimeValue<'a>,
    node: &Node<'a>,
    source: &str,
    context: &mut Context<'a>,
) -> Result<(), RuntimeError> {
    // Extract routes from the router
    let routes = match router_val {
        RuntimeValue::Record(fields) => match fields.get("routes") {
            Some(RuntimeValue::List(routes)) => routes.clone(),
            _ => return Err(err_at("Server.listen! expects a Router", node, context)),
        },
        _ => return Err(err_at("Server.listen! second argument must be a Router", node, context)),
    };

    let addr = format!("0.0.0.0:{}", port);
    let server = tiny_http::Server::http(&addr)
        .map_err(|e| err_at(format!("Failed to start server on {}: {}", addr, e), node, context))?;

    eprintln!("[server] Listening on http://0.0.0.0:{}", port);

    for mut request in server.incoming_requests() {
        let req_method = request.method().to_string();
        let req_path = request.url().to_string();
        let mut req_body = String::new();
        if let Err(e) = request.as_reader().read_to_string(&mut req_body) {
            eprintln!("[server] Failed to read request body: {}", e);
            let response = tiny_http::Response::from_string("Internal Server Error")
                .with_status_code(tiny_http::StatusCode(500));
            let _ = request.respond(response);
            continue;
        }

        // Build request record: { method, url, headers, body }
        let req_headers: Vec<RuntimeValue> = request.headers().iter().map(|h| {
            RuntimeValue::Tuple(vec![
                RuntimeValue::String(h.field.to_string()),
                RuntimeValue::String(h.value.to_string()),
            ])
        }).collect();
        let mut req_fields = std::collections::HashMap::new();
        req_fields.insert("method".to_string(), RuntimeValue::String(req_method.clone()));
        req_fields.insert("url".to_string(), RuntimeValue::String(req_path.clone()));
        req_fields.insert("headers".to_string(), RuntimeValue::List(req_headers));
        req_fields.insert("body".to_string(), RuntimeValue::String(req_body));
        let req_record = RuntimeValue::Record(req_fields);

        // Find matching route and build response
        let (status, resp_headers, body) = match find_and_invoke_handler(&routes, &req_method, &req_path, &req_record, node, source, context) {
            Some(Ok(handler_result)) => extract_response(handler_result),
            Some(Err(e)) => {
                eprintln!("[server] Handler error: {}", e);
                (500, Vec::new(), "Internal Server Error".to_string())
            }
            None => (404, Vec::new(), "Not Found".to_string()),
        };

        let mut response = tiny_http::Response::from_string(&body)
            .with_status_code(tiny_http::StatusCode(status));
        for (name, value) in &resp_headers {
            if let Ok(header) = tiny_http::Header::from_bytes(name.as_bytes(), value.as_bytes()) {
                response = response.with_header(header);
            }
        }
        let _ = request.respond(response);
    }

    Ok(())
}

/// Extract (status, headers, body) from a handler return value.
///
/// Supports multiple return conventions:
/// - Response record: `{ status: Int, headers: List<(String, String)>, body: String }`
/// - Result<Response, _>: `Ok(response_record)` extracts the record; `Err(msg)` → 500
/// - Result<String, String>: `Ok(body)` → 200; `Err(msg)` → 500 (backwards compat)
/// - Plain String: body with 200 (backwards compat)
fn extract_response(value: RuntimeValue) -> (u16, Vec<(String, String)>, String) {
    match value {
        // Ok(payload)
        RuntimeValue::Enum(ref name, ref payload) if name == "Ok" && payload.len() == 1 => {
            extract_response(payload[0].clone())
        }
        // Err(msg) → 500
        RuntimeValue::Enum(ref name, ref payload) if name == "Err" && payload.len() == 1 => {
            (500, Vec::new(), payload[0].to_string())
        }
        // Response record: { status, headers, body }
        RuntimeValue::Record(ref fields) if fields.contains_key("status") => {
            let status = match fields.get("status") {
                Some(RuntimeValue::Int(s)) => *s as u16,
                _ => 200,
            };
            let body = match fields.get("body") {
                Some(RuntimeValue::String(b)) => b.clone(),
                Some(other) => other.to_string(),
                None => String::new(),
            };
            let headers = match fields.get("headers") {
                Some(RuntimeValue::List(items)) => {
                    items.iter().filter_map(|item| {
                        if let RuntimeValue::Tuple(pair) = item {
                            if pair.len() == 2 {
                                if let (RuntimeValue::String(k), RuntimeValue::String(v)) = (&pair[0], &pair[1]) {
                                    return Some((k.clone(), v.clone()));
                                }
                            }
                        }
                        None
                    }).collect()
                }
                _ => Vec::new(),
            };
            (status, headers, body)
        }
        // Plain string → 200 (backwards compat)
        RuntimeValue::String(s) => (200, Vec::new(), s),
        // Anything else → 200 with stringified body
        other => (200, Vec::new(), other.to_string()),
    }
}

/// Find a matching route and invoke its handler. Returns None for no match.
///
/// Uses pattern matching to support `:name` path parameters.
/// Most-specific route wins (fewest parameter segments); route order breaks ties.
fn find_and_invoke_handler<'a>(
    routes: &[RuntimeValue<'a>],
    req_method: &str,
    req_path: &str,
    req_record: &RuntimeValue<'a>,
    node: &Node<'a>,
    source: &str,
    context: &mut Context<'a>,
) -> Option<Result<RuntimeValue<'a>, RuntimeError>> {
    let mut best: Option<(&RuntimeValue<'a>, Vec<(String, String)>)> = None;
    let mut best_param_count = usize::MAX;

    for route in routes {
        if let RuntimeValue::Record(route_fields) = route {
            let route_method = match route_fields.get("method") {
                Some(RuntimeValue::String(m)) => m.as_str(),
                _ => continue,
            };
            let route_path = match route_fields.get("path") {
                Some(RuntimeValue::String(p)) => p.as_str(),
                _ => continue,
            };
            let handler = match route_fields.get("handler") {
                Some(h) => h,
                None => continue,
            };

            if route_method != req_method {
                continue;
            }

            if let Some(params) = router::match_path(route_path, req_path) {
                let pc = params.len();
                if pc < best_param_count {
                    best = Some((handler, params));
                    best_param_count = pc;
                    if pc == 0 {
                        break; // exact match, can't get more specific
                    }
                }
            }
        }
    }

    if let Some((handler, params)) = best {
        let enriched = inject_params(req_record, &params);
        return Some(apply_function(handler, &[enriched], source, context, Some(node)));
    }

    None
}

/// Inject path parameters into a request record as a `params` field.
fn inject_params<'a>(
    req: &RuntimeValue<'a>,
    params: &[(String, String)],
) -> RuntimeValue<'a> {
    let mut fields = match req {
        RuntimeValue::Record(f) => f.clone(),
        _ => return req.clone(),
    };
    let mut param_fields = std::collections::HashMap::new();
    for (key, value) in params {
        param_fields.insert(key.clone(), RuntimeValue::String(value.clone()));
    }
    fields.insert("params".to_string(), RuntimeValue::Record(param_fields));
    RuntimeValue::Record(fields)
}

/// Apply a NativeClosure by prepending the piped value to its captured args.
fn apply_native_closure<'a>(
    name: &str,
    piped_val: &RuntimeValue<'a>,
    captured_args: &[RuntimeValue<'a>],
    node: &Node<'a>,
    _source: &str,
    _context: &mut Context<'a>,
) -> Result<RuntimeValue<'a>, RuntimeError> {
    match name {
        "Router.get" | "Router.post" | "Router.put" | "Router.delete" => {
            let method = name.strip_prefix("Router.").unwrap().to_uppercase();
            if captured_args.len() != 2 {
                return Err(err_at(
                    format!("{} NativeClosure expects 2 captured args, got {}", name, captured_args.len()),
                    node, _context,
                ));
            }
            router::add_route(&method, piped_val, &captured_args[0], &captured_args[1])
                .map_err(|msg| err_at(msg, node, _context))
        }
        _ => Err(err_at(
            format!("Unknown NativeClosure: {}", name),
            node, _context,
        )),
    }
}

pub fn eval<'a>(node: &Node<'a>, source: &str, context: &mut Context<'a>) -> Result<RuntimeValue<'a>, RuntimeError> {
    match node.kind() {
        "source_file" | "module_decl" => {
            let mut last_val = RuntimeValue::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                last_val = eval(&child, source, context)?;
            }
            Ok(last_val)
        }
        // Import declarations and where blocks are processed before eval; skip them.
        "import_decl" | "where_block" | "inline_test" | "prelude_decl" => {
            Ok(RuntimeValue::Unit)
        }
        "function_def" => {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            let body_node = node.child_by_field_name("body").unwrap();

            if body_node.kind() == "lambda" {
                // Lambda evaluates to Function(args, body) — store it
                let body_val = eval(&body_node, source, context)?;
                context.set(name, body_val);
            } else {
                // Non-lambda body (block, expression, constructor, etc.)
                // Store as zero-arg function for deferred evaluation
                context.set(name, RuntimeValue::Function(Vec::new(), body_node));
            }
            Ok(RuntimeValue::Unit)
        }
        "type_def" => {
            // Register enum constructors as values in context
            if let Some(def_node) = node.child_by_field_name("def") {
                if def_node.kind() == "variant_list" {
                    let mut cursor = def_node.walk();
                    for child in def_node.children(&mut cursor) {
                        if child.kind() == "variant" {
                            if let Some(vname_node) = child.child_by_field_name("name") {
                                let vname = vname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                                // Register nullary constructors as enum values
                                // Constructors with payload will be created via call_expression
                                context.set(vname.clone(), RuntimeValue::Enum(vname, Vec::new()));
                            }
                        }
                    }
                }
            }
            Ok(RuntimeValue::Unit)
        }
        "lambda" | "lambda_expression" => {
            let mut args = Vec::new();
            let count = node.named_child_count();
            // All named children except the last are parameters; last is body
            for i in 0..count - 1 {
                let child = node.named_child(i).unwrap();
                let name = extract_param_name(&child, source);
                if let Some(n) = name {
                    args.push(n);
                }
            }

            let body = node.named_child(count - 1).unwrap();
            let captured = context.snapshot();
            Ok(RuntimeValue::Closure(args, body, captured))
        }
        "call_expression" => {
             // func(arg1, arg2)
             let func_node = node.named_child(0).unwrap();

             // Check for builtin dispatch: Module.method!(args)
             if func_node.kind() == "field_expression" {
                 let obj_node = func_node.named_child(0).unwrap();
                 let method_node = func_node.named_child(1).unwrap();
                 let obj_name = obj_node.utf8_text(source.as_bytes()).unwrap();
                 let method_name = method_node.utf8_text(source.as_bytes()).unwrap();
                 let qualified = format!("{}.{}", obj_name, method_name);

                 if let Some(builtin_fn) = context.builtins.get(&qualified) {
                     let builtin_fn = *builtin_fn;
                     let mut arg_strs = Vec::new();
                     let total_children = node.named_child_count();
                     for i in 1..total_children {
                         let arg_node = node.named_child(i).unwrap();
                         let val = eval(&arg_node, source, context)?;
                         propagate!(val);
                         arg_strs.push(val.to_string());
                     }
                     let result_str = builtin_fn(&arg_strs)
                         .map_err(|msg| err_at(msg, node, context))?;
                     return Ok(parse_builtin_result(&result_str));
                 }

                 // HOF dispatch: functions that take closures and need eval access
                 if let Some(result) = dispatch_hof(&qualified, node, source, context)? {
                     return Ok(result);
                 }

                 if let Some(native_fn) = context.natives.get(&qualified) {
                     let native_fn = *native_fn;
                     let total_children = node.named_child_count();
                     let mut arg_vals = Vec::new();
                     for i in 1..total_children {
                         let arg_node = node.named_child(i).unwrap();
                         let val = eval(&arg_node, source, context)?;
                         propagate!(val);
                         arg_vals.push(val);
                     }
                     return native_fn(&arg_vals)
                         .map_err(|msg| err_at(msg, node, context));
                 }

                 // No builtin/native matched — check if it's a known module not in scope
                 if obj_node.kind() == "type_identifier" {
                     if let Some(err_msg) = context.check_module_scope(obj_name) {
                         return Err(err_at(err_msg, node, context));
                     }
                 }
             }

             let func_val = eval(&func_node, source, context)?;
             propagate!(func_val);

             // Eval args (shared for all call forms)
             let mut arg_vals = Vec::new();
             let total_children = node.named_child_count();
             for i in 1..total_children {
                 let arg_node = node.named_child(i).unwrap();
                 let val = eval(&arg_node, source, context)?;
                 propagate!(val);
                 arg_vals.push(val);
             }

             // Resolve the function name for stack frame tracking
             let func_name = func_node.utf8_text(source.as_bytes()).unwrap_or("<unknown>");

             match func_val {
                 RuntimeValue::Function(param_names, body_node) => {
                     if arg_vals.len() != param_names.len() {
                         return Err(err_at(
                             format!("Arg count mismatch: expected {}, got {}", param_names.len(), arg_vals.len()),
                             node, context,
                         ));
                     }

                     context.push_frame(func_name.to_string(), node);
                     context.enter_scope();
                     for (name, val) in param_names.iter().zip(arg_vals.into_iter()) {
                         context.set(name.clone(), val);
                     }

                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     context.pop_frame();
                     unwrap_early_return(result)
                 }
                 RuntimeValue::Closure(param_names, body_node, captured_env) => {
                     if arg_vals.len() != param_names.len() {
                         return Err(err_at(
                             format!("Arg count mismatch: expected {}, got {}", param_names.len(), arg_vals.len()),
                             node, context,
                         ));
                     }

                     context.push_frame(func_name.to_string(), node);
                     context.enter_scope();
                     for (k, v) in &captured_env {
                         context.set(k.clone(), v.clone());
                     }
                     for (name, val) in param_names.iter().zip(arg_vals.into_iter()) {
                         context.set(name.clone(), val);
                     }

                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     context.pop_frame();
                     unwrap_early_return(result)
                }
                RuntimeValue::Enum(ctor_name, _) => {
                     Ok(RuntimeValue::Enum(ctor_name, arg_vals))
                 }
                 RuntimeValue::NativeClosure(name, captured_args) => {
                     if arg_vals.len() != 1 {
                         return Err(err_at(
                             format!("NativeClosure {} expects 1 argument, got {}", name, arg_vals.len()),
                             node, context,
                         ));
                     }
                     apply_native_closure(&name, &arg_vals[0], &captured_args, node, source, context)
                 }
                 _ => Err(err_at(
                     format!("Not a function: {}", func_name),
                     node, context,
                 ))
             }
        }
        "block" | "block_expression" => {
            context.enter_scope();
            let mut last_val = RuntimeValue::Unit;
            let count = node.named_child_count();
            for i in 0..count {
                let child = node.named_child(i).unwrap();
                last_val = eval(&child, source, context)?;
                if matches!(last_val, RuntimeValue::EarlyReturn(_)) {
                    break;
                }
            }
            context.exit_scope();
            Ok(last_val)
        }
        "let_binding" | "let_expression" => {
            let pattern = node.named_child(0).unwrap();
            let expr = node.named_child(1).unwrap();
            let val = eval(&expr, source, context)?;

            // Propagate EarlyReturn without binding
            if matches!(val, RuntimeValue::EarlyReturn(_)) {
                return Ok(val);
            }

            bind_pattern(&pattern, &val, source, context)
                .map_err(|msg| err_at(msg, node, context))?;
            Ok(RuntimeValue::Unit)
        }
        "identifier" | "lower_identifier" | "effect_identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            context.get(name).cloned().ok_or_else(|| err_at(format!("Undefined variable: {}", name), node, context))
        }
        "type_identifier" => {
            // Nullary constructor or type reference in expression context
            let name = node.utf8_text(source.as_bytes()).unwrap();
            if let Some(val) = context.get(name) {
                Ok(val.clone())
            } else {
                // Treat as nullary enum constructor
                Ok(RuntimeValue::Enum(name.to_string(), Vec::new()))
            }
        }
        "integer_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            let val = text.parse::<i64>()
                .map_err(|_| err_at("Invalid integer", node, context))?;
            Ok(RuntimeValue::Int(val))
        }
        "float_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            let val = text.parse::<f64>()
                .map_err(|_| err_at("Invalid float", node, context))?;
            Ok(RuntimeValue::Float(val))
        }
        "string_literal" => {
            // Range-based approach: extract raw text between named children
            // (interpolation and escape_sequence are named; raw text is absorbed by tree-sitter)
            let mut result = String::new();
            let bytes = source.as_bytes();

            // Start after the opening quote
            let str_start = node.start_byte() + 1; // skip "
            let str_end = node.end_byte() - 1;     // skip "

            let named_count = node.named_child_count();
            if named_count == 0 {
                // Simple string with no interpolation or escapes
                let raw = &source[str_start..str_end];
                result.push_str(raw);
            } else {
                let mut cursor = str_start;
                for i in 0..named_count {
                    let child = node.named_child(i).unwrap();
                    let child_start = child.start_byte();
                    let child_end = child.end_byte();

                    // Raw text before this named child
                    if cursor < child_start {
                        result.push_str(&source[cursor..child_start]);
                    }

                    match child.kind() {
                        "interpolation" => {
                            let expr = child.named_child(0).unwrap();
                            let val = eval(&expr, source, context)?;
                            propagate!(val);
                            result.push_str(&val.to_string());
                        }
                        "escape_sequence" => {
                            let esc = child.utf8_text(bytes).unwrap();
                            match esc {
                                "\\n" => result.push('\n'),
                                "\\t" => result.push('\t'),
                                "\\r" => result.push('\r'),
                                "\\\\" => result.push('\\'),
                                "\\\"" => result.push('"'),
                                "\\0" => result.push('\0'),
                                other => result.push_str(other),
                            }
                        }
                        _ => {}
                    }
                    cursor = child_end;
                }
                // Raw text after the last named child
                if cursor < str_end {
                    result.push_str(&source[cursor..str_end]);
                }
            }
            Ok(RuntimeValue::String(result))
        }
        "boolean_literal" => {
             let text = node.utf8_text(source.as_bytes()).unwrap();
             Ok(RuntimeValue::Bool(text == "true"))
        }
        "binary_expression" => {
             let op = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();

             // Short-circuit logical operators: evaluate left first,
             // only evaluate right if needed
             match op {
                 "&&" => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     propagate!(left);
                     match left {
                         RuntimeValue::Bool(false) => Ok(RuntimeValue::Bool(false)),
                         RuntimeValue::Bool(true) => {
                             let right = eval(&node.named_child(1).unwrap(), source, context)?;
                             propagate!(right);
                             match right {
                                 RuntimeValue::Bool(b) => Ok(RuntimeValue::Bool(b)),
                                 _ => Err(err_at("Right operand of && must be Bool", node, context)),
                             }
                         }
                         _ => Err(err_at("Left operand of && must be Bool", node, context)),
                     }
                 }
                 "||" => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     propagate!(left);
                     match left {
                         RuntimeValue::Bool(true) => Ok(RuntimeValue::Bool(true)),
                         RuntimeValue::Bool(false) => {
                             let right = eval(&node.named_child(1).unwrap(), source, context)?;
                             propagate!(right);
                             match right {
                                 RuntimeValue::Bool(b) => Ok(RuntimeValue::Bool(b)),
                                 _ => Err(err_at("Right operand of || must be Bool", node, context)),
                             }
                         }
                         _ => Err(err_at("Left operand of || must be Bool", node, context)),
                     }
                 }
                 _ => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     propagate!(left);
                     let right = eval(&node.named_child(1).unwrap(), source, context)?;
                     propagate!(right);
                     match (left, right) {
                        (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                            "+" => Ok(RuntimeValue::Int(l + r)),
                            "-" => Ok(RuntimeValue::Int(l - r)),
                            "*" => Ok(RuntimeValue::Int(l * r)),
                            "/" => {
                                if r == 0 {
                                    Err(err_at("Division by zero", node, context))
                                } else {
                                    Ok(RuntimeValue::Int(l / r))
                                }
                            }
                            "%" => {
                                if r == 0 {
                                    Err(err_at("Modulo by zero", node, context))
                                } else {
                                    Ok(RuntimeValue::Int(l % r))
                                }
                            }
                            "==" => Ok(RuntimeValue::Bool(l == r)),
                            "!=" => Ok(RuntimeValue::Bool(l != r)),
                            "<" => Ok(RuntimeValue::Bool(l < r)),
                            ">" => Ok(RuntimeValue::Bool(l > r)),
                            "<=" => Ok(RuntimeValue::Bool(l <= r)),
                            ">=" => Ok(RuntimeValue::Bool(l >= r)),
                            _ => Err(err_at(format!("Unknown int operator {}", op), node, context))
                        },
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => match op {
                            "+" => Ok(RuntimeValue::Float(l + r)),
                            "-" => Ok(RuntimeValue::Float(l - r)),
                            "*" => Ok(RuntimeValue::Float(l * r)),
                            "/" => {
                                if r == 0.0 {
                                    Err(err_at("Division by zero", node, context))
                                } else {
                                    Ok(RuntimeValue::Float(l / r))
                                }
                            }
                            "%" => {
                                if r == 0.0 {
                                    Err(err_at("Modulo by zero", node, context))
                                } else {
                                    Ok(RuntimeValue::Float(l % r))
                                }
                            }
                            "==" => Ok(RuntimeValue::Bool(l == r)),
                            "!=" => Ok(RuntimeValue::Bool(l != r)),
                            "<" => Ok(RuntimeValue::Bool(l < r)),
                            ">" => Ok(RuntimeValue::Bool(l > r)),
                            "<=" => Ok(RuntimeValue::Bool(l <= r)),
                            ">=" => Ok(RuntimeValue::Bool(l >= r)),
                            _ => Err(err_at(format!("Unknown float operator {}", op), node, context))
                        },
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => match op {
                            "+" => Ok(RuntimeValue::String(format!("{}{}", l, r))),
                            "==" => Ok(RuntimeValue::Bool(l == r)),
                            "!=" => Ok(RuntimeValue::Bool(l != r)),
                            _ => Err(err_at(format!("Unknown string operator {}", op), node, context))
                        },
                        (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => match op {
                            "==" => Ok(RuntimeValue::Bool(l == r)),
                            "!=" => Ok(RuntimeValue::Bool(l != r)),
                            _ => Err(err_at(format!("Unknown bool operator {}", op), node, context))
                        },
                        (l, r) => Err(err_at(format!("Invalid operands for {}: {} and {}", op, l, r), node, context))
                     }
                 }
             }
        }
        "unary_expression" => {
             let op = node.child(0).unwrap().utf8_text(source.as_bytes()).unwrap();
             let operand = eval(&node.named_child(0).unwrap(), source, context)?;
             propagate!(operand);
             match (op, operand) {
                 ("not", RuntimeValue::Bool(b)) => Ok(RuntimeValue::Bool(!b)),
                 ("-", RuntimeValue::Int(i)) => Ok(RuntimeValue::Int(-i)),
                 ("-", RuntimeValue::Float(f)) => Ok(RuntimeValue::Float(-f)),
                 ("not", v) => Err(err_at(format!("Cannot apply not to {}", v), node, context)),
                 ("-", v) => Err(err_at(format!("Cannot negate {}", v), node, context)),
                 _ => Err(err_at(format!("Unknown unary operator {}", op), node, context)),
             }
        }
        "try_expression" => {
            // expr? — unwrap Some/Ok or propagate None/Err as EarlyReturn
            let operand = eval(&node.named_child(0).unwrap(), source, context)?;
            match &operand {
                RuntimeValue::Enum(name, payload) => match name.as_str() {
                    "Some" if payload.len() == 1 => Ok(payload[0].clone()),
                    "None" if payload.is_empty() => Ok(RuntimeValue::EarlyReturn(Box::new(operand))),
                    "Ok" if payload.len() == 1 => Ok(payload[0].clone()),
                    "Err" => Ok(RuntimeValue::EarlyReturn(Box::new(operand))),
                    _ => Err(err_at(format!("? operator requires Option or Result, got {}(..)", name), node, context)),
                },
                RuntimeValue::EarlyReturn(_) => Ok(operand), // propagate
                _ => Err(err_at(format!("? operator requires Option or Result, got {}", operand), node, context)),
            }
        }
        "range_expression" => {
             let start = eval(&node.named_child(0).unwrap(), source, context)?;
             propagate!(start);
             let end = eval(&node.named_child(1).unwrap(), source, context)?;
             propagate!(end);
             match (start, end) {
                 (RuntimeValue::Int(s), RuntimeValue::Int(e)) => {
                     let vals: Vec<RuntimeValue<'a>> = (s..e)
                         .map(RuntimeValue::Int)
                         .collect();
                     Ok(RuntimeValue::List(vals))
                 }
                 (s, e) => Err(err_at(format!("Range requires Int operands, got {} and {}", s, e), node, context)),
             }
        }
        "if_expression" => {
            let cond = eval(&node.named_child(0).unwrap(), source, context)?;
            propagate!(cond);
            if let RuntimeValue::Bool(b) = cond {
                if b {
                     eval(&node.named_child(1).unwrap(), source, context)
                } else if let Some(else_branch) = node.named_child(2) {
                     eval(&else_branch, source, context)
                } else {
                     Ok(RuntimeValue::Unit)
                }
            } else {
                Err(err_at("Condition must be boolean", node, context))
            }
        }
        "parenthesized_expression" => {
             eval(&node.named_child(0).unwrap(), source, context)
        }
        "literal" => {
             if let Some(child) = node.named_child(0) {
                 eval(&child, source, context)
             } else {
                 Ok(RuntimeValue::Unit)
             }
        }
        "tuple_expression" => {
             let count = node.named_child_count();
             if count == 0 {
                 return Ok(RuntimeValue::Unit);
             }
             let mut vals = Vec::new();
             for i in 0..count {
                 let val = eval(&node.named_child(i).unwrap(), source, context)?;
                 propagate!(val);
                 vals.push(val);
             }
             Ok(RuntimeValue::Tuple(vals))
        }
        "list_expression" => {
             // [expr, expr]
             let mut vals = Vec::new();
             let mut cursor = node.walk();
             for child in node.children(&mut cursor) {
                 if child.kind() == "[" || child.kind() == "]" || child.kind() == "," {
                     continue;
                 }
                 let val = eval(&child, source, context)?;
                 propagate!(val);
                 vals.push(val);
             }
             Ok(RuntimeValue::List(vals))
        }
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            let mut fields = HashMap::new();
            let count = node.named_child_count();
            for i in 1..count {
                 let field_init = node.named_child(i).unwrap();
                 let fname_node = field_init.child(0).unwrap();
                 let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                 let val_node = field_init.child(2).unwrap();
                 let val = eval(&val_node, source, context)?;
                 propagate!(val);
                 fields.insert(fname, val);
            }
            Ok(RuntimeValue::Struct(type_name, fields))
        }
        "record_expression" => {
            let mut fields = HashMap::new();
            let count = node.named_child_count();
            for i in 0..count {
                 let field_init = node.named_child(i).unwrap();
                 let fname_node = field_init.child(0).unwrap();
                 let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                 let val_node = field_init.child(2).unwrap();
                 let val = eval(&val_node, source, context)?;
                 propagate!(val);
                 fields.insert(fname, val);
            }
            Ok(RuntimeValue::Record(fields))
        }
        "field_expression" => {
             let obj_node = node.named_child(0).unwrap();
             let field_node = node.named_child(1).unwrap();
             let field_name = field_node.utf8_text(source.as_bytes()).unwrap();

             // Check for module-not-in-scope before evaluating
             if obj_node.kind() == "type_identifier" {
                 let obj_name = obj_node.utf8_text(source.as_bytes()).unwrap();
                 if let Some(err_msg) = context.check_module_scope(obj_name) {
                     return Err(err_at(err_msg, node, context));
                 }
             }

             let obj_val = eval(&obj_node, source, context)?;
             propagate!(obj_val);
             match obj_val {
                 RuntimeValue::Struct(_, fields) | RuntimeValue::Record(fields) => {
                      fields.get(field_name).cloned().ok_or_else(|| err_at(format!("Field not found: {}", field_name), node, context))
                 }
                 _ => Err(err_at(format!("Cannot access field {} on non-struct", field_name), node, context))
             }
        }
        "pipe_expression" => {
             let left_node = node.named_child(0).unwrap();
             let right_node = node.named_child(1).unwrap();

             let left_val = eval(&left_node, source, context)?;
             propagate!(left_val);
             let right_val = eval(&right_node, source, context)?;
             propagate!(right_val);

             match right_val {
                 RuntimeValue::Function(params, body_node) => {
                     if params.len() != 1 {
                         return Err(err_at(format!("Pipe expects a function with 1 argument, found {}", params.len()), node, context));
                     }
                     context.enter_scope();
                     context.set(params[0].clone(), left_val);
                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                 }
                 RuntimeValue::Closure(params, body_node, captured_env) => {
                     if params.len() != 1 {
                         return Err(err_at(format!("Pipe expects a function with 1 argument, found {}", params.len()), node, context));
                     }
                     context.enter_scope();
                     for (k, v) in &captured_env {
                         context.set(k.clone(), v.clone());
                     }
                     context.set(params[0].clone(), left_val);
                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                 }
                 RuntimeValue::NativeClosure(name, captured_args) => {
                     apply_native_closure(&name, &left_val, &captured_args, node, source, context)
                 }
                 _ => Err(err_at("Pipe target must be a function", node, context)),
             }
        }
        "for_expression" => {
            // for <pattern> in <collection> do <body>
            let pat_node = node.named_child(0).unwrap();
            let collection_node = node.named_child(1).unwrap();
            let body_node = node.named_child(2).unwrap();

            let collection = eval(&collection_node, source, context)?;
            propagate!(collection);
            match collection {
                RuntimeValue::List(items) => {
                    let mut last_val = RuntimeValue::Unit;
                    for item in &items {
                        if let Some(bindings) = match_pattern(&pat_node, item, source) {
                            context.enter_scope();
                            for (k, v) in bindings {
                                context.set(k, v);
                            }
                            last_val = eval(&body_node, source, context)?;
                            context.exit_scope();
                            if matches!(last_val, RuntimeValue::EarlyReturn(_)) {
                                break;
                            }
                        }
                    }
                    Ok(last_val)
                }
                _ => Err(err_at("for..in requires a List", node, context)),
            }
        }
        "match_expression" => {
             // match expr { pat -> body, ... }
             let expr_node = node.named_child(0).unwrap();
             let val = eval(&expr_node, source, context)?;
             propagate!(val);

             let count = node.named_child_count();
             for i in 1..count {
                 let arm = node.named_child(i).unwrap();
                 // match_arm: pattern -> expression
                 let pat = arm.child(0).unwrap();
                 let body = arm.child(2).unwrap();

                 if let Some(bindings) = match_pattern(&pat, &val, source) {
                     context.enter_scope();
                     for (k, v) in bindings {
                         context.set(k, v);
                     }
                     let result = eval(&body, source, context);
                     context.exit_scope();
                     return result;
                 }
             }
             Err(err_at("No match found", node, context))
        }
        _ => {
            // Ignore others or return Unit
            Ok(RuntimeValue::Unit)
        }
    }
}

/// Convert a builtin result string back to a RuntimeValue.
fn parse_builtin_result<'a>(s: &str) -> RuntimeValue<'a> {
    match s {
        "()" => RuntimeValue::Unit,
        "true" => RuntimeValue::Bool(true),
        "false" => RuntimeValue::Bool(false),
        _ => {
            if let Ok(i) = s.parse::<i64>() {
                RuntimeValue::Int(i)
            } else if let Ok(f) = s.parse::<f64>() {
                RuntimeValue::Float(f)
            } else {
                RuntimeValue::String(s.to_string())
            }
        }
    }
}

/// Extract a parameter name from a pattern node (handles identifier,
/// lower_identifier, identifier_pattern, lambda_parameter wrappers).
fn extract_param_name(node: &Node, source: &str) -> Option<String> {
    match node.kind() {
        "identifier" | "lower_identifier" | "effect_identifier" => {
            Some(node.utf8_text(source.as_bytes()).unwrap().to_string())
        }
        "identifier_pattern" | "lambda_parameter" => {
            if let Some(child) = node.named_child(0) {
                extract_param_name(&child, source)
            } else {
                Some(node.utf8_text(source.as_bytes()).unwrap().to_string())
            }
        }
        _ => None,
    }
}

fn match_pattern<'a>(
    pattern: &Node<'a>,
    value: &RuntimeValue<'a>,
    source: &str,
) -> Option<HashMap<String, RuntimeValue<'a>>> {
    match pattern.kind() {
        "wildcard_pattern" => Some(HashMap::new()),
        "identifier" => {
            let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
            let mut map = HashMap::new();
            map.insert(name, value.clone());
            Some(map)
        }
        "type_identifier" => {
            // Nullary constructor pattern: Red, None, Active
            let ctor_name = pattern.utf8_text(source.as_bytes()).unwrap();
            if let RuntimeValue::Enum(vname, payload) = value {
                if vname == ctor_name && payload.is_empty() {
                    Some(HashMap::new())
                } else {
                    None
                }
            } else {
                None
            }
        }
        "constructor_pattern" => {
            // Constructor pattern: Some(v), Pending(msg)
            let ctor_name_node = pattern.child(0).unwrap();
            let ctor_name = ctor_name_node.utf8_text(source.as_bytes()).unwrap();

            if let RuntimeValue::Enum(vname, payload) = value {
                if vname != ctor_name {
                    return None;
                }

                // Collect sub-patterns (skip type_identifier, parens, commas)
                let mut sub_patterns = Vec::new();
                let child_count = pattern.child_count();
                for i in 0..child_count {
                    let child = pattern.child(i).unwrap();
                    let k = child.kind();
                    if k != "type_identifier" && k != "(" && k != ")" && k != "," {
                        sub_patterns.push(child);
                    }
                }

                if sub_patterns.len() != payload.len() {
                    return None;
                }

                let mut bindings = HashMap::new();
                for (sub_pat, sub_val) in sub_patterns.iter().zip(payload.iter()) {
                    let sub_bindings = match_pattern(sub_pat, sub_val, source)?;
                    bindings.extend(sub_bindings);
                }
                Some(bindings)
            } else {
                None
            }
        }
        "literal" => {
             if let Some(child) = pattern.named_child(0) {
                 match_pattern(&child, value, source)
             } else {
                 None
             }
        }
        "integer_literal" => {
            let text = pattern.utf8_text(source.as_bytes()).unwrap();
            let p_val = text.parse::<i64>().ok()?;
            if let RuntimeValue::Int(v) = value {
                if *v == p_val { Some(HashMap::new()) } else { None }
            } else { None }
        }
        "boolean_literal" => {
            let text = pattern.utf8_text(source.as_bytes()).unwrap();
             if let RuntimeValue::Bool(v) = value {
                if *v == (text == "true") { Some(HashMap::new()) } else { None }
            } else { None }
        }
        "tuple_pattern" => {
            if let RuntimeValue::Tuple(vals) = value {
                let count = pattern.named_child_count();
                if count != vals.len() {
                    return None;
                }
                let mut bindings = HashMap::new();
                for i in 0..count {
                    let sub_pat = pattern.named_child(i).unwrap();
                    let sub_val = &vals[i];
                    let sub_bindings = match_pattern(&sub_pat, sub_val, source)?;
                    bindings.extend(sub_bindings);
                }
                Some(bindings)
            } else {
                None
            }
        }
        "identifier_pattern" => {
            // identifier_pattern wraps a lower_identifier
            if let Some(child) = pattern.named_child(0) {
                match_pattern(&child, value, source)
            } else {
                let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
                let mut map = HashMap::new();
                map.insert(name, value.clone());
                Some(map)
            }
        }
        "lower_identifier" => {
            let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
            let mut map = HashMap::new();
            map.insert(name, value.clone());
            Some(map)
        }
        _ => None
    }
}

/// Bind a pattern to a value in the current scope.
/// Returns a String error (converted to RuntimeError by callers with node context).
fn bind_pattern<'a>(
    pattern: &Node<'a>,
    value: &RuntimeValue<'a>,
    source: &str,
    context: &mut Context<'a>,
) -> Result<(), String> {
    match pattern.kind() {
        "identifier" | "lower_identifier" => {
            let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
            context.set(name, value.clone());
            Ok(())
        }
        "identifier_pattern" => {
            if let Some(child) = pattern.named_child(0) {
                bind_pattern(&child, value, source, context)
            } else {
                let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
                context.set(name, value.clone());
                Ok(())
            }
        }
        "tuple_pattern" => {
            if let RuntimeValue::Tuple(vals) = value {
                let count = pattern.named_child_count();
                if count != vals.len() {
                    return Err(format!(
                        "Tuple pattern has {} elements but value has {}",
                        count,
                        vals.len()
                    ));
                }
                for i in 0..count {
                    let sub_pat = pattern.named_child(i).unwrap();
                    bind_pattern(&sub_pat, &vals[i], source, context)?;
                }
                Ok(())
            } else {
                Err(format!("Cannot destructure {} as tuple", value))
            }
        }
        "wildcard_pattern" => Ok(()),
        _ => {
            // Fallback: treat as identifier
            let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
            context.set(name, value.clone());
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    /// Parse and evaluate Baseline source, returning main's evaluated body.
    /// Source should define main as: `main : () -> T\nmain = || expr`
    fn eval_source(source: &str) -> Result<RuntimeValue<'static>, String> {
        eval_with_prelude(source, Prelude::Script)
    }

    fn eval_with_prelude(source: &str, prelude: Prelude) -> Result<RuntimeValue<'static>, String> {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Failed to load language");
        let tree = parser.parse(source, None).expect("Failed to parse");
        let tree = Box::leak(Box::new(tree));
        let root = tree.root_node();
        let mut context = Context::with_prelude(prelude);
        eval(&root, source, &mut context).map_err(|e| e.message.clone())?;

        let main_val = context
            .get("main!")
            .or_else(|| context.get("main"))
            .cloned()
            .ok_or_else(|| "No 'main' or 'main!' binding found".to_string())?;

        // main is a Function or Closure — evaluate its body to get the actual value
        let result = match main_val {
            RuntimeValue::Function(_, body_node) => {
                context.enter_scope();
                let result = eval(&body_node, source, &mut context);
                context.exit_scope();
                result
            }
            RuntimeValue::Closure(_, body_node, captured_env) => {
                context.enter_scope();
                for (k, v) in &captured_env {
                    context.set(k.clone(), v.clone());
                }
                let result = eval(&body_node, source, &mut context);
                context.exit_scope();
                result
            }
            other => Ok(other),
        };
        unwrap_early_return(result).map_err(|e| e.message)
    }

    // -- Comparison operators --

    #[test]
    fn test_less_than_or_equal_true() {
        let result = eval_source("main : () -> Bool\nmain = || 3 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_less_than_or_equal_equal() {
        let result = eval_source("main : () -> Bool\nmain = || 5 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_less_than_or_equal_false() {
        let result = eval_source("main : () -> Bool\nmain = || 7 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_greater_than_or_equal_true() {
        let result = eval_source("main : () -> Bool\nmain = || 5 >= 3");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_greater_than_or_equal_equal() {
        let result = eval_source("main : () -> Bool\nmain = || 5 >= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_greater_than_or_equal_false() {
        let result = eval_source("main : () -> Bool\nmain = || 3 >= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    // -- Logical operators --

    #[test]
    fn test_and_true_true() {
        let result = eval_source("main : () -> Bool\nmain = || true && true");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_and_true_false() {
        let result = eval_source("main : () -> Bool\nmain = || true && false");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_and_false_short_circuits() {
        // false && <anything> should return false without evaluating right side
        let result = eval_source("main : () -> Bool\nmain = || false && true");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_or_false_false() {
        let result = eval_source("main : () -> Bool\nmain = || false || false");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_or_false_true() {
        let result = eval_source("main : () -> Bool\nmain = || false || true");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_or_true_short_circuits() {
        // true || <anything> should return true without evaluating right side
        let result = eval_source("main : () -> Bool\nmain = || true || false");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    // -- Combined expressions --

    #[test]
    fn test_comparison_in_if() {
        let source = "main : () -> Int\nmain = || if 3 <= 5 then 42 else 0";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_logical_and_with_comparisons() {
        let source = "main : () -> Bool\nmain = || 3 <= 5 && 10 >= 8";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    // -- String operations --

    #[test]
    fn test_string_concatenation() {
        let result = eval_source("main : () -> String\nmain = || \"hello\" + \" world\"");
        assert_eq!(result, Ok(RuntimeValue::String("hello world".to_string())));
    }

    #[test]
    fn test_string_equality() {
        let result = eval_source("main : () -> Bool\nmain = || \"abc\" == \"abc\"");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_string_inequality() {
        let result = eval_source("main : () -> Bool\nmain = || \"abc\" != \"xyz\"");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_string_equality_false() {
        let result = eval_source("main : () -> Bool\nmain = || \"abc\" == \"xyz\"");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    // -- Unary expressions --

    #[test]
    fn test_not_true() {
        let result = eval_source("main : () -> Bool\nmain = || not true");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_not_false() {
        let result = eval_source("main : () -> Bool\nmain = || not false");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_negate_int() {
        let result = eval_source("main : () -> Int\nmain = || -5");
        assert_eq!(result, Ok(RuntimeValue::Int(-5)));
    }

    #[test]
    fn test_negate_positive() {
        let result = eval_source("main : () -> Int\nmain = || -42");
        assert_eq!(result, Ok(RuntimeValue::Int(-42)));
    }

    // -- Range expressions --

    #[test]
    fn test_range_basic() {
        let result = eval_source("main : () -> List\nmain = || 1..5");
        assert_eq!(
            result,
            Ok(RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
                RuntimeValue::Int(4),
            ]))
        );
    }

    #[test]
    fn test_range_single() {
        let result = eval_source("main : () -> List\nmain = || 3..4");
        assert_eq!(
            result,
            Ok(RuntimeValue::List(vec![RuntimeValue::Int(3)]))
        );
    }

    #[test]
    fn test_range_empty() {
        let result = eval_source("main : () -> List\nmain = || 5..5");
        assert_eq!(result, Ok(RuntimeValue::List(vec![])));
    }

    // -- Tuple expressions --

    #[test]
    fn test_tuple_creation() {
        let result = eval_source("main : () -> (Int, String)\nmain = || (1, \"a\")");
        assert_eq!(
            result,
            Ok(RuntimeValue::Tuple(vec![
                RuntimeValue::Int(1),
                RuntimeValue::String("a".to_string()),
            ]))
        );
    }

    #[test]
    fn test_tuple_three_elements() {
        let result = eval_source("main : () -> (Int, Int, Int)\nmain = || (1, 2, 3)");
        assert_eq!(
            result,
            Ok(RuntimeValue::Tuple(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ]))
        );
    }

    #[test]
    fn test_unit_literal() {
        let result = eval_source("main : () -> Unit\nmain = || ()");
        assert_eq!(result, Ok(RuntimeValue::Unit));
    }

    #[test]
    fn test_tuple_destructuring() {
        let source = "main : () -> Int\nmain = || {\n  let (x, y) = (10, 20)\n  x + y\n}";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(30)));
    }

    // -- Entry point detection --

    #[test]
    fn test_effectful_main() {
        // main! should be found and executed
        let result = eval_source("main! : () -> {Console} Int\nmain! = || 42");
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    // -- Block expressions --

    #[test]
    fn test_block_multiple_statements() {
        let source = "main : () -> Int\nmain = || {\n  let x = 10\n  let y = 20\n  x + y\n}";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(30)));
    }

    // -- Float expressions --

    #[test]
    fn test_float_literal() {
        let result = eval_source("main : () -> Float\nmain = || 3.14");
        assert_eq!(result, Ok(RuntimeValue::Float(3.14)));
    }

    #[test]
    fn test_float_addition() {
        let result = eval_source("main : () -> Float\nmain = || 1.5 + 2.5");
        assert_eq!(result, Ok(RuntimeValue::Float(4.0)));
    }

    #[test]
    fn test_float_division() {
        let result = eval_source("main : () -> Float\nmain = || 10.0 / 3.0");
        match result {
            Ok(RuntimeValue::Float(f)) => {
                let expected = 10.0_f64 / 3.0_f64;
                assert!((f - expected).abs() < f64::EPSILON, "Expected approximately {}, got {}", expected, f);
            }
            other => panic!("Expected Ok(Float(...)), got {:?}", other),
        }
    }

    #[test]
    fn test_float_comparison() {
        let result = eval_source("main : () -> Bool\nmain = || 3.14 > 2.0");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_float_negate() {
        let result = eval_source("main : () -> Float\nmain = || -3.14");
        assert_eq!(result, Ok(RuntimeValue::Float(-3.14)));
    }

    #[test]
    fn test_float_division_by_zero() {
        let result = eval_source("main : () -> Float\nmain = || 1.0 / 0.0");
        assert!(result.is_err(), "Expected error for division by zero, got {:?}", result);
    }

    // -- Option type --

    #[test]
    fn test_option_some_constructor() {
        let result = eval_source("main : () -> Option\nmain = || Some(42)");
        assert_eq!(result, Ok(RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(42)])));
    }

    #[test]
    fn test_option_none_constructor() {
        let result = eval_source("main : () -> Option\nmain = || None");
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_option_unwrap_some() {
        let source = "main : () -> Int\nmain = || Option.unwrap(Some(42))";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_option_unwrap_none_errors() {
        let source = "main : () -> Int\nmain = || Option.unwrap(None)";
        let result = eval_source(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_option_unwrap_or_some() {
        let source = "main : () -> Int\nmain = || Option.unwrap_or(Some(42), 0)";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_option_unwrap_or_none() {
        let source = "main : () -> Int\nmain = || Option.unwrap_or(None, 99)";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(99)));
    }

    #[test]
    fn test_option_is_some() {
        let source = "main : () -> Bool\nmain = || Option.is_some(Some(1))";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_option_is_none() {
        let source = "main : () -> Bool\nmain = || Option.is_none(None)";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_option_map_some() {
        let source = "main : () -> Option\nmain = || Option.map(Some(10), |x| x + 1)";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(11)])));
    }

    #[test]
    fn test_option_map_none() {
        let source = "main : () -> Option\nmain = || Option.map(None, |x| x + 1)";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_option_match() {
        let source = "main : () -> Int\nmain = || {\n  let x = Some(42)\n  match x\n    Some(v) -> v\n    None -> 0\n}";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    // -- Result type --

    #[test]
    fn test_result_ok_constructor() {
        let result = eval_source("main : () -> Result\nmain = || Ok(42)");
        assert_eq!(result, Ok(RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(42)])));
    }

    #[test]
    fn test_result_err_constructor() {
        let result = eval_source("main : () -> Result\nmain = || Err(\"failed\")");
        assert_eq!(result, Ok(RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("failed".into())])));
    }

    #[test]
    fn test_result_unwrap_ok() {
        let result = eval_source("main : () -> Int\nmain = || Result.unwrap(Ok(42))");
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_result_unwrap_err_errors() {
        let result = eval_source("main : () -> Int\nmain = || Result.unwrap(Err(\"e\"))");
        assert!(result.is_err());
    }

    #[test]
    fn test_result_is_ok() {
        let result = eval_source("main : () -> Bool\nmain = || Result.is_ok(Ok(1))");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_result_is_err() {
        let result = eval_source("main : () -> Bool\nmain = || Result.is_err(Err(\"e\"))");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_result_map_ok() {
        let result = eval_source("main : () -> Result\nmain = || Result.map(Ok(10), |x| x + 1)");
        assert_eq!(result, Ok(RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(11)])));
    }

    #[test]
    fn test_result_map_err() {
        let result = eval_source("main : () -> Result\nmain = || Result.map(Err(\"e\"), |x| x + 1)");
        assert_eq!(result, Ok(RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("e".into())])));
    }

    // -- String module --

    #[test]
    fn test_string_length() {
        let result = eval_source("main : () -> Int\nmain = || String.length(\"hello\")");
        assert_eq!(result, Ok(RuntimeValue::Int(5)));
    }

    #[test]
    fn test_string_trim() {
        let result = eval_source("main : () -> String\nmain = || String.trim(\" hi \")");
        assert_eq!(result, Ok(RuntimeValue::String("hi".into())));
    }

    #[test]
    fn test_string_contains() {
        let result = eval_source("main : () -> Bool\nmain = || String.contains(\"hello\", \"ell\")");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_string_starts_with() {
        let result = eval_source("main : () -> Bool\nmain = || String.starts_with(\"hello\", \"he\")");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_string_to_upper() {
        let result = eval_source("main : () -> String\nmain = || String.to_upper(\"hi\")");
        assert_eq!(result, Ok(RuntimeValue::String("HI".into())));
    }

    #[test]
    fn test_string_split() {
        let result = eval_source("main : () -> List\nmain = || String.split(\"a,b,c\", \",\")");
        assert_eq!(result, Ok(RuntimeValue::List(vec![
            RuntimeValue::String("a".into()),
            RuntimeValue::String("b".into()),
            RuntimeValue::String("c".into()),
        ])));
    }

    #[test]
    fn test_string_join() {
        let result = eval_source("main : () -> String\nmain = || String.join([\"a\", \"b\"], \"-\")");
        assert_eq!(result, Ok(RuntimeValue::String("a-b".into())));
    }

    #[test]
    fn test_string_slice() {
        let result = eval_source("main : () -> String\nmain = || String.slice(\"hello\", 1, 3)");
        assert_eq!(result, Ok(RuntimeValue::String("el".into())));
    }

    // -- List module --

    #[test]
    fn test_list_length() {
        let result = eval_source("main : () -> Int\nmain = || List.length([1, 2, 3])");
        assert_eq!(result, Ok(RuntimeValue::Int(3)));
    }

    #[test]
    fn test_list_head() {
        let result = eval_source("main : () -> Option\nmain = || List.head([1, 2, 3])");
        assert_eq!(result, Ok(RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(1)])));
    }

    #[test]
    fn test_list_reverse() {
        let result = eval_source("main : () -> List\nmain = || List.reverse([1, 2, 3])");
        assert_eq!(result, Ok(RuntimeValue::List(vec![
            RuntimeValue::Int(3), RuntimeValue::Int(2), RuntimeValue::Int(1),
        ])));
    }

    #[test]
    fn test_list_sort() {
        let result = eval_source("main : () -> List\nmain = || List.sort([3, 1, 2])");
        assert_eq!(result, Ok(RuntimeValue::List(vec![
            RuntimeValue::Int(1), RuntimeValue::Int(2), RuntimeValue::Int(3),
        ])));
    }

    #[test]
    fn test_list_map() {
        let result = eval_source("main : () -> List\nmain = || List.map([1, 2, 3], |x| x * 2)");
        assert_eq!(result, Ok(RuntimeValue::List(vec![
            RuntimeValue::Int(2), RuntimeValue::Int(4), RuntimeValue::Int(6),
        ])));
    }

    #[test]
    fn test_list_filter() {
        let result = eval_source("main : () -> List\nmain = || List.filter([1, 2, 3, 4], |x| x > 2)");
        assert_eq!(result, Ok(RuntimeValue::List(vec![
            RuntimeValue::Int(3), RuntimeValue::Int(4),
        ])));
    }

    #[test]
    fn test_list_fold() {
        let result = eval_source("main : () -> Int\nmain = || List.fold([1, 2, 3], 0, |acc, x| acc + x)");
        assert_eq!(result, Ok(RuntimeValue::Int(6)));
    }

    #[test]
    fn test_list_find_some() {
        let result = eval_source("main : () -> Option\nmain = || List.find([1, 2, 3], |x| x == 2)");
        assert_eq!(result, Ok(RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(2)])));
    }

    #[test]
    fn test_list_find_none() {
        let result = eval_source("main : () -> Option\nmain = || List.find([1, 2, 3], |x| x == 9)");
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    // -- Try (?) operator --

    #[test]
    fn test_try_some_unwraps() {
        let result = eval_source(
            "get : () -> Option<Int>\nget = || Some(42)\nmain : () -> Int\nmain = || get()?",
        );
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_try_none_propagates() {
        let result = eval_source(
            "get : () -> Option<Int>\nget = || None\nmain : () -> Option<Int>\nmain = || {\n  let x = get()?\n  Some(x + 1)\n}",
        );
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_try_ok_unwraps() {
        let result = eval_source(
            "get : () -> Result<Int, String>\nget = || Ok(10)\nmain : () -> Int\nmain = || get()?",
        );
        assert_eq!(result, Ok(RuntimeValue::Int(10)));
    }

    #[test]
    fn test_try_err_propagates() {
        let result = eval_source(
            "get : () -> Result<Int, String>\nget = || Err(\"fail\")\nmain : () -> Result<Int, String>\nmain = || {\n  let x = get()?\n  Ok(x)\n}",
        );
        assert_eq!(
            result,
            Ok(RuntimeValue::Enum(
                "Err".into(),
                vec![RuntimeValue::String("fail".into())]
            ))
        );
    }

    #[test]
    fn test_try_on_plain_value_errors() {
        let result = eval_source("main : () -> Int\nmain = || 42?");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("? operator requires Option or Result"));
    }

    // -- Early return propagation through nested expressions --

    #[test]
    fn test_try_propagates_through_binary_op() {
        let result = eval_source(
            "get : () -> Option<Int>\nget = || None\nmain : () -> Option<Int>\nmain = || {\n  let r = get()? + 1\n  Some(r)\n}",
        );
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_try_first_none_skips_subsequent() {
        // first() returns None, so second() should never be reached
        let result = eval_source(
            "first : () -> Option<Int>\nfirst = || None\nsecond : () -> Option<Int>\nsecond = || Some(99)\nmain : () -> Option<Int>\nmain = || {\n  let a = first()?\n  let b = second()?\n  Some(a + b)\n}",
        );
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_try_propagates_through_function_arg() {
        // ? inside a function argument should propagate
        let result = eval_source(
            "get : () -> Option<Int>\nget = || None\nadd : Int -> Int\nadd = |x| x + 1\nmain : () -> Option<Int>\nmain = || {\n  let r = add(get()?)\n  Some(r)\n}",
        );
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    #[test]
    fn test_try_propagates_through_if_condition() {
        let result = eval_source(
            "get : () -> Option<Bool>\nget = || None\nmain : () -> Option<Int>\nmain = || {\n  let r = if get()? then 1 else 2\n  Some(r)\n}",
        );
        assert_eq!(result, Ok(RuntimeValue::Enum("None".into(), vec![])));
    }

    // -- Module namespace resolution --

    #[test]
    fn test_module_in_scope_resolves() {
        let result = eval_with_prelude(
            "main : () -> Int\nmain = || String.length(\"hello\")",
            Prelude::Core,
        );
        assert_eq!(result, Ok(RuntimeValue::Int(5)));
    }

    #[test]
    fn test_module_not_in_scope_error() {
        let result = eval_with_prelude(
            "main : () -> Int\nmain = || String.length(\"hello\")",
            Prelude::Minimal,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("Module `String` is not available"), "got: {}", err);
    }

    #[test]
    fn test_effect_module_not_in_pure_prelude() {
        let result = eval_with_prelude(
            "main! : () -> {Console} ()\nmain! = || Console.println!(\"hi\")",
            Prelude::Pure,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("Module `Console` is not available"), "got: {}", err);
    }
}
