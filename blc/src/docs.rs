//! Documentation generator for the Baseline standard library.
//!
//! Walks the builtin registry, native function list, and type inference schemas
//! to emit structured JSON or human-readable markdown describing all available
//! modules and their functions.

use std::collections::{BTreeMap, HashMap};

use serde::Serialize;

use crate::analysis::infer::{builtin_generic_schemas, InferCtx};
use crate::analysis::types::Type;
use crate::prelude::Prelude;

// ---------------------------------------------------------------------------
// Output types
// ---------------------------------------------------------------------------

#[derive(Serialize)]
pub struct DocsOutput {
    pub modules: Vec<ModuleDoc>,
}

#[derive(Serialize)]
pub struct ModuleDoc {
    pub name: String,
    pub functions: Vec<FunctionDoc>,
}

#[derive(Serialize)]
pub struct FunctionDoc {
    pub name: String,
    pub signature: String,
    pub effects: Vec<String>,
    pub prelude_level: String,
    pub description: Option<String>,
}

// ---------------------------------------------------------------------------
// Prelude level detection
// ---------------------------------------------------------------------------

/// All prelude levels in order from most restrictive to least.
const PRELUDE_LEVELS: &[(Prelude, &str)] = &[
    (Prelude::Minimal, "minimal"),
    (Prelude::Pure, "pure"),
    (Prelude::Core, "core"),
    (Prelude::Script, "script"),
    (Prelude::Server, "server"),
];

/// Determine the earliest prelude level that includes a given module.
fn earliest_prelude_for_module(module: &str) -> &'static str {
    for (prelude, label) in PRELUDE_LEVELS {
        let in_native = prelude.native_modules().contains(&module);
        let in_builtin = prelude.builtin_modules().contains(&module);
        if in_native || in_builtin {
            return label;
        }
    }
    "server"
}

// ---------------------------------------------------------------------------
// Effect inference for functions
// ---------------------------------------------------------------------------

/// Determine the effects required by a function based on its name.
fn effects_for_function(qualified_name: &str) -> Vec<String> {
    if !qualified_name.ends_with('!') {
        return vec![];
    }

    let module = qualified_name
        .split('.')
        .next()
        .unwrap_or(qualified_name);

    let effect = match module {
        "Server" => "Http",
        other => other,
    };

    vec![effect.to_string()]
}

// ---------------------------------------------------------------------------
// Known function registry
// ---------------------------------------------------------------------------

/// All known stdlib functions with their qualified names and descriptions.
/// This is the single source of truth derived from:
/// - NativeRegistry::register_all() in vm/natives/mod.rs
/// - builtin_type_signatures() in analysis/types/builtins.rs
/// - builtin_generic_schemas() in analysis/infer.rs
///
/// Entries are (module, function_name, description).
fn known_functions() -> Vec<(&'static str, &'static str, &'static str)> {
    vec![
        // Console
        ("Console", "println!", "Print a string to stdout followed by a newline."),
        ("Console", "print!", "Print a string to stdout without a trailing newline."),
        ("Console", "error!", "Print a string to stderr."),
        ("Console", "read_line!", "Read a line of input from stdin."),
        // Log
        ("Log", "info!", "Log a message at info level."),
        ("Log", "warn!", "Log a message at warning level."),
        ("Log", "error!", "Log a message at error level."),
        ("Log", "debug!", "Log a message at debug level."),
        // Math
        ("Math", "abs", "Return the absolute value of an integer."),
        ("Math", "min", "Return the smaller of two integers."),
        ("Math", "max", "Return the larger of two integers."),
        ("Math", "clamp", "Constrain a value between a minimum and maximum."),
        ("Math", "pow", "Raise a base to an exponent."),
        // String
        ("String", "length", "Return the number of characters in a string."),
        ("String", "to_upper", "Convert all characters to uppercase."),
        ("String", "to_lower", "Convert all characters to lowercase."),
        ("String", "trim", "Remove leading and trailing whitespace."),
        ("String", "contains", "Check if a string contains a substring."),
        ("String", "starts_with", "Check if a string starts with a prefix."),
        ("String", "ends_with", "Check if a string ends with a suffix."),
        ("String", "split", "Split a string by a delimiter into a list of strings."),
        ("String", "join", "Join a list of strings with a separator."),
        ("String", "slice", "Extract a substring by start and end index."),
        ("String", "chars", "Split a string into a list of single-character strings."),
        ("String", "char_at", "Return the character at a given index."),
        ("String", "index_of", "Return the index of the first occurrence of a substring, or -1."),
        ("String", "to_int", "Parse a string as an integer."),
        ("String", "from_char_code", "Create a single-character string from a Unicode code point."),
        ("String", "char_code", "Return the Unicode code point of the first character."),
        ("String", "replace", "Replace all occurrences of a pattern with a replacement."),
        // List
        ("List", "length", "Return the number of elements in a list."),
        ("List", "head", "Return the first element, or None if the list is empty."),
        ("List", "tail", "Return all elements except the first."),
        ("List", "reverse", "Return the list in reverse order."),
        ("List", "sort", "Return the list sorted in ascending order."),
        ("List", "concat", "Concatenate two lists."),
        ("List", "contains", "Check if a list contains a given element."),
        ("List", "get", "Return the element at an index, or None if out of bounds."),
        ("List", "map", "Apply a function to every element, returning a new list."),
        ("List", "filter", "Return elements that satisfy a predicate."),
        ("List", "fold", "Reduce a list to a single value using an accumulator function."),
        ("List", "find", "Return the first element that satisfies a predicate, or None."),
        // Option
        ("Option", "unwrap", "Extract the value from Some, or panic on None."),
        ("Option", "unwrap_or", "Extract the value from Some, or return a default."),
        ("Option", "is_some", "Return true if the option contains a value."),
        ("Option", "is_none", "Return true if the option is None."),
        ("Option", "map", "Apply a function to the contained value, if present."),
        ("Option", "flat_map", "Apply a function that returns an Option, flattening the result."),
        // Result
        ("Result", "unwrap", "Extract the Ok value, or panic on Err."),
        ("Result", "unwrap_or", "Extract the Ok value, or return a default."),
        ("Result", "is_ok", "Return true if the result is Ok."),
        ("Result", "is_err", "Return true if the result is Err."),
        ("Result", "map", "Apply a function to the Ok value, if present."),
        ("Result", "and_then", "Chain a function that returns a Result, flattening the result."),
        // Int
        ("Int", "to_string", "Convert an integer to its string representation."),
        ("Int", "parse", "Parse a string as an integer."),
        // Json
        ("Json", "parse", "Parse a JSON string into a value."),
        ("Json", "to_string", "Serialize a value to a compact JSON string."),
        ("Json", "to_string_pretty", "Serialize a value to a pretty-printed JSON string."),
        // Map
        ("Map", "empty", "Create an empty map."),
        ("Map", "insert", "Insert a key-value pair, returning a new map."),
        ("Map", "get", "Look up a key, returning Some(value) or None."),
        ("Map", "remove", "Remove a key, returning a new map."),
        ("Map", "contains", "Check if a key exists in the map."),
        ("Map", "keys", "Return all keys as a list."),
        ("Map", "values", "Return all values as a list."),
        ("Map", "len", "Return the number of entries in the map."),
        ("Map", "from_list", "Create a map from a list of (key, value) pairs."),
        // Set
        ("Set", "empty", "Create an empty set."),
        ("Set", "insert", "Add an element, returning a new set."),
        ("Set", "remove", "Remove an element, returning a new set."),
        ("Set", "contains", "Check if an element exists in the set."),
        ("Set", "union", "Return the union of two sets."),
        ("Set", "intersection", "Return the intersection of two sets."),
        ("Set", "len", "Return the number of elements in the set."),
        ("Set", "from_list", "Create a set from a list of elements."),
        // Weak
        ("Weak", "downgrade", "Create a weak reference from a strong reference."),
        ("Weak", "upgrade", "Attempt to upgrade a weak reference to a strong one, returning an Option."),
        // Time
        ("Time", "now!", "Return the current Unix timestamp in milliseconds."),
        ("Time", "sleep!", "Pause execution for the given number of milliseconds."),
        // Random
        ("Random", "int!", "Generate a random integer between min and max (inclusive)."),
        ("Random", "bool!", "Generate a random boolean."),
        ("Random", "uuid!", "Generate a random UUID v4 string."),
        // Env
        ("Env", "get!", "Read an environment variable, returning None if unset."),
        ("Env", "set!", "Set an environment variable."),
        // Fs
        ("Fs", "read!", "Read a file's contents as a string."),
        ("Fs", "write!", "Write a string to a file, creating or overwriting it."),
        ("Fs", "exists!", "Check if a file path exists."),
        ("Fs", "read_file!", "Read a file's contents as a string."),
        ("Fs", "write_file!", "Write a string to a file, creating or overwriting it."),
        ("Fs", "list_dir!", "List the entries in a directory."),
        ("Fs", "with_file!", "Open a file, pass it to a callback, and close it automatically."),
        // Http
        ("Http", "get!", "Send an HTTP GET request to a URL."),
        ("Http", "post!", "Send an HTTP POST request with a body."),
        ("Http", "put!", "Send an HTTP PUT request with a body."),
        ("Http", "delete!", "Send an HTTP DELETE request to a URL."),
        ("Http", "request!", "Send a custom HTTP request from a request record."),
        // Response
        ("Response", "ok", "Create a 200 OK response with a text body."),
        ("Response", "json", "Create a 200 OK response with a JSON body."),
        ("Response", "created", "Create a 201 Created response with a text body."),
        ("Response", "no_content", "Create a 204 No Content response."),
        ("Response", "bad_request", "Create a 400 Bad Request response."),
        ("Response", "not_found", "Create a 404 Not Found response."),
        ("Response", "error", "Create a 500 Internal Server Error response."),
        ("Response", "status", "Create a response with a custom status code and body."),
        ("Response", "with_header", "Add a single header to a response."),
        ("Response", "with_headers", "Add multiple headers to a response."),
        ("Response", "redirect", "Create a 302 temporary redirect to a URL."),
        ("Response", "redirect_permanent", "Create a 301 permanent redirect to a URL."),
        // Request
        ("Request", "header", "Read a header value from the request."),
        ("Request", "method", "Return the HTTP method of the request."),
        ("Request", "body_json", "Parse the request body as JSON."),
        ("Request", "with_state", "Attach a named state value to the request."),
        ("Request", "state", "Retrieve a named state value from the request."),
        // Router
        ("Router", "new", "Create a new empty router."),
        ("Router", "routes", "Return the configured route table."),
        ("Router", "get", "Register a handler for GET requests at a path."),
        ("Router", "post", "Register a handler for POST requests at a path."),
        ("Router", "put", "Register a handler for PUT requests at a path."),
        ("Router", "delete", "Register a handler for DELETE requests at a path."),
        ("Router", "patch", "Register a handler for PATCH requests at a path."),
        ("Router", "options", "Register a handler for OPTIONS requests at a path."),
        ("Router", "head", "Register a handler for HEAD requests at a path."),
        ("Router", "any", "Register a handler for all HTTP methods at a path."),
        ("Router", "use", "Add middleware to the router."),
        ("Router", "group", "Group routes under a shared path prefix."),
        // Server
        ("Server", "listen!", "Start the HTTP server on the given port."),
    ]
}

// ---------------------------------------------------------------------------
// Signature building
// ---------------------------------------------------------------------------

/// Build the type signatures map. This mirrors builtin_type_signatures()
/// from the type checker, using Server prelude (most permissive).
fn build_type_signatures() -> HashMap<String, Type> {
    let mut sigs = HashMap::new();

    // Console
    sigs.insert("Console.println!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.print!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.error!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.read_line!".into(), Type::Function(vec![], Box::new(Type::String)));

    // Log
    sigs.insert("Log.info!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.warn!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.error!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.debug!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));

    // Time
    sigs.insert("Time.now!".into(), Type::Function(vec![], Box::new(Type::Int)));
    sigs.insert("Time.sleep!".into(), Type::Function(vec![Type::Int], Box::new(Type::Unit)));

    // Random
    sigs.insert("Random.int!".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Random.bool!".into(), Type::Function(vec![], Box::new(Type::Bool)));
    sigs.insert("Random.uuid!".into(), Type::Function(vec![], Box::new(Type::String)));

    // Env
    let option_string = Type::Enum(
        "Option".to_string(),
        vec![("Some".to_string(), vec![Type::String]), ("None".to_string(), vec![])],
    );
    sigs.insert("Env.get!".into(), Type::Function(vec![Type::String], Box::new(option_string)));
    sigs.insert("Env.set!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));

    // Fs
    sigs.insert("Fs.read!".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("Fs.write!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));
    sigs.insert("Fs.exists!".into(), Type::Function(vec![Type::String], Box::new(Type::Bool)));
    sigs.insert("Fs.read_file!".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("Fs.write_file!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));
    sigs.insert("Fs.list_dir!".into(), Type::Function(vec![Type::String], Box::new(Type::List(Box::new(Type::String)))));

    // Math
    sigs.insert("Math.abs".into(), Type::Function(vec![Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.min".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.max".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.clamp".into(), Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.pow".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));

    // String
    sigs.insert("String.length".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.trim".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.contains".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.starts_with".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.ends_with".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.to_upper".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.to_lower".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.split".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::List(Box::new(Type::String)))));
    sigs.insert("String.join".into(), Type::Function(vec![Type::List(Box::new(Type::String)), Type::String], Box::new(Type::String)));
    sigs.insert("String.slice".into(), Type::Function(vec![Type::String, Type::Int, Type::Int], Box::new(Type::String)));
    sigs.insert("String.chars".into(), Type::Function(vec![Type::String], Box::new(Type::List(Box::new(Type::String)))));
    sigs.insert("String.char_at".into(), Type::Function(vec![Type::String, Type::Int], Box::new(Type::String)));
    sigs.insert("String.index_of".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Int)));
    sigs.insert("String.to_int".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.from_char_code".into(), Type::Function(vec![Type::Int], Box::new(Type::String)));
    sigs.insert("String.char_code".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.replace".into(), Type::Function(vec![Type::String, Type::String, Type::String], Box::new(Type::String)));

    // Int
    sigs.insert("Int.to_string".into(), Type::Function(vec![Type::Int], Box::new(Type::String)));
    sigs.insert("Int.parse".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));

    // Json
    sigs.insert("Json.parse".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Json.to_string".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));
    sigs.insert("Json.to_string_pretty".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));

    // Option (non-generic fallbacks)
    sigs.insert("Option.is_some".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));
    sigs.insert("Option.is_none".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));

    // Result (non-generic fallbacks)
    sigs.insert("Result.is_ok".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));
    sigs.insert("Result.is_err".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));

    // List (non-generic)
    sigs.insert("List.length".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));
    sigs.insert("List.contains".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Bool)));

    // Map (non-generic)
    sigs.insert("Map.len".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));

    // Set (non-generic)
    sigs.insert("Set.len".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));

    // Http
    sigs.insert("Http.get!".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.post!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.put!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.delete!".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.request!".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));

    // Response
    sigs.insert("Response.ok".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.json".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Response.created".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.no_content".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("Response.bad_request".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.not_found".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.error".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.status".into(), Type::Function(vec![Type::Int, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.with_header".into(), Type::Function(vec![Type::Unknown, Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.with_headers".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Response.redirect".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.redirect_permanent".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));

    // Request
    sigs.insert("Request.header".into(), Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Request.method".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));
    sigs.insert("Request.body_json".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Request.with_state".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Request.state".into(), Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)));

    // Router
    sigs.insert("Router.new".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("Router.routes".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.get".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.post".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.put".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.delete".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.patch".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.options".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.head".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.any".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.use".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.group".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));

    // Server
    sigs.insert("Server.listen!".into(), Type::Function(vec![Type::Unknown, Type::Int], Box::new(Type::Unit)));

    sigs
}

// ---------------------------------------------------------------------------
// Signature formatting with generic type variables
// ---------------------------------------------------------------------------

/// Build a signature from a generic schema using readable type parameter names.
fn build_signature_from_schema(qualified_name: &str) -> Option<String> {
    let schemas = builtin_generic_schemas();
    let schema = schemas.get(qualified_name)?;

    let mut ctx = InferCtx::new();
    let fn_type = (schema.build)(&mut ctx);

    let var_names = extract_var_names(qualified_name, schema.type_params);

    let func_name = qualified_name
        .rsplit('.')
        .next()
        .unwrap_or(qualified_name);

    match &fn_type {
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| format_type_with_vars(p, &var_names))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = format_type_with_vars(ret, &var_names);
            Some(format!("fn {}({}) -> {}", func_name, params_str, ret_str))
        }
        _ => Some(format!("fn {}() -> {}", func_name, fn_type)),
    }
}

/// Pick readable type parameter names based on the function context.
fn extract_var_names(qualified_name: &str, type_params: u32) -> HashMap<u32, String> {
    let mut names = HashMap::new();

    let param_names: Vec<&str> = if qualified_name.starts_with("Map.") {
        vec!["K", "V", "T", "U"]
    } else if qualified_name.starts_with("Result.")
        || qualified_name.contains("Result")
    {
        match type_params {
            2 => vec!["T", "E"],
            3 => vec!["T", "U", "E"],
            _ => vec!["T", "E", "U", "V"],
        }
    } else {
        vec!["T", "U", "V", "W"]
    };

    for i in 0..type_params {
        let name = param_names.get(i as usize).unwrap_or(&"T");
        names.insert(i, name.to_string());
    }

    names
}

/// Format a type, replacing Var(id) with readable names.
fn format_type_with_vars(ty: &Type, var_names: &HashMap<u32, String>) -> String {
    match ty {
        Type::Var(id) => var_names
            .get(id)
            .cloned()
            .unwrap_or_else(|| format!("?{}", id)),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Unknown => "<unknown>".to_string(),
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| format_type_with_vars(p, var_names))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "({}) -> {}",
                params_str,
                format_type_with_vars(ret, var_names)
            )
        }
        Type::List(inner) => {
            format!("List<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Map(k, v) => format!(
            "Map<{}, {}>",
            format_type_with_vars(k, var_names),
            format_type_with_vars(v, var_names)
        ),
        Type::Set(inner) => {
            format!("Set<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Weak(inner) => {
            format!("Weak<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Scoped(inner) => {
            format!("Scoped<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Enum(name, variants) => match name.as_str() {
            "Option" => {
                if let Some((_, payload)) = variants.first()
                    && let Some(inner) = payload.first()
                {
                    return format!(
                        "Option<{}>",
                        format_type_with_vars(inner, var_names)
                    );
                }
                "Option".to_string()
            }
            "Result" => {
                let ok_t = variants.first().and_then(|(_, p)| p.first());
                let err_t = variants.get(1).and_then(|(_, p)| p.first());
                match (ok_t, err_t) {
                    (Some(ok), Some(err)) => format!(
                        "Result<{}, {}>",
                        format_type_with_vars(ok, var_names),
                        format_type_with_vars(err, var_names)
                    ),
                    _ => "Result".to_string(),
                }
            }
            _ => name.clone(),
        },
        Type::Tuple(elems) => {
            let elems_str = elems
                .iter()
                .map(|e| format_type_with_vars(e, var_names))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", elems_str)
        }
        _ => ty.to_string(),
    }
}

/// Build a human-readable signature string from a Type.
fn build_signature(func_name: &str, ty: &Type) -> String {
    match ty {
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn {}({}) -> {}", func_name, params_str, ret)
        }
        _ => format!("fn {}() -> {}", func_name, ty),
    }
}

// ---------------------------------------------------------------------------
// Core: collect all documentation
// ---------------------------------------------------------------------------

/// Collect documentation for all standard library modules and functions.
pub fn generate_docs() -> DocsOutput {
    let type_sigs = build_type_signatures();
    let functions = known_functions();

    let mut modules: BTreeMap<String, Vec<FunctionDoc>> = BTreeMap::new();

    for (module, func_name, desc) in &functions {
        let qualified = format!("{}.{}", module, func_name);
        let base_name = func_name.trim_end_matches('!');

        // Build signature: prefer generic schema, then type signature, then fallback
        let signature = build_signature_from_schema(&qualified)
            .or_else(|| {
                type_sigs
                    .get(&qualified)
                    .map(|ty| build_signature(base_name, ty))
            })
            .unwrap_or_else(|| format!("fn {}(...)", base_name));

        let effects = effects_for_function(&qualified);
        let prelude_level = earliest_prelude_for_module(module).to_string();

        modules
            .entry(module.to_string())
            .or_default()
            .push(FunctionDoc {
                name: func_name.to_string(),
                signature,
                effects,
                prelude_level,
                description: Some(desc.to_string()),
            });
    }

    let modules_vec: Vec<ModuleDoc> = modules
        .into_iter()
        .map(|(name, functions)| ModuleDoc { name, functions })
        .collect();

    DocsOutput {
        modules: modules_vec,
    }
}

// ---------------------------------------------------------------------------
// Markdown rendering
// ---------------------------------------------------------------------------

/// Render docs as human-readable markdown.
pub fn render_markdown(docs: &DocsOutput) -> String {
    let mut out = String::new();
    out.push_str("# Baseline Standard Library Reference\n\n");

    for module in &docs.modules {
        out.push_str(&format!("## {}\n\n", module.name));

        for func in &module.functions {
            out.push_str(&format!("### `{}.{}`\n\n", module.name, func.name));
            out.push_str(&format!("```\n{}\n```\n\n", func.signature));

            if !func.effects.is_empty() {
                out.push_str(&format!(
                    "**Effects:** {}\n\n",
                    func.effects.join(", ")
                ));
            }

            out.push_str(&format!(
                "**Prelude:** `@prelude({})`\n\n",
                func.prelude_level
            ));

            if let Some(desc) = &func.description {
                out.push_str(&format!("{}\n\n", desc));
            }

            out.push_str("---\n\n");
        }
    }

    out
}
