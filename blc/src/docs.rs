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

/// All known stdlib functions with their qualified names.
/// This is the single source of truth derived from:
/// - NativeRegistry::register_all() in vm/natives/mod.rs
/// - builtin_type_signatures() in analysis/types/builtins.rs
/// - builtin_generic_schemas() in analysis/infer.rs
///
/// Entries are (qualified_name, is_effectful).
/// Duplicate ! and non-! variants are collapsed to a single entry.
fn known_functions() -> Vec<(&'static str, &'static str)> {
    // (module, function_base_name)
    vec![
        // Console
        ("Console", "println!"),
        ("Console", "print!"),
        ("Console", "error!"),
        ("Console", "read_line!"),
        // Log
        ("Log", "info!"),
        ("Log", "warn!"),
        ("Log", "error!"),
        ("Log", "debug!"),
        // Math
        ("Math", "abs"),
        ("Math", "min"),
        ("Math", "max"),
        ("Math", "clamp"),
        ("Math", "pow"),
        // String
        ("String", "length"),
        ("String", "to_upper"),
        ("String", "to_lower"),
        ("String", "trim"),
        ("String", "contains"),
        ("String", "starts_with"),
        ("String", "ends_with"),
        ("String", "split"),
        ("String", "join"),
        ("String", "slice"),
        ("String", "chars"),
        ("String", "char_at"),
        ("String", "index_of"),
        ("String", "to_int"),
        ("String", "from_char_code"),
        ("String", "char_code"),
        ("String", "replace"),
        // List
        ("List", "length"),
        ("List", "head"),
        ("List", "tail"),
        ("List", "reverse"),
        ("List", "sort"),
        ("List", "concat"),
        ("List", "contains"),
        ("List", "get"),
        ("List", "map"),
        ("List", "filter"),
        ("List", "fold"),
        ("List", "find"),
        // Option
        ("Option", "unwrap"),
        ("Option", "unwrap_or"),
        ("Option", "is_some"),
        ("Option", "is_none"),
        ("Option", "map"),
        ("Option", "flat_map"),
        // Result
        ("Result", "unwrap"),
        ("Result", "unwrap_or"),
        ("Result", "is_ok"),
        ("Result", "is_err"),
        ("Result", "map"),
        ("Result", "and_then"),
        // Int
        ("Int", "to_string"),
        ("Int", "parse"),
        // Json
        ("Json", "parse"),
        ("Json", "to_string"),
        ("Json", "to_string_pretty"),
        // Map
        ("Map", "empty"),
        ("Map", "insert"),
        ("Map", "get"),
        ("Map", "remove"),
        ("Map", "contains"),
        ("Map", "keys"),
        ("Map", "values"),
        ("Map", "len"),
        ("Map", "from_list"),
        // Set
        ("Set", "empty"),
        ("Set", "insert"),
        ("Set", "remove"),
        ("Set", "contains"),
        ("Set", "union"),
        ("Set", "intersection"),
        ("Set", "len"),
        ("Set", "from_list"),
        // Weak
        ("Weak", "downgrade"),
        ("Weak", "upgrade"),
        // Time
        ("Time", "now!"),
        ("Time", "sleep!"),
        // Random
        ("Random", "int!"),
        ("Random", "bool!"),
        // Env
        ("Env", "get!"),
        ("Env", "set!"),
        // Fs
        ("Fs", "read!"),
        ("Fs", "write!"),
        ("Fs", "exists!"),
        ("Fs", "read_file!"),
        ("Fs", "write_file!"),
        ("Fs", "list_dir!"),
        ("Fs", "with_file!"),
        // Http
        ("Http", "get!"),
        ("Http", "post!"),
        ("Http", "put!"),
        ("Http", "delete!"),
        ("Http", "request!"),
        // Response
        ("Response", "ok"),
        ("Response", "json"),
        ("Response", "created"),
        ("Response", "no_content"),
        ("Response", "bad_request"),
        ("Response", "not_found"),
        ("Response", "error"),
        ("Response", "status"),
        ("Response", "with_header"),
        ("Response", "with_headers"),
        ("Response", "redirect"),
        ("Response", "redirect_permanent"),
        // Request
        ("Request", "header"),
        ("Request", "method"),
        ("Request", "body_json"),
        ("Request", "with_state"),
        ("Request", "state"),
        // Router
        ("Router", "new"),
        ("Router", "routes"),
        ("Router", "get"),
        ("Router", "post"),
        ("Router", "put"),
        ("Router", "delete"),
        ("Router", "patch"),
        ("Router", "options"),
        ("Router", "head"),
        ("Router", "any"),
        ("Router", "use"),
        ("Router", "group"),
        // Server
        ("Server", "listen!"),
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

    for (module, func_name) in &functions {
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
                description: None,
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
