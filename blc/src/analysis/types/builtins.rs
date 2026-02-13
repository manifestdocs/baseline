use super::type_def::Type;
use crate::prelude::Prelude;
use std::collections::HashMap;

/// Build a map of "Module.method" -> Function type for all builtins and native
/// stdlib functions gated by the given prelude.
pub(super) fn builtin_type_signatures(prelude: &Prelude) -> HashMap<String, Type> {
    let mut sigs = HashMap::new();

    let builtin_modules = prelude.builtin_modules();
    let native_modules = prelude.native_modules();

    // -- Console builtins (effect: Console) --
    if builtin_modules.contains(&"Console") {
        sigs.insert(
            "Console.println!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Console.print!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Console.error!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Console.read_line!".into(),
            Type::Function(vec![], Box::new(Type::String)),
        );
    }

    // -- Log builtins (effect: Log) --
    if builtin_modules.contains(&"Log") {
        sigs.insert(
            "Log.info!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Log.warn!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Log.error!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Log.debug!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
    }

    // -- Time builtins (effect: Time) --
    if builtin_modules.contains(&"Time") {
        sigs.insert(
            "Time.now!".into(),
            Type::Function(vec![], Box::new(Type::Int)),
        );
        sigs.insert(
            "Time.sleep!".into(),
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
    }

    // -- Random builtins (effect: Random) --
    if builtin_modules.contains(&"Random") {
        sigs.insert(
            "Random.int!".into(),
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        sigs.insert(
            "Random.bool!".into(),
            Type::Function(vec![], Box::new(Type::Bool)),
        );
    }

    // -- Env builtins (effect: Env) --
    if builtin_modules.contains(&"Env") {
        sigs.insert(
            "Env.get!".into(),
            Type::Function(
                vec![Type::String],
                Box::new(Type::Enum(
                    "Option".to_string(),
                    vec![
                        ("Some".to_string(), vec![Type::String]),
                        ("None".to_string(), vec![]),
                    ],
                )),
            ),
        );
        sigs.insert(
            "Env.set!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
        );
    }

    // -- Fs builtins (effect: Fs) --
    if builtin_modules.contains(&"Fs") {
        sigs.insert(
            "Fs.read!".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Fs.write!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Fs.exists!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Fs.delete!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
    }

    // -- Fs native methods (VM-side) --
    if native_modules.contains(&"Fs") {
        sigs.insert(
            "Fs.read_file!".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Fs.write_file!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Fs.list_dir!".into(),
            Type::Function(
                vec![Type::String],
                Box::new(Type::List(Box::new(Type::String))),
            ),
        );
    }

    // -- Map native methods (generic — use Unknown for type params) --
    if native_modules.contains(&"Map") {
        sigs.insert(
            "Map.empty".into(),
            Type::Function(vec![], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Map.set".into(),
            Type::Function(
                vec![Type::Unknown, Type::Unknown, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Map.insert".into(),
            Type::Function(
                vec![Type::Unknown, Type::Unknown, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Map.get".into(),
            Type::Function(
                vec![Type::Unknown, Type::Unknown],
                Box::new(Type::Enum(
                    "Option".to_string(),
                    vec![
                        ("Some".to_string(), vec![Type::Unknown]),
                        ("None".to_string(), vec![]),
                    ],
                )),
            ),
        );
        sigs.insert(
            "Map.has".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Map.contains".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Map.remove".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Map.keys".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Map.values".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Map.size".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Int)),
        );
        sigs.insert(
            "Map.len".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Int)),
        );
        sigs.insert(
            "Map.from_list".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Set native methods (use Unknown for element type) --
    if native_modules.contains(&"Set") {
        sigs.insert(
            "Set.empty".into(),
            Type::Function(vec![], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Set.insert".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Set.remove".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Set.contains".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Set.union".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Set.intersection".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Set.len".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Int)),
        );
        sigs.insert(
            "Set.from_list".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Weak native methods (generic — use Unknown for type params) --
    if native_modules.contains(&"Weak") {
        sigs.insert(
            "Weak.downgrade".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Weak.upgrade".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Http natives (effect: Http) — returns Response records --
    if native_modules.contains(&"Http") {
        // Http.get!(url) -> Response, Http.post!(url, body) -> Response, etc.
        // Return type is Unknown (record type) since we don't have named record types yet.
        sigs.insert(
            "Http.get!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Http.post!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Http.put!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Http.delete!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        // Http.request!(request_record) -> Response
        sigs.insert(
            "Http.request!".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Math builtins (pure, no effect) --
    // Math functions accept both Int and Float at runtime; use Unknown args to avoid false positives.
    if builtin_modules.contains(&"Math") {
        sigs.insert(
            "Math.abs".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Math.min".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Math.max".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Math.clamp".into(),
            Type::Function(
                vec![Type::Unknown, Type::Unknown, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Math.pow".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Json native methods (pure) --
    if native_modules.contains(&"Json") {
        sigs.insert(
            "Json.parse".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Json.to_string".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::String)),
        );
        sigs.insert(
            "Json.to_string_pretty".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::String)),
        );
    }

    // -- String native methods --
    if native_modules.contains(&"String") {
        sigs.insert(
            "String.length".into(),
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        sigs.insert(
            "String.trim".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "String.contains".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "String.starts_with".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "String.to_upper".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "String.to_lower".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "String.split".into(),
            Type::Function(
                vec![Type::String, Type::String],
                Box::new(Type::List(Box::new(Type::String))),
            ),
        );
        sigs.insert(
            "String.join".into(),
            Type::Function(
                vec![Type::List(Box::new(Type::String)), Type::String],
                Box::new(Type::String),
            ),
        );
        sigs.insert(
            "String.slice".into(),
            Type::Function(
                vec![Type::String, Type::Int, Type::Int],
                Box::new(Type::String),
            ),
        );
        sigs.insert(
            "String.ends_with".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "String.chars".into(),
            Type::Function(
                vec![Type::String],
                Box::new(Type::List(Box::new(Type::String))),
            ),
        );
        sigs.insert(
            "String.char_at".into(),
            Type::Function(vec![Type::String, Type::Int], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "String.index_of".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "String.to_int".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "String.from_char_code".into(),
            Type::Function(vec![Type::Int], Box::new(Type::String)),
        );
        sigs.insert(
            "String.char_code".into(),
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        sigs.insert(
            "String.replace".into(),
            Type::Function(
                vec![Type::String, Type::String, Type::String],
                Box::new(Type::String),
            ),
        );
    }

    // -- Int native methods --
    if native_modules.contains(&"Int") {
        sigs.insert(
            "Int.to_string".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::String)),
        );
        sigs.insert(
            "Int.parse".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
    }

    // -- Option native methods (generic — use Unknown for type params) --
    if native_modules.contains(&"Option") {
        sigs.insert(
            "Option.unwrap".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Option.unwrap_or".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Option.is_some".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Option.is_none".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Bool)),
        );
    }

    // -- Result native methods (generic — use Unknown for type params) --
    if native_modules.contains(&"Result") {
        sigs.insert(
            "Result.unwrap".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Result.unwrap_or".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Result.is_ok".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Result.is_err".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Bool)),
        );
    }

    // -- List native methods (generic — use Unknown for element type) --
    if native_modules.contains(&"List") {
        sigs.insert(
            "List.length".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Int)),
        );
        sigs.insert(
            "List.head".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "List.tail".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "List.reverse".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "List.sort".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "List.concat".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Response builder (pure) --
    if native_modules.contains(&"Response") {
        sigs.insert(
            "Response.ok".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.json".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.created".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.no_content".into(),
            Type::Function(vec![], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.bad_request".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.not_found".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.error".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.status".into(),
            Type::Function(vec![Type::Int, Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.with_header".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::String],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Response.with_headers".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.redirect".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Response.redirect_permanent".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unknown)),
        );
    }

    // -- Router native methods (pure) --
    if native_modules.contains(&"Router") {
        sigs.insert(
            "Router.new".into(),
            Type::Function(vec![], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Router.get".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.post".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.put".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.delete".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.patch".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.options".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.head".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.any".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Router.routes".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Router.use".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Router.group".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
    }

    // -- Request helpers (pure) --
    if native_modules.contains(&"Request") {
        sigs.insert(
            "Request.header".into(),
            Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Request.method".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::String)),
        );
        sigs.insert(
            "Request.body_json".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Request.with_state".into(),
            Type::Function(
                vec![Type::Unknown, Type::String, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Request.state".into(),
            Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)),
        );
    }

    // -- Server builtins (effect: Http) --
    if builtin_modules.contains(&"Server") {
        sigs.insert(
            "Server.listen!".into(),
            Type::Function(vec![Type::Unknown, Type::Int], Box::new(Type::Unit)),
        );
    }

    sigs
}
