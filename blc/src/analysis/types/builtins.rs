use super::type_def::Type;
use crate::prelude::Prelude;
use std::collections::HashMap;

/// Concrete record type for HTTP Request.
/// Shape: { method: String, url: String, headers: List<(String, String)>,
///          body: String, params: Record, query: Record, cookies: Record, ..r3 }
/// Open record (row variable) so handlers can declare a subset of fields.
fn request_type() -> Type {
    let mut fields = HashMap::new();
    fields.insert("method".to_string(), Type::String);
    fields.insert("url".to_string(), Type::String);
    fields.insert(
        "headers".to_string(),
        Type::List(Box::new(Type::Tuple(vec![Type::String, Type::String]))),
    );
    fields.insert("body".to_string(), Type::String);
    // params, query, cookies are open records (dynamic keys)
    fields.insert("params".to_string(), Type::Record(HashMap::new(), Some(0)));
    fields.insert("query".to_string(), Type::Record(HashMap::new(), Some(1)));
    fields.insert("cookies".to_string(), Type::Record(HashMap::new(), Some(2)));
    Type::Record(fields, Some(3))
}

/// Concrete record type for HTTP Response.
/// Shape: { status: Int, headers: List<(String, String)>, body: String }
fn response_type() -> Type {
    let mut fields = HashMap::new();
    fields.insert("status".to_string(), Type::Int);
    fields.insert(
        "headers".to_string(),
        Type::List(Box::new(Type::Tuple(vec![Type::String, Type::String]))),
    );
    fields.insert("body".to_string(), Type::String);
    Type::Record(fields, None)
}

/// Concrete record type for Router.
/// Shape: { routes: List<Unknown>, middleware: List<Unknown> }
fn router_type() -> Type {
    let mut fields = HashMap::new();
    fields.insert(
        "routes".to_string(),
        Type::List(Box::new(Type::Unknown)),
    );
    fields.insert(
        "middleware".to_string(),
        Type::List(Box::new(Type::Unknown)),
    );
    Type::Record(fields, None)
}

/// HttpError sum type — all standard HTTP error variants.
fn http_error_type() -> Type {
    Type::Enum(
        "HttpError".to_string(),
        vec![
            ("BadRequest".to_string(), vec![Type::String]),
            ("Unauthorized".to_string(), vec![Type::String]),
            ("Forbidden".to_string(), vec![Type::String]),
            ("NotFound".to_string(), vec![Type::String]),
            ("MethodNotAllowed".to_string(), vec![Type::String]),
            ("Conflict".to_string(), vec![Type::String]),
            ("Unprocessable".to_string(), vec![Type::String]),
            ("TooManyRequests".to_string(), vec![Type::String]),
            ("Internal".to_string(), vec![Type::String]),
            ("BadGateway".to_string(), vec![Type::String]),
            ("ServiceUnavailable".to_string(), vec![Type::String]),
            ("GatewayTimeout".to_string(), vec![Type::String]),
        ],
    )
}

/// Handler type: (Request) -> {Http} Result<Response, HttpError>
fn handler_type() -> Type {
    Type::Function(
        vec![request_type()],
        Box::new(Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![response_type()]),
                ("Err".to_string(), vec![http_error_type()]),
            ],
        )),
    )
}

/// Middleware type: (Request, (Request) -> Result<Response, HttpError>) -> Result<Response, HttpError>
fn middleware_type() -> Type {
    let next_fn = Type::Function(
        vec![request_type()],
        Box::new(Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![response_type()]),
                ("Err".to_string(), vec![http_error_type()]),
            ],
        )),
    );
    Type::Function(
        vec![request_type(), next_fn],
        Box::new(Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![response_type()]),
                ("Err".to_string(), vec![http_error_type()]),
            ],
        )),
    )
}

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
    // Accepts 1 arg (message) or 2 args (message + structured fields record).
    // The type checker allows optional extra args for these via variadic builtin handling.
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
        sigs.insert(
            "Random.uuid!".into(),
            Type::Function(vec![], Box::new(Type::String)),
        );
        sigs.insert(
            "Random.bytes!".into(),
            Type::Function(vec![Type::Int], Box::new(Type::String)),
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
        sigs.insert(
            "Env.args!".into(),
            Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
        );
    }

    // -- Crypto native methods (pure) --
    if native_modules.contains(&"Crypto") {
        sigs.insert(
            "Crypto.sha256".into(),
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Crypto.hmac_sha256".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Crypto.constant_time_eq".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
    }

    // -- DateTime native methods (effectful: now!, pure: parse, to_string, add, diff) --
    if native_modules.contains(&"DateTime") {
        sigs.insert(
            "DateTime.now!".into(),
            Type::Function(vec![], Box::new(Type::Int)),
        );
        sigs.insert(
            "DateTime.parse".into(),
            Type::Function(
                vec![Type::String],
                Box::new(Type::Enum(
                    "Result".to_string(),
                    vec![
                        ("Ok".to_string(), vec![Type::Int]),
                        ("Err".to_string(), vec![Type::String]),
                    ],
                )),
            ),
        );
        sigs.insert(
            "DateTime.to_string".into(),
            Type::Function(vec![Type::Int], Box::new(Type::String)),
        );
        sigs.insert(
            "DateTime.add".into(),
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        sigs.insert(
            "DateTime.diff".into(),
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
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
        let resp = response_type();
        sigs.insert(
            "Http.get!".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Http.post!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Http.put!".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Http.delete!".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Http.request!".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp)),
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
        sigs.insert(
            "Json.to_camel_case".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Json.to_snake_case".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
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
        sigs.insert(
            "String.matches".into(),
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "String.find_matches".into(),
            Type::Function(
                vec![Type::String, Type::String],
                Box::new(Type::List(Box::new(Type::String))),
            ),
        );
        sigs.insert(
            "String.replace_regex".into(),
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
        sigs.insert(
            "Result.map_err".into(),
            Type::Function(
                vec![Type::Unknown, Type::Unknown],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Result.context".into(),
            Type::Function(
                vec![Type::Unknown, Type::String],
                Box::new(Type::Unknown),
            ),
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
            Type::Function(
                vec![Type::Unknown],
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

    // -- HttpError constructors (pure) — return HttpError enum --
    if native_modules.contains(&"HttpError") {
        let he = http_error_type();
        for method in &[
            "HttpError.bad_request",
            "HttpError.unauthorized",
            "HttpError.forbidden",
            "HttpError.not_found",
            "HttpError.method_not_allowed",
            "HttpError.conflict",
            "HttpError.unprocessable",
            "HttpError.too_many_requests",
            "HttpError.internal",
            "HttpError.bad_gateway",
            "HttpError.service_unavailable",
            "HttpError.gateway_timeout",
        ] {
            sigs.insert(
                (*method).into(),
                Type::Function(vec![Type::String], Box::new(he.clone())),
            );
        }
    }

    // -- Response builder (pure) — returns typed Response records --
    if native_modules.contains(&"Response") {
        let resp = response_type();
        let headers_list = Type::List(Box::new(Type::Tuple(vec![Type::String, Type::String])));
        sigs.insert(
            "Response.ok".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.json".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.created".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.no_content".into(),
            Type::Function(vec![], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.bad_request".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.not_found".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.error".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.status".into(),
            Type::Function(vec![Type::Int, Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.with_header".into(),
            Type::Function(
                vec![resp.clone(), Type::String, Type::String],
                Box::new(resp.clone()),
            ),
        );
        sigs.insert(
            "Response.with_headers".into(),
            Type::Function(vec![resp.clone(), headers_list], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.unauthorized".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.forbidden".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.conflict".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.unprocessable".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.too_many_requests".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.method_not_allowed".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.bad_gateway".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.service_unavailable".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.gateway_timeout".into(),
            Type::Function(vec![Type::Unknown], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.redirect".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.redirect_permanent".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.redirect_temporary".into(),
            Type::Function(vec![Type::String], Box::new(resp.clone())),
        );
        sigs.insert(
            "Response.set_cookie".into(),
            Type::Function(
                vec![resp.clone(), Type::String, Type::String],
                Box::new(resp),
            ),
        );
    }

    // -- Request.decode (type-driven validation) --
    if native_modules.contains(&"Request") {
        let req = request_type();
        let resp = response_type();
        let result_unknown_resp = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::Unknown]),
                ("Err".to_string(), vec![resp]),
            ],
        );
        sigs.insert(
            "Request.decode".into(),
            Type::Function(
                vec![req, Type::String],
                Box::new(result_unknown_resp),
            ),
        );
    }

    // -- Middleware helpers (pure) --
    if native_modules.contains(&"Middleware") {
        let he = http_error_type();
        let req = request_type();
        let result_string_he = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::String]),
                ("Err".to_string(), vec![he.clone()]),
            ],
        );
        let result_tuple_he = Type::Enum(
            "Result".to_string(),
            vec![
                (
                    "Ok".to_string(),
                    vec![Type::Tuple(vec![Type::String, Type::String])],
                ),
                ("Err".to_string(), vec![he]),
            ],
        );
        sigs.insert(
            "Middleware.extract_bearer".into(),
            Type::Function(vec![req.clone()], Box::new(result_string_he)),
        );
        sigs.insert(
            "Middleware.extract_basic".into(),
            Type::Function(vec![req], Box::new(result_tuple_he)),
        );
        // Config builders return open records (Unknown for flexibility)
        sigs.insert(
            "Middleware.cors_config".into(),
            Type::Function(
                vec![Type::List(Box::new(Type::String))],
                Box::new(Type::Unknown),
            ),
        );
        sigs.insert(
            "Middleware.rate_limit_config".into(),
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Unknown)),
        );
    }

    // -- Router native methods (pure) — typed Router record --
    if native_modules.contains(&"Router") {
        let rtr = router_type();
        let handler = handler_type();
        let mw = middleware_type();
        sigs.insert(
            "Router.new".into(),
            Type::Function(vec![], Box::new(rtr.clone())),
        );
        // All route methods: (Router, String, Handler) -> Router
        for method_name in &[
            "Router.get", "Router.post", "Router.put", "Router.delete",
            "Router.patch", "Router.options", "Router.head", "Router.any",
        ] {
            sigs.insert(
                (*method_name).into(),
                Type::Function(
                    vec![rtr.clone(), Type::String, handler.clone()],
                    Box::new(rtr.clone()),
                ),
            );
        }
        sigs.insert(
            "Router.routes".into(),
            Type::Function(
                vec![rtr.clone()],
                Box::new(Type::List(Box::new(Type::Unknown))),
            ),
        );
        sigs.insert(
            "Router.use".into(),
            Type::Function(vec![rtr.clone(), mw], Box::new(rtr.clone())),
        );
        sigs.insert(
            "Router.group".into(),
            Type::Function(
                vec![rtr.clone(), Type::String, rtr.clone()],
                Box::new(rtr.clone()),
            ),
        );
        // Router.resources(router, path, { index, show, create, update, destroy }) -> Router
        sigs.insert(
            "Router.resources".into(),
            Type::Function(
                vec![rtr.clone(), Type::String, Type::Unknown],
                Box::new(rtr.clone()),
            ),
        );
        sigs.insert(
            "Router.state".into(),
            Type::Function(
                vec![rtr.clone(), Type::String, Type::Unknown],
                Box::new(rtr.clone()),
            ),
        );
        sigs.insert(
            "Router.docs_json".into(),
            Type::Function(vec![rtr.clone()], Box::new(Type::String)),
        );
        sigs.insert(
            "Router.ws".into(),
            Type::Function(
                vec![rtr.clone(), Type::String, handler.clone()],
                Box::new(rtr),
            ),
        );
    }

    // -- Ws (WebSocket) natives (effect: Ws) --
    if native_modules.contains(&"Ws") {
        let result_string_string = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::String]),
                ("Err".to_string(), vec![Type::String]),
            ],
        );
        sigs.insert(
            "Ws.send!".into(),
            Type::Function(vec![Type::String], Box::new(Type::Unit)),
        );
        sigs.insert(
            "Ws.receive!".into(),
            Type::Function(vec![], Box::new(result_string_string)),
        );
        sigs.insert(
            "Ws.close!".into(),
            Type::Function(vec![], Box::new(Type::Unit)),
        );
    }

    // -- Request helpers (pure) — typed Request record --
    if native_modules.contains(&"Request") {
        let req = request_type();
        let option_str = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![Type::String]),
                ("None".to_string(), vec![]),
            ],
        );
        sigs.insert(
            "Request.header".into(),
            Type::Function(vec![req.clone(), Type::String], Box::new(option_str.clone())),
        );
        sigs.insert(
            "Request.method".into(),
            Type::Function(vec![req.clone()], Box::new(Type::String)),
        );
        sigs.insert(
            "Request.body_json".into(),
            Type::Function(
                vec![req.clone()],
                Box::new(Type::Enum(
                    "Result".to_string(),
                    vec![
                        ("Ok".to_string(), vec![Type::Unknown]),
                        ("Err".to_string(), vec![http_error_type()]),
                    ],
                )),
            ),
        );
        let result_string_string = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::String]),
                ("Err".to_string(), vec![Type::String]),
            ],
        );
        let result_int_string = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::Int]),
                ("Err".to_string(), vec![Type::String]),
            ],
        );
        sigs.insert(
            "Request.param".into(),
            Type::Function(
                vec![req.clone(), Type::String],
                Box::new(result_string_string.clone()),
            ),
        );
        sigs.insert(
            "Request.param_int".into(),
            Type::Function(
                vec![req.clone(), Type::String],
                Box::new(result_int_string.clone()),
            ),
        );
        sigs.insert(
            "Request.query".into(),
            Type::Function(
                vec![req.clone(), Type::String],
                Box::new(option_str.clone()),
            ),
        );
        sigs.insert(
            "Request.query_int".into(),
            Type::Function(
                vec![req.clone(), Type::String],
                Box::new(result_int_string),
            ),
        );
    }

    // -- Async builtins (effect: Async) --
    // scope! is a standalone effectful call (like println!) that takes a closure.
    if builtin_modules.contains(&"Async") {
        sigs.insert(
            "scope!".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Scope native methods (effect: Async) --
    if native_modules.contains(&"Scope") {
        sigs.insert(
            "Scope.spawn!".into(),
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
        );
    }

    // -- Cell native methods (effect: Async) --
    if native_modules.contains(&"Cell") {
        sigs.insert(
            "Cell.await!".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)),
        );
        sigs.insert(
            "Cell.cancel!".into(),
            Type::Function(vec![Type::Unknown], Box::new(Type::Unit)),
        );
    }

    // -- Server builtins (effect: Http) --
    if builtin_modules.contains(&"Server") {
        sigs.insert(
            "Server.listen!".into(),
            Type::Function(vec![router_type(), Type::Int], Box::new(Type::Unit)),
        );
    }

    // -- Database builtins (effect: Db) --
    if builtin_modules.contains(&"Sqlite") {
        // Connect/execute/query type shapes shared across backends
        let connect_ty = Type::Function(vec![Type::String], Box::new(Type::Unit));
        let execute_ty = Type::Function(
            vec![Type::String, Type::List(Box::new(Type::String))],
            Box::new(Type::Int),
        );
        let query_ty = Type::Function(
            vec![Type::String, Type::List(Box::new(Type::String))],
            Box::new(Type::List(Box::new(Type::Row))),
        );
        let query_one_ty = Type::Function(
            vec![Type::String, Type::List(Box::new(Type::String))],
            Box::new(Type::Enum(
                "Option".to_string(),
                vec![
                    ("Some".to_string(), vec![Type::Row]),
                    ("None".to_string(), vec![]),
                ],
            )),
        );

        // Sqlite.*
        sigs.insert("Sqlite.connect!".into(), connect_ty.clone());
        sigs.insert("Sqlite.execute!".into(), execute_ty.clone());
        sigs.insert("Sqlite.query!".into(), query_ty.clone());
        sigs.insert("Sqlite.query_one!".into(), query_one_ty.clone());

        // Postgres.*
        sigs.insert("Postgres.connect!".into(), connect_ty.clone());
        sigs.insert("Postgres.execute!".into(), execute_ty.clone());
        sigs.insert("Postgres.query!".into(), query_ty.clone());
        sigs.insert("Postgres.query_one!".into(), query_one_ty.clone());

        // Mysql.*
        sigs.insert("Mysql.connect!".into(), connect_ty);
        sigs.insert("Mysql.execute!".into(), execute_ty);
        sigs.insert("Mysql.query!".into(), query_ty);
        sigs.insert("Mysql.query_one!".into(), query_one_ty);

        // Sql.migrate!(dir: String) -> Result<Int, String>
        sigs.insert(
            "Sql.migrate!".into(),
            Type::Function(
                vec![Type::String],
                Box::new(Type::Enum(
                    "Result".to_string(),
                    vec![
                        ("Ok".to_string(), vec![Type::Int]),
                        ("Err".to_string(), vec![Type::String]),
                    ],
                )),
            ),
        );
    }

    // -- Row accessors (typed column extraction) --
    if native_modules.contains(&"Row") {
        let option_string = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![Type::String]),
                ("None".to_string(), vec![]),
            ],
        );
        let option_int = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![Type::Int]),
                ("None".to_string(), vec![]),
            ],
        );
        sigs.insert(
            "Row.string".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Row.int".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(Type::Int)),
        );
        sigs.insert(
            "Row.float".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(Type::Float)),
        );
        sigs.insert(
            "Row.bool".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Row.optional_string".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(option_string)),
        );
        sigs.insert(
            "Row.optional_int".into(),
            Type::Function(vec![Type::Row, Type::String], Box::new(option_int)),
        );
    }

    // -- Db helpers (pure row-parsing functions) --
    if native_modules.contains(&"Db") {
        let row_ty = Type::Map(Box::new(Type::String), Box::new(Type::String));
        let rows_ty = Type::List(Box::new(row_ty.clone()));
        let option_string = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![Type::String]),
                ("None".to_string(), vec![]),
            ],
        );
        let option_row = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![row_ty.clone()]),
                ("None".to_string(), vec![]),
            ],
        );

        sigs.insert(
            "Db.require".into(),
            Type::Function(vec![row_ty.clone(), Type::String], Box::new(Type::String)),
        );
        sigs.insert(
            "Db.optional".into(),
            Type::Function(vec![row_ty.clone(), Type::String], Box::new(option_string)),
        );
        sigs.insert(
            "Db.int_field".into(),
            Type::Function(vec![row_ty.clone(), Type::String], Box::new(Type::Int)),
        );
        sigs.insert(
            "Db.bool_field".into(),
            Type::Function(vec![row_ty, Type::String], Box::new(Type::Bool)),
        );
        sigs.insert(
            "Db.first_row".into(),
            Type::Function(vec![rows_ty.clone()], Box::new(option_row)),
        );
        sigs.insert(
            "Db.has_rows".into(),
            Type::Function(vec![rows_ty], Box::new(Type::Bool)),
        );
    }

    sigs
}
