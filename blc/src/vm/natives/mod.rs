mod console;
mod crypto;
mod datetime;
mod db;
pub(crate) mod db_backend;
pub(crate) mod db_helpers;
mod db_migrate;
#[cfg(feature = "mysql")]
mod db_mysql;
#[cfg(feature = "postgres")]
mod db_postgres;
mod env;
pub use env::set_program_args;
mod fs;
pub(crate) mod fs_sandbox;
pub(crate) mod http_error;
pub use fs_sandbox::set_fs_sandbox;
mod int_conv;
pub(crate) mod json;
mod jwt;
mod list;
mod log;
mod map;
mod math;
mod middleware;
mod multipart;
mod option;
mod random;
mod regex;
mod request;
mod response;
mod result;
mod router;
pub mod schema;
mod session;
mod set;
mod string;
mod time;
mod validate;
mod weak;
pub(crate) mod ws;

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

/// An owning native function: takes owned Vec<NValue> args for CoW optimizations.
type OwningFn = fn(Vec<NValue>) -> Result<NValue, NativeError>;

/// A native function entry.
struct NativeEntry {
    pub name: &'static str,
    pub func: SimpleFn,
    /// Optional owning variant for CoW dispatch.
    pub owning_func: Option<OwningFn>,
    /// Expected argument count (None = variadic/unchecked).
    pub arity: Option<u8>,
    /// Pre-computed dispatch flags.
    pub flags: u8,
}

/// Dispatch flag constants — pre-computed at registration time.
const FLAG_HOF: u8 = 0x01;
const FLAG_ASYNC: u8 = 0x02;
const FLAG_SERVER_LISTEN: u8 = 0x04;
/// Flag indicating the native has an owning variant for clone-on-write dispatch.
const FLAG_OWNING: u8 = 0x08;

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
        let entry = unsafe { self.entries.get_unchecked(id as usize) };
        if let Some(arity) = entry.arity
            && args.len() != arity as usize
        {
            return Err(NativeError(format!(
                "{}: expected {} argument(s), got {}",
                entry.name,
                arity,
                args.len()
            )));
        }
        (entry.func)(args)
    }

    /// Check if a function is a HOF — O(1) flag lookup.
    #[inline(always)]
    pub fn is_hof(&self, id: u16) -> bool {
        unsafe { self.entries.get_unchecked(id as usize).flags & FLAG_HOF != 0 }
    }

    /// Check if a function requires VM re-entrancy — O(1) flag lookup.
    #[inline(always)]
    pub fn is_server_listen(&self, id: u16) -> bool {
        unsafe { self.entries.get_unchecked(id as usize).flags & FLAG_SERVER_LISTEN != 0 }
    }

    /// Check if a function is an async fiber primitive — O(1) flag lookup.
    #[inline(always)]
    pub fn is_async(&self, id: u16) -> bool {
        unsafe { self.entries.get_unchecked(id as usize).flags & FLAG_ASYNC != 0 }
    }

    /// Check if a function has an owning (CoW) variant — O(1) flag lookup.
    #[inline(always)]
    pub fn is_owning(&self, id: u16) -> bool {
        unsafe { self.entries.get_unchecked(id as usize).flags & FLAG_OWNING != 0 }
    }

    /// Call the owning variant of a native function by ID.
    /// The caller must drain the args off the stack into a Vec before calling.
    pub fn call_owning(&self, id: u16, args: Vec<NValue>) -> Result<NValue, NativeError> {
        let entry = unsafe { self.entries.get_unchecked(id as usize) };
        if let Some(arity) = entry.arity
            && args.len() != arity as usize
        {
            return Err(NativeError(format!(
                "{}: expected {} argument(s), got {}",
                entry.name,
                arity,
                args.len()
            )));
        }
        (entry.owning_func.unwrap())(args)
    }

    /// Get the function name by ID (for error messages).
    #[inline(always)]
    pub fn name(&self, id: u16) -> &str {
        unsafe { self.entries.get_unchecked(id as usize).name }
    }

    fn register(&mut self, name: &'static str, func: SimpleFn) {
        let flags = Self::compute_flags(name);
        let idx = self.entries.len() as u16;
        self.entries.push(NativeEntry {
            name,
            func,
            owning_func: None,
            arity: None,
            flags,
        });
        self.name_index.insert(name, idx);
    }

    #[allow(dead_code)]
    fn register_with_arity(&mut self, name: &'static str, func: SimpleFn, arity: u8) {
        let flags = Self::compute_flags(name);
        let idx = self.entries.len() as u16;
        self.entries.push(NativeEntry {
            name,
            func,
            owning_func: None,
            arity: Some(arity),
            flags,
        });
        self.name_index.insert(name, idx);
    }

    /// Register a native with both borrowed and owning (CoW) variants.
    fn register_owning(
        &mut self,
        name: &'static str,
        func: SimpleFn,
        owning: OwningFn,
    ) {
        let mut flags = Self::compute_flags(name);
        flags |= FLAG_OWNING;
        let idx = self.entries.len() as u16;
        self.entries.push(NativeEntry {
            name,
            func,
            owning_func: Some(owning),
            arity: None,
            flags,
        });
        self.name_index.insert(name, idx);
    }

    /// Compute dispatch flags from function name at registration time.
    fn compute_flags(name: &str) -> u8 {
        let mut flags = 0u8;
        if matches!(
            name,
            "List.map"
                | "List.filter"
                | "List.fold"
                | "List.find"
                | "Option.map"
                | "Option.flat_map"
                | "Result.map"
                | "Result.map_err"
                | "Result.and_then"
                | "Fs.with_file!"
                | "Fs.with_file"
                | "Sqlite.query_map!"
                | "Sqlite.query_map"
                | "Postgres.query_map!"
                | "Postgres.query_map"
                | "Mysql.query_map!"
                | "Mysql.query_map"
                | "Sqlite.query_as!"
                | "Sqlite.query_as"
                | "Postgres.query_as!"
                | "Postgres.query_as"
                | "Mysql.query_as!"
                | "Mysql.query_as"
                | "Sqlite.query_one_as!"
                | "Sqlite.query_one_as"
                | "Postgres.query_one_as!"
                | "Postgres.query_one_as"
                | "Mysql.query_one_as!"
                | "Mysql.query_one_as"
        ) {
            flags |= FLAG_HOF;
        }
        if matches!(
            name,
            "scope!"
                | "scope"
                | "Scope.spawn!"
                | "Scope.spawn"
                | "Cell.await!"
                | "Cell.await"
                | "Cell.cancel!"
                | "Cell.cancel"
                | "Async.parallel!"
                | "Async.parallel"
                | "Async.race!"
                | "Async.race"
                | "Async.scatter_gather!"
                | "Async.scatter_gather"
                | "Async.delay!"
                | "Async.delay"
                | "Async.interval!"
                | "Async.interval"
                | "Async.timeout!"
                | "Async.timeout"
                | "Channel.bounded"
                | "Channel.send!"
                | "Channel.send"
                | "Channel.recv!"
                | "Channel.recv"
                | "Channel.close!"
                | "Channel.close"
        ) {
            flags |= FLAG_ASYNC;
        }
        if matches!(
            name,
            "Server.listen!" | "Server.listen" | "Server.listen_async!" | "Server.listen_async"
        ) {
            flags |= FLAG_SERVER_LISTEN;
        }
        flags
    }

    fn register_all(&mut self) {
        use console::*;
        use env::*;
        use fs::*;
        use int_conv::*;
        use json::*;
        use list::*;
        use log::*;
        use map::*;
        use math::*;
        use option::*;
        use random::*;
        use request::*;
        use response::*;
        use result::*;
        use router::*;
        use set::*;
        use string::*;
        use time::*;

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
        self.register("String.ends_with", native_string_ends_with);
        self.register("String.chars", native_string_chars);
        self.register("String.char_at", native_string_char_at);
        self.register("String.index_of", native_string_index_of);
        self.register("String.to_int", native_string_to_int);
        self.register("String.from_char_code", native_string_from_char_code);
        self.register("String.char_code", native_string_char_code);

        // -- String (additional) --
        self.register("String.replace", native_string_replace);

        // -- String (regex) --
        self.register("String.matches", regex::native_string_matches);
        self.register("String.find_matches", regex::native_string_find_matches);
        self.register("String.replace_regex", regex::native_string_replace_regex);

        // -- List (non-HOF) --
        self.register("List.length", native_list_length);
        self.register("List.head", native_list_head);
        self.register("List.tail", native_list_tail);
        self.register_owning("List.reverse", native_list_reverse, native_list_reverse_owning);
        self.register_owning("List.sort", native_list_sort, native_list_sort_owning);
        self.register_owning("List.concat", native_list_concat, native_list_concat_owning);
        self.register("List.contains", native_list_contains);
        self.register("List.get", native_list_get);

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
        self.register("Option.ok_or", native_option_ok_or);
        self.register("Option.map", native_hof_placeholder);
        self.register("Option.flat_map", native_hof_placeholder);

        // -- Result --
        self.register("Result.unwrap", native_result_unwrap);
        self.register("Result.unwrap_or", native_result_unwrap_or);
        self.register("Result.is_ok", native_result_is_ok);
        self.register("Result.is_err", native_result_is_err);
        self.register("Result.map", native_hof_placeholder);
        self.register("Result.map_err", native_hof_placeholder);
        self.register("Result.and_then", native_hof_placeholder);
        self.register("Result.ensure", native_result_ensure);
        self.register("Result.context", native_result_context);

        // -- Time --
        self.register("Time.now!", native_time_now);
        self.register("Time.now", native_time_now);
        self.register("Time.sleep!", native_time_sleep);
        self.register("Time.sleep", native_time_sleep);

        // -- DateTime --
        self.register("DateTime.now!", datetime::native_datetime_now);
        self.register("DateTime.now", datetime::native_datetime_now);
        self.register("DateTime.parse", datetime::native_datetime_parse);
        self.register("DateTime.to_string", datetime::native_datetime_to_string);
        self.register("DateTime.add", datetime::native_datetime_add);
        self.register("DateTime.diff", datetime::native_datetime_diff);

        // -- Random --
        self.register("Random.int!", native_random_int);
        self.register("Random.int", native_random_int);
        self.register("Random.bool!", native_random_bool);
        self.register("Random.bool", native_random_bool);
        self.register("Random.uuid!", native_random_uuid);
        self.register("Random.uuid", native_random_uuid);

        // -- Crypto --
        self.register("Crypto.sha256", crypto::native_crypto_sha256);
        self.register("Crypto.hmac_sha256", crypto::native_crypto_hmac_sha256);
        self.register(
            "Crypto.constant_time_eq",
            crypto::native_crypto_constant_time_eq,
        );

        // -- Random (additional) --
        self.register("Random.bytes!", crypto::native_random_bytes);
        self.register("Random.bytes", crypto::native_random_bytes);

        // -- Env --
        self.register("Env.get!", native_env_get);
        self.register("Env.get", native_env_get);
        self.register("Env.set!", native_env_set);
        self.register("Env.set", native_env_set);
        self.register("Env.args!", native_env_args);
        self.register("Env.args", native_env_args);

        // -- Console (read) --
        self.register("Console.read_line!", native_console_read_line);
        self.register("Console.read_line", native_console_read_line);

        // -- Int/String conversion --
        self.register("Int.to_string", native_int_to_string);
        self.register("Int.parse", native_int_parse);

        // -- Json --
        self.register("Json.parse", native_json_parse);
        self.register("Json.to_string", native_json_to_string);
        self.register("Json.to_string_pretty", native_json_to_string_pretty);
        self.register("Json.to_camel_case", native_json_to_camel_case);
        self.register("Json.to_snake_case", native_json_to_snake_case);

        // -- Jwt --
        self.register("Jwt.sign", jwt::native_jwt_sign);
        self.register("Jwt.sign_with", jwt::native_jwt_sign_with);
        self.register("Jwt.verify", jwt::native_jwt_verify);
        self.register("Jwt.decode", jwt::native_jwt_decode);

        // -- Session --
        self.register("Session.create!", session::native_session_create);
        self.register("Session.create", session::native_session_create);
        self.register("Session.get!", session::native_session_get);
        self.register("Session.get", session::native_session_get);
        self.register("Session.delete!", session::native_session_delete);
        self.register("Session.delete", session::native_session_delete);
        self.register("Session.set!", session::native_session_set);
        self.register("Session.set", session::native_session_set);

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
        self.register("Response.unauthorized", native_response_unauthorized);
        self.register("Response.forbidden", native_response_forbidden);
        self.register("Response.conflict", native_response_conflict);
        self.register("Response.unprocessable", native_response_unprocessable);
        self.register(
            "Response.too_many_requests",
            native_response_too_many_requests,
        );
        self.register(
            "Response.method_not_allowed",
            native_response_method_not_allowed,
        );
        self.register("Response.bad_gateway", native_response_bad_gateway);
        self.register(
            "Response.service_unavailable",
            native_response_service_unavailable,
        );
        self.register("Response.gateway_timeout", native_response_gateway_timeout);
        self.register("Response.redirect", native_response_redirect);
        self.register(
            "Response.redirect_permanent",
            native_response_redirect_permanent,
        );
        self.register(
            "Response.redirect_temporary",
            native_response_redirect_temporary,
        );
        self.register("Response.set_cookie", native_response_set_cookie);

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
        self.register("Router.resources", native_router_resources);
        self.register("Router.state", native_router_state);
        self.register("Router.docs_json", native_router_docs_json);
        self.register("Router.ws", native_router_ws);

        // -- Ws (WebSocket) --
        self.register("Ws.send!", ws::native_ws_send);
        self.register("Ws.send", ws::native_ws_send);
        self.register("Ws.receive!", ws::native_ws_receive);
        self.register("Ws.receive", ws::native_ws_receive);
        self.register("Ws.close!", ws::native_ws_close);
        self.register("Ws.close", ws::native_ws_close);

        // -- HttpError --
        self.register(
            "HttpError.bad_request",
            http_error::native_http_error_bad_request,
        );
        self.register(
            "HttpError.not_found",
            http_error::native_http_error_not_found,
        );
        self.register(
            "HttpError.unauthorized",
            http_error::native_http_error_unauthorized,
        );
        self.register(
            "HttpError.forbidden",
            http_error::native_http_error_forbidden,
        );
        self.register("HttpError.conflict", http_error::native_http_error_conflict);
        self.register(
            "HttpError.unprocessable",
            http_error::native_http_error_unprocessable,
        );
        self.register("HttpError.internal", http_error::native_http_error_internal);
        self.register(
            "HttpError.method_not_allowed",
            http_error::native_http_error_method_not_allowed,
        );
        self.register(
            "HttpError.too_many_requests",
            http_error::native_http_error_too_many_requests,
        );
        self.register(
            "HttpError.bad_gateway",
            http_error::native_http_error_bad_gateway,
        );
        self.register(
            "HttpError.service_unavailable",
            http_error::native_http_error_service_unavailable,
        );
        self.register(
            "HttpError.gateway_timeout",
            http_error::native_http_error_gateway_timeout,
        );
        self.register(
            "HttpError.with_context",
            http_error::native_http_error_with_context,
        );
        self.register(
            "HttpError.with_code",
            http_error::native_http_error_with_code,
        );
        self.register(
            "HttpError.to_response",
            http_error::native_http_error_to_response,
        );

        // -- Request --
        self.register("Request.header", native_request_header);
        self.register("Request.method", native_request_method);
        self.register("Request.body_json", native_request_body_json);
        self.register("Request.param", native_request_param);
        self.register("Request.param_int", native_request_param_int);
        self.register("Request.query", native_request_query);
        self.register("Request.query_int", native_request_query_int);
        self.register("Request.decode", schema::native_request_decode);
        self.register("Request.multipart", multipart::native_request_multipart);

        // -- Validate --
        self.register("Validate.required", validate::native_validate_required);
        self.register("Validate.string", validate::native_validate_string);
        self.register("Validate.int", validate::native_validate_int);
        self.register("Validate.boolean", validate::native_validate_boolean);
        self.register("Validate.min_length", validate::native_validate_min_length);
        self.register("Validate.max_length", validate::native_validate_max_length);
        self.register("Validate.min", validate::native_validate_min);
        self.register("Validate.max", validate::native_validate_max);
        self.register("Validate.one_of", validate::native_validate_one_of);

        // -- Middleware --
        self.register(
            "Middleware.extract_bearer",
            middleware::native_middleware_extract_bearer,
        );
        self.register(
            "Middleware.extract_basic",
            middleware::native_middleware_extract_basic,
        );
        self.register(
            "Middleware.cors_config",
            middleware::native_middleware_cors_config,
        );
        self.register(
            "Middleware.rate_limit_config",
            middleware::native_middleware_rate_limit_config,
        );

        // -- Map --
        self.register("Map.empty", native_map_empty);
        self.register_owning("Map.insert", native_map_insert, native_map_insert_owning);
        self.register("Map.get", native_map_get);
        self.register_owning("Map.remove", native_map_remove, native_map_remove_owning);
        self.register("Map.contains", native_map_contains);
        self.register("Map.keys", native_map_keys);
        self.register("Map.values", native_map_values);
        self.register("Map.len", native_map_len);
        // Spec-aligned aliases
        self.register_owning("Map.set", native_map_insert, native_map_insert_owning);
        self.register("Map.has", native_map_contains);
        self.register("Map.size", native_map_len);
        self.register("Map.from_list", native_map_from_list);

        // -- Set --
        self.register("Set.empty", native_set_empty);
        self.register("Set.insert", native_set_insert);
        self.register("Set.remove", native_set_remove);
        self.register("Set.contains", native_set_contains);
        self.register("Set.union", native_set_union);
        self.register("Set.intersection", native_set_intersection);
        self.register("Set.len", native_set_len);
        self.register("Set.from_list", native_set_from_list);

        // -- Weak --
        self.register("Weak.downgrade", weak::native_weak_downgrade);
        self.register("Weak.upgrade", weak::native_weak_upgrade);

        // -- Sqlite --
        self.register("Sqlite.connect!", db::native_sqlite_connect);
        self.register("Sqlite.connect", db::native_sqlite_connect);
        self.register("Sqlite.execute!", db::native_sqlite_execute);
        self.register("Sqlite.execute", db::native_sqlite_execute);
        self.register("Sqlite.query!", db::native_sqlite_query);
        self.register("Sqlite.query", db::native_sqlite_query);
        self.register("Sqlite.query_one!", db::native_sqlite_query_one);
        self.register("Sqlite.query_one", db::native_sqlite_query_one);
        self.register("Sqlite.query_map!", db::native_sqlite_query); // HOF placeholder
        self.register("Sqlite.query_map", db::native_sqlite_query); // HOF placeholder

        // -- Row accessors --
        self.register("Row.string", db_helpers::native_row_string);
        self.register("Row.int", db_helpers::native_row_int);
        self.register("Row.float", db_helpers::native_row_float);
        self.register("Row.bool", db_helpers::native_row_bool);
        self.register(
            "Row.optional_string",
            db_helpers::native_row_optional_string,
        );
        self.register("Row.optional_int", db_helpers::native_row_optional_int);
        self.register("Row.decode", db_helpers::native_row_decode);

        // -- query_as! / query_one_as! (HOF placeholders — actual dispatch in hof.rs) --
        self.register("Sqlite.query_as!", db::native_sqlite_query);
        self.register("Sqlite.query_as", db::native_sqlite_query);
        self.register("Sqlite.query_one_as!", db::native_sqlite_query_one);
        self.register("Sqlite.query_one_as", db::native_sqlite_query_one);

        // -- Postgres (behind feature flag) --
        #[cfg(feature = "postgres")]
        {
            self.register("Postgres.connect!", db_postgres::native_postgres_connect);
            self.register("Postgres.connect", db_postgres::native_postgres_connect);
            self.register("Postgres.execute!", db_postgres::native_postgres_execute);
            self.register("Postgres.execute", db_postgres::native_postgres_execute);
            self.register("Postgres.query!", db_postgres::native_postgres_query);
            self.register("Postgres.query", db_postgres::native_postgres_query);
            self.register("Postgres.query_one!", db::native_query_one);
            self.register("Postgres.query_one", db::native_query_one);
            self.register("Postgres.query_map!", db_postgres::native_postgres_query); // HOF placeholder
            self.register("Postgres.query_map", db_postgres::native_postgres_query); // HOF placeholder
            self.register("Postgres.query_as!", db_postgres::native_postgres_query);
            self.register("Postgres.query_as", db_postgres::native_postgres_query);
            self.register("Postgres.query_one_as!", db::native_query_one);
            self.register("Postgres.query_one_as", db::native_query_one);
        }

        // -- Mysql (behind feature flag) --
        #[cfg(feature = "mysql")]
        {
            self.register("Mysql.connect!", db_mysql::native_mysql_connect);
            self.register("Mysql.connect", db_mysql::native_mysql_connect);
            self.register("Mysql.execute!", db_mysql::native_mysql_execute);
            self.register("Mysql.execute", db_mysql::native_mysql_execute);
            self.register("Mysql.query!", db_mysql::native_mysql_query);
            self.register("Mysql.query", db_mysql::native_mysql_query);
            self.register("Mysql.query_one!", db::native_query_one);
            self.register("Mysql.query_one", db::native_query_one);
            self.register("Mysql.query_map!", db_mysql::native_mysql_query); // HOF placeholder
            self.register("Mysql.query_map", db_mysql::native_mysql_query); // HOF placeholder
            self.register("Mysql.query_as!", db_mysql::native_mysql_query);
            self.register("Mysql.query_as", db_mysql::native_mysql_query);
            self.register("Mysql.query_one_as!", db::native_query_one);
            self.register("Mysql.query_one_as", db::native_query_one);
        }

        // -- Sql.migrate! (generic migration runner) --
        self.register("Sql.migrate!", db_migrate::native_sql_migrate);
        self.register("Sql.migrate", db_migrate::native_sql_migrate);

        // -- Fs (VM-side) --
        self.register("Fs.read_file!", native_fs_read_file);
        self.register("Fs.read_file", native_fs_read_file);
        self.register("Fs.write_file!", native_fs_write_file);
        self.register("Fs.write_file", native_fs_write_file);
        self.register("Fs.exists!", native_fs_exists);
        self.register("Fs.exists", native_fs_exists);
        self.register("Fs.list_dir!", native_fs_list_dir);
        self.register("Fs.list_dir", native_fs_list_dir);
        // CST-compatible aliases
        self.register("Fs.read!", native_fs_read_file);
        self.register("Fs.read", native_fs_read_file);
        self.register("Fs.write!", native_fs_write_file);
        self.register("Fs.write", native_fs_write_file);

        // -- Test internals --
        self.register("__test_contains", native_test_contains);

        // -- Server (VM-reentrant, dispatched specially in vm.rs) --
        self.register("Server.listen!", native_server_listen_placeholder);
        self.register("Server.listen", native_server_listen_placeholder);
        self.register("Server.listen_async!", native_server_listen_placeholder);
        self.register("Server.listen_async", native_server_listen_placeholder);

        // -- Async fiber primitives (dispatched specially in vm.rs) --
        self.register("scope!", native_async_placeholder);
        self.register("scope", native_async_placeholder);
        self.register("Scope.spawn!", native_async_placeholder);
        self.register("Scope.spawn", native_async_placeholder);
        self.register("Cell.await!", native_async_placeholder);
        self.register("Cell.await", native_async_placeholder);
        self.register("Cell.cancel!", native_async_placeholder);
        self.register("Cell.cancel", native_async_placeholder);

        self.register("Async.parallel!", native_async_placeholder);
        self.register("Async.parallel", native_async_placeholder);
        self.register("Async.race!", native_async_placeholder);
        self.register("Async.race", native_async_placeholder);
        self.register("Async.scatter_gather!", native_async_placeholder);
        self.register("Async.scatter_gather", native_async_placeholder);

        self.register("Channel.bounded", native_async_placeholder);
        self.register("Channel.send!", native_async_placeholder);
        self.register("Channel.send", native_async_placeholder);
        self.register("Channel.recv!", native_async_placeholder);
        self.register("Channel.recv", native_async_placeholder);
        self.register("Channel.close!", native_async_placeholder);
        self.register("Channel.close", native_async_placeholder);

        self.register("Async.delay!", native_async_placeholder);
        self.register("Async.delay", native_async_placeholder);
        self.register("Async.interval!", native_async_placeholder);
        self.register("Async.interval", native_async_placeholder);
        self.register("Async.timeout!", native_async_placeholder);
        self.register("Async.timeout", native_async_placeholder);
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

fn native_server_listen_placeholder(_args: &[NValue]) -> Result<NValue, NativeError> {
    Err(NativeError(
        "Server.listen! must be executed by VM, not called directly".into(),
    ))
}

fn native_async_placeholder(_args: &[NValue]) -> Result<NValue, NativeError> {
    Err(NativeError(
        "Async primitives must be executed by VM, not called directly".into(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use console::*;
    use list::*;
    use math::*;
    use option::*;
    use result::*;
    use string::*;

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

    #[test]
    fn list_contains_found() {
        let list = NValue::list(vec![NValue::int(1), NValue::int(2), NValue::int(3)]);
        let result = native_list_contains(&[list, NValue::int(2)]).unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn list_contains_not_found() {
        let list = NValue::list(vec![NValue::int(1), NValue::int(2)]);
        let result = native_list_contains(&[list, NValue::int(5)]).unwrap();
        assert!(!result.as_bool());
    }

    #[test]
    fn test_contains_string() {
        let result = native_test_contains(&[
            NValue::string("hello world".into()),
            NValue::string("world".into()),
        ])
        .unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn test_contains_string_not_found() {
        let result =
            native_test_contains(&[NValue::string("hello".into()), NValue::string("xyz".into())])
                .unwrap();
        assert!(!result.as_bool());
    }

    #[test]
    fn test_contains_list() {
        let list = NValue::list(vec![NValue::int(10), NValue::int(20)]);
        let result = native_test_contains(&[list, NValue::int(10)]).unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn test_contains_list_not_found() {
        let list = NValue::list(vec![NValue::int(10)]);
        let result = native_test_contains(&[list, NValue::int(99)]).unwrap();
        assert!(!result.as_bool());
    }

    #[test]
    fn response_unauthorized_registered() {
        let reg = NativeRegistry::new();
        assert!(reg.lookup("Response.unauthorized").is_some());
        assert!(reg.lookup("Response.forbidden").is_some());
        assert!(reg.lookup("Response.conflict").is_some());
        assert!(reg.lookup("Response.unprocessable").is_some());
        assert!(reg.lookup("Response.too_many_requests").is_some());
        assert!(reg.lookup("Response.redirect_temporary").is_some());
    }

    #[test]
    fn response_unauthorized_status_401() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.unauthorized").unwrap();
        let result = reg.call(id, &[NValue::string("no auth".into())]).unwrap();
        let fields = result.as_record().unwrap();
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .unwrap()
            .1
            .as_any_int();
        assert_eq!(status, 401);
    }

    #[test]
    fn response_forbidden_status_403() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.forbidden").unwrap();
        let result = reg.call(id, &[NValue::string("denied".into())]).unwrap();
        let fields = result.as_record().unwrap();
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .unwrap()
            .1
            .as_any_int();
        assert_eq!(status, 403);
    }

    #[test]
    fn response_conflict_status_409() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.conflict").unwrap();
        let result = reg.call(id, &[NValue::string("dup".into())]).unwrap();
        let fields = result.as_record().unwrap();
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .unwrap()
            .1
            .as_any_int();
        assert_eq!(status, 409);
    }

    #[test]
    fn response_too_many_requests_status_429() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.too_many_requests").unwrap();
        let result = reg.call(id, &[NValue::string("slow down".into())]).unwrap();
        let fields = result.as_record().unwrap();
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .unwrap()
            .1
            .as_any_int();
        assert_eq!(status, 429);
    }

    #[test]
    fn response_redirect_temporary_status_307() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.redirect_temporary").unwrap();
        let result = reg.call(id, &[NValue::string("/new".into())]).unwrap();
        let fields = result.as_record().unwrap();
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .unwrap()
            .1
            .as_any_int();
        assert_eq!(status, 307);
    }

    #[test]
    fn response_created_auto_serializes_record() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.created").unwrap();
        let record = NValue::record(vec![
            ("id".into(), NValue::int(1)),
            ("name".into(), NValue::string("test".into())),
        ]);
        let result = reg.call(id, &[record]).unwrap();
        let fields = result.as_record().unwrap();
        let body = fields
            .iter()
            .find(|(k, _)| &**k == "body")
            .unwrap()
            .1
            .as_string()
            .unwrap();
        assert!(body.contains("\"id\""));
        assert!(body.contains("\"name\""));
        // Check Content-Type header was set
        let headers = fields
            .iter()
            .find(|(k, _)| &**k == "headers")
            .unwrap()
            .1
            .as_list()
            .unwrap();
        assert!(!headers.is_empty());
    }

    #[test]
    fn response_bad_request_auto_serializes_record() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Response.bad_request").unwrap();
        let record = NValue::record(vec![("error".into(), NValue::string("invalid".into()))]);
        let result = reg.call(id, &[record]).unwrap();
        let fields = result.as_record().unwrap();
        let body = fields
            .iter()
            .find(|(k, _)| &**k == "body")
            .unwrap()
            .1
            .as_string()
            .unwrap();
        assert!(body.contains("\"error\""));
    }

    #[test]
    fn http_error_constructors_registered() {
        let reg = NativeRegistry::new();
        assert!(reg.lookup("HttpError.bad_request").is_some());
        assert!(reg.lookup("HttpError.not_found").is_some());
        assert!(reg.lookup("HttpError.unauthorized").is_some());
        assert!(reg.lookup("HttpError.forbidden").is_some());
        assert!(reg.lookup("HttpError.conflict").is_some());
        assert!(reg.lookup("HttpError.unprocessable").is_some());
        assert!(reg.lookup("HttpError.internal").is_some());
    }

    #[test]
    fn http_error_bad_request_creates_enum() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("HttpError.bad_request").unwrap();
        let result = reg
            .call(id, &[NValue::string("invalid input".into())])
            .unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "BadRequest");
        assert_eq!(payload.as_string().unwrap().as_ref(), "invalid input");
    }

    #[test]
    fn http_error_not_found_creates_enum() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("HttpError.not_found").unwrap();
        let result = reg
            .call(id, &[NValue::string("user not found".into())])
            .unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "NotFound");
        assert_eq!(payload.as_string().unwrap().as_ref(), "user not found");
    }

    #[test]
    fn http_error_unauthorized_creates_enum() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("HttpError.unauthorized").unwrap();
        let result = reg.call(id, &[NValue::string("bad token".into())]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Unauthorized");
        assert_eq!(payload.as_string().unwrap().as_ref(), "bad token");
    }

    // -- Request.body_json now returns HttpError --

    #[test]
    fn request_body_json_error_returns_http_error() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Request.body_json").unwrap();
        let req = NValue::record(vec![
            ("body".into(), NValue::string("not json".into())),
            ("method".into(), NValue::string("POST".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
        // Should be HttpError.bad_request, not a plain string
        let (err_tag, _) = payload.as_enum().unwrap();
        assert_eq!(err_tag.as_ref(), "BadRequest");
    }

    #[test]
    fn request_body_json_empty_returns_http_error() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Request.body_json").unwrap();
        let req = NValue::record(vec![
            ("body".into(), NValue::string("".into())),
            ("method".into(), NValue::string("POST".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
        let (err_tag, _) = payload.as_enum().unwrap();
        assert_eq!(err_tag.as_ref(), "BadRequest");
    }

    // -- Router.state tests --

    #[test]
    fn router_state_adds_state_to_router() {
        let reg = NativeRegistry::new();
        let new_id = reg.lookup("Router.new").unwrap();
        let state_id = reg.lookup("Router.state").unwrap();
        let router = reg.call(new_id, &[]).unwrap();
        let updated = reg
            .call(
                state_id,
                &[
                    router,
                    NValue::string("db_url".into()),
                    NValue::string("postgres://localhost".into()),
                ],
            )
            .unwrap();
        let fields = updated.as_record().unwrap();
        let state_field = fields.iter().find(|(k, _)| &**k == "state").unwrap();
        let state_rec = state_field.1.as_record().unwrap();
        let db_url = state_rec.iter().find(|(k, _)| &**k == "db_url").unwrap();
        assert_eq!(
            db_url.1.as_string().unwrap().as_ref(),
            "postgres://localhost"
        );
    }

    #[test]
    fn router_state_multiple_keys() {
        let reg = NativeRegistry::new();
        let new_id = reg.lookup("Router.new").unwrap();
        let state_id = reg.lookup("Router.state").unwrap();
        let router = reg.call(new_id, &[]).unwrap();
        let r1 = reg
            .call(
                state_id,
                &[
                    router,
                    NValue::string("key1".into()),
                    NValue::string("val1".into()),
                ],
            )
            .unwrap();
        let r2 = reg
            .call(
                state_id,
                &[r1, NValue::string("key2".into()), NValue::int(42)],
            )
            .unwrap();
        let fields = r2.as_record().unwrap();
        let state_rec = fields
            .iter()
            .find(|(k, _)| &**k == "state")
            .unwrap()
            .1
            .as_record()
            .unwrap();
        assert_eq!(state_rec.len(), 2);
    }

    // -- Middleware module tests --

    #[test]
    fn middleware_extract_bearer_valid() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.extract_bearer").unwrap();
        let req = NValue::record(vec![
            (
                "headers".into(),
                NValue::list(vec![NValue::tuple(vec![
                    NValue::string("Authorization".into()),
                    NValue::string("Bearer abc123token".into()),
                ])]),
            ),
            ("method".into(), NValue::string("GET".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");
        assert_eq!(payload.as_string().unwrap().as_ref(), "abc123token");
    }

    #[test]
    fn middleware_extract_bearer_missing_header() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.extract_bearer").unwrap();
        let req = NValue::record(vec![
            ("headers".into(), NValue::list(vec![])),
            ("method".into(), NValue::string("GET".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
        let (err_tag, _) = payload.as_enum().unwrap();
        assert_eq!(err_tag.as_ref(), "Unauthorized");
    }

    #[test]
    fn middleware_extract_bearer_wrong_scheme() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.extract_bearer").unwrap();
        let req = NValue::record(vec![
            (
                "headers".into(),
                NValue::list(vec![NValue::tuple(vec![
                    NValue::string("Authorization".into()),
                    NValue::string("Basic dXNlcjpwYXNz".into()),
                ])]),
            ),
            ("method".into(), NValue::string("GET".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
        let (err_tag, _) = payload.as_enum().unwrap();
        assert_eq!(err_tag.as_ref(), "Unauthorized");
    }

    #[test]
    fn middleware_extract_basic_valid() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.extract_basic").unwrap();
        // "user:pass" base64 = "dXNlcjpwYXNz"
        let req = NValue::record(vec![
            (
                "headers".into(),
                NValue::list(vec![NValue::tuple(vec![
                    NValue::string("Authorization".into()),
                    NValue::string("Basic dXNlcjpwYXNz".into()),
                ])]),
            ),
            ("method".into(), NValue::string("GET".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");
        // Should return a tuple (username, password)
        let elems = payload.as_tuple().unwrap();
        assert_eq!(elems[0].as_string().unwrap().as_ref(), "user");
        assert_eq!(elems[1].as_string().unwrap().as_ref(), "pass");
    }

    #[test]
    fn middleware_extract_basic_missing() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.extract_basic").unwrap();
        let req = NValue::record(vec![
            ("headers".into(), NValue::list(vec![])),
            ("method".into(), NValue::string("GET".into())),
        ]);
        let result = reg.call(id, &[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
        let (err_tag, _) = payload.as_enum().unwrap();
        assert_eq!(err_tag.as_ref(), "Unauthorized");
    }

    #[test]
    fn middleware_cors_config_returns_record() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.cors_config").unwrap();
        let origins = NValue::list(vec![NValue::string("http://localhost:3000".into())]);
        let result = reg.call(id, &[origins]).unwrap();
        let fields = result.as_record().unwrap();
        // Should have allowed_origins field
        let origins_field = fields
            .iter()
            .find(|(k, _)| &**k == "allowed_origins")
            .unwrap();
        let origins_list = origins_field.1.as_list().unwrap();
        assert_eq!(origins_list.len(), 1);
        assert_eq!(
            origins_list[0].as_string().unwrap().as_ref(),
            "http://localhost:3000"
        );
    }

    #[test]
    fn middleware_rate_limit_config_returns_record() {
        let reg = NativeRegistry::new();
        let id = reg.lookup("Middleware.rate_limit_config").unwrap();
        let result = reg.call(id, &[NValue::int(50), NValue::int(100)]).unwrap();
        let fields = result.as_record().unwrap();
        let rps = fields
            .iter()
            .find(|(k, _)| &**k == "requests_per_second")
            .unwrap();
        assert_eq!(rps.1.as_any_int(), 50);
        let burst = fields.iter().find(|(k, _)| &**k == "burst_size").unwrap();
        assert_eq!(burst.1.as_any_int(), 100);
    }

    #[test]
    fn sqlite_functions_registered() {
        let reg = NativeRegistry::new();
        assert!(reg.lookup("Sqlite.connect!").is_some());
        assert!(reg.lookup("Sqlite.connect").is_some());
        assert!(reg.lookup("Sqlite.execute!").is_some());
        assert!(reg.lookup("Sqlite.execute").is_some());
        assert!(reg.lookup("Sqlite.query!").is_some());
        assert!(reg.lookup("Sqlite.query").is_some());
    }

    // Compile-time assertion: NativeRegistry must be Send+Sync
    // for cross-fiber sharing in the structured concurrency runtime.
    #[allow(dead_code)]
    const _: () = {
        fn assert_send_sync<T: Send + Sync>() {}
        fn check() {
            assert_send_sync::<super::NativeRegistry>();
        }
    };
}
