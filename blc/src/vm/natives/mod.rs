mod console;
mod env;
pub use env::set_program_args;
mod fs;
pub(crate) mod fs_sandbox;
pub use fs_sandbox::set_fs_sandbox;
mod int_conv;
pub(crate) mod json;
mod list;
mod log;
mod map;
mod math;
mod option;
mod random;
mod request;
mod response;
mod result;
mod router;
mod set;
mod string;
mod time;

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
                        | "Option.flat_map"
                        | "Result.map"
                        | "Result.and_then"
                )
            })
            .unwrap_or(false)
    }

    /// Check if a function requires VM re-entrancy (server dispatch).
    pub fn is_server_listen(&self, id: u16) -> bool {
        self.entries
            .get(id as usize)
            .map(|e| matches!(e.name, "Server.listen!" | "Server.listen" | "Server.listen_async!" | "Server.listen_async"))
            .unwrap_or(false)
    }

    /// Check if a function is an async fiber primitive (scope!, spawn!, await!, cancel!).
    pub fn is_async(&self, id: u16) -> bool {
        self.entries
            .get(id as usize)
            .map(|e| {
                matches!(
                    e.name,
                    "scope!" | "scope"
                        | "Scope.spawn!" | "Scope.spawn"
                        | "Cell.await!" | "Cell.await"
                        | "Cell.cancel!" | "Cell.cancel"
                )
            })
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

        // -- List (non-HOF) --
        self.register("List.length", native_list_length);
        self.register("List.head", native_list_head);
        self.register("List.tail", native_list_tail);
        self.register("List.reverse", native_list_reverse);
        self.register("List.sort", native_list_sort);
        self.register("List.concat", native_list_concat);
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
        self.register("Option.map", native_hof_placeholder);
        self.register("Option.flat_map", native_hof_placeholder);

        // -- Result --
        self.register("Result.unwrap", native_result_unwrap);
        self.register("Result.unwrap_or", native_result_unwrap_or);
        self.register("Result.is_ok", native_result_is_ok);
        self.register("Result.is_err", native_result_is_err);
        self.register("Result.map", native_hof_placeholder);
        self.register("Result.and_then", native_hof_placeholder);

        // -- Time --
        self.register("Time.now!", native_time_now);
        self.register("Time.now", native_time_now);
        self.register("Time.sleep!", native_time_sleep);
        self.register("Time.sleep", native_time_sleep);

        // -- Random --
        self.register("Random.int!", native_random_int);
        self.register("Random.int", native_random_int);
        self.register("Random.bool!", native_random_bool);
        self.register("Random.bool", native_random_bool);
        self.register("Random.uuid!", native_random_uuid);
        self.register("Random.uuid", native_random_uuid);

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

        // -- Map --
        self.register("Map.empty", native_map_empty);
        self.register("Map.insert", native_map_insert);
        self.register("Map.get", native_map_get);
        self.register("Map.remove", native_map_remove);
        self.register("Map.contains", native_map_contains);
        self.register("Map.keys", native_map_keys);
        self.register("Map.values", native_map_values);
        self.register("Map.len", native_map_len);
        // Spec-aligned aliases
        self.register("Map.set", native_map_insert);
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
        let result = native_test_contains(&[
            NValue::string("hello".into()),
            NValue::string("xyz".into()),
        ])
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

    // Compile-time assertion: NativeRegistry must be Send+Sync
    // for cross-fiber sharing in the structured concurrency runtime.
    const _: () = {
        fn assert_send_sync<T: Send + Sync>() {}
        fn check() {
            assert_send_sync::<super::NativeRegistry>();
        }
    };
}
