pub mod http;
pub mod json;
pub mod list;
pub mod option;
pub mod response;
pub mod result;
pub mod router;
pub mod string;

use std::collections::HashMap;
use crate::interpreter::RuntimeValue;
use crate::prelude::Prelude;

/// Native function signature operating directly on RuntimeValues.
pub type NativeFn = for<'a> fn(&[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String>;

/// Registry of native functions keyed by qualified name (e.g., "Option.unwrap").
pub struct NativeRegistry {
    fns: HashMap<String, NativeFn>,
}

impl Default for NativeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl NativeRegistry {
    /// Create a registry with ALL native modules (backwards compat for tests).
    pub fn new() -> Self {
        Self::with_prelude(Prelude::Script)
    }

    /// Create a registry gated by the given prelude.
    pub fn with_prelude(prelude: Prelude) -> Self {
        let mut registry = Self {
            fns: HashMap::new(),
        };
        let modules = prelude.native_modules();
        if modules.contains(&"Option") {
            option::register(&mut registry);
        }
        if modules.contains(&"Result") {
            result::register(&mut registry);
        }
        if modules.contains(&"String") {
            string::register(&mut registry);
        }
        if modules.contains(&"List") {
            list::register(&mut registry);
        }
        if modules.contains(&"Json") {
            json::register(&mut registry);
        }
        if modules.contains(&"Router") {
            router::register(&mut registry);
        }
        if modules.contains(&"Http") {
            http::register(&mut registry);
        }
        if modules.contains(&"Response") {
            response::register(&mut registry);
        }
        registry
    }

    pub fn register(&mut self, name: &str, f: NativeFn) {
        self.fns.insert(name.to_string(), f);
    }

    pub fn get(&self, name: &str) -> Option<&NativeFn> {
        self.fns.get(name)
    }
}
