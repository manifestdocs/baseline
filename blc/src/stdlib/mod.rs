pub mod list;
pub mod option;
pub mod result;
pub mod string;

use std::collections::HashMap;
use crate::interpreter::RuntimeValue;

/// Native function signature operating directly on RuntimeValues.
pub type NativeFn = for<'a> fn(&[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String>;

/// Registry of native functions keyed by qualified name (e.g., "Option.unwrap").
pub struct NativeRegistry {
    fns: HashMap<String, NativeFn>,
}

impl NativeRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            fns: HashMap::new(),
        };
        option::register(&mut registry);
        result::register(&mut registry);
        string::register(&mut registry);
        list::register(&mut registry);
        registry
    }

    pub fn register(&mut self, name: &str, f: NativeFn) {
        self.fns.insert(name.to_string(), f);
    }

    pub fn get(&self, name: &str) -> Option<&NativeFn> {
        self.fns.get(name)
    }
}
