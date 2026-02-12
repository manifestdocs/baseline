use super::builtins::builtin_type_signatures;
use super::type_def::{Type, TypeMap};
use crate::prelude::{self, Prelude};
use super::super::infer::{GenericSchema, UserGenericSchema, builtin_generic_schemas};
use std::collections::{HashMap, HashSet};

pub struct SymbolTable {
    pub(super) scopes: Vec<HashMap<String, Type>>,
    pub(super) types: HashMap<String, Type>, // Registry for named types (structs)
    pub(super) module_methods: HashMap<String, Type>, // "Module.method" -> Function type
    pub(super) generic_schemas: HashMap<String, GenericSchema>, // "Module.method" -> generic schema
    pub(super) user_generic_schemas: HashMap<String, UserGenericSchema>, // user function -> generic schema
    /// Accumulated type map: CST node start_byte -> resolved Type.
    /// Populated during type checking for use by the VM compiler.
    pub type_map: TypeMap,
}

impl SymbolTable {
    pub(super) fn with_prelude(prelude: Prelude) -> Self {
        let mut table = Self {
            scopes: vec![HashMap::new()],
            types: HashMap::new(),
            module_methods: builtin_type_signatures(&prelude),
            generic_schemas: builtin_generic_schemas(),
            user_generic_schemas: HashMap::new(),
            type_map: TypeMap::new(),
        };

        // Option/Result types and constructors are language primitives — always registered.
        let option_type = Type::Enum(
            "Option".to_string(),
            vec![
                ("Some".to_string(), vec![Type::Unknown]),
                ("None".to_string(), vec![]),
            ],
        );
        table.insert_type("Option".to_string(), option_type.clone());
        table.insert("None".to_string(), option_type.clone());
        table.insert(
            "Some".to_string(),
            Type::Function(vec![Type::Unknown], Box::new(option_type.clone())),
        );

        let result_type = Type::Enum(
            "Result".to_string(),
            vec![
                ("Ok".to_string(), vec![Type::Unknown]),
                ("Err".to_string(), vec![Type::Unknown]),
            ],
        );
        table.insert_type("Result".to_string(), result_type.clone());
        table.insert(
            "Ok".to_string(),
            Type::Function(vec![Type::Unknown], Box::new(result_type.clone())),
        );
        table.insert(
            "Err".to_string(),
            Type::Function(vec![Type::Unknown], Box::new(result_type.clone())),
        );

        // Register module namespaces gated by prelude.
        for module in prelude.type_modules() {
            table.insert(module.to_string(), Type::Module(module.to_string()));
            // Also register the short alias (e.g. "str" -> Module("String")).
            if let Some(alias) = prelude::reverse_module_alias(module) {
                table.insert(alias.to_string(), Type::Module(module.to_string()));
            }
        }

        table
    }

    pub(super) fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(super) fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub(super) fn insert(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub(super) fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    pub(super) fn insert_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    pub(super) fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    pub(super) fn lookup_module_method(&self, qualified_name: &str) -> Option<&Type> {
        self.module_methods.get(qualified_name)
    }

    pub(super) fn lookup_generic_schema(&self, qualified_name: &str) -> Option<&GenericSchema> {
        self.generic_schemas.get(qualified_name)
    }

    pub(super) fn lookup_user_generic_schema(&self, name: &str) -> Option<&UserGenericSchema> {
        self.user_generic_schemas.get(name)
    }

    /// Collect all visible bindings from current scope chain (for typed holes).
    pub(super) fn visible_bindings(&self) -> Vec<(String, Type)> {
        let mut seen = HashSet::new();
        let mut bindings = Vec::new();
        for scope in self.scopes.iter().rev() {
            for (name, ty) in scope {
                if seen.insert(name.clone()) {
                    bindings.push((name.clone(), ty.clone()));
                }
            }
        }
        bindings.sort_by(|a, b| a.0.cmp(&b.0));
        bindings
    }
}
