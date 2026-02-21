use super::super::infer::{GenericSchema, UserGenericSchema, builtin_generic_schemas};
use super::builtins::builtin_type_signatures;
use super::type_def::{Type, TypeMap};
use crate::prelude::{self, Prelude};
use std::collections::{HashMap, HashSet};

/// A trait definition: trait name + method signatures.
#[allow(dead_code)]
pub struct TraitDef {
    pub name: String,
    /// Method signatures: (method_name, fn_type with Self as TypeParam).
    pub methods: Vec<(String, Type)>,
    /// Default method CST byte offsets: method_name -> start_byte of function_def node.
    /// Used by the lowerer to re-lower the default body per concrete type.
    pub default_method_bytes: HashMap<String, usize>,
    /// Supertrait names (direct parents only).
    pub supertraits: Vec<String>,
}

/// A concrete impl of a trait for a specific type.
#[allow(dead_code)]
pub struct TraitImpl {
    pub trait_name: String,
    pub target_type: Type,
    /// method_name -> mangled function name (e.g. "Show$Int$show").
    pub methods: HashMap<String, String>,
}

pub struct SymbolTable {
    pub(super) scopes: Vec<HashMap<String, Type>>,
    pub(super) types: HashMap<String, Type>, // Registry for named types (structs)
    pub(super) module_methods: HashMap<String, Type>, // "Module.method" -> Function type
    pub(super) generic_schemas: HashMap<String, GenericSchema>, // "Module.method" -> generic schema
    pub(super) user_generic_schemas: HashMap<String, UserGenericSchema>, // user function -> generic schema
    /// Parameter names for known functions (for named argument resolution).
    pub(super) fn_params: HashMap<String, Vec<String>>,
    /// Accumulated type map: CST node start_byte -> resolved Type.
    /// Populated during type checking for use by the VM compiler.
    pub type_map: TypeMap,
    /// Modules explicitly imported by the user (vs builtin pseudo-modules).
    pub(super) user_modules: HashSet<String>,
    /// Trait definitions: trait_name -> TraitDef.
    pub(super) trait_defs: HashMap<String, TraitDef>,
    /// Trait implementations: (trait_name, type_key) -> TraitImpl.
    pub(super) trait_impls: HashMap<(String, String), TraitImpl>,
    /// Dictionary info for bounded generic call sites.
    /// Key: call_expression start_byte. Value: dictionary entries.
    pub dict_map: HashMap<usize, Vec<DictEntry>>,
    /// Active trait bounds in current generic function scope.
    pub(super) current_bounds: HashMap<String, Vec<String>>,
}

/// Dictionary entry for a single trait bound at a call site.
pub struct DictEntry {
    pub trait_name: String,
    pub methods: Vec<(String, String)>, // (method_name, mangled_impl_name)
}

/// Dictionary map: call_expression start_byte -> dictionary entries for hidden args.
pub type DictMap = HashMap<usize, Vec<DictEntry>>;

impl SymbolTable {
    pub(super) fn with_prelude(prelude: Prelude) -> Self {
        let mut table = Self {
            scopes: vec![HashMap::new()],
            types: HashMap::new(),
            module_methods: builtin_type_signatures(&prelude),
            generic_schemas: builtin_generic_schemas(),
            user_generic_schemas: HashMap::new(),
            fn_params: HashMap::new(),
            type_map: TypeMap::new(),
            user_modules: HashSet::new(),
            trait_defs: HashMap::new(),
            trait_impls: HashMap::new(),
            dict_map: HashMap::new(),
            current_bounds: HashMap::new(),
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

        // HttpError type — gated by server/script prelude.
        if prelude.native_modules().contains(&"HttpError") {
            let http_error_type = Type::Enum(
                "HttpError".to_string(),
                vec![
                    ("BadRequest".to_string(), vec![Type::String]),
                    ("NotFound".to_string(), vec![Type::String]),
                    ("Unauthorized".to_string(), vec![Type::String]),
                    ("Forbidden".to_string(), vec![Type::String]),
                    ("Conflict".to_string(), vec![Type::String]),
                    ("Unprocessable".to_string(), vec![Type::String]),
                    ("Internal".to_string(), vec![Type::String]),
                ],
            );
            table.insert_type("HttpError".to_string(), http_error_type);
        }

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

    pub(super) fn is_user_module(&self, name: &str) -> bool {
        self.user_modules.contains(name)
    }

    pub(super) fn lookup_generic_schema(&self, qualified_name: &str) -> Option<&GenericSchema> {
        self.generic_schemas.get(qualified_name)
    }

    pub(super) fn lookup_user_generic_schema(&self, name: &str) -> Option<&UserGenericSchema> {
        self.user_generic_schemas.get(name)
    }

    pub(super) fn insert_fn_params(&mut self, name: String, params: Vec<String>) {
        self.fn_params.insert(name, params);
    }

    pub(super) fn lookup_fn_params(&self, name: &str) -> Option<&Vec<String>> {
        self.fn_params.get(name)
    }

    /// Convert a type to a string key for trait impl lookup.
    pub(super) fn type_key(ty: &Type) -> String {
        match ty {
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Struct(name, _) => name.clone(),
            Type::Enum(name, _) => name.clone(),
            Type::List(inner) => format!("List<{}>", Self::type_key(inner)),
            Type::Refined(_, name) => name.clone(),
            _ => format!("{}", ty),
        }
    }

    pub(super) fn lookup_trait(&self, name: &str) -> Option<&TraitDef> {
        self.trait_defs.get(name)
    }

    pub(super) fn lookup_trait_impl(&self, trait_name: &str, type_key: &str) -> Option<&TraitImpl> {
        self.trait_impls
            .get(&(trait_name.to_string(), type_key.to_string()))
    }

    /// All supertraits transitively (flattened, deduplicated).
    pub(super) fn all_supertraits(&self, trait_name: &str) -> Vec<String> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        self.collect_supertraits(trait_name, &mut result, &mut visited);
        result
    }

    fn collect_supertraits(
        &self,
        trait_name: &str,
        result: &mut Vec<String>,
        visited: &mut HashSet<String>,
    ) {
        if let Some(td) = self.trait_defs.get(trait_name) {
            for st in &td.supertraits {
                if visited.insert(st.clone()) {
                    result.push(st.clone());
                    self.collect_supertraits(st, result, visited);
                }
            }
        }
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
