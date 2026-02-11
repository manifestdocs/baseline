use super::infer::{GenericSchema, InferCtx, UserGenericSchema, builtin_generic_schemas};
use crate::diagnostics::{Diagnostic, Location, Patch, Severity, Suggestion};
use crate::prelude::{self, Prelude};
use crate::resolver::{ImportKind, ModuleLoader};
use std::collections::{HashMap, HashSet};
use tree_sitter::Node;

/// Maps CST node `start_byte()` to the resolved Type for that expression.
/// Used by the VM compiler for type-directed opcode specialization.
pub type TypeMap = HashMap<usize, Type>;

/// Classification of what a single match arm pattern covers.
enum PatternCoverage {
    CatchAll,
    Variant(String),
    BoolLiteral(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Function(Vec<Type>, Box<Type>),
    List(Box<Type>),
    Struct(String, HashMap<String, Type>),  // Name, Fields
    Record(HashMap<String, Type>, Option<u32>), // Anonymous record, optional row var
    Tuple(Vec<Type>),                       // (T, U, ...)
    Enum(String, Vec<(String, Vec<Type>)>), // Name, [(VariantName, PayloadTypes)]
    Map(Box<Type>, Box<Type>),             // Map<K, V>
    Set(Box<Type>),                        // Set<T>
    Module(String),                         // Module/Effect namespace
    Var(u32),                               // Inference type variable
    TypeParam(String),                      // Named type parameter (e.g., T, A, K)
    Refined(Box<Type>, String),             // Refined type: base type + alias name
    Unknown,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "Unit"),
            Type::Function(args, ret) => {
                let args_str = args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", args_str, ret)
            }
            Type::List(inner) => write!(f, "List<{}>", inner),
            Type::Struct(name, _) => write!(f, "{}", name),
            Type::Record(fields, row) => {
                let mut sorted_fields: Vec<_> = fields.iter().collect();
                sorted_fields.sort_by_key(|(k, _)| *k);
                let fields_str = sorted_fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(var) = row {
                    write!(f, "{{ {}, ..r{} }}", fields_str, var)
                } else {
                    write!(f, "{{ {} }}", fields_str)
                }
            }
            Type::Tuple(elems) => {
                let elems_str = elems
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({})", elems_str)
            }
            Type::Enum(name, variants) => {
                // Show type parameters for Option<T> and Result<T, E>
                match name.as_str() {
                    "Option" => {
                        if let Some((_, payload)) = variants.first()
                            && let Some(inner) = payload.first()
                            && *inner != Type::Unknown
                        {
                            return write!(f, "Option<{}>", inner);
                        }
                        write!(f, "Option")
                    }
                    "Result" => {
                        let ok_t = variants.first().and_then(|(_, p)| p.first());
                        let err_t = variants.get(1).and_then(|(_, p)| p.first());
                        if let (Some(ok), Some(err)) = (ok_t, err_t)
                            && (*ok != Type::Unknown || *err != Type::Unknown)
                        {
                            return write!(f, "Result<{}, {}>", ok, err);
                        }
                        write!(f, "Result")
                    }
                    _ => write!(f, "{}", name),
                }
            }
            Type::Map(k, v) => write!(f, "Map<{}, {}>", k, v),
            Type::Set(inner) => write!(f, "Set<{}>", inner),
            Type::Module(name) => write!(f, "Module({})", name),
            Type::Var(id) => write!(f, "?{}", id),
            Type::TypeParam(name) => write!(f, "{}", name),
            Type::Refined(_, name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}

/// Build a Location from a tree-sitter Node.

/// Check a single `inline_test` node: the expression must type-check to Bool.
fn check_inline_test(
    test_node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // inline_test: "test" string_literal "=" _expression
    // The expression is the last named child
    let count = test_node.named_child_count();
    if count == 0 {
        return;
    }
    let expr_node = test_node.named_child(count - 1).unwrap();
    // Skip string_literal (the test name)
    if expr_node.kind() == "string_literal" {
        return;
    }
    let test_type = check_node(&expr_node, source, file, symbols, diagnostics);
    if test_type != Type::Bool && test_type != Type::Unknown {
        diagnostics.push(Diagnostic {
            code: "TYP_026".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file,&expr_node),
            message: format!("Inline test expression must be Bool, found {}", test_type),
            context: "Test expressions should evaluate to true or false.".to_string(),
            suggestions: vec![],
        });
    }
}

/// Check all inline_test nodes within a where_block child of a function_def.
fn check_where_block(
    func_node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut cursor = func_node.walk();
    for child in func_node.children(&mut cursor) {
        if child.kind() == "where_block" {
            let mut test_cursor = child.walk();
            for test_node in child.children(&mut test_cursor) {
                if test_node.kind() == "inline_test" {
                    check_inline_test(&test_node, source, file, symbols, diagnostics);
                }
            }
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
    types: HashMap<String, Type>, // Registry for named types (structs)
    module_methods: HashMap<String, Type>, // "Module.method" -> Function type
    generic_schemas: HashMap<String, GenericSchema>, // "Module.method" -> generic schema
    user_generic_schemas: HashMap<String, UserGenericSchema>, // user function -> generic schema
    /// Accumulated type map: CST node start_byte → resolved Type.
    /// Populated during type checking for use by the VM compiler.
    pub type_map: TypeMap,
}

impl SymbolTable {
    fn with_prelude(prelude: Prelude) -> Self {
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

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    fn insert_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    fn lookup_module_method(&self, qualified_name: &str) -> Option<&Type> {
        self.module_methods.get(qualified_name)
    }

    fn lookup_generic_schema(&self, qualified_name: &str) -> Option<&GenericSchema> {
        self.generic_schemas.get(qualified_name)
    }

    fn lookup_user_generic_schema(&self, name: &str) -> Option<&UserGenericSchema> {
        self.user_generic_schemas.get(name)
    }

    /// Collect all visible bindings from current scope chain (for typed holes).
    fn visible_bindings(&self) -> Vec<(String, Type)> {
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

/// Build a map of "Module.method" -> Function type for all builtins and native
/// stdlib functions gated by the given prelude.
fn builtin_type_signatures(prelude: &Prelude) -> HashMap<String, Type> {
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
            Type::Function(vec![Type::String], Box::new(Type::String)),
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
            Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)),
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

/// Check if two types are compatible, treating Unknown as a wildcard.
/// Same-named Enums check payload types recursively for generic enforcement.
fn types_compatible(a: &Type, b: &Type) -> bool {
    // Unwrap refined types to their base for compatibility checks
    let a = match a {
        Type::Refined(base, _) => base.as_ref(),
        other => other,
    };
    let b = match b {
        Type::Refined(base, _) => base.as_ref(),
        other => other,
    };
    if a == b {
        return true;
    }
    if *a == Type::Unknown || *b == Type::Unknown {
        return true;
    }
    match (a, b) {
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        // TypeParam matches itself (same name) or Unknown/Var
        (Type::TypeParam(na), Type::TypeParam(nb)) => na == nb,
        (Type::TypeParam(_), _) | (_, Type::TypeParam(_)) => false,
        (Type::Enum(na, va), Type::Enum(nb, vb)) => {
            if na != nb {
                return false;
            }
            // Same-named enums: check payload types are compatible
            for ((_, pa), (_, pb)) in va.iter().zip(vb.iter()) {
                for (ta, tb) in pa.iter().zip(pb.iter()) {
                    if !types_compatible(ta, tb) {
                        return false;
                    }
                }
            }
            true
        }
        (Type::List(ia), Type::List(ib)) => types_compatible(ia, ib),
        (Type::Map(ka, va), Type::Map(kb, vb)) => {
            types_compatible(ka, kb) && types_compatible(va, vb)
        }
        (Type::Set(ia), Type::Set(ib)) => types_compatible(ia, ib),
        // Row polymorphism: open record { a: T, ..r } is compatible with any record
        // that has at least those fields with compatible types
        (Type::Record(fields_a, Some(_)), Type::Record(fields_b, _))
        | (Type::Record(fields_b, _), Type::Record(fields_a, Some(_))) => {
            // The open record's fields must all exist in the other record with compatible types
            fields_a.iter().all(|(k, ta)| {
                fields_b.get(k).map_or(false, |tb| types_compatible(ta, tb))
            })
        }
        (Type::Record(fa, None), Type::Record(fb, None)) => {
            // Closed records: exact field match
            fa.len() == fb.len()
                && fa.iter().all(|(k, ta)| {
                    fb.get(k).map_or(false, |tb| types_compatible(ta, tb))
                })
        }
        // Open record with Struct (structs have fixed fields)
        (Type::Record(fields, Some(_)), Type::Struct(_, sfields))
        | (Type::Struct(_, sfields), Type::Record(fields, Some(_))) => {
            fields.iter().all(|(k, ta)| {
                sfields.get(k).map_or(false, |tb| types_compatible(ta, tb))
            })
        }
        _ => false,
    }
}

/// Extract explicit type parameter names from a function_def's type_params node.
/// Returns an empty vec if no type_params present.
fn extract_explicit_type_params(func_node: &Node, source: &str) -> Vec<String> {
    let mut cursor = func_node.walk();
    for child in func_node.children(&mut cursor) {
        if child.kind() == "type_params" {
            let mut params = Vec::new();
            let mut inner = child.walk();
            for tp in child.named_children(&mut inner) {
                if tp.kind() == "type_identifier" {
                    params.push(tp.utf8_text(source.as_bytes()).unwrap().to_string());
                }
            }
            return params;
        }
    }
    Vec::new()
}

/// Detect implicit type parameters: scan a function's param and return types
/// for single-letter uppercase type identifiers that aren't known types.
fn detect_implicit_type_params(
    func_node: &Node,
    source: &str,
    symbols: &SymbolTable,
) -> Vec<String> {
    let mut type_params = Vec::new();
    let mut seen = HashSet::new();

    fn scan_for_type_params(
        node: &Node,
        source: &str,
        symbols: &SymbolTable,
        type_params: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        if node.kind() == "type_identifier" {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            // Implicit type params: single uppercase letter not matching a known type
            if name.len() == 1
                && name.chars().next().map_or(false, |c| c.is_ascii_uppercase())
                && !is_builtin_type_name(name)
                && symbols.lookup_type(name).is_none()
                && !seen.contains(name)
            {
                seen.insert(name.to_string());
                type_params.push(name.to_string());
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            scan_for_type_params(&child, source, symbols, type_params, seen);
        }
    }

    if let Some(params) = func_node.child_by_field_name("params") {
        scan_for_type_params(&params, source, symbols, &mut type_params, &mut seen);
    }
    if let Some(ret) = func_node.child_by_field_name("return_type") {
        scan_for_type_params(&ret, source, symbols, &mut type_params, &mut seen);
    }
    type_params
}

fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "Int" | "Float" | "String" | "Bool" | "Unit" | "List" | "Option" | "Result" | "Map"
            | "Set"
    )
}

/// Pre-scan top-level function signatures so forward references resolve.
/// Also registers UserGenericSchema for functions with type parameters.
fn collect_signatures(node: &Node, source: &str, symbols: &mut SymbolTable) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "function_def" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

                    // Detect type parameters: explicit <T, U> or implicit single-letter
                    let mut type_param_names = extract_explicit_type_params(&child, source);
                    if type_param_names.is_empty() {
                        type_param_names = detect_implicit_type_params(&child, source, symbols);
                    }

                    let type_param_set: HashSet<String> =
                        type_param_names.iter().cloned().collect();
                    let ty =
                        parse_function_type_with_params(&child, source, symbols, &type_param_set);
                    symbols.insert(name.clone(), ty.clone());

                    // Register user generic schema if the function has type parameters
                    if !type_param_names.is_empty() {
                        symbols.user_generic_schemas.insert(
                            name,
                            UserGenericSchema {
                                type_param_names,
                                fn_type: ty,
                            },
                        );
                    }
                }
            }
            "module_decl" => collect_signatures(&child, source, symbols),
            _ => {}
        }
    }
}

/// Build a Type::Function from a function_def, treating names in `type_params` as TypeParam.
fn parse_function_type_with_params(
    func_node: &Node,
    source: &str,
    symbols: &SymbolTable,
    type_params: &HashSet<String>,
) -> Type {
    let mut arg_types = Vec::new();
    if let Some(params) = func_node.child_by_field_name("params") {
        let mut cursor = params.walk();
        for param in params.named_children(&mut cursor) {
            if param.kind() == "param"
                && let Some(type_node) = param.child_by_field_name("type")
            {
                arg_types.push(parse_type_ext(&type_node, source, symbols, type_params));
            }
        }
    }
    let ret_type = if let Some(ret_node) = func_node.child_by_field_name("return_type") {
        parse_type_ext(&ret_node, source, symbols, type_params)
    } else {
        Type::Unit
    };
    Type::Function(arg_types, Box::new(ret_type))
}

pub fn check_types(root: &Node, source: &str, file: &str) -> Vec<Diagnostic> {
    check_types_with_loader(root, source, file, None)
}

pub fn check_types_with_loader(
    root: &Node,
    source: &str,
    file: &str,
    loader: Option<&mut ModuleLoader>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Extract prelude from the AST to gate module availability.
    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(msg) => {
            diagnostics.push(Diagnostic {
                code: "PRE_001".to_string(),
                severity: Severity::Error,
                location: Location {
                    file: file.to_string(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
                message: msg,
                context: "Valid prelude variants are: core, script.".to_string(),
                suggestions: vec![],
            });
            return diagnostics;
        }
    };

    let mut symbols = SymbolTable::with_prelude(prelude);

    // Process imports before collecting signatures
    if let Some(loader) = loader {
        process_imports(root, source, file, loader, &mut symbols, &mut diagnostics);
    }

    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);
    diagnostics
}

/// Run the type checker and return both diagnostics and a TypeMap.
/// The TypeMap maps CST node start_byte to resolved Type, enabling
/// the VM compiler to emit specialized opcodes.
pub fn check_types_with_map(root: &Node, source: &str, file: &str) -> (Vec<Diagnostic>, TypeMap) {
    let mut diagnostics = Vec::new();

    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(msg) => {
            diagnostics.push(Diagnostic {
                code: "PRE_001".to_string(),
                severity: Severity::Error,
                location: Location {
                    file: file.to_string(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
                message: msg,
                context: "Valid prelude variants are: core, script.".to_string(),
                suggestions: vec![],
            });
            return (diagnostics, TypeMap::new());
        }
    };

    let mut symbols = SymbolTable::with_prelude(prelude);
    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);
    let type_map = symbols.type_map;
    (diagnostics, type_map)
}

/// Process import declarations: resolve each import, type-check the imported module,
/// and register its exports into the current symbol table.
fn process_imports(
    root: &Node,
    source: &str,
    file: &str,
    loader: &mut ModuleLoader,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let imports = ModuleLoader::parse_imports(root, source);

    for (import, import_node) in imports {
        // Resolve module path to a file
        let file_path = match loader.resolve_path(&import.module_name, &import_node, file) {
            Ok(p) => p,
            Err(diag) => {
                diagnostics.push(diag);
                continue;
            }
        };

        // Load and parse the module
        let module_idx = match loader.load_module(&file_path, &import_node, file) {
            Ok(idx) => idx,
            Err(diag) => {
                diagnostics.push(diag);
                continue;
            }
        };

        // Get the module file path (owned) so we can release the borrow
        let mod_file = match loader.get_module(module_idx) {
            Some((_, _, path)) => path.display().to_string(),
            None => continue,
        };

        // Type-check the imported module with recursive import support.
        // Clone source/path to release the borrow on loader, so we can pass &mut loader.
        {
            let (mod_root, mod_source, _) = loader.get_module(module_idx).unwrap();
            let mod_source_owned = mod_source.to_string();
            let mod_file_owned = mod_file.clone();
            let _ = mod_root;
            // Re-parse to get an owned tree (required because mod_root borrows loader)
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&tree_sitter_baseline::LANGUAGE.into())
                .expect("Failed to load Baseline grammar");
            let tree = parser.parse(&mod_source_owned, None).unwrap();
            let mod_root = tree.root_node();
            let mod_diagnostics = check_types_with_loader(
                &mod_root,
                &mod_source_owned,
                &mod_file_owned,
                Some(loader),
            );
            let has_errors = mod_diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error);
            if has_errors {
                diagnostics.push(Diagnostic {
                    code: "IMP_003".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&import_node),
                    message: format!(
                        "Imported module `{}` has {} type error(s)",
                        import.module_name,
                        mod_diagnostics
                            .iter()
                            .filter(|d| d.severity == Severity::Error)
                            .count()
                    ),
                    context: "Fix the errors in the imported module first.".to_string(),
                    suggestions: vec![],
                });
                continue;
            }
        }

        // Re-borrow to extract exports and build module's symbol table
        let (mod_root, mod_source, _) = loader.get_module(module_idx).unwrap();

        let exports = ModuleLoader::extract_exports(&mod_root, mod_source);

        // Get the short module name (last segment for dotted paths)
        let short_name = import
            .module_name
            .split('.')
            .next_back()
            .unwrap_or(&import.module_name);

        // Build a temporary SymbolTable for the imported module to parse types
        let mod_prelude = prelude::extract_prelude(&mod_root, mod_source).unwrap_or(Prelude::Core);
        let mut mod_symbols = SymbolTable::with_prelude(mod_prelude);
        collect_signatures(&mod_root, mod_source, &mut mod_symbols);

        // Register the module namespace
        symbols.insert(short_name.to_string(), Type::Module(short_name.to_string()));

        // Register each exported function as a module method
        for (func_name, _sig_text) in &exports.functions {
            let qualified = format!("{}.{}", short_name, func_name);
            // Look up the function type from the module's symbol table
            let func_type = mod_symbols
                .lookup(func_name)
                .cloned()
                .unwrap_or(Type::Unknown);
            symbols.module_methods.insert(qualified, func_type.clone());

            // For selective/wildcard imports, also put the symbol directly in scope
            match &import.kind {
                ImportKind::Selective(names) => {
                    if names.contains(func_name) {
                        symbols.insert(func_name.clone(), func_type);
                    }
                }
                ImportKind::Wildcard => {
                    symbols.insert(func_name.clone(), func_type);
                }
                ImportKind::Qualified => {}
            }
        }

        // Register exported types
        for type_name in &exports.types {
            let type_def = mod_symbols.lookup_type(type_name).cloned();
            if let Some(ty) = type_def {
                // For selective/wildcard, register type directly
                match &import.kind {
                    ImportKind::Selective(names) => {
                        if names.contains(type_name) {
                            symbols.insert_type(type_name.clone(), ty);
                        }
                    }
                    ImportKind::Wildcard => {
                        symbols.insert_type(type_name.clone(), ty);
                    }
                    ImportKind::Qualified => {}
                }
            }
        }

        // Validate selective imports: check that requested symbols exist
        if let ImportKind::Selective(names) = &import.kind {
            let exported_names: Vec<&str> = exports
                .functions
                .iter()
                .map(|(n, _)| n.as_str())
                .chain(exports.types.iter().map(|n| n.as_str()))
                .collect();
            for name in names {
                if !exported_names.contains(&name.as_str()) {
                    diagnostics.push(Diagnostic {
                        code: "IMP_004".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&import_node),
                        message: format!(
                            "Symbol `{}` not found in module `{}`",
                            name, import.module_name
                        ),
                        context: format!("Available exports: {}", exported_names.join(", ")),
                        suggestions: vec![],
                    });
                }
            }
        }
    }
}

/// Extract a qualified name like "List.map" from a field_expression node.
fn extract_qualified_name(node: &Node, source: &str) -> Option<String> {
    if node.kind() == "field_expression" {
        let obj = node.named_child(0)?;
        let field = node.named_child(1)?;
        let obj_name = obj.utf8_text(source.as_bytes()).ok()?;
        let field_name = field.utf8_text(source.as_bytes()).ok()?;
        Some(format!("{}.{}", obj_name, field_name))
    } else {
        None
    }
}

fn check_node(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let ty = check_node_inner(node, source, file, symbols, diagnostics);
    // Record the resolved type for this node (used by the VM compiler for specialization).
    symbols.type_map.insert(node.start_byte(), ty.clone());
    ty
}

fn check_node_inner(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let k = node.kind();

    match k {
        "source_file" | "module_decl" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "prelude_decl" | "import_decl" => Type::Unit,
        "spec_block" => {
            // Walk into spec_block to type-check the wrapped definition
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "function_def" || child.kind() == "type_def" || child.kind() == "effect_def" {
                    check_node(&child, source, file, symbols, diagnostics);
                }
                // spec_attribute children are ignored by the type checker
            }
            Type::Unit
        }
        "spec_attribute" | "spec_decl" | "given_clause" | "returns_clause"
        | "requires_clause" | "ensures_clause" | "assume_clause"
        | "pure_attribute" | "total_attribute" => Type::Unit,
        "try_expression" => {
            // expr? — unwraps Option<T> to T or Result<T,E> to T
            let inner = check_node(
                &node.named_child(0).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            match &inner {
                Type::Enum(name, variants) if name == "Option" => variants
                    .iter()
                    .find(|(v, _)| v == "Some")
                    .and_then(|(_, payloads)| payloads.first().cloned())
                    .unwrap_or(Type::Unknown),
                Type::Enum(name, variants) if name == "Result" => variants
                    .iter()
                    .find(|(v, _)| v == "Ok")
                    .and_then(|(_, payloads)| payloads.first().cloned())
                    .unwrap_or(Type::Unknown),
                _ => Type::Unknown,
            }
        }
        "effect_def" => {
            // effect_def: seq(..., 'effect', $.type_identifier, ...)
            // Manual search since field access creates panic
            let mut name = String::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "type_identifier" {
                    name = child.utf8_text(source.as_bytes()).unwrap().to_string();
                    break;
                }
            }
            if !name.is_empty() {
                symbols.insert_type(name.clone(), Type::Module(name));
            }
            Type::Unit
        }
        "type_def" => {
            // type Point = { x: Int, y: Int }
            // type Status = | Active | Inactive | Pending(String)
            let name_node = node
                .child_by_field_name("name")
                .unwrap_or_else(|| node.child(1).unwrap());
            let name = name_node
                .utf8_text(source.as_bytes())
                .unwrap_or("Unknown")
                .to_string();

            // Def node is after '=' — use field if available, else positional
            let def_node_candidate = node
                .child_by_field_name("def")
                .unwrap_or_else(|| node.child(3).unwrap());

            if def_node_candidate.kind() == "variant_list" {
                // Sum type / enum
                let mut variants = Vec::new();
                let mut cursor = def_node_candidate.walk();
                for child in def_node_candidate.children(&mut cursor) {
                    if child.kind() == "variant" {
                        let vname_node = child.child_by_field_name("name").unwrap();
                        let vname = vname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let mut payload_types = Vec::new();

                        // Collect payload types: variant children after name that are type exprs
                        let vcount = child.child_count();
                        for vi in 0..vcount {
                            let vc = child.child(vi).unwrap();
                            if (vc.kind() != "type_identifier" || vc.id() != vname_node.id())
                                && vc.kind() != "|"
                                && vc.kind() != "("
                                && vc.kind() != ")"
                                && vc.kind() != ","
                            {
                                let pt = parse_type(&vc, source, symbols);
                                if pt != Type::Unknown {
                                    payload_types.push(pt);
                                }
                            }
                        }

                        variants.push((vname, payload_types));
                    }
                }

                let enum_type = Type::Enum(name.clone(), variants.clone());
                symbols.insert_type(name.clone(), enum_type.clone());

                // Register constructors as functions or values in the symbol table
                for (vname, payload) in &variants {
                    if payload.is_empty() {
                        // Nullary constructor: Active -> Enum type directly
                        symbols.insert(vname.clone(), enum_type.clone());
                    } else {
                        // Constructor with payload: Some(T) -> function T -> Enum
                        symbols.insert(
                            vname.clone(),
                            Type::Function(payload.clone(), Box::new(enum_type.clone())),
                        );
                    }
                }
            } else {
                let ty = parse_type(&def_node_candidate, source, symbols);

                // Check for refinement clause (e.g., `type Email = String where ...`)
                let has_refinement = {
                    let mut cursor2 = node.walk();
                    node.children(&mut cursor2)
                        .any(|c| c.kind() == "refinement_clause")
                };

                // If it's a record, wrap it in a Struct with the name
                let defined_type = match ty {
                    Type::Record(fields, _) => Type::Struct(name.clone(), fields),
                    _ if has_refinement => Type::Refined(Box::new(ty), name.clone()),
                    _ => ty,
                };

                symbols.insert_type(name, defined_type);
            }
            Type::Unit
        }
        "function_def" => {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            // Detect type parameters (explicit or implicit)
            let mut type_param_names = extract_explicit_type_params(node, source);
            if type_param_names.is_empty() {
                type_param_names = detect_implicit_type_params(node, source, symbols);
            }
            let type_param_set: HashSet<String> = type_param_names.iter().cloned().collect();

            // Build function type with TypeParam for generic params
            let function_type =
                parse_function_type_with_params(node, source, symbols, &type_param_set);

            symbols.insert(name.clone(), function_type.clone());

            // Register user generic schema if type params present
            if !type_param_names.is_empty() {
                symbols.user_generic_schemas.insert(
                    name.clone(),
                    UserGenericSchema {
                        type_param_names,
                        fn_type: function_type.clone(),
                    },
                );
            }

            if let Type::Function(arg_types, ret_type) = &function_type {
                symbols.enter_scope();

                // Bind params from param_list
                if let Some(params) = node.child_by_field_name("params") {
                    let mut cursor = params.walk();
                    let mut i = 0;
                    for param in params.named_children(&mut cursor) {
                        if param.kind() == "param"
                            && let Some(name_node) = param.child_by_field_name("name")
                        {
                            let arg_name =
                                name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                            if i < arg_types.len() {
                                symbols.insert(arg_name, arg_types[i].clone());
                            }
                            i += 1;
                        }
                    }
                }

                let body_node = node.child_by_field_name("body").unwrap();
                let body_type = check_node(&body_node, source, file, symbols, diagnostics);
                if !types_compatible(&body_type, ret_type) && **ret_type != Type::Unit {
                    diagnostics.push(Diagnostic {
                        code: "TYP_006".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body_node),
                        message: format!(
                            "Function `{}` declares return type {}, body returns {}",
                            name, ret_type, body_type
                        ),
                        context: "Function body return type must match signature.".to_string(),
                        suggestions: vec![],
                    });
                }

                // Check where_block inline tests (expressions must be Bool)
                check_where_block(node, source, file, symbols, diagnostics);

                symbols.exit_scope();
            }

            Type::Unit
        }
        "inline_test" => {
            // Top-level inline test: test "name" = expr
            check_inline_test(node, source, file, symbols, diagnostics);
            Type::Unit
        }
        "describe_block" => {
            // BDD describe/context block — type-check all items within
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "it_block" => {
            // BDD it block — body should be Bool (like inline_test)
            if let Some(body) = node.child_by_field_name("body") {
                let body_type = check_node(&body, source, file, symbols, diagnostics);
                if body_type != Type::Bool && body_type != Type::Unknown {
                    diagnostics.push(Diagnostic {
                        code: "TYP_026".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body),
                        message: format!("Test expression must be Bool, found {}", body_type),
                        context: "it block body should evaluate to true or false.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
            Type::Unit
        }
        "before_each_block" | "after_each_block" => {
            // Hook expressions — type-check body
            let count = node.named_child_count();
            if count > 0 {
                if let Some(expr) = node.named_child(count - 1) {
                    check_node(&expr, source, file, symbols, diagnostics);
                }
            }
            Type::Unit
        }
        "expect_expression" => {
            // expect <actual> <matcher> — type-check actual, return Bool
            if let Some(actual) = node.child_by_field_name("actual") {
                check_node(&actual, source, file, symbols, diagnostics);
            }
            if let Some(matcher) = node.child_by_field_name("matcher") {
                check_node(&matcher, source, file, symbols, diagnostics);
            }
            Type::Bool
        }
        "matcher" => {
            // Matcher expressions — type-check any sub-expressions
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Bool
        }
        "map_literal" => {
            // #{ key: value, ... } — infer Map<K, V> from entries
            let mut key_type = Type::Unknown;
            let mut val_type = Type::Unknown;
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "map_entry" {
                    if let Some(k) = child.child_by_field_name("key") {
                        let kt = check_node(&k, source, file, symbols, diagnostics);
                        if key_type == Type::Unknown {
                            key_type = kt;
                        }
                    }
                    if let Some(v) = child.child_by_field_name("value") {
                        let vt = check_node(&v, source, file, symbols, diagnostics);
                        if val_type == Type::Unknown {
                            val_type = vt;
                        }
                    }
                }
            }
            Type::Map(Box::new(key_type), Box::new(val_type))
        }
        "map_entry" => {
            // Handled within map_literal
            Type::Unit
        }
        "set_literal" => {
            // #{ val1, val2, ... } — infer Set<T> from first element
            let mut elem_type = Type::Unknown;
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                let t = check_node(&child, source, file, symbols, diagnostics);
                if elem_type == Type::Unknown {
                    elem_type = t;
                }
            }
            Type::Set(Box::new(elem_type))
        }
        "let_binding" => {
            // Grammar: let pattern [: type_annotation] = expression
            // named_child(0) = pattern, last named_child = expression
            // type_annotation accessed via field name "type"
            let named_count = node.named_child_count();
            if let Some(pattern) = node.named_child(0) {
                let expr_node = node.named_child(named_count - 1).unwrap();
                let expr_type = check_node(&expr_node, source, file, symbols, diagnostics);

                // Check type annotation if present
                if let Some(ann_node) = node.child_by_field_name("type") {
                    // type_annotation is `: Type`, the type_expr is its named child
                    if let Some(type_node) = ann_node.named_child(0) {
                        let declared_type = parse_type(&type_node, source, symbols);
                        if !types_compatible(&expr_type, &declared_type) {
                            diagnostics.push(Diagnostic {
                                code: "TYP_021".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&expr_node),
                                message: format!(
                                    "Binding type mismatch: declared {}, found {}",
                                    declared_type, expr_type
                                ),
                                context: "Expression type must match declared type annotation."
                                    .to_string(),
                                suggestions: vec![],
                            });
                        }
                        bind_pattern(&pattern, declared_type, source, symbols);
                    } else {
                        bind_pattern(&pattern, expr_type, source, symbols);
                    }
                } else {
                    bind_pattern(&pattern, expr_type, source, symbols);
                }
            }
            Type::Unit
        }
        "call_expression" => {
            let func_node = node.named_child(0).unwrap();
            let func_type = check_node(&func_node, source, file, symbols, diagnostics);

            // Named argument validation
            let total_children = node.named_child_count();
            let params_provided = total_children.saturating_sub(1);

            // Check: no positional args after named args, no duplicate names
            let mut seen_named = false;
            let mut named_names: Vec<String> = Vec::new();
            for i in 1..total_children {
                let arg = node.named_child(i).unwrap();
                if arg.kind() == "named_argument" {
                    seen_named = true;
                    if let Some(name_node) = arg.child_by_field_name("name") {
                        let name = name_node.utf8_text(source.as_bytes()).unwrap_or("").to_string();
                        if named_names.contains(&name) {
                            diagnostics.push(Diagnostic {
                                code: "TYP_031".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&arg),
                                message: format!("Duplicate named argument: {}", name),
                                context: "Each named argument may only appear once.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        named_names.push(name);
                    }
                } else if seen_named {
                    diagnostics.push(Diagnostic {
                        code: "TYP_030".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&arg),
                        message: "Positional argument after named argument".to_string(),
                        context: "All positional arguments must come before named arguments.".to_string(),
                        suggestions: vec![],
                    });
                }
            }

            // STY_001: suggest pipe for nested single-arg calls like f(g(x))
            if params_provided == 1 {
                let single_arg = node.named_child(1).unwrap();
                if single_arg.kind() == "call_expression" {
                    diagnostics.push(Diagnostic {
                        code: "STY_001".to_string(),
                        severity: Severity::Warning,
                        location: Location::from_node(file,node),
                        message: "Nested call could use pipe syntax".to_string(),
                        context: "Consider rewriting f(g(x)) as x |> g |> f for readability."
                            .to_string(),
                        suggestions: vec![Suggestion {
                            strategy: "rewrite".to_string(),
                            description: "Use pipe operator |> instead of nested calls".to_string(),
                            confidence: None,
                            patch: None,
                        }],
                    });
                }
            }

            // Try generic schema inference for module methods (List.map) and constructors (Some, Ok)
            let schema_name = extract_qualified_name(&func_node, source).or_else(|| {
                let k = func_node.kind();
                if k == "identifier" || k == "type_identifier" {
                    func_node
                        .utf8_text(source.as_bytes())
                        .ok()
                        .map(|s| s.to_string())
                } else {
                    None
                }
            });
            // Try builtin generic schema first, then user-defined generic schema
            let builtin_schema = schema_name
                .as_ref()
                .and_then(|qn| symbols.lookup_generic_schema(qn));
            let user_schema = if builtin_schema.is_none() {
                schema_name
                    .as_ref()
                    .and_then(|qn| symbols.lookup_user_generic_schema(qn))
            } else {
                None
            };

            if builtin_schema.is_some() || user_schema.is_some() {
                let mut ctx = InferCtx::new();
                let instantiated = if let Some(schema) = builtin_schema {
                    (schema.build)(&mut ctx)
                } else {
                    user_schema.unwrap().instantiate(&mut ctx)
                };
                if let Type::Function(schema_params, schema_ret) = instantiated {
                    // Check arg count
                    if params_provided != schema_params.len() {
                        diagnostics.push(Diagnostic {
                            code: "TYP_007".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,node),
                            message: format!(
                                "Function call expects {} arguments, found {}",
                                schema_params.len(),
                                params_provided
                            ),
                            context: "Argument count mismatch.".to_string(),
                            suggestions: vec![],
                        });
                    }

                    // Unify each arg with the schema param, using lambda inference for HOFs
                    for (i, schema_param) in schema_params
                        .iter()
                        .enumerate()
                        .take(std::cmp::min(params_provided, schema_params.len()))
                    {
                        let raw_arg = node.named_child(i + 1).unwrap();
                        let arg_expr = call_arg_expr(&raw_arg);

                        // For lambda args with a Function-typed schema param, use
                        // check_lambda_with_expected with the resolved param types
                        let arg_type = if arg_expr.kind() == "lambda" {
                            let resolved_param = ctx.apply(schema_param);
                            if let Type::Function(ref expected_params, ref expected_ret) =
                                resolved_param
                            {
                                check_lambda_with_expected(
                                    &arg_expr,
                                    expected_params,
                                    expected_ret,
                                    source,
                                    file,
                                    symbols,
                                    diagnostics,
                                )
                            } else {
                                check_node(&arg_expr, source, file, symbols, diagnostics)
                            }
                        } else {
                            check_node(&arg_expr, source, file, symbols, diagnostics)
                        };

                        let _ = ctx.unify(&arg_type, schema_param);
                    }

                    return ctx.apply(&schema_ret);
                }
            }

            if let Type::Function(arg_types, ret_type) = func_type {
                if params_provided != arg_types.len() {
                    // Allow partial application in pipe contexts
                    let in_pipe = node
                        .parent()
                        .map(|p| p.kind() == "pipe_expression")
                        .unwrap_or(false);
                    if params_provided < arg_types.len() && in_pipe {
                        // Partial application: return function for remaining params
                        let remaining = arg_types[params_provided..].to_vec();
                        return Type::Function(remaining, ret_type);
                    }
                    diagnostics.push(Diagnostic {
                        code: "TYP_007".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,node),
                        message: format!(
                            "Function call expects {} arguments, found {}",
                            arg_types.len(),
                            params_provided
                        ),
                        context: "Argument count mismatch.".to_string(),
                        suggestions: vec![],
                    });
                }

                for (i, expected_arg_type) in arg_types
                    .iter()
                    .enumerate()
                    .take(std::cmp::min(params_provided, arg_types.len()))
                {
                    let raw_arg = node.named_child(i + 1).unwrap();
                    let arg_expr = call_arg_expr(&raw_arg);

                    // Lambda argument inference: if expected type is Function and arg is
                    // a lambda, check the lambda body with expected param types injected.
                    let arg_type = if arg_expr.kind() == "lambda" {
                        if let Type::Function(ref expected_params, ref expected_ret) =
                            *expected_arg_type
                        {
                            check_lambda_with_expected(
                                &arg_expr,
                                expected_params,
                                expected_ret,
                                source,
                                file,
                                symbols,
                                diagnostics,
                            )
                        } else {
                            check_node(&arg_expr, source, file, symbols, diagnostics)
                        }
                    } else {
                        check_node(&arg_expr, source, file, symbols, diagnostics)
                    };

                    if !types_compatible(&arg_type, expected_arg_type) {
                        diagnostics.push(Diagnostic {
                            code: "TYP_008".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&arg_expr),
                            message: format!(
                                "Argument {} mismatch: expected {}, found {}",
                                i + 1,
                                expected_arg_type,
                                arg_type
                            ),
                            context: "Argument type must match function signature.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }
                *ret_type
            } else if func_type == Type::Unknown {
                Type::Unknown
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_009".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&func_node),
                    message: format!("Called expression is not a function, it is {}", func_type),
                    context: "Only functions can be called.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "with_expression" => {
            // with Console.println! { body } → (T, List<String>)
            // Effect field is child 0, body block is child 1
            let body_node = node
                .child_by_field_name("body")
                .unwrap_or_else(|| node.named_child(1).unwrap());
            let body_type = check_node(&body_node, source, file, symbols, diagnostics);
            Type::Tuple(vec![body_type, Type::List(Box::new(Type::String))])
        }
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap();

            if let Some(Type::Struct(name, fields)) = symbols.lookup_type(type_name).cloned() {
                let mut initialized_fields = std::collections::HashSet::new();

                let count = node.named_child_count();
                for i in 1..count {
                    let field_init = node.named_child(i).unwrap();
                    let fname_node = field_init.child(0).unwrap();
                    let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();

                    let fexpr_node = field_init.child(2).unwrap();
                    let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                    if let Some(expected_type) = fields.get(&fname) {
                        if !types_compatible(&ftype, expected_type) {
                            diagnostics.push(Diagnostic {
                                code: "TYP_010".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&fexpr_node),
                                message: format!(
                                    "Field `{}` expects {}, found {}",
                                    fname, expected_type, ftype
                                ),
                                context: "Struct field type mismatch.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        initialized_fields.insert(fname);
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_011".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&fname_node),
                            message: format!("Struct `{}` has no field `{}`", name, fname),
                            context: "Field not defined in struct.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }

                for required_field in fields.keys() {
                    if !initialized_fields.contains(required_field) {
                        diagnostics.push(Diagnostic {
                            code: "TYP_012".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,node),
                            message: format!("Missing field `{}`", required_field),
                            context: "All struct fields must be initialized.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }

                Type::Struct(name, fields)
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_013".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&type_name_node),
                    message: format!("Unknown type `{}`", type_name),
                    context: "Type must be defined before use.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "record_expression" => {
            // { x: 1, y: 2 } — anonymous record literal
            let mut fields = std::collections::HashMap::new();
            let count = node.named_child_count();
            for i in 0..count {
                let field_init = node.named_child(i).unwrap();
                if field_init.kind() == "record_field_init" {
                    let fname_node = field_init.child(0).unwrap();
                    let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                    let fexpr_node = field_init.child(2).unwrap();
                    let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);
                    fields.insert(fname, ftype);
                }
            }
            Type::Record(fields, None)
        }
        "record_update" => {
            // { ..base, field: newValue }
            // named_child(0) = base expression, named_child(1..) = record_field_init overrides
            let base_node = node.named_child(0).unwrap();
            let base_type = check_node(&base_node, source, file, symbols, diagnostics);

            match base_type {
                Type::Struct(ref name, ref fields) => {
                    let count = node.named_child_count();
                    for i in 1..count {
                        let field_init = node.named_child(i).unwrap();
                        let fname_node = field_init.child(0).unwrap();
                        let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let fexpr_node = field_init.child(2).unwrap();
                        let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                        if let Some(expected_type) = fields.get(&fname) {
                            if !types_compatible(&ftype, expected_type) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_028".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,&fexpr_node),
                                    message: format!(
                                        "Field `{}` expects {}, found {}",
                                        fname, expected_type, ftype
                                    ),
                                    context: "Record update field type mismatch.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        } else {
                            diagnostics.push(Diagnostic {
                                code: "TYP_029".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&fname_node),
                                message: format!("Struct `{}` has no field `{}`", name, fname),
                                context: "Field not defined in struct.".to_string(),
                                suggestions: vec![],
                            });
                        }
                    }
                    base_type.clone()
                }
                Type::Record(ref fields, _) => {
                    let count = node.named_child_count();
                    for i in 1..count {
                        let field_init = node.named_child(i).unwrap();
                        let fname_node = field_init.child(0).unwrap();
                        let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let fexpr_node = field_init.child(2).unwrap();
                        let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                        if let Some(expected_type) = fields.get(&fname) {
                            if !types_compatible(&ftype, expected_type) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_028".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,&fexpr_node),
                                    message: format!(
                                        "Field `{}` expects {}, found {}",
                                        fname, expected_type, ftype
                                    ),
                                    context: "Record update field type mismatch.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        } else {
                            diagnostics.push(Diagnostic {
                                code: "TYP_029".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&fname_node),
                                message: format!("Record has no field `{}`", fname),
                                context: "Field not defined in record.".to_string(),
                                suggestions: vec![],
                            });
                        }
                    }
                    base_type.clone()
                }
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_027".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&base_node),
                        message: format!(
                            "Record update requires a record or struct, found {}",
                            base_type
                        ),
                        context: "Only records and structs support update syntax.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "field_expression" => {
            let obj_node = node.named_child(0).unwrap();
            let field_node = node.named_child(1).unwrap();
            let field_name = field_node.utf8_text(source.as_bytes()).unwrap();

            let obj_type = check_node(&obj_node, source, file, symbols, diagnostics);

            match obj_type {
                Type::Struct(name, fields) => {
                    if let Some(ty) = fields.get(field_name) {
                        ty.clone()
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_014".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&field_node),
                            message: format!("Struct `{}` has no field `{}`", name, field_name),
                            context: "Field access error.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Record(ref fields, _) => {
                    if let Some(ty) = fields.get(field_name) {
                        ty.clone()
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_014".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&field_node),
                            message: format!("Record has no field `{}`", field_name),
                            context: "Field access error.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Module(ref module_name) => {
                    let qualified = format!("{}.{}", module_name, field_name);
                    if let Some(ty) = symbols.lookup_module_method(&qualified) {
                        ty.clone()
                    } else {
                        // Unknown method — allow for forward compat / effect-only checking
                        Type::Unknown
                    }
                }
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_015".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&obj_node),
                        message: format!("Type {} excludes field access", obj_type),
                        context: "Only Structs, Records, and Modules support field access."
                            .to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "block" => {
            symbols.enter_scope();
            let mut last_type = Type::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "let_binding" {
                    check_node(&child, source, file, symbols, diagnostics);
                    last_type = Type::Unit;
                } else if child.kind().ends_with("_expression")
                    || child.kind() == "identifier"
                    || child.kind().ends_with("_literal")
                    || child.kind() == "literal"
                {
                    last_type = check_node(&child, source, file, symbols, diagnostics);
                }
            }
            symbols.exit_scope();
            last_type
        }
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            if let Some(ty) = symbols.lookup(name) {
                ty.clone()
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_002".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,node),
                    message: format!("Undefined variable `{}`", name),
                    context: "Variable must be defined before use.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "effect_identifier" | "effectful_identifier" => {
            // TODO: Lookup in a real Effect Registry for return types.
            // For now, assume most effects return Unit or Unknown to allow flow.
            Type::Unknown
        }
        "named_argument" => {
            // Named argument: name: expression — type-check the expression
            let count = node.named_child_count();
            if count > 0 {
                check_node(&node.named_child(count - 1).unwrap(), source, file, symbols, diagnostics)
            } else {
                Type::Unknown
            }
        }
        "hole_expression" => {
            // Typed hole: ?? — report expected type and available bindings
            let expected = infer_expected_type(node, source, symbols);
            let bindings = symbols.visible_bindings();

            let mut msg = format!("Typed hole (??) — expected type: {}", expected);
            if !bindings.is_empty() {
                msg.push_str("\n  Available bindings:");
                for (name, ty) in &bindings {
                    msg.push_str(&format!("\n    {}: {}", name, ty));
                }
            }

            diagnostics.push(Diagnostic {
                code: "HOLE_001".to_string(),
                severity: Severity::Warning,
                location: Location::from_node(file,node),
                message: msg,
                context: "Replace ?? with an expression of the expected type.".to_string(),
                suggestions: vec![],
            });

            expected
        }
        "literal" | "parenthesized_expression" => {
            if let Some(child) = node.named_child(0) {
                check_node(&child, source, file, symbols, diagnostics)
            } else {
                Type::Unit
            }
        }
        "integer_literal" => Type::Int,
        "float_literal" => Type::Float,
        "string_literal" | "multiline_string_literal" => {
            // Recurse into interpolation children to type-check embedded expressions
            let named_count = node.named_child_count();
            for i in 0..named_count {
                let child = node.named_child(i).unwrap();
                if child.kind() == "interpolation"
                    && let Some(expr) = child.named_child(0)
                {
                    check_node(&expr, source, file, symbols, diagnostics);
                }
            }
            Type::String
        }
        "raw_string_literal" | "raw_hash_string_literal" => Type::String,
        "boolean_literal" => Type::Bool,
        "binary_expression" => {
            let left_type = check_node(
                &node.named_child(0).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            let right_type = check_node(
                &node.named_child(1).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            let op_str = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();

            match op_str {
                "+" | "-" | "*" | "/" | "%" => {
                    if left_type == Type::Int && right_type == Type::Int {
                        Type::Int
                    } else if left_type == Type::Float && right_type == Type::Float {
                        Type::Float
                    } else if (left_type == Type::Int && right_type == Type::Float)
                        || (left_type == Type::Float && right_type == Type::Int)
                    {
                        // Int/Float promotion: mixed arithmetic produces Float
                        Type::Float
                    } else {
                        if left_type != Type::Unknown && right_type != Type::Unknown {
                            diagnostics.push(Diagnostic {
                                code: "TYP_001".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,node),
                                message: format!("Binary operator `{}` requires matching Int or Float operands, found {} and {}", op_str, left_type, right_type),
                                context: "Arithmetic operations require matching numeric types.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        Type::Unknown
                    }
                }
                "++" => {
                    // List concatenation: List<T> ++ List<T> -> List<T>
                    match (&left_type, &right_type) {
                        (Type::List(inner_l), Type::List(inner_r)) => {
                            if types_compatible(inner_l, inner_r) {
                                left_type.clone()
                            } else if **inner_l == Type::Unknown {
                                right_type.clone()
                            } else {
                                Type::List(inner_l.clone())
                            }
                        }
                        (Type::List(_), Type::Unknown) | (Type::Unknown, Type::List(_)) | (Type::Unknown, Type::Unknown) => {
                            if matches!(left_type, Type::List(_)) { left_type.clone() } else { right_type.clone() }
                        }
                        _ => {
                            if left_type != Type::Unknown && right_type != Type::Unknown {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_001".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,node),
                                    message: format!("Operator `++` requires List operands, found {} and {}", left_type, right_type),
                                    context: "The ++ operator concatenates two lists of the same type.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                            Type::Unknown
                        }
                    }
                }
                "==" | "!=" | "<" | ">" | "<=" | ">=" => Type::Bool,
                _ => Type::Unknown,
            }
        }
        "if_expression" => {
            let cond = node.named_child(0).unwrap();
            let cond_type = check_node(&cond, source, file, symbols, diagnostics);

            if cond_type != Type::Bool && cond_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_003".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&cond),
                    message: format!("If condition must be Boolean, found {}", cond_type),
                    context: "Control flow conditions must evaluate to true or false.".to_string(),
                    suggestions: vec![],
                });
            }

            let then_branch = node.named_child(1).unwrap();
            let then_type = check_node(&then_branch, source, file, symbols, diagnostics);

            if let Some(else_branch) = node.named_child(2) {
                let else_type = check_node(&else_branch, source, file, symbols, diagnostics);

                if !types_compatible(&then_type, &else_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_004".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&else_branch),
                        message: format!(
                            "If branches match mismatch: then is {}, else is {}",
                            then_type, else_type
                        ),
                        context: "Both branches of an if expression must return the same type."
                            .to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                } else {
                    then_type
                }
            } else {
                Type::Unit
            }
        }
        "tuple_expression" => {
            let count = node.named_child_count();
            if count == 0 {
                Type::Unit
            } else {
                let elems: Vec<Type> = (0..count)
                    .map(|i| {
                        check_node(
                            &node.named_child(i).unwrap(),
                            source,
                            file,
                            symbols,
                            diagnostics,
                        )
                    })
                    .collect();
                if elems.len() == 1 {
                    // (expr) is parenthesized, not a 1-tuple
                    elems.into_iter().next().unwrap()
                } else {
                    Type::Tuple(elems)
                }
            }
        }
        "list_expression" => {
            // [1, 2, 3]
            let mut element_type = Type::Unknown;
            let mut first = true;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "[" || child.kind() == "]" || child.kind() == "," {
                    continue;
                }

                let ty = check_node(&child, source, file, symbols, diagnostics);

                if first {
                    element_type = ty;
                    first = false;
                } else if !types_compatible(&ty, &element_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_016".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&child),
                        message: format!(
                            "List element type mismatch: expected {}, found {}",
                            element_type, ty
                        ),
                        context: "All elements in a list must have the same type.".to_string(),
                        suggestions: vec![],
                    });
                }
            }

            Type::List(Box::new(element_type))
        }
        "lambda" => {
            // |x| body
            let mut arg_types = Vec::new();

            // Last named child is body, others are args
            let count = node.named_child_count();
            symbols.enter_scope();

            for i in 0..count - 1 {
                let arg = node.named_child(i).unwrap();
                if arg.kind() == "identifier" {
                    let name = arg.utf8_text(source.as_bytes()).unwrap().to_string();
                    symbols.insert(name, Type::Unknown);
                    arg_types.push(Type::Unknown);
                }
            }

            let body = node.named_child(count - 1).unwrap();
            let body_type = check_node(&body, source, file, symbols, diagnostics);

            symbols.exit_scope();

            Type::Function(arg_types, Box::new(body_type))
        }
        "match_expression" => {
            let expr_node = node.named_child(0).unwrap();
            let expr_type = check_node(&expr_node, source, file, symbols, diagnostics);

            let mut ret_type = Type::Unknown;
            let mut first = true;

            let count = node.named_child_count();
            for i in 1..count {
                let arm = node.named_child(i).unwrap();
                let pat = arm.child(0).unwrap();
                let body = arm.child(2).unwrap();

                symbols.enter_scope();
                check_pattern(&pat, &expr_type, source, symbols, diagnostics);

                let body_type = check_node(&body, source, file, symbols, diagnostics);
                symbols.exit_scope();

                if first {
                    ret_type = body_type;
                    first = false;
                } else if !types_compatible(&body_type, &ret_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_017".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body),
                        message: format!(
                            "Match arm mismatch: expected {}, found {}",
                            ret_type, body_type
                        ),
                        context: "All match arms must return same type.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
            check_match_exhaustiveness(node, &expr_type, source, file, diagnostics);
            ret_type
        }
        "pipe_expression" => {
            let left_node = node.named_child(0).unwrap();
            let right_node = node.named_child(1).unwrap();

            let left_type = check_node(&left_node, source, file, symbols, diagnostics);
            let right_type = check_node(&right_node, source, file, symbols, diagnostics);

            if let Type::Function(args, ret) = right_type {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic {
                        code: "TYP_018".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&right_node),
                        message: format!("Pipe function expects 1 argument, found {}", args.len()),
                        context: "Pipe operator expects a unary function.".to_string(),
                        suggestions: vec![],
                    });
                } else if !types_compatible(&left_type, &args[0]) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_019".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&left_node),
                        message: format!(
                            "Pipe argument mismatch: expected {}, found {}",
                            args[0], left_type
                        ),
                        context: "Argument type must match function signature.".to_string(),
                        suggestions: vec![],
                    });
                }
                *ret
            } else if right_type == Type::Unknown {
                Type::Unknown
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_020".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&right_node),
                    message: format!("Pipe target is not a function, it is {}", right_type),
                    context: "Left side must be piped into a function.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "type_identifier" => {
            // In expression context, could be a constructor, module, or type reference
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            // First check if it's a constructor in the value namespace
            if let Some(ty) = symbols.lookup(&name) {
                ty.clone()
            } else if let Some(ty) = symbols.lookup_type(&name) {
                ty.clone()
            } else {
                // Assume it might be a module not yet defined or external
                Type::Module(name)
            }
        }
        "for_expression" => {
            // for <pattern> in <collection> do <body>
            let pat_node = node.named_child(0).unwrap();
            let collection_node = node.named_child(1).unwrap();
            let body_node = node.named_child(2).unwrap();

            let collection_type = check_node(&collection_node, source, file, symbols, diagnostics);

            let element_type = match &collection_type {
                Type::List(inner) => *inner.clone(),
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_023".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&collection_node),
                        message: format!("For loop requires a List, found {}", collection_type),
                        context: "The collection in a for..in must be a List.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            };

            symbols.enter_scope();
            bind_pattern(&pat_node, element_type, source, symbols);
            let body_type = check_node(&body_node, source, file, symbols, diagnostics);
            symbols.exit_scope();

            // For loops are for side effects only. If the body returns a non-Unit type,
            // warn: use List.map for transformations (One Way Principle).
            if body_type != Type::Unit && body_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_033".to_string(),
                    severity: Severity::Warning,
                    location: Location::from_node(file,&body_node),
                    message: format!(
                        "For loop body returns {}, but for loops are for side effects only",
                        body_type
                    ),
                    context: "Use List.map() to transform data, for loops to perform effects.".to_string(),
                    suggestions: vec![Suggestion {
                        strategy: "replace".to_string(),
                        description: "Use List.map() for transformations".to_string(),
                        confidence: None,
                        patch: None,
                    }],
                });
            }

            Type::Unit
        }
        "range_expression" => {
            // start..end — both must be Int, produces List<Int>
            let start_node = node.named_child(0).unwrap();
            let end_node = node.named_child(1).unwrap();

            let start_type = check_node(&start_node, source, file, symbols, diagnostics);
            let end_type = check_node(&end_node, source, file, symbols, diagnostics);

            if start_type != Type::Int && start_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_024".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&start_node),
                    message: format!("Range operand must be Int, found {}", start_type),
                    context: "Range expressions require Int operands.".to_string(),
                    suggestions: vec![],
                });
            }
            if end_type != Type::Int && end_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_024".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&end_node),
                    message: format!("Range operand must be Int, found {}", end_type),
                    context: "Range expressions require Int operands.".to_string(),
                    suggestions: vec![],
                });
            }

            Type::List(Box::new(Type::Int))
        }
        "unary_expression" => {
            // !expr or -expr
            let op = node.child(0).unwrap().utf8_text(source.as_bytes()).unwrap();
            let operand_node = node.named_child(0).unwrap();
            let operand_type = check_node(&operand_node, source, file, symbols, diagnostics);

            match op {
                "not" => {
                    if operand_type != Type::Bool && operand_type != Type::Unknown {
                        diagnostics.push(Diagnostic {
                            code: "TYP_025".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&operand_node),
                            message: format!("Logical NOT requires Bool, found {}", operand_type),
                            context: "The not operator can only be applied to Bool values."
                                .to_string(),
                            suggestions: vec![],
                        });
                    }
                    Type::Bool
                }
                "-" => {
                    if operand_type != Type::Int
                        && operand_type != Type::Float
                        && operand_type != Type::Unknown
                    {
                        diagnostics.push(Diagnostic {
                            code: "TYP_025".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&operand_node),
                            message: format!(
                                "Negation requires Int or Float, found {}",
                                operand_type
                            ),
                            context: "The - operator can only be applied to numeric values."
                                .to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    } else {
                        operand_type
                    }
                }
                _ => Type::Unknown,
            }
        }
        _ => {
            Type::Unit // simplified
        }
    }
}

/// Infer the expected type for a typed hole from its parent context.
fn infer_expected_type(node: &Node, source: &str, symbols: &SymbolTable) -> Type {
    if let Some(parent) = node.parent() {
        match parent.kind() {
            "let_binding" => {
                // let x: T = ?? → look for type annotation
                if let Some(ann_node) = parent.child_by_field_name("type") {
                    if let Some(type_node) = ann_node.named_child(0) {
                        return parse_type(&type_node, source, symbols);
                    }
                }
            }
            "function_def" => {
                // fn foo() -> T = ?? → look for return type
                if let Some(ret_node) = parent.child_by_field_name("return_type") {
                    return parse_type(&ret_node, source, symbols);
                }
            }
            _ => {}
        }
    }
    Type::Unknown
}

/// Extract the expression node from a call argument (handles named_argument).
fn call_arg_expr<'a>(arg: &Node<'a>) -> Node<'a> {
    if arg.kind() == "named_argument" {
        // named_argument: name ':' expression
        // The expression is the last named child
        let count = arg.named_child_count();
        arg.named_child(count - 1).unwrap_or(*arg)
    } else {
        *arg
    }
}

fn parse_type(node: &Node, source: &str, symbols: &SymbolTable) -> Type {
    parse_type_ext(node, source, symbols, &HashSet::new())
}

/// Parse a type expression, treating names in `type_params` as TypeParam.
fn parse_type_ext(
    node: &Node,
    source: &str,
    symbols: &SymbolTable,
    type_params: &HashSet<String>,
) -> Type {
    match node.kind() {
        "type_identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            match name {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                "Unit" => Type::Unit,
                _ => {
                    if type_params.contains(name) {
                        Type::TypeParam(name.to_string())
                    } else if let Some(ty) = symbols.lookup_type(name) {
                        ty.clone()
                    } else {
                        Type::Unknown
                    }
                }
            }
        }
        "type_signature" => {
            // A -> B
            // If A is a Tuple, we decompose it into args.
            let left = node.child(0).unwrap();
            let right = node.child(node.child_count() - 1).unwrap();
            let right_type = parse_type_ext(&right, source, symbols, type_params);

            if left.kind() == "tuple_type" {
                // Decompose tuple into args
                let mut args = Vec::new();
                let mut cursor = left.walk();

                // tuple_type children: (, type, ,, type, ...)
                for child in left.children(&mut cursor) {
                    let k = child.kind();
                    if k != "(" && k != "," && k != ")" {
                        args.push(parse_type_ext(&child, source, symbols, type_params));
                    }
                }
                Type::Function(args, Box::new(right_type))
            } else {
                let left_type = parse_type_ext(&left, source, symbols, type_params);
                Type::Function(vec![left_type], Box::new(right_type))
            }
        }
        "function_type" => {
            // (T, U) -> V in other contexts
            let mut args = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "->" {
                    break;
                }
                if child.kind() != "(" && child.kind() != "," && child.kind() != ")" {
                    args.push(parse_type_ext(&child, source, symbols, type_params));
                }
            }
            let ret_node = node.child(node.child_count() - 1).unwrap();
            let ret = parse_type_ext(&ret_node, source, symbols, type_params);
            Type::Function(args, Box::new(ret))
        }
        "tuple_type" => {
            let count = node.named_child_count();
            if count == 0 {
                Type::Unit
            } else if count == 1 {
                // (T) is just parenthesized, not a 1-tuple
                parse_type_ext(&node.named_child(0).unwrap(), source, symbols, type_params)
            } else {
                let elems: Vec<Type> = (0..count)
                    .map(|i| {
                        parse_type_ext(
                            &node.named_child(i).unwrap(),
                            source,
                            symbols,
                            type_params,
                        )
                    })
                    .collect();
                Type::Tuple(elems)
            }
        }
        "record_type" => {
            let mut fields = HashMap::new();
            let mut row_var = None;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "record_field_def" {
                    let name_node = child.child(0).unwrap();
                    let type_node = child.child(2).unwrap();

                    let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                    let ty = parse_type_ext(&type_node, source, symbols, type_params);
                    fields.insert(name, ty);
                } else if child.kind() == "row_variable" {
                    // ..r — row variable for open record types
                    // Use the InferCtx's next_var counter convention
                    // For now, generate a unique var ID from the row variable's byte position
                    row_var = Some(child.start_byte() as u32);
                }
            }
            Type::Record(fields, row_var)
        }

        "generic_type" => {
            // Name<T, ...> — named_child(0) is name, named_child(1..) are type args
            let name_node = node.named_child(0).unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap();

            match name {
                "List" => {
                    let arg = node.named_child(1).unwrap();
                    let inner = parse_type_ext(&arg, source, symbols, type_params);
                    Type::List(Box::new(inner))
                }
                "Map" => {
                    let key = node.named_child(1).unwrap();
                    let val = node.named_child(2).unwrap();
                    Type::Map(
                        Box::new(parse_type_ext(&key, source, symbols, type_params)),
                        Box::new(parse_type_ext(&val, source, symbols, type_params)),
                    )
                }
                "Set" => {
                    let arg = node.named_child(1).unwrap();
                    Type::Set(Box::new(parse_type_ext(&arg, source, symbols, type_params)))
                }
                "Option" => {
                    let arg = node.named_child(1).unwrap();
                    let inner = parse_type_ext(&arg, source, symbols, type_params);
                    Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![inner]),
                            ("None".to_string(), vec![]),
                        ],
                    )
                }
                "Result" => {
                    let ok_node = node.named_child(1).unwrap();
                    let ok_type = parse_type_ext(&ok_node, source, symbols, type_params);
                    let err_type = node
                        .named_child(2)
                        .map(|n| parse_type_ext(&n, source, symbols, type_params))
                        .unwrap_or(Type::Unknown);
                    Type::Enum(
                        "Result".to_string(),
                        vec![
                            ("Ok".to_string(), vec![ok_type]),
                            ("Err".to_string(), vec![err_type]),
                        ],
                    )
                }
                _ => Type::Unknown,
            }
        }
        "option_type" => {
            // T? desugars to Option<T>
            let inner_node = node.named_child(0).unwrap();
            let inner = parse_type_ext(&inner_node, source, symbols, type_params);
            Type::Enum(
                "Option".to_string(),
                vec![
                    ("Some".to_string(), vec![inner]),
                    ("None".to_string(), vec![]),
                ],
            )
        }
        _ => Type::Unknown,
    }
}

/// Check a lambda with expected parameter types inferred from the call site.
/// Returns the lambda's type with concrete param types instead of Unknown.
fn check_lambda_with_expected(
    node: &Node,
    expected_params: &[Type],
    _expected_ret: &Type,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let count = node.named_child_count();
    let arg_count = if count > 0 { count - 1 } else { 0 };

    symbols.enter_scope();

    let mut param_types = Vec::new();
    for i in 0..arg_count {
        let arg = node.named_child(i).unwrap();
        if arg.kind() == "identifier" {
            let name = arg.utf8_text(source.as_bytes()).unwrap().to_string();
            let ty = if i < expected_params.len() {
                expected_params[i].clone()
            } else {
                Type::Unknown
            };
            symbols.insert(name, ty.clone());
            param_types.push(ty);
        }
    }

    let body_type = if count > 0 {
        let body = node.named_child(count - 1).unwrap();
        check_node(&body, source, file, symbols, diagnostics)
    } else {
        Type::Unit
    };

    symbols.exit_scope();

    Type::Function(param_types, Box::new(body_type))
}

fn bind_pattern(node: &Node, ty: Type, source: &str, symbols: &mut SymbolTable) {
    match node.kind() {
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            symbols.insert(name, ty);
        }
        "tuple_pattern" => {
            let count = node.named_child_count();
            if let Type::Tuple(ref elem_types) = ty {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    let elem_ty = if i < elem_types.len() {
                        elem_types[i].clone()
                    } else {
                        Type::Unknown
                    };
                    bind_pattern(&sub_pat, elem_ty, source, symbols);
                }
            } else {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    bind_pattern(&sub_pat, Type::Unknown, source, symbols);
                }
            }
        }
        _ => {}
    }
}

#[allow(clippy::only_used_in_recursion)]
fn check_pattern(
    node: &Node,
    expected_type: &Type,
    source: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match node.kind() {
        "wildcard_pattern" => {}
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            symbols.insert(name, expected_type.clone());
        }
        "type_identifier" => {
            // Nullary constructor pattern: None, Active
            // Verify it's a valid constructor for the expected type
        }
        "constructor_pattern" => {
            // Constructor pattern: Some(v), Pending(msg)
            let ctor_name_node = node.child(0).unwrap();
            let ctor_name = ctor_name_node.utf8_text(source.as_bytes()).unwrap();

            // Extract payload types from the expected enum type if available,
            // otherwise fall back to the constructor's signature in the symbol table.
            let payload_types: Vec<Type> = if let Type::Enum(_, variants) = expected_type {
                variants
                    .iter()
                    .find(|(name, _)| name == ctor_name)
                    .map(|(_, payloads)| payloads.clone())
                    .unwrap_or_default()
            } else {
                symbols
                    .lookup(ctor_name)
                    .and_then(|ty| match ty {
                        Type::Function(params, _) => Some(params.clone()),
                        _ => None,
                    })
                    .unwrap_or_default()
            };

            // Bind each sub-pattern to its payload type
            let mut pattern_idx = 0;
            let child_count = node.child_count();
            for ci in 0..child_count {
                let child = node.child(ci).unwrap();
                if child.kind() != "type_identifier"
                    && child.kind() != "("
                    && child.kind() != ")"
                    && child.kind() != ","
                    && pattern_idx < payload_types.len()
                {
                    check_pattern(
                        &child,
                        &payload_types[pattern_idx],
                        source,
                        symbols,
                        diagnostics,
                    );
                    pattern_idx += 1;
                }
            }
        }
        "literal" => {
            if let Some(child) = node.named_child(0) {
                check_pattern(&child, expected_type, source, symbols, diagnostics);
            }
        }
        "tuple_pattern" => {
            let count = node.named_child_count();
            if let Type::Tuple(elem_types) = expected_type {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    let elem_ty = if i < elem_types.len() {
                        elem_types[i].clone()
                    } else {
                        Type::Unknown
                    };
                    check_pattern(&sub_pat, &elem_ty, source, symbols, diagnostics);
                }
            } else {
                // Expected type is not a tuple — bind sub-patterns as Unknown
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    check_pattern(&sub_pat, &Type::Unknown, source, symbols, diagnostics);
                }
            }
        }
        "integer_literal" => {
            // Basic check
        }
        _ => {}
    }
}

/// Extract what a pattern covers at the top level.
fn extract_pattern_coverage(node: &Node, source: &str) -> Option<PatternCoverage> {
    match node.kind() {
        "wildcard_pattern" => Some(PatternCoverage::CatchAll),
        "identifier" => Some(PatternCoverage::CatchAll),
        "type_identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            Some(PatternCoverage::Variant(name))
        }
        "constructor_pattern" => {
            let ctor = node.child(0).unwrap();
            let name = ctor.utf8_text(source.as_bytes()).unwrap().to_string();
            Some(PatternCoverage::Variant(name))
        }
        "literal" => {
            if let Some(child) = node.named_child(0) {
                extract_pattern_coverage(&child, source)
            } else {
                None
            }
        }
        "boolean_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            Some(PatternCoverage::BoolLiteral(text == "true"))
        }
        _ => None,
    }
}

/// Check whether a match expression covers all variants of the matched type.
fn check_match_exhaustiveness(
    node: &Node,
    expr_type: &Type,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let count = node.named_child_count();
    let mut has_catch_all = false;
    let mut covered_variants: HashSet<String> = HashSet::new();
    let mut covered_true = false;
    let mut covered_false = false;

    for i in 1..count {
        let arm = node.named_child(i).unwrap();
        let pat = arm.child(0).unwrap();
        if let Some(coverage) = extract_pattern_coverage(&pat, source) {
            match coverage {
                PatternCoverage::CatchAll => {
                    has_catch_all = true;
                }
                PatternCoverage::Variant(name) => {
                    covered_variants.insert(name);
                }
                PatternCoverage::BoolLiteral(val) => {
                    if val {
                        covered_true = true;
                    } else {
                        covered_false = true;
                    }
                }
            }
        }
    }

    if has_catch_all {
        return;
    }

    let (missing_desc, type_name) = match expr_type {
        Type::Enum(name, variants) => {
            let all: HashSet<String> = variants.iter().map(|(v, _)| v.clone()).collect();
            let missing: Vec<String> = all.difference(&covered_variants).cloned().collect();
            if missing.is_empty() {
                return;
            }
            (missing, name.clone())
        }
        Type::Bool => {
            if covered_true && covered_false {
                return;
            }
            let mut missing = Vec::new();
            if !covered_true {
                missing.push("true".to_string());
            }
            if !covered_false {
                missing.push("false".to_string());
            }
            (missing, "Bool".to_string())
        }
        Type::Int | Type::String | Type::Float => (
            vec!["_".to_string()],
            match expr_type {
                Type::Int => "Int".to_string(),
                Type::String => "String".to_string(),
                Type::Float => "Float".to_string(),
                _ => unreachable!(),
            },
        ),
        _ => return,
    };

    let missing_str = missing_desc.join(", ");
    let last_arm = node.named_child(count - 1).unwrap();
    let insert_line = last_arm.end_position().row + 2; // 1-indexed, after last arm

    diagnostics.push(Diagnostic {
        code: "TYP_022".to_string(),
        severity: Severity::Error,
        location: Location::from_node(file,node),
        message: format!(
            "Non-exhaustive match on '{}': missing variant(s) {}",
            type_name, missing_str
        ),
        context: "All variants must be handled, or add a wildcard '_' arm.".to_string(),
        suggestions: vec![
            Suggestion {
                strategy: "add_missing_arms".to_string(),
                description: format!("Add arms for {}", missing_str),
                confidence: None,
                patch: Some(Patch {
                    start_line: insert_line,
                    original_text: None,
                    replacement_text: Some(
                        missing_desc
                            .iter()
                            .map(|v| format!("    {} -> todo", v))
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    operation: Some("insert".to_string()),
                }),
            },
            Suggestion {
                strategy: "add_wildcard".to_string(),
                description: "Add a wildcard '_' arm".to_string(),
                confidence: None,
                patch: Some(Patch {
                    start_line: insert_line,
                    original_text: None,
                    replacement_text: Some("    _ -> todo".to_string()),
                    operation: Some("insert".to_string()),
                }),
            },
        ],
    });
}
