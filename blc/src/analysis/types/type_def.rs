use std::collections::HashMap;

/// Maps CST node `start_byte()` to the resolved Type for that expression.
/// Used by the VM compiler for type-directed opcode specialization.
pub type TypeMap = HashMap<usize, Type>;

/// Classification of what a single match arm pattern covers.
pub(super) enum PatternCoverage {
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
    Struct(String, HashMap<String, Type>),      // Name, Fields
    Record(HashMap<String, Type>, Option<u32>), // Anonymous record, optional row var
    Tuple(Vec<Type>),                           // (T, U, ...)
    Enum(String, Vec<(String, Vec<Type>)>),     // Name, [(VariantName, PayloadTypes)]
    Map(Box<Type>, Box<Type>),                  // Map<K, V>
    Set(Box<Type>),                             // Set<T>
    Weak(Box<Type>),                            // Weak<T> — weak reference
    Module(String),                             // Module/Effect namespace
    Var(u32),                                   // Inference type variable
    TypeParam(String),                          // Named type parameter (e.g., T, A, K)
    Refined(Box<Type>, String),                 // Refined type: base type + alias name
    Scoped(Box<Type>),                          // Scoped resource handle (cannot escape closure)
    Row,                                        // Typed database row
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
            Type::Weak(inner) => write!(f, "Weak<{}>", inner),
            Type::Module(name) => write!(f, "Module({})", name),
            Type::Var(id) => write!(f, "?{}", id),
            Type::TypeParam(name) => write!(f, "{}", name),
            Type::Refined(_, name) => write!(f, "{}", name),
            Type::Scoped(inner) => write!(f, "Scoped<{}>", inner),
            Type::Row => write!(f, "Row"),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}
