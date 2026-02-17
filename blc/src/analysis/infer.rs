use super::types::Type;
use std::collections::HashMap;

/// A parameterized type template for generic builtins.
/// `type_params` is the number of generic params (e.g., 1 for List<T>, 2 for Result<T,E>).
/// `build` instantiates the schema with fresh type variables.
pub struct GenericSchema {
    pub type_params: u32,
    pub build: fn(&mut InferCtx) -> Type,
}

/// A generic schema for user-defined functions with named type parameters.
/// Stores the function type template with `Type::TypeParam` placeholders.
pub struct UserGenericSchema {
    pub type_param_names: Vec<String>,
    pub fn_type: Type,
    /// Trait bounds per type parameter: param_name -> [trait_name].
    pub bounds: HashMap<String, Vec<String>>,
}

impl UserGenericSchema {
    /// Instantiate the schema with fresh type variables for each named type parameter.
    pub fn instantiate(&self, ctx: &mut InferCtx) -> Type {
        let mut mapping = HashMap::new();
        for name in &self.type_param_names {
            mapping.insert(name.clone(), ctx.fresh_var());
        }
        substitute_type_params(&self.fn_type, &mapping)
    }
}

/// Replace all `Type::TypeParam(name)` in a type with their mapped types.
pub fn substitute_type_params(ty: &Type, mapping: &HashMap<String, Type>) -> Type {
    match ty {
        Type::TypeParam(name) => mapping.get(name).cloned().unwrap_or(Type::Unknown),
        Type::Function(params, ret) => {
            let params = params
                .iter()
                .map(|p| substitute_type_params(p, mapping))
                .collect();
            let ret = Box::new(substitute_type_params(ret, mapping));
            Type::Function(params, ret)
        }
        Type::List(inner) => Type::List(Box::new(substitute_type_params(inner, mapping))),
        Type::Tuple(elems) => {
            Type::Tuple(elems.iter().map(|e| substitute_type_params(e, mapping)).collect())
        }
        Type::Enum(name, variants) => {
            let variants = variants
                .iter()
                .map(|(vname, payloads)| {
                    let payloads = payloads
                        .iter()
                        .map(|p| substitute_type_params(p, mapping))
                        .collect();
                    (vname.clone(), payloads)
                })
                .collect();
            Type::Enum(name.clone(), variants)
        }
        Type::Record(fields, row) => {
            let fields = fields
                .iter()
                .map(|(k, v)| (k.clone(), substitute_type_params(v, mapping)))
                .collect();
            Type::Record(fields, *row)
        }
        Type::Map(k, v) => Type::Map(
            Box::new(substitute_type_params(k, mapping)),
            Box::new(substitute_type_params(v, mapping)),
        ),
        Type::Set(inner) => Type::Set(Box::new(substitute_type_params(inner, mapping))),
        Type::Weak(inner) => Type::Weak(Box::new(substitute_type_params(inner, mapping))),
        Type::Scoped(inner) => Type::Scoped(Box::new(substitute_type_params(inner, mapping))),
        // Concrete types pass through unchanged
        _ => ty.clone(),
    }
}

/// Inference context managing type variables, unification, and substitution.
pub struct InferCtx {
    next_var: u32,
    substitution: HashMap<u32, Type>,
}

impl Default for InferCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            substitution: HashMap::new(),
        }
    }

    /// Create a fresh type variable.
    pub fn fresh_var(&mut self) -> Type {
        let id = self.next_var;
        self.next_var += 1;
        Type::Var(id)
    }

    /// Unify two types, updating the substitution on success.
    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), String> {
        let a = self.resolve(a);
        let b = self.resolve(b);

        match (&a, &b) {
            _ if a == b => Ok(()),

            // Unknown acts as a wildcard — always unifies
            (Type::Unknown, _) | (_, Type::Unknown) => Ok(()),

            // Var binds to concrete type
            (Type::Var(id), _) => {
                if self.occurs_check(*id, &b) {
                    return Err(format!("Occurs check failed: ?{} in {}", id, b));
                }
                self.substitution.insert(*id, b);
                Ok(())
            }
            (_, Type::Var(id)) => {
                if self.occurs_check(*id, &a) {
                    return Err(format!("Occurs check failed: ?{} in {}", id, a));
                }
                self.substitution.insert(*id, a);
                Ok(())
            }

            // Structural unification
            (Type::Function(params_a, ret_a), Type::Function(params_b, ret_b)) => {
                if params_a.len() != params_b.len() {
                    return Err(format!(
                        "Function arity mismatch: {} vs {}",
                        params_a.len(),
                        params_b.len()
                    ));
                }
                for (pa, pb) in params_a.iter().zip(params_b.iter()) {
                    self.unify(pa, pb)?;
                }
                self.unify(ret_a, ret_b)
            }
            (Type::List(inner_a), Type::List(inner_b)) => self.unify(inner_a, inner_b),
            (Type::Weak(inner_a), Type::Weak(inner_b)) => self.unify(inner_a, inner_b),
            (Type::Tuple(elems_a), Type::Tuple(elems_b)) => {
                if elems_a.len() != elems_b.len() {
                    return Err(format!(
                        "Tuple length mismatch: {} vs {}",
                        elems_a.len(),
                        elems_b.len()
                    ));
                }
                for (ea, eb) in elems_a.iter().zip(elems_b.iter()) {
                    self.unify(ea, eb)?;
                }
                Ok(())
            }
            (Type::Scoped(a), Type::Scoped(b)) => self.unify(a, b),
            (Type::Enum(na, va), Type::Enum(nb, vb)) if na == nb => {
                // Same-named enums: unify variant payloads pairwise
                for ((_name_a, payloads_a), (_name_b, payloads_b)) in va.iter().zip(vb.iter()) {
                    for (pa, pb) in payloads_a.iter().zip(payloads_b.iter()) {
                        self.unify(pa, pb)?;
                    }
                }
                Ok(())
            }

            // TypeParam — treat like a named variable for unification
            (Type::TypeParam(na), Type::TypeParam(nb)) if na == nb => Ok(()),

            // Concrete mismatch — fail silently (let the type checker report errors)
            _ => Err(format!("Cannot unify {} with {}", a, b)),
        }
    }

    /// Apply the current substitution to a type, recursively resolving variables.
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(resolved) = self.substitution.get(id) {
                    self.apply(resolved)
                } else {
                    // Unresolved var — fall back to Unknown for backward compat
                    Type::Unknown
                }
            }
            Type::Function(params, ret) => {
                let params = params.iter().map(|p| self.apply(p)).collect();
                let ret = Box::new(self.apply(ret));
                Type::Function(params, ret)
            }
            Type::List(inner) => Type::List(Box::new(self.apply(inner))),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| self.apply(e)).collect()),
            Type::Enum(name, variants) => {
                let variants = variants
                    .iter()
                    .map(|(vname, payloads)| {
                        let payloads = payloads.iter().map(|p| self.apply(p)).collect();
                        (vname.clone(), payloads)
                    })
                    .collect();
                Type::Enum(name.clone(), variants)
            }
            Type::Scoped(inner) => Type::Scoped(Box::new(self.apply(inner))),
            Type::Weak(inner) => Type::Weak(Box::new(self.apply(inner))),
            // TypeParam passes through (only appears in templates before instantiation)
            Type::TypeParam(_) => ty.clone(),
            // All other types are concrete — return as-is
            _ => ty.clone(),
        }
    }

    /// Check if a variable occurs in a type (prevents infinite types).
    fn occurs_check(&self, var: u32, ty: &Type) -> bool {
        match ty {
            Type::Var(id) => {
                if *id == var {
                    return true;
                }
                if let Some(resolved) = self.substitution.get(id) {
                    self.occurs_check(var, resolved)
                } else {
                    false
                }
            }
            Type::Function(params, ret) => {
                params.iter().any(|p| self.occurs_check(var, p)) || self.occurs_check(var, ret)
            }
            Type::List(inner) => self.occurs_check(var, inner),
            Type::Tuple(elems) => elems.iter().any(|e| self.occurs_check(var, e)),
            Type::Enum(_, variants) => variants
                .iter()
                .any(|(_, payloads)| payloads.iter().any(|p| self.occurs_check(var, p))),
            Type::Scoped(inner) => self.occurs_check(var, inner),
            Type::Weak(inner) => self.occurs_check(var, inner),
            _ => false,
        }
    }

    /// Resolve a type by following substitution chains for variables.
    fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(resolved) = self.substitution.get(id) {
                    self.resolve(resolved)
                } else {
                    ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }
}

/// Build the generic schema registry for all generic builtins.
pub fn builtin_generic_schemas() -> HashMap<String, GenericSchema> {
    let mut schemas = HashMap::new();

    // List.map : (List<A>, (A) -> B) -> List<B>
    schemas.insert(
        "List.map".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::List(Box::new(a.clone())),
                        Type::Function(vec![a], Box::new(b.clone())),
                    ],
                    Box::new(Type::List(Box::new(b))),
                )
            },
        },
    );

    // List.filter : (List<A>, (A) -> Bool) -> List<A>
    schemas.insert(
        "List.filter".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::List(Box::new(a.clone())),
                        Type::Function(vec![a.clone()], Box::new(Type::Bool)),
                    ],
                    Box::new(Type::List(Box::new(a))),
                )
            },
        },
    );

    // List.fold : (List<A>, B, (B, A) -> B) -> B
    schemas.insert(
        "List.fold".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::List(Box::new(a.clone())),
                        b.clone(),
                        Type::Function(vec![b.clone(), a], Box::new(b.clone())),
                    ],
                    Box::new(b),
                )
            },
        },
    );

    // List.find : (List<A>, (A) -> Bool) -> Option<A>
    schemas.insert(
        "List.find".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::List(Box::new(a.clone())),
                        Type::Function(vec![a.clone()], Box::new(Type::Bool)),
                    ],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![("Some".to_string(), vec![a]), ("None".to_string(), vec![])],
                    )),
                )
            },
        },
    );

    // List.head : (List<A>) -> Option<A>
    schemas.insert(
        "List.head".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(a.clone()))],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![a]),
                            ("None".to_string(), vec![]),
                        ],
                    )),
                )
            },
        },
    );

    // List.tail : (List<A>) -> List<A>
    schemas.insert(
        "List.tail".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(a.clone()))],
                    Box::new(Type::List(Box::new(a))),
                )
            },
        },
    );

    // List.reverse : (List<A>) -> List<A>
    schemas.insert(
        "List.reverse".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(a.clone()))],
                    Box::new(Type::List(Box::new(a))),
                )
            },
        },
    );

    // List.sort : (List<A>) -> List<A>
    schemas.insert(
        "List.sort".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(a.clone()))],
                    Box::new(Type::List(Box::new(a))),
                )
            },
        },
    );

    // List.concat : (List<A>, List<A>) -> List<A>
    schemas.insert(
        "List.concat".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::List(Box::new(a.clone())),
                        Type::List(Box::new(a.clone())),
                    ],
                    Box::new(Type::List(Box::new(a))),
                )
            },
        },
    );

    // List.get : (List<A>, Int) -> Option<A>
    schemas.insert(
        "List.get".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(a.clone())), Type::Int],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![a]),
                            ("None".to_string(), vec![]),
                        ],
                    )),
                )
            },
        },
    );

    // Option.unwrap : (Option<A>) -> A
    schemas.insert(
        "Option.unwrap".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![a.clone()]),
                            ("None".to_string(), vec![]),
                        ],
                    )],
                    Box::new(a),
                )
            },
        },
    );

    // Option.unwrap_or : (Option<A>, A) -> A
    schemas.insert(
        "Option.unwrap_or".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Enum(
                            "Option".to_string(),
                            vec![
                                ("Some".to_string(), vec![a.clone()]),
                                ("None".to_string(), vec![]),
                            ],
                        ),
                        a.clone(),
                    ],
                    Box::new(a),
                )
            },
        },
    );

    // Option.map : (Option<A>, (A) -> B) -> Option<B>
    schemas.insert(
        "Option.map".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Enum(
                            "Option".to_string(),
                            vec![
                                ("Some".to_string(), vec![a.clone()]),
                                ("None".to_string(), vec![]),
                            ],
                        ),
                        Type::Function(vec![a], Box::new(b.clone())),
                    ],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![("Some".to_string(), vec![b]), ("None".to_string(), vec![])],
                    )),
                )
            },
        },
    );

    // Option.flat_map : (Option<A>, (A) -> Option<B>) -> Option<B>
    schemas.insert(
        "Option.flat_map".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                let option_b = Type::Enum(
                    "Option".to_string(),
                    vec![
                        ("Some".to_string(), vec![b.clone()]),
                        ("None".to_string(), vec![]),
                    ],
                );
                Type::Function(
                    vec![
                        Type::Enum(
                            "Option".to_string(),
                            vec![
                                ("Some".to_string(), vec![a.clone()]),
                                ("None".to_string(), vec![]),
                            ],
                        ),
                        Type::Function(vec![a], Box::new(option_b.clone())),
                    ],
                    Box::new(option_b),
                )
            },
        },
    );

    // Result.unwrap : (Result<A,E>) -> A
    schemas.insert(
        "Result.unwrap".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                Type::Function(
                    vec![Type::Enum(
                        "Result".to_string(),
                        vec![
                            ("Ok".to_string(), vec![a.clone()]),
                            ("Err".to_string(), vec![e]),
                        ],
                    )],
                    Box::new(a),
                )
            },
        },
    );

    // Result.unwrap_or : (Result<A,E>, A) -> A
    schemas.insert(
        "Result.unwrap_or".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Enum(
                            "Result".to_string(),
                            vec![
                                ("Ok".to_string(), vec![a.clone()]),
                                ("Err".to_string(), vec![e]),
                            ],
                        ),
                        a.clone(),
                    ],
                    Box::new(a),
                )
            },
        },
    );

    // Result.map : (Result<A,E>, (A) -> B) -> Result<B,E>
    schemas.insert(
        "Result.map".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                let e = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Enum(
                            "Result".to_string(),
                            vec![
                                ("Ok".to_string(), vec![a.clone()]),
                                ("Err".to_string(), vec![e.clone()]),
                            ],
                        ),
                        Type::Function(vec![a], Box::new(b.clone())),
                    ],
                    Box::new(Type::Enum(
                        "Result".to_string(),
                        vec![("Ok".to_string(), vec![b]), ("Err".to_string(), vec![e])],
                    )),
                )
            },
        },
    );

    // Result.and_then : (Result<A,E>, (A) -> Result<B,E>) -> Result<B,E>
    schemas.insert(
        "Result.and_then".into(),
        GenericSchema {
            type_params: 3,
            build: |ctx| {
                let a = ctx.fresh_var();
                let b = ctx.fresh_var();
                let e = ctx.fresh_var();
                let result_be = Type::Enum(
                    "Result".to_string(),
                    vec![
                        ("Ok".to_string(), vec![b.clone()]),
                        ("Err".to_string(), vec![e.clone()]),
                    ],
                );
                Type::Function(
                    vec![
                        Type::Enum(
                            "Result".to_string(),
                            vec![
                                ("Ok".to_string(), vec![a.clone()]),
                                ("Err".to_string(), vec![e]),
                            ],
                        ),
                        Type::Function(vec![a], Box::new(result_be.clone())),
                    ],
                    Box::new(result_be),
                )
            },
        },
    );

    // Result.map_err : (Result<A,E>, (E) -> F) -> Result<A,F>
    schemas.insert(
        "Result.map_err".into(),
        GenericSchema {
            type_params: 3,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                let f = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Enum(
                            "Result".to_string(),
                            vec![
                                ("Ok".to_string(), vec![a.clone()]),
                                ("Err".to_string(), vec![e.clone()]),
                            ],
                        ),
                        Type::Function(vec![e], Box::new(f.clone())),
                    ],
                    Box::new(Type::Enum(
                        "Result".to_string(),
                        vec![
                            ("Ok".to_string(), vec![a]),
                            ("Err".to_string(), vec![f]),
                        ],
                    )),
                )
            },
        },
    );

    // Result.context : (Result<A,E>, String) -> Result<A, { error: E, context: String }>
    schemas.insert(
        "Result.context".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                let mut err_fields = HashMap::new();
                err_fields.insert("error".to_string(), e.clone());
                err_fields.insert("context".to_string(), Type::String);
                Type::Function(
                    vec![
                        Type::Enum(
                            "Result".to_string(),
                            vec![
                                ("Ok".to_string(), vec![a.clone()]),
                                ("Err".to_string(), vec![e]),
                            ],
                        ),
                        Type::String,
                    ],
                    Box::new(Type::Enum(
                        "Result".to_string(),
                        vec![
                            ("Ok".to_string(), vec![a]),
                            (
                                "Err".to_string(),
                                vec![Type::Record(err_fields, None)],
                            ),
                        ],
                    )),
                )
            },
        },
    );

    // -- Constructors: produce concrete generic types via inference --

    // Some : (A) -> Option<A>
    schemas.insert(
        "Some".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![a.clone()],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![("Some".to_string(), vec![a]), ("None".to_string(), vec![])],
                    )),
                )
            },
        },
    );

    // Ok : (A) -> Result<A, E>
    schemas.insert(
        "Ok".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                Type::Function(
                    vec![a.clone()],
                    Box::new(Type::Enum(
                        "Result".to_string(),
                        vec![("Ok".to_string(), vec![a]), ("Err".to_string(), vec![e])],
                    )),
                )
            },
        },
    );

    // Err : (E) -> Result<A, E>
    schemas.insert(
        "Err".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let a = ctx.fresh_var();
                let e = ctx.fresh_var();
                Type::Function(
                    vec![e.clone()],
                    Box::new(Type::Enum(
                        "Result".to_string(),
                        vec![("Ok".to_string(), vec![a]), ("Err".to_string(), vec![e])],
                    )),
                )
            },
        },
    );

    // --- Map operations ---

    // Map.empty : () -> Map<K, V>
    schemas.insert(
        "Map.empty".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(vec![], Box::new(Type::Map(Box::new(k), Box::new(v))))
            },
        },
    );

    // Map.insert : (Map<K, V>, K, V) -> Map<K, V>
    schemas.insert(
        "Map.insert".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::Map(Box::new(k.clone()), Box::new(v.clone())),
                        k.clone(),
                        v.clone(),
                    ],
                    Box::new(Type::Map(Box::new(k), Box::new(v))),
                )
            },
        },
    );

    // Map.get : (Map<K, V>, K) -> Option<V>
    schemas.insert(
        "Map.get".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k.clone()), Box::new(v.clone())), k],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![v]),
                            ("None".to_string(), vec![]),
                        ],
                    )),
                )
            },
        },
    );

    // Map.remove : (Map<K, V>, K) -> Map<K, V>
    schemas.insert(
        "Map.remove".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k.clone()), Box::new(v.clone())), k.clone()],
                    Box::new(Type::Map(Box::new(k), Box::new(v))),
                )
            },
        },
    );

    // Map.contains : (Map<K, V>, K) -> Bool
    schemas.insert(
        "Map.contains".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k.clone()), Box::new(v)), k],
                    Box::new(Type::Bool),
                )
            },
        },
    );

    // Map.keys : (Map<K, V>) -> List<K>
    schemas.insert(
        "Map.keys".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k.clone()), Box::new(v))],
                    Box::new(Type::List(Box::new(k))),
                )
            },
        },
    );

    // Map.values : (Map<K, V>) -> List<V>
    schemas.insert(
        "Map.values".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k), Box::new(v.clone()))],
                    Box::new(Type::List(Box::new(v))),
                )
            },
        },
    );

    // Map.len : (Map<K, V>) -> Int
    schemas.insert(
        "Map.len".into(),
        GenericSchema {
            type_params: 2,
            build: |ctx| {
                let k = ctx.fresh_var();
                let v = ctx.fresh_var();
                Type::Function(
                    vec![Type::Map(Box::new(k), Box::new(v))],
                    Box::new(Type::Int),
                )
            },
        },
    );

    // --- Set operations ---

    // Set.empty : () -> Set<T>
    schemas.insert(
        "Set.empty".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(vec![], Box::new(Type::Set(Box::new(t))))
            },
        },
    );

    // Set.insert : (Set<T>, T) -> Set<T>
    schemas.insert(
        "Set.insert".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t.clone())), t.clone()],
                    Box::new(Type::Set(Box::new(t))),
                )
            },
        },
    );

    // Set.remove : (Set<T>, T) -> Set<T>
    schemas.insert(
        "Set.remove".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t.clone())), t.clone()],
                    Box::new(Type::Set(Box::new(t))),
                )
            },
        },
    );

    // Set.contains : (Set<T>, T) -> Bool
    schemas.insert(
        "Set.contains".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t.clone())), t],
                    Box::new(Type::Bool),
                )
            },
        },
    );

    // Set.union : (Set<T>, Set<T>) -> Set<T>
    schemas.insert(
        "Set.union".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t.clone())), Type::Set(Box::new(t.clone()))],
                    Box::new(Type::Set(Box::new(t))),
                )
            },
        },
    );

    // Set.intersection : (Set<T>, Set<T>) -> Set<T>
    schemas.insert(
        "Set.intersection".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t.clone())), Type::Set(Box::new(t.clone()))],
                    Box::new(Type::Set(Box::new(t))),
                )
            },
        },
    );

    // Set.len : (Set<T>) -> Int
    schemas.insert(
        "Set.len".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::Set(Box::new(t))],
                    Box::new(Type::Int),
                )
            },
        },
    );

    // Set.from_list : (List<T>) -> Set<T>
    schemas.insert(
        "Set.from_list".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![Type::List(Box::new(t.clone()))],
                    Box::new(Type::Set(Box::new(t))),
                )
            },
        },
    );

    // --- Weak references ---

    // Weak.downgrade : (A) -> Weak<A>
    schemas.insert(
        "Weak.downgrade".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![a.clone()],
                    Box::new(Type::Weak(Box::new(a))),
                )
            },
        },
    );

    // Weak.upgrade : (Weak<A>) -> Option<A>
    schemas.insert(
        "Weak.upgrade".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let a = ctx.fresh_var();
                Type::Function(
                    vec![Type::Weak(Box::new(a.clone()))],
                    Box::new(Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![a]),
                            ("None".to_string(), vec![]),
                        ],
                    )),
                )
            },
        },
    );

    // --- Database query_map! HOFs ---

    // Sqlite.query_map! : (String, List<String>, (Row) -> T) -> List<T>
    let query_map_schema = GenericSchema {
        type_params: 1,
        build: |ctx| {
            let t = ctx.fresh_var();
            Type::Function(
                vec![
                    Type::String,
                    Type::List(Box::new(Type::String)),
                    Type::Function(vec![Type::Row], Box::new(t.clone())),
                ],
                Box::new(Type::List(Box::new(t))),
            )
        },
    };
    schemas.insert("Sqlite.query_map!".into(), query_map_schema);

    schemas.insert(
        "Sqlite.query_map".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::List(Box::new(Type::String)),
                        Type::Function(vec![Type::Row], Box::new(t.clone())),
                    ],
                    Box::new(Type::List(Box::new(t))),
                )
            },
        },
    );

    schemas.insert(
        "Postgres.query_map!".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::List(Box::new(Type::String)),
                        Type::Function(vec![Type::Row], Box::new(t.clone())),
                    ],
                    Box::new(Type::List(Box::new(t))),
                )
            },
        },
    );

    schemas.insert(
        "Postgres.query_map".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::List(Box::new(Type::String)),
                        Type::Function(vec![Type::Row], Box::new(t.clone())),
                    ],
                    Box::new(Type::List(Box::new(t))),
                )
            },
        },
    );

    schemas.insert(
        "Mysql.query_map!".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::List(Box::new(Type::String)),
                        Type::Function(vec![Type::Row], Box::new(t.clone())),
                    ],
                    Box::new(Type::List(Box::new(t))),
                )
            },
        },
    );

    schemas.insert(
        "Mysql.query_map".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::List(Box::new(Type::String)),
                        Type::Function(vec![Type::Row], Box::new(t.clone())),
                    ],
                    Box::new(Type::List(Box::new(t))),
                )
            },
        },
    );

    // --- Scoped resource HOFs ---

    // Fs.with_file! : (String, (Scoped<String>) -> T) -> T
    schemas.insert(
        "Fs.with_file!".into(),
        GenericSchema {
            type_params: 1,
            build: |ctx| {
                let t = ctx.fresh_var();
                Type::Function(
                    vec![
                        Type::String,
                        Type::Function(
                            vec![Type::Scoped(Box::new(Type::String))],
                            Box::new(t.clone()),
                        ),
                    ],
                    Box::new(t),
                )
            },
        },
    );

    schemas
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_var_with_int() {
        let mut ctx = InferCtx::new();
        let var = ctx.fresh_var();
        assert!(ctx.unify(&var, &Type::Int).is_ok());
        assert_eq!(ctx.apply(&var), Type::Int);
    }

    #[test]
    fn unify_function_types() {
        let mut ctx = InferCtx::new();
        let a = ctx.fresh_var(); // ?0
        let b = ctx.fresh_var(); // ?1
        let generic_fn = Type::Function(vec![a.clone()], Box::new(b.clone()));
        let concrete_fn = Type::Function(vec![Type::Int], Box::new(Type::Bool));

        assert!(ctx.unify(&generic_fn, &concrete_fn).is_ok());
        assert_eq!(ctx.apply(&a), Type::Int);
        assert_eq!(ctx.apply(&b), Type::Bool);
    }

    #[test]
    fn unify_occurs_check() {
        let mut ctx = InferCtx::new();
        let var = ctx.fresh_var(); // ?0
        let recursive = Type::List(Box::new(var.clone()));
        assert!(ctx.unify(&var, &recursive).is_err());
    }

    #[test]
    fn apply_substitution() {
        let mut ctx = InferCtx::new();
        let a = ctx.fresh_var(); // ?0
        let b = ctx.fresh_var(); // ?1
        ctx.unify(&a, &Type::Int).unwrap();
        ctx.unify(&b, &Type::Bool).unwrap();

        let fn_type = Type::Function(vec![a, b], Box::new(Type::String));
        let applied = ctx.apply(&fn_type);
        assert_eq!(
            applied,
            Type::Function(vec![Type::Int, Type::Bool], Box::new(Type::String))
        );
    }

    #[test]
    fn unresolved_var_becomes_unknown() {
        let mut ctx = InferCtx::new();
        let var = ctx.fresh_var();
        assert_eq!(ctx.apply(&var), Type::Unknown);
    }

    #[test]
    fn unify_list_types() {
        let mut ctx = InferCtx::new();
        let a = ctx.fresh_var();
        let generic_list = Type::List(Box::new(a.clone()));
        let concrete_list = Type::List(Box::new(Type::Int));

        assert!(ctx.unify(&generic_list, &concrete_list).is_ok());
        assert_eq!(ctx.apply(&a), Type::Int);
    }

    #[test]
    fn unify_unknown_with_anything() {
        let mut ctx = InferCtx::new();
        assert!(ctx.unify(&Type::Unknown, &Type::Int).is_ok());
        assert!(ctx.unify(&Type::String, &Type::Unknown).is_ok());
    }
}
