use std::collections::HashMap;

use crate::analysis::refinements::{Constraint, Interval, StringConstraint};
use crate::analysis::Type;

use super::model::{ApiConstraint, ApiField, ApiSchema, ApiSchemaType, ApiType};

/// Convert a Type + refinement constraints into an ApiType.
pub fn type_to_api_type(ty: &Type) -> ApiType {
    match ty {
        Type::String => ApiType::String,
        Type::Int => ApiType::Int,
        Type::Float => ApiType::Float,
        Type::Bool => ApiType::Bool,
        Type::List(inner) => ApiType::List(Box::new(type_to_api_type(inner))),
        Type::Struct(name, _) => ApiType::Ref(name.clone()),
        Type::Refined(_, alias) => ApiType::Ref(alias.clone()),
        Type::Enum(name, _) => ApiType::Ref(name.clone()),
        _ => ApiType::Any,
    }
}

/// Convert a Constraint into an ApiConstraint.
pub fn constraint_to_api(constraint: &Constraint) -> ApiConstraint {
    match constraint {
        Constraint::IntInterval(iv) => interval_to_api(iv),
        Constraint::StringConstraint(sc) => string_constraint_to_api(sc),
    }
}

fn interval_to_api(iv: &Interval) -> ApiConstraint {
    ApiConstraint::IntRange {
        min: if iv.min == i64::MIN { None } else { Some(iv.min) },
        max: if iv.max == i64::MAX { None } else { Some(iv.max) },
    }
}

fn string_constraint_to_api(sc: &StringConstraint) -> ApiConstraint {
    match sc {
        StringConstraint::Matches(pattern) => ApiConstraint::StringPattern(pattern.clone()),
        StringConstraint::Length { min, max } => ApiConstraint::StringLength {
            min: *min,
            max: *max,
        },
        StringConstraint::Equals(val) => ApiConstraint::StringEnum(vec![val.clone()]),
        StringConstraint::Any(parts) => {
            // Check if all parts are Equals -- this is an enum
            let enum_values: Vec<String> = parts
                .iter()
                .filter_map(|p| match p {
                    StringConstraint::Equals(v) => Some(v.clone()),
                    _ => None,
                })
                .collect();
            if enum_values.len() == parts.len() {
                ApiConstraint::StringEnum(enum_values)
            } else {
                ApiConstraint::All(parts.iter().map(string_constraint_to_api).collect())
            }
        }
        StringConstraint::All(parts) => {
            ApiConstraint::All(parts.iter().map(string_constraint_to_api).collect())
        }
        StringConstraint::StartsWith(_)
        | StringConstraint::EndsWith(_)
        | StringConstraint::Contains(_) => {
            // These don't map cleanly to JSON Schema; use pattern as approximation
            ApiConstraint::StringPattern(sc.describe())
        }
    }
}

/// Build ApiSchema entries from type_defs and refined_types.
pub fn build_api_schemas(
    type_defs: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) -> HashMap<String, ApiSchema> {
    let mut schemas = HashMap::new();

    // Add refined type aliases (e.g., type Title = String where ...)
    for (name, constraint) in refined_types {
        let base_type = type_defs
            .get(name)
            .map(|t| match t {
                Type::Refined(base, _) => type_to_api_type(base),
                _ => type_to_api_type(t),
            })
            .unwrap_or_else(|| {
                // Infer base type from constraint
                match constraint {
                    Constraint::IntInterval(_) => ApiType::Int,
                    Constraint::StringConstraint(_) => ApiType::String,
                }
            });

        schemas.insert(
            name.clone(),
            ApiSchema {
                schema_type: ApiSchemaType::Alias(base_type, Some(constraint_to_api(constraint))),
            },
        );
    }

    // Add struct/record types
    for (name, ty) in type_defs {
        if schemas.contains_key(name) {
            continue; // Already added as refined alias
        }
        match ty {
            Type::Struct(_, field_map) => {
                let fields = build_fields(field_map, refined_types);
                if !fields.is_empty() {
                    schemas.insert(
                        name.clone(),
                        ApiSchema {
                            schema_type: ApiSchemaType::Object { fields },
                        },
                    );
                }
            }
            Type::Enum(_, variants) => {
                // Check if this is a simple string enum (all variants have no payload)
                let all_unit = variants.iter().all(|(_, payloads)| payloads.is_empty());
                if all_unit {
                    let values: Vec<String> = variants.iter().map(|(name, _)| name.clone()).collect();
                    schemas.insert(
                        name.clone(),
                        ApiSchema {
                            schema_type: ApiSchemaType::Enum(values),
                        },
                    );
                }
            }
            _ => {}
        }
    }

    schemas
}

fn build_fields(
    field_map: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) -> Vec<ApiField> {
    let mut fields: Vec<ApiField> = field_map
        .iter()
        .map(|(name, ty)| {
            let (field_type, constraints) = match ty {
                Type::Refined(_, alias) => {
                    let api_type = ApiType::Ref(alias.clone());
                    (api_type, None)
                }
                _ => (type_to_api_type(ty), None),
            };
            // Suppress unused variable warning -- we need refined_types for future expansion
            let _ = refined_types;
            ApiField {
                name: name.clone(),
                field_type,
                required: true,
                constraints,
            }
        })
        .collect();
    fields.sort_by(|a, b| a.name.cmp(&b.name));
    fields
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interval_to_api() {
        let iv = Interval { min: 1, max: 65535 };
        match interval_to_api(&iv) {
            ApiConstraint::IntRange { min, max } => {
                assert_eq!(min, Some(1));
                assert_eq!(max, Some(65535));
            }
            _ => panic!("expected IntRange"),
        }
    }

    #[test]
    fn test_string_enum_constraint() {
        let sc = StringConstraint::Any(vec![
            StringConstraint::Equals("pending".to_string()),
            StringConstraint::Equals("done".to_string()),
        ]);
        match string_constraint_to_api(&sc) {
            ApiConstraint::StringEnum(values) => {
                assert_eq!(values, vec!["pending", "done"]);
            }
            _ => panic!("expected StringEnum"),
        }
    }

    #[test]
    fn test_string_length_constraint() {
        let sc = StringConstraint::Length {
            min: Some(1),
            max: Some(100),
        };
        match string_constraint_to_api(&sc) {
            ApiConstraint::StringLength { min, max } => {
                assert_eq!(min, Some(1));
                assert_eq!(max, Some(100));
            }
            _ => panic!("expected StringLength"),
        }
    }

    #[test]
    fn test_build_api_schemas_struct() {
        let mut type_defs = HashMap::new();
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Type::String);
        fields.insert("age".to_string(), Type::Int);
        type_defs.insert(
            "CreateUser".to_string(),
            Type::Struct("CreateUser".to_string(), fields),
        );

        let refined = HashMap::new();
        let schemas = build_api_schemas(&type_defs, &refined);
        assert!(schemas.contains_key("CreateUser"));
    }

    #[test]
    fn test_build_api_schemas_refined() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Port".to_string(),
            Type::Refined(Box::new(Type::Int), "Port".to_string()),
        );

        let mut refined = HashMap::new();
        refined.insert(
            "Port".to_string(),
            Constraint::IntInterval(Interval { min: 1, max: 65535 }),
        );

        let schemas = build_api_schemas(&type_defs, &refined);
        assert!(schemas.contains_key("Port"));
    }
}
