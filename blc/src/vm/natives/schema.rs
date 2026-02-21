use std::cell::RefCell;
use std::collections::HashMap;

use crate::analysis::Type;
use crate::analysis::refinements::{Constraint, Interval, StringConstraint};

use super::json::serde_to_nvalue;
use super::{NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Schema Registry — thread-local store of runtime schemas
// ---------------------------------------------------------------------------

/// Runtime schema for a single field.
#[derive(Debug, Clone)]
pub enum FieldSchema {
    /// Must be a string value.
    StringField,
    /// Must be an integer value.
    IntField,
    /// Must be a boolean value.
    BoolField,
    /// String with refinement constraints.
    RefinedString(StringConstraint),
    /// Int with range constraints.
    RefinedInt(Interval),
    /// Any type accepted (no validation beyond presence).
    AnyField,
}

/// Runtime schema for a named type (struct or record with refined fields).
#[derive(Debug, Clone)]
pub struct SchemaEntry {
    pub fields: Vec<(String, FieldSchema)>,
}

thread_local! {
    static SCHEMA_REGISTRY: RefCell<HashMap<String, SchemaEntry>> = RefCell::new(HashMap::new());
}

/// Populate the schema registry from type definitions and refinement constraints.
///
/// Called once after type checking and refinement collection, before VM execution.
pub fn populate_schemas(
    type_defs: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) {
    let schemas = build_schemas(type_defs, refined_types);
    SCHEMA_REGISTRY.with(|cell| {
        *cell.borrow_mut() = schemas;
    });
}

/// Build runtime schemas by combining type definitions with refinement constraints.
fn build_schemas(
    type_defs: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) -> HashMap<String, SchemaEntry> {
    let mut schemas = HashMap::new();

    for (name, ty) in type_defs {
        let fields = match ty {
            Type::Struct(_, field_map) => extract_fields_from_map(field_map, refined_types),
            Type::Record(field_map, _) => extract_fields_from_map(field_map, refined_types),
            _ => continue,
        };

        if !fields.is_empty() {
            schemas.insert(name.clone(), SchemaEntry { fields });
        }
    }

    schemas
}

/// Extract field schemas from a type's field map.
fn extract_fields_from_map(
    field_map: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) -> Vec<(String, FieldSchema)> {
    let mut fields: Vec<(String, FieldSchema)> = field_map
        .iter()
        .map(|(field_name, field_type): (&String, &Type)| {
            let schema = type_to_field_schema(field_type, refined_types);
            (field_name.clone(), schema)
        })
        .collect();
    fields.sort_by(|a, b| a.0.cmp(&b.0));
    fields
}

/// Convert a Type into a FieldSchema, resolving refined types via the constraint map.
fn type_to_field_schema(ty: &Type, refined_types: &HashMap<String, Constraint>) -> FieldSchema {
    match ty {
        Type::String => FieldSchema::StringField,
        Type::Int => FieldSchema::IntField,
        Type::Bool => FieldSchema::BoolField,
        Type::Refined(base, alias) => {
            // Look up the refinement constraint by alias name
            if let Some(constraint) = refined_types.get(alias) {
                match constraint {
                    Constraint::StringConstraint(sc) => FieldSchema::RefinedString(sc.clone()),
                    Constraint::IntInterval(iv) => FieldSchema::RefinedInt(*iv),
                }
            } else {
                // Fallback to the base type
                type_to_field_schema(base, refined_types)
            }
        }
        _ => FieldSchema::AnyField,
    }
}

// ---------------------------------------------------------------------------
// Request.decode native function
// ---------------------------------------------------------------------------

/// `Request.decode(req, type_name) -> Result<Record, Response>`
///
/// Parses the request body as JSON and validates each field against the
/// registered schema for `type_name`. Returns `Ok(record)` on success,
/// or `Err(response)` with accumulated JSON:API-style errors on failure.
pub(super) fn native_request_decode(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.decode expects 2 arguments (req, type_name), got {}",
            args.len()
        )));
    }

    // Extract request record
    let req_fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.decode: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };

    // Extract type name
    let type_name = match args[1].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Request.decode: second arg must be String (type name), got {}",
                args[1]
            )));
        }
    };

    // Look up schema
    let schema = SCHEMA_REGISTRY.with(|cell| cell.borrow().get(&type_name).cloned());

    let schema = match schema {
        Some(s) => s,
        None => {
            return Ok(decode_error_response(&[format!(
                "No schema registered for type '{}'",
                type_name
            )]));
        }
    };

    // Parse request body as JSON
    let body = req_fields
        .iter()
        .find(|(k, _)| &**k == "body")
        .map(|(_, v)| v.clone())
        .unwrap_or_else(|| NValue::string("".into()));

    let body_str = match body.as_string() {
        Some(s) => s.to_string(),
        None => {
            return Ok(decode_error_response(&[
                "Request body is not a string".to_string()
            ]));
        }
    };

    if body_str.is_empty() {
        return Ok(decode_error_response(
            &["Request body is empty".to_string()],
        ));
    }

    let json_value: serde_json::Value = match serde_json::from_str(&body_str) {
        Ok(v) => v,
        Err(e) => {
            return Ok(decode_error_response(&[format!("JSON parse error: {}", e)]));
        }
    };

    let json_obj = match json_value.as_object() {
        Some(obj) => obj,
        None => {
            return Ok(decode_error_response(&[
                "Request body must be a JSON object".to_string(),
            ]));
        }
    };

    // Validate each field against the schema
    let mut errors: Vec<String> = Vec::new();
    let mut record_fields: Vec<(RcStr, NValue)> = Vec::new();

    for (field_name, field_schema) in &schema.fields {
        let json_val = json_obj.get(field_name);

        match json_val {
            None => {
                errors.push(format!("'{}' is required", field_name));
            }
            Some(serde_json::Value::Null) => {
                errors.push(format!("'{}' is required", field_name));
            }
            Some(val) => match validate_field(field_name, val, field_schema) {
                Ok(nval) => {
                    record_fields.push((RcStr::from(field_name.as_str()), nval));
                }
                Err(field_errors) => {
                    errors.extend(field_errors);
                }
            },
        }
    }

    if errors.is_empty() {
        Ok(NValue::enum_val("Ok".into(), NValue::record(record_fields)))
    } else {
        Ok(decode_error_response(&errors))
    }
}

/// Validate a single field value against its schema.
/// Returns Ok(NValue) on success or Err(Vec<String>) with error messages.
fn validate_field(
    field_name: &str,
    value: &serde_json::Value,
    schema: &FieldSchema,
) -> Result<NValue, Vec<String>> {
    match schema {
        FieldSchema::StringField => match value.as_str() {
            Some(s) => Ok(NValue::string(s.into())),
            None => Err(vec![format!("'{}' must be a string", field_name)]),
        },
        FieldSchema::IntField => match value.as_i64() {
            Some(n) => Ok(NValue::int(n)),
            None => Err(vec![format!("'{}' must be an integer", field_name)]),
        },
        FieldSchema::BoolField => match value.as_bool() {
            Some(b) => Ok(NValue::bool(b)),
            None => Err(vec![format!("'{}' must be a boolean", field_name)]),
        },
        FieldSchema::RefinedString(constraint) => {
            let s = match value.as_str() {
                Some(s) => s,
                None => {
                    return Err(vec![format!("'{}' must be a string", field_name)]);
                }
            };
            match constraint.check(s) {
                Ok(()) => Ok(NValue::string(s.into())),
                Err(reason) => Err(vec![format!("'{}' {}", field_name, reason)]),
            }
        }
        FieldSchema::RefinedInt(interval) => {
            let n = match value.as_i64() {
                Some(n) => n,
                None => {
                    return Err(vec![format!("'{}' must be an integer", field_name)]);
                }
            };
            if !interval.contains(n) {
                let desc = match (interval.min != i64::MIN, interval.max != i64::MAX) {
                    (true, true) => {
                        format!("must be between {} and {}", interval.min, interval.max)
                    }
                    (true, false) => format!("must be at least {}", interval.min),
                    (false, true) => format!("must be at most {}", interval.max),
                    (false, false) => "invalid".to_string(),
                };
                return Err(vec![format!("'{}' {}", field_name, desc)]);
            }
            Ok(NValue::int(n))
        }
        FieldSchema::AnyField => Ok(serde_to_nvalue(value.clone())),
    }
}

/// Build an `Err(Response)` with JSON:API-style error body.
fn decode_error_response(messages: &[String]) -> NValue {
    let errors: Vec<serde_json::Value> = messages
        .iter()
        .map(|msg| {
            serde_json::json!({
                "status": "422",
                "title": "Validation Error",
                "detail": msg
            })
        })
        .collect();

    let body = serde_json::json!({ "errors": errors });
    let body_str = serde_json::to_string(&body).unwrap_or_else(|_| "{}".to_string());

    // Return Err(Response { status: 422, body, headers })
    let response = NValue::record(vec![
        (RcStr::from("status"), NValue::int(422)),
        (RcStr::from("body"), NValue::string(body_str.into())),
        (
            RcStr::from("headers"),
            NValue::list(vec![NValue::tuple(vec![
                NValue::string("content-type".into()),
                NValue::string("application/json".into()),
            ])]),
        ),
    ]);

    NValue::enum_val("Err".into(), response)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn populate_and_decode_simple_schema() {
        let mut type_defs = HashMap::new();
        let mut field_map = HashMap::new();
        field_map.insert("name".to_string(), Type::String);
        field_map.insert("age".to_string(), Type::Int);
        type_defs.insert(
            "CreateUser".to_string(),
            Type::Struct("CreateUser".to_string(), field_map),
        );

        let refined_types = HashMap::new();
        populate_schemas(&type_defs, &refined_types);

        // Valid decode
        let req = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"name":"Alice","age":30}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("CreateUser".into())]).unwrap();
        let (tag, _payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");
    }

    #[test]
    fn decode_missing_field_returns_err() {
        let mut type_defs = HashMap::new();
        let mut field_map = HashMap::new();
        field_map.insert("name".to_string(), Type::String);
        field_map.insert("age".to_string(), Type::Int);
        type_defs.insert(
            "User".to_string(),
            Type::Struct("User".to_string(), field_map),
        );

        let refined_types = HashMap::new();
        populate_schemas(&type_defs, &refined_types);

        let req = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"name":"Alice"}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("User".into())]).unwrap();
        let (tag, _payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
    }

    #[test]
    fn decode_wrong_type_returns_err() {
        let mut type_defs = HashMap::new();
        let mut field_map = HashMap::new();
        field_map.insert("age".to_string(), Type::Int);
        type_defs.insert(
            "AgeOnly".to_string(),
            Type::Struct("AgeOnly".to_string(), field_map),
        );

        let refined_types = HashMap::new();
        populate_schemas(&type_defs, &refined_types);

        let req = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"age":"not_a_number"}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("AgeOnly".into())]).unwrap();
        let (tag, _payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
    }

    #[test]
    fn decode_with_refined_string() {
        let mut type_defs = HashMap::new();
        let mut field_map = HashMap::new();
        field_map.insert(
            "status".to_string(),
            Type::Refined(Box::new(Type::String), "Status".to_string()),
        );
        type_defs.insert(
            "UpdateTask".to_string(),
            Type::Struct("UpdateTask".to_string(), field_map),
        );

        let mut refined_types = HashMap::new();
        refined_types.insert(
            "Status".to_string(),
            Constraint::StringConstraint(StringConstraint::Any(vec![
                StringConstraint::Equals("pending".to_string()),
                StringConstraint::Equals("done".to_string()),
            ])),
        );

        populate_schemas(&type_defs, &refined_types);

        // Valid: status = "pending"
        let req = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"status":"pending"}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("UpdateTask".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");

        // Invalid: status = "invalid"
        let req2 = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"status":"invalid"}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result2 = native_request_decode(&[req2, NValue::string("UpdateTask".into())]).unwrap();
        let (tag2, _) = result2.as_enum().unwrap();
        assert_eq!(tag2.as_ref(), "Err");
    }

    #[test]
    fn decode_with_refined_int() {
        let mut type_defs = HashMap::new();
        let mut field_map = HashMap::new();
        field_map.insert(
            "port".to_string(),
            Type::Refined(Box::new(Type::Int), "Port".to_string()),
        );
        type_defs.insert(
            "Config".to_string(),
            Type::Struct("Config".to_string(), field_map),
        );

        let mut refined_types = HashMap::new();
        refined_types.insert(
            "Port".to_string(),
            Constraint::IntInterval(Interval { min: 1, max: 65535 }),
        );

        populate_schemas(&type_defs, &refined_types);

        // Valid
        let req = NValue::record(vec![
            (
                RcStr::from("body"),
                NValue::string(r#"{"port":8080}"#.into()),
            ),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("Config".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");

        // Invalid: port = 0
        let req2 = NValue::record(vec![
            (RcStr::from("body"), NValue::string(r#"{"port":0}"#.into())),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result2 = native_request_decode(&[req2, NValue::string("Config".into())]).unwrap();
        let (tag2, _) = result2.as_enum().unwrap();
        assert_eq!(tag2.as_ref(), "Err");
    }

    #[test]
    fn decode_unknown_schema_returns_err() {
        // Clear registry
        SCHEMA_REGISTRY.with(|cell| cell.borrow_mut().clear());

        let req = NValue::record(vec![
            (RcStr::from("body"), NValue::string(r#"{"x":1}"#.into())),
            (RcStr::from("method"), NValue::string("POST".into())),
            (RcStr::from("headers"), NValue::list(vec![])),
        ]);
        let result = native_request_decode(&[req, NValue::string("NoSuchType".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
    }
}
