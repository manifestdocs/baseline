use serde_json::{json, Map, Value};

use super::model::*;

/// Serialize an ApiModel to OpenAPI 3.1.0 JSON.
pub fn to_openapi_json(model: &ApiModel, title: &str, version: &str) -> Value {
    let mut doc = json!({
        "openapi": "3.1.0",
        "info": {
            "title": title,
            "version": version
        }
    });

    // Build paths
    let paths = build_paths(&model.routes);
    doc["paths"] = paths;

    // Build component schemas
    if !model.schemas.is_empty() {
        let schemas = build_component_schemas(&model.schemas);
        doc["components"] = json!({ "schemas": schemas });
    }

    doc
}

fn build_paths(routes: &[ApiRoute]) -> Value {
    let mut paths: Map<String, Value> = Map::new();

    for route in routes {
        let method = route.method.to_lowercase();
        let operation = build_operation(route);

        let path_entry = paths
            .entry(route.path.clone())
            .or_insert_with(|| json!({}));

        if let Value::Object(map) = path_entry {
            map.insert(method, operation);
        }
    }

    Value::Object(paths)
}

fn build_operation(route: &ApiRoute) -> Value {
    let mut op = json!({
        "operationId": route.operation_id
    });

    if let Some(summary) = &route.summary {
        op["summary"] = json!(summary);
    }

    // Parameters
    if !route.parameters.is_empty() {
        let params: Vec<Value> = route
            .parameters
            .iter()
            .map(|p| {
                json!({
                    "name": p.name,
                    "in": param_location_str(&p.location),
                    "required": p.required,
                    "schema": api_type_to_schema(&p.param_type)
                })
            })
            .collect();
        op["parameters"] = json!(params);
    }

    // Request body
    if let Some(ref body_type) = route.request_body {
        op["requestBody"] = json!({
            "required": true,
            "content": {
                "application/json": {
                    "schema": {
                        "$ref": format!("#/components/schemas/{}", body_type)
                    }
                }
            }
        });
    }

    // Responses
    let mut responses: Map<String, Value> = Map::new();
    for resp in &route.responses {
        let mut resp_obj = json!({
            "description": resp.description
        });

        if let Some(schema_ref) = &resp.schema {
            let schema = match schema_ref {
                ApiSchemaRef::Ref(name) => json!({
                    "$ref": format!("#/components/schemas/{}", name)
                }),
                ApiSchemaRef::Inline(ty) => api_type_to_schema(ty),
            };
            resp_obj["content"] = json!({
                "application/json": {
                    "schema": schema
                }
            });
        }

        responses.insert(resp.status.to_string(), resp_obj);
    }
    op["responses"] = Value::Object(responses);

    op
}

fn build_component_schemas(
    schemas: &std::collections::HashMap<String, ApiSchema>,
) -> Value {
    let mut result: Map<String, Value> = Map::new();

    let mut sorted_keys: Vec<&String> = schemas.keys().collect();
    sorted_keys.sort();

    for name in sorted_keys {
        let schema = &schemas[name];
        result.insert(name.clone(), api_schema_to_json(schema));
    }

    Value::Object(result)
}

fn api_schema_to_json(schema: &ApiSchema) -> Value {
    match &schema.schema_type {
        ApiSchemaType::Object { fields } => {
            let mut properties: Map<String, Value> = Map::new();
            let mut required: Vec<String> = Vec::new();

            for field in fields {
                let mut field_schema = api_type_to_schema(&field.field_type);

                if let Some(constraint) = &field.constraints {
                    merge_constraint(&mut field_schema, constraint);
                }

                properties.insert(field.name.clone(), field_schema);

                if field.required {
                    required.push(field.name.clone());
                }
            }

            let mut obj = json!({
                "type": "object",
                "properties": Value::Object(properties)
            });

            if !required.is_empty() {
                obj["required"] = json!(required);
            }

            obj
        }
        ApiSchemaType::Enum(values) => {
            json!({
                "type": "string",
                "enum": values
            })
        }
        ApiSchemaType::Alias(base_type, constraint) => {
            let mut schema = api_type_to_schema(base_type);
            if let Some(c) = constraint {
                merge_constraint(&mut schema, c);
            }
            schema
        }
    }
}

fn api_type_to_schema(ty: &ApiType) -> Value {
    match ty {
        ApiType::String => json!({ "type": "string" }),
        ApiType::Int => json!({ "type": "integer" }),
        ApiType::Float => json!({ "type": "number" }),
        ApiType::Bool => json!({ "type": "boolean" }),
        ApiType::List(inner) => json!({
            "type": "array",
            "items": api_type_to_schema(inner)
        }),
        ApiType::Ref(name) => json!({
            "$ref": format!("#/components/schemas/{}", name)
        }),
        ApiType::Any => json!({}),
    }
}

fn merge_constraint(schema: &mut Value, constraint: &ApiConstraint) {
    match constraint {
        ApiConstraint::IntRange { min, max } => {
            if let Some(min_val) = min {
                schema["minimum"] = json!(min_val);
            }
            if let Some(max_val) = max {
                schema["maximum"] = json!(max_val);
            }
        }
        ApiConstraint::StringPattern(pattern) => {
            schema["pattern"] = json!(pattern);
        }
        ApiConstraint::StringLength { min, max } => {
            if let Some(min_val) = min {
                schema["minLength"] = json!(min_val);
            }
            if let Some(max_val) = max {
                schema["maxLength"] = json!(max_val);
            }
        }
        ApiConstraint::StringEnum(values) => {
            schema["enum"] = json!(values);
        }
        ApiConstraint::All(parts) => {
            for part in parts {
                merge_constraint(schema, part);
            }
        }
    }
}

fn param_location_str(loc: &ParamLocation) -> &'static str {
    match loc {
        ParamLocation::Path => "path",
        ParamLocation::Query => "query",
        ParamLocation::Header => "header",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_empty_api_model() {
        let model = ApiModel {
            routes: vec![],
            schemas: HashMap::new(),
        };
        let doc = to_openapi_json(&model, "Test API", "1.0.0");
        assert_eq!(doc["openapi"], "3.1.0");
        assert_eq!(doc["info"]["title"], "Test API");
        assert_eq!(doc["paths"], json!({}));
    }

    #[test]
    fn test_simple_route() {
        let model = ApiModel {
            routes: vec![ApiRoute {
                method: "GET".to_string(),
                path: "/health".to_string(),
                operation_id: "health".to_string(),
                summary: Some("Health check".to_string()),
                request_body: None,
                parameters: vec![],
                responses: vec![ApiResponse {
                    status: 200,
                    description: "OK".to_string(),
                    schema: None,
                }],
            }],
            schemas: HashMap::new(),
        };
        let doc = to_openapi_json(&model, "Test", "0.1.0");
        assert!(doc["paths"]["/health"]["get"]["operationId"] == "health");
        assert!(doc["paths"]["/health"]["get"]["summary"] == "Health check");
    }

    #[test]
    fn test_route_with_request_body() {
        let model = ApiModel {
            routes: vec![ApiRoute {
                method: "POST".to_string(),
                path: "/todos".to_string(),
                operation_id: "create_todo".to_string(),
                summary: None,
                request_body: Some("CreateTodo".to_string()),
                parameters: vec![],
                responses: vec![
                    ApiResponse {
                        status: 201,
                        description: "Created".to_string(),
                        schema: None,
                    },
                ],
            }],
            schemas: HashMap::new(),
        };
        let doc = to_openapi_json(&model, "Test", "0.1.0");
        let req_body = &doc["paths"]["/todos"]["post"]["requestBody"];
        assert_eq!(
            req_body["content"]["application/json"]["schema"]["$ref"],
            "#/components/schemas/CreateTodo"
        );
    }

    #[test]
    fn test_refined_type_schema() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "Port".to_string(),
            ApiSchema {
                schema_type: ApiSchemaType::Alias(
                    ApiType::Int,
                    Some(ApiConstraint::IntRange {
                        min: Some(1),
                        max: Some(65535),
                    }),
                ),
            },
        );
        schemas.insert(
            "Status".to_string(),
            ApiSchema {
                schema_type: ApiSchemaType::Alias(
                    ApiType::String,
                    Some(ApiConstraint::StringEnum(vec![
                        "pending".to_string(),
                        "done".to_string(),
                    ])),
                ),
            },
        );

        let model = ApiModel {
            routes: vec![],
            schemas,
        };
        let doc = to_openapi_json(&model, "Test", "0.1.0");

        let port_schema = &doc["components"]["schemas"]["Port"];
        assert_eq!(port_schema["type"], "integer");
        assert_eq!(port_schema["minimum"], 1);
        assert_eq!(port_schema["maximum"], 65535);

        let status_schema = &doc["components"]["schemas"]["Status"];
        assert_eq!(status_schema["type"], "string");
        assert_eq!(status_schema["enum"], json!(["pending", "done"]));
    }

    #[test]
    fn test_string_length_schema() {
        let mut schemas = HashMap::new();
        schemas.insert(
            "Title".to_string(),
            ApiSchema {
                schema_type: ApiSchemaType::Alias(
                    ApiType::String,
                    Some(ApiConstraint::StringLength {
                        min: Some(1),
                        max: Some(100),
                    }),
                ),
            },
        );

        let model = ApiModel {
            routes: vec![],
            schemas,
        };
        let doc = to_openapi_json(&model, "Test", "0.1.0");

        let title_schema = &doc["components"]["schemas"]["Title"];
        assert_eq!(title_schema["type"], "string");
        assert_eq!(title_schema["minLength"], 1);
        assert_eq!(title_schema["maxLength"], 100);
    }
}
