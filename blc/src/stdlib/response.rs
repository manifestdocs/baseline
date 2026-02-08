use super::NativeRegistry;
use crate::interpreter::RuntimeValue;
use std::collections::HashMap;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Response.ok", response_ok);
    registry.register("Response.json", response_json);
    registry.register("Response.created", response_created);
    registry.register("Response.no_content", response_no_content);
    registry.register("Response.bad_request", response_bad_request);
    registry.register("Response.not_found", response_not_found);
    registry.register("Response.error", response_error);
    registry.register("Response.status", response_status);
    registry.register("Response.with_header", response_with_header);
}

/// Build a Response record: { status: Int, headers: List<(String, String)>, body: String }
fn build<'a>(status: i64, headers: Vec<RuntimeValue<'a>>, body: String) -> RuntimeValue<'a> {
    let mut fields = HashMap::new();
    fields.insert("status".to_string(), RuntimeValue::Int(status));
    fields.insert("headers".to_string(), RuntimeValue::List(headers));
    fields.insert("body".to_string(), RuntimeValue::String(body));
    RuntimeValue::Record(fields)
}

fn simple_response<'a>(status: i64, body: String) -> RuntimeValue<'a> {
    build(status, Vec::new(), body)
}

// Response.ok(body) -> { status: 200, headers: [], body }
fn response_ok<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.ok expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => return Err(format!("Response.ok expects String body, got {}", other)),
    };
    Ok(simple_response(200, body))
}

// Response.json(body) -> { status: 200, headers: [("Content-Type", "application/json")], body }
// Accepts String (used as-is) or any other value (auto-serialized to JSON).
fn response_json<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.json expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            let serde_val = super::json::runtime_to_serde(other)?;
            serde_json::to_string(&serde_val)
                .map_err(|e| format!("Response.json: {}", e))?
        }
    };
    let headers = vec![RuntimeValue::Tuple(vec![
        RuntimeValue::String("Content-Type".to_string()),
        RuntimeValue::String("application/json".to_string()),
    ])];
    Ok(build(200, headers, body))
}

// Response.created(body) -> { status: 201, headers: [], body }
fn response_created<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.created expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.created expects String body, got {}",
                other
            ));
        }
    };
    Ok(simple_response(201, body))
}

// Response.no_content() -> { status: 204, headers: [], body: "" }
fn response_no_content<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if !args.is_empty() {
        return Err(format!(
            "Response.no_content expects 0 arguments, got {}",
            args.len()
        ));
    }
    Ok(simple_response(204, String::new()))
}

// Response.bad_request(body) -> { status: 400, headers: [], body }
fn response_bad_request<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.bad_request expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.bad_request expects String body, got {}",
                other
            ));
        }
    };
    Ok(simple_response(400, body))
}

// Response.not_found(body) -> { status: 404, headers: [], body }
fn response_not_found<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.not_found expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.not_found expects String body, got {}",
                other
            ));
        }
    };
    Ok(simple_response(404, body))
}

// Response.error(body) -> { status: 500, headers: [], body }
fn response_error<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Response.error expects 1 argument, got {}",
            args.len()
        ));
    }
    let body = match &args[0] {
        RuntimeValue::String(s) => s.clone(),
        other => return Err(format!("Response.error expects String body, got {}", other)),
    };
    Ok(simple_response(500, body))
}

// Response.status(code, body) -> { status: code, headers: [], body }
fn response_status<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "Response.status expects 2 arguments, got {}",
            args.len()
        ));
    }
    let code = match &args[0] {
        RuntimeValue::Int(i) => *i,
        other => {
            return Err(format!(
                "Response.status expects Int status code, got {}",
                other
            ));
        }
    };
    let body = match &args[1] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.status expects String body, got {}",
                other
            ));
        }
    };
    Ok(simple_response(code, body))
}

// Response.with_header(response, name, value) -> response with header prepended
fn response_with_header<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 3 {
        return Err(format!(
            "Response.with_header expects 3 arguments, got {}",
            args.len()
        ));
    }
    let fields = match &args[0] {
        RuntimeValue::Record(f) => f,
        other => {
            return Err(format!(
                "Response.with_header expects Response record, got {}",
                other
            ));
        }
    };
    let name = match &args[1] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.with_header expects String header name, got {}",
                other
            ));
        }
    };
    let value = match &args[2] {
        RuntimeValue::String(s) => s.clone(),
        other => {
            return Err(format!(
                "Response.with_header expects String header value, got {}",
                other
            ));
        }
    };

    let mut new_fields = fields.clone();
    let mut headers = match fields.get("headers") {
        Some(RuntimeValue::List(list)) => list.clone(),
        _ => Vec::new(),
    };
    headers.push(RuntimeValue::Tuple(vec![
        RuntimeValue::String(name),
        RuntimeValue::String(value),
    ]));
    new_fields.insert("headers".to_string(), RuntimeValue::List(headers));
    Ok(RuntimeValue::Record(new_fields))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ok_returns_200() {
        let result = response_ok(&[RuntimeValue::String("hello".into())]).unwrap();
        if let RuntimeValue::Record(fields) = result {
            assert_eq!(fields.get("status"), Some(&RuntimeValue::Int(200)));
            assert_eq!(
                fields.get("body"),
                Some(&RuntimeValue::String("hello".into()))
            );
            assert_eq!(fields.get("headers"), Some(&RuntimeValue::List(vec![])));
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn json_sets_content_type() {
        let result = response_json(&[RuntimeValue::String("{}".into())]).unwrap();
        if let RuntimeValue::Record(fields) = result {
            assert_eq!(fields.get("status"), Some(&RuntimeValue::Int(200)));
            if let Some(RuntimeValue::List(headers)) = fields.get("headers") {
                assert_eq!(headers.len(), 1);
                if let RuntimeValue::Tuple(pair) = &headers[0] {
                    assert_eq!(pair[0], RuntimeValue::String("Content-Type".into()));
                    assert_eq!(pair[1], RuntimeValue::String("application/json".into()));
                } else {
                    panic!("Expected Tuple header");
                }
            } else {
                panic!("Expected List headers");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn status_codes() {
        fn check(
            f: for<'a> fn(&[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String>,
            arg: &str,
            code: i64,
        ) {
            let result = f(&[RuntimeValue::String(arg.into())]).unwrap();
            if let RuntimeValue::Record(fields) = result {
                assert_eq!(fields.get("status"), Some(&RuntimeValue::Int(code)));
            } else {
                panic!("Expected Record");
            }
        }
        check(response_created, "x", 201);
        check(response_bad_request, "x", 400);
        check(response_not_found, "x", 404);
        check(response_error, "x", 500);
    }

    #[test]
    fn no_content_is_204_empty() {
        let result = response_no_content(&[]).unwrap();
        if let RuntimeValue::Record(fields) = result {
            assert_eq!(fields.get("status"), Some(&RuntimeValue::Int(204)));
            assert_eq!(
                fields.get("body"),
                Some(&RuntimeValue::String(String::new()))
            );
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn custom_status() {
        let result =
            response_status(&[RuntimeValue::Int(301), RuntimeValue::String("Moved".into())])
                .unwrap();
        if let RuntimeValue::Record(fields) = result {
            assert_eq!(fields.get("status"), Some(&RuntimeValue::Int(301)));
            assert_eq!(
                fields.get("body"),
                Some(&RuntimeValue::String("Moved".into()))
            );
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn with_header_appends() {
        let resp = response_ok(&[RuntimeValue::String("hi".into())]).unwrap();
        let result = response_with_header(&[
            resp,
            RuntimeValue::String("X-Foo".into()),
            RuntimeValue::String("bar".into()),
        ])
        .unwrap();
        if let RuntimeValue::Record(fields) = result {
            if let Some(RuntimeValue::List(headers)) = fields.get("headers") {
                assert_eq!(headers.len(), 1);
            } else {
                panic!("Expected List headers");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn with_header_chains() {
        let resp = response_json(&[RuntimeValue::String("{}".into())]).unwrap();
        let resp = response_with_header(&[
            resp,
            RuntimeValue::String("X-A".into()),
            RuntimeValue::String("1".into()),
        ])
        .unwrap();
        let resp = response_with_header(&[
            resp,
            RuntimeValue::String("X-B".into()),
            RuntimeValue::String("2".into()),
        ])
        .unwrap();
        if let RuntimeValue::Record(fields) = resp {
            if let Some(RuntimeValue::List(headers)) = fields.get("headers") {
                // Content-Type + X-A + X-B
                assert_eq!(headers.len(), 3);
            } else {
                panic!("Expected List headers");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn json_auto_serializes_record() {
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), RuntimeValue::String("Alice".into()));
        fields.insert("age".to_string(), RuntimeValue::Int(30));
        let result = response_json(&[RuntimeValue::Record(fields)]).unwrap();
        if let RuntimeValue::Record(resp) = result {
            assert_eq!(resp.get("status"), Some(&RuntimeValue::Int(200)));
            if let Some(RuntimeValue::String(body)) = resp.get("body") {
                // Parse back to verify it's valid JSON
                let parsed: serde_json::Value = serde_json::from_str(body).unwrap();
                assert_eq!(parsed["name"], "Alice");
                assert_eq!(parsed["age"], 30);
            } else {
                panic!("Expected String body");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn json_auto_serializes_int() {
        let result = response_json(&[RuntimeValue::Int(42)]).unwrap();
        if let RuntimeValue::Record(resp) = result {
            assert_eq!(
                resp.get("body"),
                Some(&RuntimeValue::String("42".into()))
            );
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn json_auto_serializes_list() {
        let result = response_json(&[RuntimeValue::List(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Int(2),
            RuntimeValue::Int(3),
        ])])
        .unwrap();
        if let RuntimeValue::Record(resp) = result {
            if let Some(RuntimeValue::String(body)) = resp.get("body") {
                let parsed: serde_json::Value = serde_json::from_str(body).unwrap();
                assert_eq!(parsed, serde_json::json!([1, 2, 3]));
            } else {
                panic!("Expected String body");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn json_passes_string_through() {
        let json_str = r#"{"already":"serialized"}"#;
        let result = response_json(&[RuntimeValue::String(json_str.into())]).unwrap();
        if let RuntimeValue::Record(resp) = result {
            assert_eq!(
                resp.get("body"),
                Some(&RuntimeValue::String(json_str.into()))
            );
        } else {
            panic!("Expected Record");
        }
    }
}
