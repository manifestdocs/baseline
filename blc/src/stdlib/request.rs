use super::NativeRegistry;
use crate::interpreter::RuntimeValue;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Request.header", request_header);
    registry.register("Request.with_state", request_with_state);
    registry.register("Request.state", request_state);
}

/// Request.header(req, name) -> Option<String>
/// Case-insensitive header lookup.
fn request_header<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "Request.header expects 2 arguments (req, name), got {}",
            args.len()
        ));
    }
    let fields = match &args[0] {
        RuntimeValue::Record(f) => f,
        other => return Err(format!("Request.header: first arg must be record, got {}", other)),
    };
    let name = match &args[1] {
        RuntimeValue::String(s) => s.to_lowercase(),
        other => return Err(format!("Request.header: name must be String, got {}", other)),
    };

    if let Some(RuntimeValue::List(headers)) = fields.get("headers") {
        for item in headers {
            if let RuntimeValue::Tuple(pair) = item {
                if pair.len() == 2 {
                    if let RuntimeValue::String(k) = &pair[0] {
                        if k.to_lowercase() == name {
                            return Ok(RuntimeValue::Enum(
                                "Some".to_string(),
                                vec![pair[1].clone()],
                            ));
                        }
                    }
                }
            }
        }
    }
    Ok(RuntimeValue::Enum("None".to_string(), Vec::new()))
}

/// Request.with_state(req, key, value) -> Request
/// Add/update a field in the request's `state` sub-record.
fn request_with_state<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 3 {
        return Err(format!(
            "Request.with_state expects 3 arguments (req, key, value), got {}",
            args.len()
        ));
    }
    let mut fields = match &args[0] {
        RuntimeValue::Record(f) => f.clone(),
        other => {
            return Err(format!(
                "Request.with_state: first arg must be record, got {}",
                other
            ))
        }
    };
    let key = match &args[1] {
        RuntimeValue::String(s) => s.clone(),
        other => return Err(format!("Request.with_state: key must be String, got {}", other)),
    };

    let mut state = match fields.get("state") {
        Some(RuntimeValue::Record(s)) => s.clone(),
        _ => std::collections::HashMap::new(),
    };
    state.insert(key, args[2].clone());
    fields.insert("state".to_string(), RuntimeValue::Record(state));
    Ok(RuntimeValue::Record(fields))
}

/// Request.state(req, key) -> Option<Value>
/// Read a value from the request's `state` sub-record.
fn request_state<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "Request.state expects 2 arguments (req, key), got {}",
            args.len()
        ));
    }
    let fields = match &args[0] {
        RuntimeValue::Record(f) => f,
        other => return Err(format!("Request.state: first arg must be record, got {}", other)),
    };
    let key = match &args[1] {
        RuntimeValue::String(s) => s,
        other => return Err(format!("Request.state: key must be String, got {}", other)),
    };

    if let Some(RuntimeValue::Record(state)) = fields.get("state") {
        if let Some(value) = state.get(key.as_str()) {
            return Ok(RuntimeValue::Enum(
                "Some".to_string(),
                vec![value.clone()],
            ));
        }
    }
    Ok(RuntimeValue::Enum("None".to_string(), Vec::new()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn make_request(headers: Vec<(&str, &str)>) -> RuntimeValue<'static> {
        let mut fields = HashMap::new();
        fields.insert("method".to_string(), RuntimeValue::String("GET".to_string()));
        fields.insert("url".to_string(), RuntimeValue::String("/test".to_string()));
        fields.insert("body".to_string(), RuntimeValue::String(String::new()));
        let header_list: Vec<RuntimeValue> = headers
            .into_iter()
            .map(|(k, v)| {
                RuntimeValue::Tuple(vec![
                    RuntimeValue::String(k.to_string()),
                    RuntimeValue::String(v.to_string()),
                ])
            })
            .collect();
        fields.insert("headers".to_string(), RuntimeValue::List(header_list));
        RuntimeValue::Record(fields)
    }

    #[test]
    fn header_finds_exact_match() {
        let req = make_request(vec![("Content-Type", "application/json")]);
        let result = request_header(&[req, RuntimeValue::String("Content-Type".into())]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Enum(
                "Some".to_string(),
                vec![RuntimeValue::String("application/json".into())]
            )
        );
    }

    #[test]
    fn header_case_insensitive() {
        let req = make_request(vec![("Content-Type", "text/html")]);
        let result = request_header(&[req, RuntimeValue::String("content-type".into())]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Enum(
                "Some".to_string(),
                vec![RuntimeValue::String("text/html".into())]
            )
        );
    }

    #[test]
    fn header_not_found_returns_none() {
        let req = make_request(vec![("Content-Type", "text/html")]);
        let result = request_header(&[req, RuntimeValue::String("X-Missing".into())]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("None".to_string(), Vec::new()));
    }

    #[test]
    fn header_empty_headers() {
        let req = make_request(vec![]);
        let result = request_header(&[req, RuntimeValue::String("X-Foo".into())]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("None".to_string(), Vec::new()));
    }

    #[test]
    fn with_state_adds_new_key() {
        let req = make_request(vec![]);
        let result = request_with_state(&[
            req,
            RuntimeValue::String("user_id".into()),
            RuntimeValue::String("42".into()),
        ])
        .unwrap();

        if let RuntimeValue::Record(fields) = &result {
            if let Some(RuntimeValue::Record(state)) = fields.get("state") {
                assert_eq!(
                    state.get("user_id"),
                    Some(&RuntimeValue::String("42".into()))
                );
            } else {
                panic!("Expected state sub-record");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn with_state_preserves_existing_state() {
        let req = make_request(vec![]);
        let req = request_with_state(&[
            req,
            RuntimeValue::String("a".into()),
            RuntimeValue::String("1".into()),
        ])
        .unwrap();
        let req = request_with_state(&[
            req,
            RuntimeValue::String("b".into()),
            RuntimeValue::String("2".into()),
        ])
        .unwrap();

        if let RuntimeValue::Record(fields) = &req {
            if let Some(RuntimeValue::Record(state)) = fields.get("state") {
                assert_eq!(state.get("a"), Some(&RuntimeValue::String("1".into())));
                assert_eq!(state.get("b"), Some(&RuntimeValue::String("2".into())));
            } else {
                panic!("Expected state sub-record");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn state_reads_existing_key() {
        let req = make_request(vec![]);
        let req = request_with_state(&[
            req,
            RuntimeValue::String("token".into()),
            RuntimeValue::String("abc123".into()),
        ])
        .unwrap();

        let result = request_state(&[req, RuntimeValue::String("token".into())]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Enum(
                "Some".to_string(),
                vec![RuntimeValue::String("abc123".into())]
            )
        );
    }

    #[test]
    fn state_missing_key_returns_none() {
        let req = make_request(vec![]);
        let req = request_with_state(&[
            req,
            RuntimeValue::String("a".into()),
            RuntimeValue::String("1".into()),
        ])
        .unwrap();
        let result = request_state(&[req, RuntimeValue::String("missing".into())]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("None".to_string(), Vec::new()));
    }

    #[test]
    fn state_no_state_field_returns_none() {
        let req = make_request(vec![]);
        let result = request_state(&[req, RuntimeValue::String("key".into())]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("None".to_string(), Vec::new()));
    }
}
