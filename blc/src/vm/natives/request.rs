use super::{HeapObject, NValue, NativeError, RcStr};
use super::json::serde_to_nvalue;

/// Request.header(req, name) -> Option<String>
/// Case-insensitive header lookup from request record's headers list.
pub(super) fn native_request_header(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.header expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.header: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.to_string().to_lowercase(),
        None => {
            return Err(NativeError(format!(
                "Request.header: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let headers = fields
        .iter()
        .find(|(k, _)| &**k == "headers")
        .and_then(|(_, v)| v.as_list());

    if let Some(headers) = headers {
        for item in headers {
            if item.is_heap() {
                if let HeapObject::Tuple(pair) = item.as_heap_ref() {
                    if pair.len() == 2 {
                        if let Some(k) = pair[0].as_string() {
                            if k.to_lowercase() == name {
                                return Ok(NValue::enum_val("Some".into(), pair[1].clone()));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(NValue::enum_val("None".into(), NValue::unit()))
}

/// Request.method(req) -> String
/// Extract the HTTP method from a request record.
pub(super) fn native_request_method(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.method: expected Request record, got {}",
                args[0]
            )));
        }
    };
    for (k, v) in fields.iter() {
        if &**k == "method" {
            return Ok(v.clone());
        }
    }
    Ok(NValue::string("GET".into()))
}

/// Request.body_json(req) -> parsed JSON value (sugar for Json.parse(req.body)).
pub(super) fn native_request_body_json(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.body_json: expected Request record, got {}",
                args[0]
            )));
        }
    };
    let body = fields
        .iter()
        .find(|(k, _)| &**k == "body")
        .map(|(_, v)| v.clone())
        .unwrap_or_else(|| NValue::string("".into()));

    let body_str = match body.as_string() {
        Some(s) => s.to_string(),
        None => return Ok(NValue::enum_val("Err".into(), NValue::string("Request body is not a string".into()))),
    };

    if body_str.is_empty() {
        return Ok(NValue::enum_val("Err".into(), NValue::string("Request body is empty".into())));
    }

    match serde_json::from_str::<serde_json::Value>(&body_str) {
        Ok(value) => Ok(NValue::enum_val("Ok".into(), serde_to_nvalue(value))),
        Err(e) => Ok(NValue::enum_val("Err".into(), NValue::string(format!("JSON parse error: {}", e).into()))),
    }
}

/// Request.with_state(req, key, value) -> Request
/// Add/update a field in the request's `state` sub-record.
pub(super) fn native_request_with_state(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Request.with_state expects 3 arguments (req, key, value), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.with_state: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let key = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.with_state: key must be String, got {}",
                args[1]
            )));
        }
    };

    let mut new_fields = fields;
    // Find or create state sub-record
    let mut state_fields: Vec<(RcStr, NValue)> = new_fields
        .iter()
        .find(|(k, _)| &**k == "state")
        .and_then(|(_, v)| v.as_record())
        .cloned()
        .unwrap_or_default();

    // Update or insert the key
    let mut found = false;
    for (k, v) in &mut state_fields {
        if *k == key {
            *v = args[2].clone();
            found = true;
            break;
        }
    }
    if !found {
        state_fields.push((key, args[2].clone()));
    }

    // Update the state field on the request
    let mut state_updated = false;
    for (k, v) in &mut new_fields {
        if &**k == "state" {
            *v = NValue::record(state_fields.clone());
            state_updated = true;
            break;
        }
    }
    if !state_updated {
        new_fields.push(("state".into(), NValue::record(state_fields)));
    }

    Ok(NValue::record(new_fields))
}

/// Request.state(req, key) -> Option<String>
/// Read a value from the request's `state` sub-record.
pub(super) fn native_request_state(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.state expects 2 arguments (req, key), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.state: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let key = match args[1].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Request.state: key must be String, got {}",
                args[1]
            )));
        }
    };

    let state = fields
        .iter()
        .find(|(k, _)| &**k == "state")
        .and_then(|(_, v)| v.as_record());

    if let Some(state_fields) = state {
        for (k, v) in state_fields {
            if &**k == &**key {
                return Ok(NValue::enum_val("Some".into(), v.clone()));
            }
        }
    }
    Ok(NValue::enum_val("None".into(), NValue::unit()))
}
