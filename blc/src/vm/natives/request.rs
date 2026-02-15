use super::{HeapObject, NValue, NativeError};
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
            if item.is_heap()
                && let HeapObject::Tuple(pair) = item.as_heap_ref()
                && pair.len() == 2
                && let Some(k) = pair[0].as_string()
                && k.to_lowercase() == name
            {
                return Ok(NValue::enum_val("Some".into(), pair[1].clone()));
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

/// Request.body_json(req) -> Result<Unknown, HttpError>
/// Parses the request body as JSON. Returns HttpError.bad_request on failure.
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
        None => {
            return Ok(bad_request_err("Request body is not a string"));
        }
    };

    if body_str.is_empty() {
        return Ok(bad_request_err("Request body is empty"));
    }

    match serde_json::from_str::<serde_json::Value>(&body_str) {
        Ok(value) => Ok(NValue::enum_val("Ok".into(), serde_to_nvalue(value))),
        Err(e) => Ok(bad_request_err(&format!("JSON parse error: {}", e))),
    }
}

/// Wrap error message in Err(HttpError.bad_request(msg))
fn bad_request_err(msg: &str) -> NValue {
    NValue::enum_val(
        "Err".into(),
        NValue::enum_val("BadRequest".into(), NValue::string(msg.into())),
    )
}

/// Request.param(req, name) -> Result<String, String>
/// Extract a required, non-empty path parameter by name.
pub(super) fn native_request_param(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.param expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.param: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.param: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let params = fields
        .iter()
        .find(|(k, _)| &**k == "params")
        .and_then(|(_, v)| v.as_record());

    if let Some(param_fields) = params {
        for (k, v) in param_fields {
            if **k == *name {
                if let Some(s) = v.as_string() {
                    if s.is_empty() {
                        return Ok(NValue::enum_val(
                            "Err".into(),
                            NValue::string(format!("Parameter '{}' is empty", name).into()),
                        ));
                    }
                    return Ok(NValue::enum_val("Ok".into(), v.clone()));
                }
                return Ok(NValue::enum_val("Ok".into(), v.clone()));
            }
        }
    }
    Ok(NValue::enum_val(
        "Err".into(),
        NValue::string(format!("Parameter '{}' not found", name).into()),
    ))
}

/// Request.param_int(req, name) -> Result<Int, String>
/// Extract a path parameter and parse it as an integer.
pub(super) fn native_request_param_int(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.param_int expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.param_int: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.param_int: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let params = fields
        .iter()
        .find(|(k, _)| &**k == "params")
        .and_then(|(_, v)| v.as_record());

    if let Some(param_fields) = params {
        for (k, v) in param_fields {
            if **k == *name {
                if let Some(s) = v.as_string() {
                    match s.parse::<i64>() {
                        Ok(n) => return Ok(NValue::enum_val("Ok".into(), NValue::int(n))),
                        Err(_) => {
                            return Ok(NValue::enum_val(
                                "Err".into(),
                                NValue::string(
                                    format!("Parameter '{}' is not a valid integer: '{}'", name, s)
                                        .into(),
                                ),
                            ));
                        }
                    }
                }
                // If it's already an int
                if v.is_any_int() {
                    return Ok(NValue::enum_val("Ok".into(), v.clone()));
                }
                return Ok(NValue::enum_val(
                    "Err".into(),
                    NValue::string(
                        format!("Parameter '{}' is not a valid integer", name).into(),
                    ),
                ));
            }
        }
    }
    Ok(NValue::enum_val(
        "Err".into(),
        NValue::string(format!("Parameter '{}' not found", name).into()),
    ))
}

/// Request.query(req, name) -> Option<String>
/// Extract an optional query parameter by name.
pub(super) fn native_request_query(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.query expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.query: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.query: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let query = fields
        .iter()
        .find(|(k, _)| &**k == "query")
        .and_then(|(_, v)| v.as_record());

    if let Some(query_fields) = query {
        for (k, v) in query_fields {
            if **k == *name {
                return Ok(NValue::enum_val("Some".into(), v.clone()));
            }
        }
    }
    Ok(NValue::enum_val("None".into(), NValue::unit()))
}

/// Request.query_int(req, name) -> Result<Int, String>
/// Extract a query parameter and parse it as an integer.
pub(super) fn native_request_query_int(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 2 {
        return Err(NativeError(format!(
            "Request.query_int expects 2 arguments (req, name), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Request.query_int: first arg must be Request record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Request.query_int: second arg must be String, got {}",
                args[1]
            )));
        }
    };

    let query = fields
        .iter()
        .find(|(k, _)| &**k == "query")
        .and_then(|(_, v)| v.as_record());

    if let Some(query_fields) = query {
        for (k, v) in query_fields {
            if **k == *name {
                if let Some(s) = v.as_string() {
                    match s.parse::<i64>() {
                        Ok(n) => return Ok(NValue::enum_val("Ok".into(), NValue::int(n))),
                        Err(_) => {
                            return Ok(NValue::enum_val(
                                "Err".into(),
                                NValue::string(
                                    format!(
                                        "Query parameter '{}' is not a valid integer: '{}'",
                                        name, s
                                    )
                                    .into(),
                                ),
                            ));
                        }
                    }
                }
                if v.is_any_int() {
                    return Ok(NValue::enum_val("Ok".into(), v.clone()));
                }
                return Ok(NValue::enum_val(
                    "Err".into(),
                    NValue::string(
                        format!("Query parameter '{}' is not a valid integer", name).into(),
                    ),
                ));
            }
        }
    }
    Ok(NValue::enum_val(
        "Err".into(),
        NValue::string(format!("Query parameter '{}' not found", name).into()),
    ))
}

