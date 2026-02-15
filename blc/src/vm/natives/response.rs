use super::{NValue, NativeError, RcStr};

fn make_response(status: i64, headers: Vec<NValue>, body: &str) -> NValue {
    NValue::record(vec![
        ("body".into(), NValue::string(body.into())),
        ("headers".into(), NValue::list(headers)),
        ("status".into(), NValue::int(status)),
    ])
}

/// Auto-serialize: if the value is a String, use it as-is.
/// Otherwise, serialize to JSON and add Content-Type header.
fn make_response_auto(status: i64, val: &NValue) -> Result<NValue, NativeError> {
    use super::json::nvalue_to_serde;
    match val.as_string() {
        Some(s) => Ok(make_response(status, Vec::new(), s)),
        None => {
            let serde_val = nvalue_to_serde(val)?;
            let json = serde_json::to_string(&serde_val)
                .map_err(|e| NativeError(format!("Response: JSON serialization failed: {}", e)))?;
            let headers = vec![NValue::tuple(vec![
                NValue::string("Content-Type".into()),
                NValue::string("application/json".into()),
            ])];
            Ok(make_response(status, headers, &json))
        }
    }
}

pub(super) fn native_response_ok(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.ok expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(200, Vec::new(), body))
}

pub(super) fn native_response_json(args: &[NValue]) -> Result<NValue, NativeError> {
    use super::json::nvalue_to_serde;
    // Accept String as-is, or auto-serialize any other value to JSON
    let body_str: RcStr = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            let serde_val = nvalue_to_serde(&args[0])?;
            let json = serde_json::to_string(&serde_val)
                .map_err(|e| NativeError(format!("Response.json: {}", e)))?;
            RcStr::from(json.as_str())
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Content-Type".into()),
        NValue::string("application/json".into()),
    ])];
    Ok(make_response(200, headers, &body_str))
}

pub(super) fn native_response_created(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(201, &args[0])
}

pub(super) fn native_response_no_content(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(make_response(204, Vec::new(), ""))
}

pub(super) fn native_response_bad_request(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(400, &args[0])
}

pub(super) fn native_response_not_found(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(404, &args[0])
}

pub(super) fn native_response_error(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(500, &args[0])
}

pub(super) fn native_response_status(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_any_int() {
        return Err(NativeError(format!(
            "Response.status expects Int, got {}",
            args[0]
        )));
    }
    let code = args[0].as_any_int();
    let body = match args[1].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.status expects String body, got {}",
                args[1]
            )));
        }
    };
    Ok(make_response(code, Vec::new(), body))
}

pub(super) fn native_response_with_header(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects Response record, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects String header name, got {}",
                args[1]
            )));
        }
    };
    let val = match args[2].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_header expects String header value, got {}",
                args[2]
            )));
        }
    };

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            headers.push(NValue::tuple(vec![
                NValue::string(name),
                NValue::string(val),
            ]));
            *v = NValue::list(headers);
            return Ok(NValue::record(new_fields));
        }
    }
    // No headers field; add one
    new_fields.push((
        "headers".into(),
        NValue::list(vec![NValue::tuple(vec![
            NValue::string(name),
            NValue::string(val),
        ])]),
    ));
    Ok(NValue::record(new_fields))
}

/// Response.with_headers(resp, headers_list) -> Response
/// Bulk-add headers from a list of (name, value) tuples.
pub(super) fn native_response_with_headers(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Response.with_headers: expected Response record, got {}",
                args[0]
            )));
        }
    };
    let new_headers = match args[1].as_list() {
        Some(l) => l.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.with_headers: expected List of headers, got {}",
                args[1]
            )));
        }
    };

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            headers.extend(new_headers);
            *v = NValue::list(headers);
            return Ok(NValue::record(new_fields));
        }
    }
    new_fields.push(("headers".into(), NValue::list(new_headers)));
    Ok(NValue::record(new_fields))
}

/// Response.redirect(url) -> Response with 302 and Location header.
pub(super) fn native_response_redirect(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.redirect: expected String URL, got {}",
                args[0]
            )));
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Location".into()),
        NValue::string(url),
    ])];
    Ok(make_response(302, headers, ""))
}

/// Response.redirect_permanent(url) -> Response with 301 and Location header.
pub(super) fn native_response_redirect_permanent(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.redirect_permanent: expected String URL, got {}",
                args[0]
            )));
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Location".into()),
        NValue::string(url),
    ])];
    Ok(make_response(301, headers, ""))
}

/// Response.redirect_temporary(url) -> Response with 307 and Location header.
pub(super) fn native_response_redirect_temporary(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Response.redirect_temporary: expected String URL, got {}",
                args[0]
            )));
        }
    };
    let headers = vec![NValue::tuple(vec![
        NValue::string("Location".into()),
        NValue::string(url),
    ])];
    Ok(make_response(307, headers, ""))
}

/// Response.unauthorized(val) -> 401 response with auto-serialization.
pub(super) fn native_response_unauthorized(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(401, &args[0])
}

/// Response.forbidden(val) -> 403 response with auto-serialization.
pub(super) fn native_response_forbidden(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(403, &args[0])
}

/// Response.conflict(val) -> 409 response with auto-serialization.
pub(super) fn native_response_conflict(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(409, &args[0])
}

/// Response.unprocessable(val) -> 422 response with auto-serialization.
pub(super) fn native_response_unprocessable(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(422, &args[0])
}

/// Response.too_many_requests(val) -> 429 response with auto-serialization.
pub(super) fn native_response_too_many_requests(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(429, &args[0])
}

/// Response.method_not_allowed(val) -> 405 response with auto-serialization.
pub(super) fn native_response_method_not_allowed(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(405, &args[0])
}

/// Response.bad_gateway(val) -> 502 response with auto-serialization.
pub(super) fn native_response_bad_gateway(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(502, &args[0])
}

/// Response.service_unavailable(val) -> 503 response with auto-serialization.
pub(super) fn native_response_service_unavailable(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(503, &args[0])
}

/// Response.gateway_timeout(val) -> 504 response with auto-serialization.
pub(super) fn native_response_gateway_timeout(args: &[NValue]) -> Result<NValue, NativeError> {
    make_response_auto(504, &args[0])
}

/// Response.set_cookie(resp, name, value) -> Response with Set-Cookie header.
pub(super) fn native_response_set_cookie(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Response.set_cookie expects 3 arguments (response, name, value), got {}",
            args.len()
        )));
    }
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Response.set_cookie: first arg must be Response, got {}",
                args[0]
            )));
        }
    };
    let name = match args[1].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Response.set_cookie: second arg must be String name, got {}",
                args[1]
            )));
        }
    };
    let value = match args[2].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Response.set_cookie: third arg must be String value, got {}",
                args[2]
            )));
        }
    };

    let cookie_str = format!("{}={}; Path=/; HttpOnly", name, value);

    // Append the Set-Cookie header tuple to the response's headers list
    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "headers" {
            let mut headers = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            headers.push(NValue::tuple(vec![
                NValue::string("Set-Cookie".into()),
                NValue::string(cookie_str.into()),
            ]));
            *v = NValue::list(headers);
            return Ok(NValue::record(new_fields));
        }
    }
    // No headers field — add one
    new_fields.push((
        "headers".into(),
        NValue::list(vec![NValue::tuple(vec![
            NValue::string("Set-Cookie".into()),
            NValue::string(cookie_str.into()),
        ])]),
    ));
    Ok(NValue::record(new_fields))
}
