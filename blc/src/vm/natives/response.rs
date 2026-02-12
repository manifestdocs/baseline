use super::{NValue, NativeError, RcStr};

fn make_response(status: i64, headers: Vec<NValue>, body: &str) -> NValue {
    NValue::record(vec![
        ("body".into(), NValue::string(body.into())),
        ("headers".into(), NValue::list(headers)),
        ("status".into(), NValue::int(status)),
    ])
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
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.created expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(201, Vec::new(), body))
}

pub(super) fn native_response_no_content(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(make_response(204, Vec::new(), ""))
}

pub(super) fn native_response_bad_request(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.bad_request expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(400, Vec::new(), body))
}

pub(super) fn native_response_not_found(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.not_found expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(404, Vec::new(), body))
}

pub(super) fn native_response_error(args: &[NValue]) -> Result<NValue, NativeError> {
    let body = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Response.error expects String body, got {}",
                args[0]
            )));
        }
    };
    Ok(make_response(500, Vec::new(), body))
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
