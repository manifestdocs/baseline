use super::{NValue, NativeError, RcStr};

/// Find the Authorization header value from a request record.
fn find_auth_header(req: &NValue) -> Option<RcStr> {
    let fields = req.as_record()?;
    let headers_val = fields.iter().find(|(k, _)| &**k == "headers")?.1.clone();
    let headers = headers_val.as_list()?;
    for h in headers {
        if let Some(elems) = h.as_tuple()
            && elems.len() >= 2
            && let Some(name) = elems[0].as_string()
            && name.eq_ignore_ascii_case("authorization")
        {
            return elems[1].as_string().cloned();
        }
    }
    None
}

fn unauthorized_err(msg: &str) -> NValue {
    NValue::enum_val(
        "Err".into(),
        NValue::enum_val("Unauthorized".into(), NValue::string(msg.into())),
    )
}

/// Middleware.extract_bearer(req) -> Result<String, HttpError>
/// Extracts the Bearer token from the Authorization header.
pub(super) fn native_middleware_extract_bearer(args: &[NValue]) -> Result<NValue, NativeError> {
    let auth = match find_auth_header(&args[0]) {
        Some(a) => a,
        None => return Ok(unauthorized_err("Missing Authorization header")),
    };
    let auth_str: &str = &auth;
    if let Some(token) = auth_str.strip_prefix("Bearer ") {
        let trimmed = token.trim();
        if trimmed.is_empty() {
            return Ok(unauthorized_err("Empty Bearer token"));
        }
        Ok(NValue::enum_val(
            "Ok".into(),
            NValue::string(trimmed.into()),
        ))
    } else {
        Ok(unauthorized_err("Expected Bearer authentication scheme"))
    }
}

/// Middleware.extract_basic(req) -> Result<(String, String), HttpError>
/// Extracts and decodes Basic auth credentials from the Authorization header.
pub(super) fn native_middleware_extract_basic(args: &[NValue]) -> Result<NValue, NativeError> {
    let auth = match find_auth_header(&args[0]) {
        Some(a) => a,
        None => return Ok(unauthorized_err("Missing Authorization header")),
    };
    let auth_str: &str = &auth;
    let encoded = match auth_str.strip_prefix("Basic ") {
        Some(e) => e.trim(),
        None => return Ok(unauthorized_err("Expected Basic authentication scheme")),
    };
    let decoded = match base64_decode(encoded) {
        Some(d) => d,
        None => return Ok(unauthorized_err("Invalid Base64 in Basic auth")),
    };
    let decoded_str = match String::from_utf8(decoded) {
        Ok(s) => s,
        Err(_) => return Ok(unauthorized_err("Invalid UTF-8 in Basic auth")),
    };
    match decoded_str.split_once(':') {
        Some((user, pass)) => Ok(NValue::enum_val(
            "Ok".into(),
            NValue::tuple(vec![
                NValue::string(user.into()),
                NValue::string(pass.into()),
            ]),
        )),
        None => Ok(unauthorized_err(
            "Invalid Basic auth format (expected user:pass)",
        )),
    }
}

fn base64_decode(input: &str) -> Option<Vec<u8>> {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = Vec::new();
    let mut buf: u32 = 0;
    let mut bits: u32 = 0;
    for &b in input.as_bytes() {
        if b == b'=' {
            break;
        }
        let val = TABLE.iter().position(|&c| c == b)? as u32;
        buf = (buf << 6) | val;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((buf >> bits) as u8);
            buf &= (1 << bits) - 1;
        }
    }
    Some(out)
}

/// Middleware.cors_config(origins) -> config record
/// Returns a CORS config record suitable for Server.start! config.
pub(super) fn native_middleware_cors_config(args: &[NValue]) -> Result<NValue, NativeError> {
    let origins = args[0].clone();
    if origins.as_list().is_none() {
        return Err(NativeError(
            "Middleware.cors_config: expected List of origin strings".into(),
        ));
    }
    Ok(NValue::record(vec![
        ("allowed_origins".into(), origins),
        (
            "allowed_methods".into(),
            NValue::list(vec![
                NValue::string("GET".into()),
                NValue::string("POST".into()),
                NValue::string("PUT".into()),
                NValue::string("DELETE".into()),
                NValue::string("PATCH".into()),
                NValue::string("OPTIONS".into()),
            ]),
        ),
        (
            "allowed_headers".into(),
            NValue::list(vec![
                NValue::string("Content-Type".into()),
                NValue::string("Authorization".into()),
            ]),
        ),
        ("max_age".into(), NValue::int(86400)),
        ("allow_credentials".into(), NValue::bool(false)),
    ]))
}

/// Middleware.rate_limit_config(requests_per_second, burst_size) -> config record
/// Returns a rate limit config record suitable for Server.start! config.
pub(super) fn native_middleware_rate_limit_config(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args[0].is_any_int() {
        return Err(NativeError(
            "Middleware.rate_limit_config: expected Int for requests_per_second".into(),
        ));
    }
    if !args[1].is_any_int() {
        return Err(NativeError(
            "Middleware.rate_limit_config: expected Int for burst_size".into(),
        ));
    }
    Ok(NValue::record(vec![
        ("requests_per_second".into(), args[0].clone()),
        ("burst_size".into(), args[1].clone()),
    ]))
}
