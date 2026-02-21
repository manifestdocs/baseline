use super::{NValue, NativeError, RcStr};

fn make_http_error(tag: &str, args: &[NValue]) -> Result<NValue, NativeError> {
    let msg = match args[0].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "HttpError.{} expects String message, got {}",
                tag.to_lowercase(),
                args[0]
            )));
        }
    };
    Ok(NValue::enum_val(tag.into(), NValue::string(msg)))
}

pub(super) fn native_http_error_bad_request(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("BadRequest", args)
}

pub(super) fn native_http_error_not_found(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("NotFound", args)
}

pub(super) fn native_http_error_unauthorized(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("Unauthorized", args)
}

pub(super) fn native_http_error_forbidden(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("Forbidden", args)
}

pub(super) fn native_http_error_conflict(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("Conflict", args)
}

pub(super) fn native_http_error_unprocessable(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("Unprocessable", args)
}

pub(super) fn native_http_error_internal(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("Internal", args)
}

pub(super) fn native_http_error_method_not_allowed(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("MethodNotAllowed", args)
}

pub(super) fn native_http_error_too_many_requests(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("TooManyRequests", args)
}

pub(super) fn native_http_error_bad_gateway(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("BadGateway", args)
}

pub(super) fn native_http_error_service_unavailable(
    args: &[NValue],
) -> Result<NValue, NativeError> {
    make_http_error("ServiceUnavailable", args)
}

pub(super) fn native_http_error_gateway_timeout(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("GatewayTimeout", args)
}

// ---------------------------------------------------------------------------
// HttpError enrichment: with_context, with_code, to_response
// ---------------------------------------------------------------------------

/// Extract tag and payload from an HttpError enum value.
/// Returns (tag, payload) or None if not an HttpError enum.
fn unwrap_http_error(val: &NValue) -> Option<(RcStr, NValue)> {
    let (tag, payload) = val.as_enum()?;
    if http_error_status_code(tag).is_some() {
        Some((tag.clone(), payload.clone()))
    } else {
        None
    }
}

/// Ensure the payload is a record with at least a "message" field.
/// If payload is a string, promote it to { message: string }.
fn ensure_record_payload(_tag: &RcStr, payload: &NValue) -> Vec<(RcStr, NValue)> {
    if let Some(fields) = payload.as_record() {
        fields.to_vec()
    } else {
        let msg = payload.as_str().unwrap_or("Unknown error");
        vec![("message".into(), NValue::string(msg.into()))]
    }
}

/// `HttpError.with_context(error, key, value)` → HttpError
/// Adds a context field to the error's record payload.
pub(super) fn native_http_error_with_context(args: &[NValue]) -> Result<NValue, NativeError> {
    let (tag, payload) = unwrap_http_error(&args[0]).ok_or_else(|| {
        NativeError("HttpError.with_context: first argument must be an HttpError".into())
    })?;
    let key = args[1]
        .as_string()
        .ok_or_else(|| NativeError("HttpError.with_context: key must be a String".into()))?;

    let mut fields = ensure_record_payload(&tag, &payload);

    // Find or create context list
    let ctx_idx = fields.iter().position(|(k, _)| &**k == "context");
    let ctx_entry = NValue::record(vec![
        ("key".into(), NValue::string(key.clone())),
        ("value".into(), args[2].clone()),
    ]);

    if let Some(idx) = ctx_idx {
        let existing = fields[idx].1.as_list().cloned().unwrap_or_default();
        let mut items = existing;
        items.push(ctx_entry);
        fields[idx].1 = NValue::list(items);
    } else {
        fields.push(("context".into(), NValue::list(vec![ctx_entry])));
    }

    Ok(NValue::enum_val(tag, NValue::record(fields)))
}

/// `HttpError.with_code(error, code)` → HttpError
/// Sets a machine-readable error code on the error payload.
pub(super) fn native_http_error_with_code(args: &[NValue]) -> Result<NValue, NativeError> {
    let (tag, payload) = unwrap_http_error(&args[0]).ok_or_else(|| {
        NativeError("HttpError.with_code: first argument must be an HttpError".into())
    })?;
    let code = args[1]
        .as_string()
        .ok_or_else(|| NativeError("HttpError.with_code: code must be a String".into()))?;

    let mut fields = ensure_record_payload(&tag, &payload);

    // Replace or add the "code" field
    if let Some(idx) = fields.iter().position(|(k, _)| &**k == "code") {
        fields[idx].1 = NValue::string(code.clone());
    } else {
        fields.push(("code".into(), NValue::string(code.clone())));
    }

    Ok(NValue::enum_val(tag, NValue::record(fields)))
}

/// `HttpError.to_response(error)` → Response
/// Converts an HttpError into a structured JSON response record.
pub(super) fn native_http_error_to_response(args: &[NValue]) -> Result<NValue, NativeError> {
    let (tag, payload) = unwrap_http_error(&args[0]).ok_or_else(|| {
        NativeError("HttpError.to_response: argument must be an HttpError".into())
    })?;
    Ok(http_error_to_response_record(&tag, &payload))
}

/// Build a Response record from an HttpError tag and payload.
/// Used by both `HttpError.to_response` and the hyper_server error rendering.
pub(crate) fn http_error_to_response_record(tag: &str, payload: &NValue) -> NValue {
    let status = http_error_status_code(tag).unwrap_or(500);
    let title = http_error_title(tag);

    // Build JSON body
    let mut body_parts: Vec<String> = Vec::new();

    if let Some(fields) = payload.as_record() {
        // Structured payload with message, code, context
        let msg = fields
            .iter()
            .find(|(k, _)| &**k == "message")
            .and_then(|(_, v)| v.as_str())
            .unwrap_or(title);
        body_parts.push(format!("\"error\":\"{title}\""));
        body_parts.push(format!("\"message\":\"{}\"", escape_json_string(msg)));
        body_parts.push(format!("\"status\":{status}"));

        if let Some(code_val) = fields.iter().find(|(k, _)| &**k == "code")
            && let Some(code) = code_val.1.as_str()
        {
            body_parts.push(format!("\"code\":\"{}\"", escape_json_string(code)));
        }

        if let Some(ctx_val) = fields.iter().find(|(k, _)| &**k == "context")
            && let Some(ctx_list) = ctx_val.1.as_list()
        {
            let mut ctx_parts: Vec<String> = Vec::new();
            for item in ctx_list {
                if let Some(ctx_fields) = item.as_record() {
                    let k = ctx_fields
                        .iter()
                        .find(|(k, _)| &**k == "key")
                        .and_then(|(_, v)| v.as_str())
                        .unwrap_or("?");
                    let v = ctx_fields
                        .iter()
                        .find(|(k, _)| &**k == "value")
                        .map(|(_, v)| format_json_value(v))
                        .unwrap_or_else(|| "null".to_string());
                    ctx_parts.push(format!("\"{}\":{}", escape_json_string(k), v));
                }
            }
            if !ctx_parts.is_empty() {
                body_parts.push(format!("\"context\":{{{}}}", ctx_parts.join(",")));
            }
        }
    } else {
        // Simple string payload
        let msg = payload.as_str().unwrap_or(title);
        body_parts.push(format!("\"error\":\"{title}\""));
        body_parts.push(format!("\"message\":\"{}\"", escape_json_string(msg)));
        body_parts.push(format!("\"status\":{status}"));
    }

    let body = format!("{{{}}}", body_parts.join(","));

    NValue::record(vec![
        ("status".into(), NValue::int(status as i64)),
        ("body".into(), NValue::string(body.into())),
        (
            "headers".into(),
            NValue::list(vec![NValue::tuple(vec![
                NValue::string("Content-Type".into()),
                NValue::string("application/json".into()),
            ])]),
        ),
    ])
}

fn escape_json_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn format_json_value(val: &NValue) -> String {
    if let Some(s) = val.as_str() {
        format!("\"{}\"", escape_json_string(s))
    } else if val.is_any_int() {
        format!("{}", val.as_any_int())
    } else if val.is_bool() {
        format!("{}", val.as_bool())
    } else {
        "null".to_string()
    }
}

/// Map an HttpError variant tag to a human-readable title.
pub(crate) fn http_error_title(tag: &str) -> &'static str {
    match tag {
        "BadRequest" => "Bad Request",
        "Unauthorized" => "Unauthorized",
        "Forbidden" => "Forbidden",
        "NotFound" => "Not Found",
        "MethodNotAllowed" => "Method Not Allowed",
        "Conflict" => "Conflict",
        "Unprocessable" => "Unprocessable Entity",
        "TooManyRequests" => "Too Many Requests",
        "Internal" => "Internal Server Error",
        "BadGateway" => "Bad Gateway",
        "ServiceUnavailable" => "Service Unavailable",
        "GatewayTimeout" => "Gateway Timeout",
        _ => "Unknown Error",
    }
}

/// Map an HttpError variant tag to its HTTP status code.
/// Returns None if the tag is not a known HttpError variant.
pub(crate) fn http_error_status_code(tag: &str) -> Option<u16> {
    match tag {
        "BadRequest" => Some(400),
        "Unauthorized" => Some(401),
        "Forbidden" => Some(403),
        "NotFound" => Some(404),
        "MethodNotAllowed" => Some(405),
        "Conflict" => Some(409),
        "Unprocessable" => Some(422),
        "TooManyRequests" => Some(429),
        "Internal" => Some(500),
        "BadGateway" => Some(502),
        "ServiceUnavailable" => Some(503),
        "GatewayTimeout" => Some(504),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn with_context_adds_context_list() {
        let err = NValue::enum_val("BadRequest".into(), NValue::string("invalid input".into()));
        let enriched = native_http_error_with_context(&[
            err,
            NValue::string("field".into()),
            NValue::string("email".into()),
        ])
        .unwrap();
        let (tag, payload) = enriched.as_enum().unwrap();
        assert_eq!(&**tag, "BadRequest");
        let fields = payload.as_record().unwrap();
        let msg = fields.iter().find(|(k, _)| &**k == "message").unwrap();
        assert_eq!(msg.1.as_str().unwrap(), "invalid input");
        let ctx = fields.iter().find(|(k, _)| &**k == "context").unwrap();
        let items = ctx.1.as_list().unwrap();
        assert_eq!(items.len(), 1);
    }

    #[test]
    fn with_code_sets_code() {
        let err = NValue::enum_val("NotFound".into(), NValue::string("missing".into()));
        let enriched =
            native_http_error_with_code(&[err, NValue::string("RESOURCE_NOT_FOUND".into())])
                .unwrap();
        let (tag, payload) = enriched.as_enum().unwrap();
        assert_eq!(&**tag, "NotFound");
        let fields = payload.as_record().unwrap();
        let code = fields.iter().find(|(k, _)| &**k == "code").unwrap();
        assert_eq!(code.1.as_str().unwrap(), "RESOURCE_NOT_FOUND");
    }

    #[test]
    fn to_response_produces_response_record() {
        let err = NValue::enum_val("Unauthorized".into(), NValue::string("bad token".into()));
        let resp = native_http_error_to_response(&[err]).unwrap();
        let fields = resp.as_record().unwrap();
        let status = fields.iter().find(|(k, _)| &**k == "status").unwrap();
        assert_eq!(status.1.as_any_int(), 401);
        let body = fields.iter().find(|(k, _)| &**k == "body").unwrap();
        let body_str = body.1.as_str().unwrap();
        assert!(body_str.contains("\"error\":\"Unauthorized\""));
        assert!(body_str.contains("\"message\":\"bad token\""));
    }

    #[test]
    fn to_response_with_enriched_error() {
        let err = NValue::enum_val("BadRequest".into(), NValue::string("fail".into()));
        let err = native_http_error_with_code(&[err, NValue::string("VALIDATION".into())]).unwrap();
        let err = native_http_error_with_context(&[
            err,
            NValue::string("field".into()),
            NValue::string("email".into()),
        ])
        .unwrap();
        let resp = native_http_error_to_response(&[err]).unwrap();
        let fields = resp.as_record().unwrap();
        let status = fields.iter().find(|(k, _)| &**k == "status").unwrap();
        assert_eq!(status.1.as_any_int(), 400);
        let body = fields.iter().find(|(k, _)| &**k == "body").unwrap();
        let body_str = body.1.as_str().unwrap();
        assert!(body_str.contains("\"code\":\"VALIDATION\""));
        assert!(body_str.contains("\"context\""));
    }
}
