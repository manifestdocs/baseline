use super::{NValue, NativeError};

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

pub(super) fn native_http_error_service_unavailable(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("ServiceUnavailable", args)
}

pub(super) fn native_http_error_gateway_timeout(args: &[NValue]) -> Result<NValue, NativeError> {
    make_http_error("GatewayTimeout", args)
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
