use crate::vm::natives::http_error::http_error_status_code;
use crate::vm::nvalue::{HeapObject, NValue};
use crate::vm::value::RcStr;

pub(crate) fn parse_url_query_nv(url: &str) -> (String, NValue) {
    let (path, query_str) = match url.split_once('?') {
        Some((p, q)) => (p.to_string(), q),
        None => (url.to_string(), ""),
    };

    let mut query_fields: Vec<(RcStr, NValue)> = Vec::new();
    if !query_str.is_empty() {
        for pair in query_str.split('&') {
            if pair.is_empty() {
                continue;
            }
            let (key, value) = match pair.split_once('=') {
                Some((k, v)) => (k.to_string(), v.to_string()),
                None => (pair.to_string(), String::new()),
            };
            query_fields.push((RcStr::from(key.as_str()), NValue::string(value.into())));
        }
    }

    (path, NValue::record(query_fields))
}

pub(crate) fn inject_params_nv(req: &NValue, params: &[(String, String)]) -> NValue {
    let fields = match req.as_record() {
        Some(f) => f,
        None => return req.clone(),
    };
    let param_fields: Vec<(RcStr, NValue)> = params
        .iter()
        .map(|(k, v)| {
            (
                RcStr::from(k.as_str()),
                NValue::string(RcStr::from(v.as_str())),
            )
        })
        .collect();

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "params" {
            *v = NValue::record(param_fields.clone());
            return NValue::record(new_fields);
        }
    }
    new_fields.push(("params".into(), NValue::record(param_fields)));
    NValue::record(new_fields)
}

pub(crate) fn extract_response_nv(value: &NValue) -> (u16, Vec<(String, String)>, String) {
    if value.is_heap() {
        match value.as_heap_ref() {
            HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                return extract_response_nv(payload);
            }
            HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                // Check if the Err payload is an HttpError variant (e.g., BadRequest, NotFound).
                // Map it to the correct HTTP status code with a JSON error body.
                if payload.is_heap() {
                    if let HeapObject::Enum { tag: err_tag, payload: err_msg, .. } = payload.as_heap_ref() {
                        if let Some(status) = http_error_status_code(err_tag) {
                            let msg = match err_msg.as_string() {
                                Some(s) => s.to_string(),
                                None => format!("{}", err_msg),
                            };
                            let body = format!(
                                "{{\"error\":\"{}\",\"message\":\"{}\"}}",
                                err_tag,
                                msg.replace('\\', "\\\\").replace('"', "\\\"")
                            );
                            return (status, vec![("Content-Type".to_string(), "application/json".to_string())], body);
                        }
                    }
                }
                // If the Err payload is a Response record (has status field),
                // extract it as a proper HTTP response instead of returning 500.
                if let Some(fields) = payload.as_record() {
                    if fields.iter().any(|(k, _)| &**k == "status") {
                        return extract_response_nv(payload);
                    }
                }
                return (500, Vec::new(), format!("{}", payload));
            }
            _ => {}
        }
    }

    if let Some(fields) = value.as_record() {
        let has_status = fields.iter().any(|(k, _)| &**k == "status");
        if has_status {
            let status = fields
                .iter()
                .find(|(k, _)| &**k == "status")
                .and_then(|(_, v)| {
                    if v.is_any_int() {
                        Some(v.as_any_int() as u16)
                    } else {
                        None
                    }
                })
                .unwrap_or(200);

            let body = fields
                .iter()
                .find(|(k, _)| &**k == "body")
                .map(|(_, v)| match v.as_string() {
                    Some(s) => s.to_string(),
                    None => format!("{}", v),
                })
                .unwrap_or_default();

            let headers = fields
                .iter()
                .find(|(k, _)| &**k == "headers")
                .and_then(|(_, v)| v.as_list())
                .map(|items| {
                    items
                        .iter()
                        .filter_map(|item| {
                            if item.is_heap()
                                && let HeapObject::Tuple(pair) = item.as_heap_ref()
                                && pair.len() == 2
                            {
                                let k = pair[0].as_string()?.to_string();
                                let v = pair[1].as_string()?.to_string();
                                return Some((k, v));
                            }
                            None
                        })
                        .collect()
                })
                .unwrap_or_default();

            return (status, headers, body);
        }
    }

    if let Some(s) = value.as_string() {
        return (200, Vec::new(), s.to_string());
    }

    (200, Vec::new(), format!("{}", value))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_http_error_bad_request() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("BadRequest".into(), NValue::string("invalid".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 400);
        assert!(body.contains("invalid"));
    }

    #[test]
    fn extract_http_error_not_found() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("NotFound".into(), NValue::string("missing".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 404);
        assert!(body.contains("missing"));
    }

    #[test]
    fn extract_http_error_unauthorized() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("Unauthorized".into(), NValue::string("no token".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 401);
        assert!(body.contains("no token"));
    }

    #[test]
    fn extract_http_error_forbidden() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("Forbidden".into(), NValue::string("denied".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 403);
        assert!(body.contains("denied"));
    }

    #[test]
    fn extract_http_error_conflict() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("Conflict".into(), NValue::string("duplicate".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 409);
        assert!(body.contains("duplicate"));
    }

    #[test]
    fn extract_http_error_unprocessable() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("Unprocessable".into(), NValue::string("bad entity".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 422);
        assert!(body.contains("bad entity"));
    }

    #[test]
    fn extract_http_error_internal() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::enum_val("Internal".into(), NValue::string("oops".into())),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 500);
        assert!(body.contains("oops"));
    }

    #[test]
    fn extract_ok_response_still_works() {
        let ok = NValue::enum_val(
            "Ok".into(),
            NValue::record(vec![
                ("status".into(), NValue::int(200)),
                ("headers".into(), NValue::list(vec![])),
                ("body".into(), NValue::string("hello".into())),
            ]),
        );
        let (status, _, body) = extract_response_nv(&ok);
        assert_eq!(status, 200);
        assert_eq!(body, "hello");
    }

    #[test]
    fn extract_err_string_still_works() {
        let err = NValue::enum_val(
            "Err".into(),
            NValue::string("something failed".into()),
        );
        let (status, _, body) = extract_response_nv(&err);
        assert_eq!(status, 500);
        assert!(body.contains("something failed"));
    }
}
