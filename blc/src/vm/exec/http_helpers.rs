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
