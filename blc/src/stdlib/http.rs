use std::collections::HashMap;
use crate::interpreter::RuntimeValue;
use super::NativeRegistry;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Http.get!", http_get);
    registry.register("Http.post!", http_post);
    registry.register("Http.put!", http_put);
    registry.register("Http.delete!", http_delete);
    registry.register("Http.request!", http_request);
}

/// Build a Response record: { status: Int, headers: List<(String, String)>, body: String }
fn build_response<'a>(status: u16, headers: Vec<(String, String)>, body: String) -> RuntimeValue<'a> {
    let header_tuples = headers.into_iter()
        .map(|(k, v)| RuntimeValue::Tuple(vec![
            RuntimeValue::String(k),
            RuntimeValue::String(v),
        ]))
        .collect();

    let mut fields = HashMap::new();
    fields.insert("status".to_string(), RuntimeValue::Int(status as i64));
    fields.insert("headers".to_string(), RuntimeValue::List(header_tuples));
    fields.insert("body".to_string(), RuntimeValue::String(body));
    RuntimeValue::Record(fields)
}

/// Execute a GET/HEAD/DELETE request (no body) and return a Response record.
fn call_without_body<'a>(builder: ureq::RequestBuilder<ureq::typestate::WithoutBody>, url: &str) -> Result<RuntimeValue<'a>, String> {
    let mut resp = builder.call()
        .map_err(|e| format!("Http request failed for \"{}\": {}", url, e))?;
    let status = resp.status().as_u16();
    let headers: Vec<(String, String)> = resp.headers().iter()
        .map(|(name, value)| (name.to_string(), value.to_str().unwrap_or("").to_string()))
        .collect();
    let body = resp.body_mut().read_to_string()
        .map_err(|e| format!("Failed to read response body: {}", e))?;
    Ok(build_response(status, headers, body))
}

/// Execute a POST/PUT/PATCH request (with body) and return a Response record.
fn call_with_body<'a>(builder: ureq::RequestBuilder<ureq::typestate::WithBody>, url: &str, body: &[u8]) -> Result<RuntimeValue<'a>, String> {
    let mut resp = builder.send(body)
        .map_err(|e| format!("Http request failed for \"{}\": {}", url, e))?;
    let status = resp.status().as_u16();
    let headers: Vec<(String, String)> = resp.headers().iter()
        .map(|(name, value)| (name.to_string(), value.to_str().unwrap_or("").to_string()))
        .collect();
    let resp_body = resp.body_mut().read_to_string()
        .map_err(|e| format!("Failed to read response body: {}", e))?;
    Ok(build_response(status, headers, resp_body))
}

fn http_get<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Http.get! expects 1 argument, got {}", args.len()));
    }
    let url = match &args[0] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.get! expects String url, got {}", other)),
    };
    call_without_body(ureq::get(url), url)
}

fn http_post<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!("Http.post! expects 2 arguments, got {}", args.len()));
    }
    let url = match &args[0] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.post! expects String url, got {}", other)),
    };
    let body = match &args[1] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.post! expects String body, got {}", other)),
    };
    call_with_body(ureq::post(url), url, body.as_bytes())
}

fn http_put<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!("Http.put! expects 2 arguments, got {}", args.len()));
    }
    let url = match &args[0] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.put! expects String url, got {}", other)),
    };
    let body = match &args[1] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.put! expects String body, got {}", other)),
    };
    call_with_body(ureq::put(url), url, body.as_bytes())
}

fn http_delete<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Http.delete! expects 1 argument, got {}", args.len()));
    }
    let url = match &args[0] {
        RuntimeValue::String(s) => s.as_str(),
        other => return Err(format!("Http.delete! expects String url, got {}", other)),
    };
    call_without_body(ureq::delete(url), url)
}

fn http_request<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Http.request! expects 1 argument (Request record), got {}", args.len()));
    }
    let fields = match &args[0] {
        RuntimeValue::Record(f) => f,
        other => return Err(format!("Http.request! expects a Request record, got {}", other)),
    };

    let method = match fields.get("method") {
        Some(RuntimeValue::String(m)) => m.to_uppercase(),
        _ => return Err("Http.request! Request record must have 'method' field (String)".to_string()),
    };
    let url = match fields.get("url") {
        Some(RuntimeValue::String(u)) => u.clone(),
        _ => return Err("Http.request! Request record must have 'url' field (String)".to_string()),
    };
    let body = match fields.get("body") {
        Some(RuntimeValue::String(b)) => b.clone(),
        _ => String::new(),
    };
    let req_headers: Vec<(String, String)> = match fields.get("headers") {
        Some(RuntimeValue::List(items)) => {
            items.iter().filter_map(|item| {
                if let RuntimeValue::Tuple(pair) = item
                    && pair.len() == 2
                        && let (RuntimeValue::String(k), RuntimeValue::String(v)) = (&pair[0], &pair[1]) {
                            return Some((k.clone(), v.clone()));
                        }
                None
            }).collect()
        }
        _ => Vec::new(),
    };

    match method.as_str() {
        "GET" | "HEAD" | "DELETE" => {
            let mut builder = match method.as_str() {
                "GET" => ureq::get(&url),
                "HEAD" => ureq::head(&url),
                "DELETE" => ureq::delete(&url),
                _ => unreachable!(),
            };
            for (k, v) in &req_headers {
                builder = builder.header(k.as_str(), v.as_str());
            }
            call_without_body(builder, &url)
        }
        "POST" | "PUT" | "PATCH" => {
            let mut builder = match method.as_str() {
                "POST" => ureq::post(&url),
                "PUT" => ureq::put(&url),
                "PATCH" => ureq::patch(&url),
                _ => unreachable!(),
            };
            for (k, v) in &req_headers {
                builder = builder.header(k.as_str(), v.as_str());
            }
            call_with_body(builder, &url, body.as_bytes())
        }
        other => Err(format!("Http.request! unsupported method: {}", other)),
    }
}
