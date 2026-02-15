use super::{NValue, NativeError, RcStr};

pub(super) fn native_router_new(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::record(vec![
        ("middleware".into(), NValue::list(Vec::new())),
        ("routes".into(), NValue::list(Vec::new())),
    ]))
}

pub(super) fn native_router_routes(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_record() {
        Some(fields) => {
            for (k, v) in fields.iter() {
                if &**k == "routes" {
                    return Ok(v.clone());
                }
            }
            Err(NativeError("Router.routes: no routes field".into()))
        }
        None => Err(NativeError(format!(
            "Router.routes: expected Router, got {}",
            args[0]
        ))),
    }
}

fn router_add_route(method: &str, args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.{}: expected Router, got {}",
                method.to_lowercase(),
                args[0]
            )));
        }
    };
    if args[1].as_string().is_none() {
        return Err(NativeError(format!(
            "Router.{}: expected String path, got {}",
            method.to_lowercase(),
            args[1]
        )));
    }

    let route = NValue::record(vec![
        ("handler".into(), args[2].clone()),
        ("method".into(), NValue::string(method.into())),
        ("path".into(), args[1].clone()),
    ]);

    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "routes" {
            let mut routes = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            routes.push(route.clone());
            new_fields.push((k.clone(), NValue::list(routes)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

pub(super) fn native_router_get(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("GET", args)
}

pub(super) fn native_router_post(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("POST", args)
}

pub(super) fn native_router_put(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("PUT", args)
}

pub(super) fn native_router_delete(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("DELETE", args)
}

pub(super) fn native_router_patch(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("PATCH", args)
}

pub(super) fn native_router_options(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("OPTIONS", args)
}

pub(super) fn native_router_head(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("HEAD", args)
}

/// Router.any(router, path, handler) — registers handler for ALL methods.
pub(super) fn native_router_any(args: &[NValue]) -> Result<NValue, NativeError> {
    let methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD"];
    let mut result = args[0].clone();
    for method in &methods {
        let fields = match result.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(NativeError(format!(
                    "Router.any: expected Router, got {}",
                    result
                )));
            }
        };
        if args[1].as_string().is_none() {
            return Err(NativeError(format!(
                "Router.any: expected String path, got {}",
                args[1]
            )));
        }
        let route = NValue::record(vec![
            ("handler".into(), args[2].clone()),
            ("method".into(), NValue::string(RcStr::from(*method))),
            ("path".into(), args[1].clone()),
        ]);
        let mut new_fields = Vec::new();
        for (k, v) in fields.iter() {
            if &**k == "routes" {
                let mut routes = match v.as_list() {
                    Some(list) => list.clone(),
                    None => Vec::new(),
                };
                routes.push(route.clone());
                new_fields.push((k.clone(), NValue::list(routes)));
            } else {
                new_fields.push((k.clone(), v.clone()));
            }
        }
        result = NValue::record(new_fields);
    }
    Ok(result)
}

pub(super) fn native_router_use(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.use: expected Router, got {}",
                args[0]
            )));
        }
    };
    let mut new_fields = Vec::new();
    for (k, v) in fields.iter() {
        if &**k == "middleware" {
            let mut mw = match v.as_list() {
                Some(list) => list.clone(),
                None => Vec::new(),
            };
            mw.push(args[1].clone());
            new_fields.push((k.clone(), NValue::list(mw)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

pub(super) fn native_router_group(args: &[NValue]) -> Result<NValue, NativeError> {
    // Router.group(router, prefix, sub_router)
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Router.group expects 3 arguments (router, prefix, sub_router), got {}",
            args.len()
        )));
    }
    let parent_fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.group: first arg must be Router, got {}",
                args[0]
            )));
        }
    };
    let prefix = match args[1].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Router.group: second arg must be String prefix, got {}",
                args[1]
            )));
        }
    };
    let sub_fields = match args[2].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.group: third arg must be Router, got {}",
                args[2]
            )));
        }
    };

    // Extract sub-router routes and prepend prefix to each path
    let sub_routes: Vec<NValue> = sub_fields
        .iter()
        .find(|(k, _)| &**k == "routes")
        .and_then(|(_, v)| v.as_list())
        .cloned()
        .unwrap_or_default();

    let prefixed_routes: Vec<NValue> = sub_routes
        .iter()
        .filter_map(|route| {
            let route_fields = route.as_record()?;
            let mut new_fields: Vec<(RcStr, NValue)> = Vec::new();
            for (k, v) in route_fields.iter() {
                if &**k == "path" {
                    if let Some(path) = v.as_string() {
                        let prefixed = format!(
                            "{}{}",
                            prefix.trim_end_matches('/'),
                            if path.starts_with('/') {
                                path.to_string()
                            } else {
                                format!("/{}", path)
                            }
                        );
                        new_fields.push((k.clone(), NValue::string(prefixed.into())));
                    } else {
                        new_fields.push((k.clone(), v.clone()));
                    }
                } else {
                    new_fields.push((k.clone(), v.clone()));
                }
            }
            Some(NValue::record(new_fields))
        })
        .collect();

    // Merge into parent router's routes
    let mut new_fields: Vec<(RcStr, NValue)> = Vec::new();
    for (k, v) in parent_fields.iter() {
        if &**k == "routes" {
            let mut routes = v.as_list().cloned().unwrap_or_default();
            routes.extend(prefixed_routes.clone());
            new_fields.push((k.clone(), NValue::list(routes)));
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    Ok(NValue::record(new_fields))
}

/// Router.state(router, key, value) -> Router
/// Attach a named value to the router's `state` sub-record.
pub(super) fn native_router_state(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.state: expected Router, got {}",
                args[0]
            )));
        }
    };
    let key = match args[1].as_string() {
        Some(s) => s.clone(),
        None => {
            return Err(NativeError(format!(
                "Router.state: key must be String, got {}",
                args[1]
            )));
        }
    };

    let mut new_fields: Vec<(RcStr, NValue)> = Vec::new();
    let mut state_found = false;
    for (k, v) in fields.iter() {
        if &**k == "state" {
            let mut state_rec = v.as_record().cloned().unwrap_or_default();
            let mut key_found = false;
            for (sk, sv) in &mut state_rec {
                if *sk == key {
                    *sv = args[2].clone();
                    key_found = true;
                    break;
                }
            }
            if !key_found {
                state_rec.push((key.clone(), args[2].clone()));
            }
            new_fields.push((k.clone(), NValue::record(state_rec)));
            state_found = true;
        } else {
            new_fields.push((k.clone(), v.clone()));
        }
    }
    if !state_found {
        new_fields.push((
            "state".into(),
            NValue::record(vec![(key, args[2].clone())]),
        ));
    }
    Ok(NValue::record(new_fields))
}

/// Router.docs_json(router) -> String
/// Serializes the route table as a JSON array of {method, path} objects.
pub(super) fn native_router_docs_json(args: &[NValue]) -> Result<NValue, NativeError> {
    let fields = match args[0].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.docs_json: expected Router, got {}",
                args[0]
            )));
        }
    };
    let routes = fields
        .iter()
        .find(|(k, _)| &**k == "routes")
        .and_then(|(_, v)| v.as_list())
        .cloned()
        .unwrap_or_default();

    let mut entries = Vec::new();
    for route in &routes {
        if let Some(rf) = route.as_record() {
            let method = rf.iter().find(|(k, _)| &**k == "method").and_then(|(_, v)| v.as_string());
            let path = rf.iter().find(|(k, _)| &**k == "path").and_then(|(_, v)| v.as_string());
            if let (Some(m), Some(p)) = (method, path) {
                entries.push(format!(r#"{{"method":"{}","path":"{}"}}"#, m, p));
            }
        }
    }
    let json = format!("[{}]", entries.join(","));
    Ok(NValue::string(json.into()))
}

/// Router.ws(router, path, handler) -> Router
/// Registers a WebSocket handler for the given path.
/// Internally stores the route with method "WS" so the server can detect
/// it and perform the HTTP upgrade instead of normal request dispatch.
pub(super) fn native_router_ws(args: &[NValue]) -> Result<NValue, NativeError> {
    router_add_route("WS", args)
}

/// Router.resources(router, path, handlers) -> Router
/// Generates 5 RESTful CRUD routes from a record of handler functions:
///   { index, show, create, update, destroy }
/// Produces:
///   GET    /path          -> index
///   GET    /path/:id      -> show
///   POST   /path          -> create
///   PUT    /path/:id      -> update
///   DELETE /path/:id      -> destroy
pub(super) fn native_router_resources(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.len() != 3 {
        return Err(NativeError(format!(
            "Router.resources expects 3 arguments (router, path, handlers), got {}",
            args.len()
        )));
    }
    let handlers = match args[2].as_record() {
        Some(f) => f,
        None => {
            return Err(NativeError(format!(
                "Router.resources: third arg must be record of handlers, got {}",
                args[2]
            )));
        }
    };
    let base_path = match args[1].as_string() {
        Some(s) => s.to_string(),
        None => {
            return Err(NativeError(format!(
                "Router.resources: second arg must be String path, got {}",
                args[1]
            )));
        }
    };
    let base = base_path.trim_end_matches('/');
    let item_path = format!("{}/:id", base);

    let mut result = args[0].clone();
    let routes: &[(&str, &str, &str)] = &[
        ("index", "GET", base),
        ("show", "GET", &item_path),
        ("create", "POST", base),
        ("update", "PUT", &item_path),
        ("destroy", "DELETE", &item_path),
    ];

    for (handler_name, method, path) in routes {
        if let Some(handler) = handlers.iter().find(|(k, _)| &**k == *handler_name) {
            let route_args = [
                result,
                NValue::string(RcStr::from(*path)),
                handler.1.clone(),
            ];
            result = router_add_route(method, &route_args)?;
        }
    }

    Ok(result)
}
