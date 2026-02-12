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
