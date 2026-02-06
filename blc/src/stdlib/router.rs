//! Router module — pure functions for building HTTP route tables.
//!
//! The Router is represented as a Record with a `routes` field containing
//! a List of route Records, each with `method`, `path`, and `handler` fields.

use std::collections::HashMap;

use crate::interpreter::RuntimeValue;
use crate::stdlib::NativeRegistry;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Router.new", router_new);
    registry.register("Router.routes", router_routes);
}

/// Router.new() -> Record { routes: [] }
fn router_new<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if !args.is_empty() {
        return Err(format!(
            "Router.new expects 0 arguments, got {}",
            args.len()
        ));
    }
    Ok(empty_router())
}

/// Router.routes(router) -> List of route records
fn router_routes<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Router.routes expects 1 argument, got {}",
            args.len()
        ));
    }
    extract_routes(&args[0]).map(RuntimeValue::List)
}

/// Create an empty router Record.
pub fn empty_router<'a>() -> RuntimeValue<'a> {
    let mut fields = HashMap::new();
    fields.insert("routes".to_string(), RuntimeValue::List(Vec::new()));
    RuntimeValue::Record(fields)
}

/// Add a route to a router, returning a new router.
pub fn add_route<'a>(
    method: &str,
    router: &RuntimeValue<'a>,
    path: &RuntimeValue<'a>,
    handler: &RuntimeValue<'a>,
) -> Result<RuntimeValue<'a>, String> {
    let mut routes = extract_routes(router)?;

    let path_str = match path {
        RuntimeValue::String(_) => {}
        _ => return Err(format!("Route path must be a String, got {}", path)),
    };
    let _ = path_str;

    let mut route_fields = HashMap::new();
    route_fields.insert(
        "method".to_string(),
        RuntimeValue::String(method.to_string()),
    );
    route_fields.insert("path".to_string(), path.clone());
    route_fields.insert("handler".to_string(), handler.clone());

    routes.push(RuntimeValue::Record(route_fields));

    let mut new_router = HashMap::new();
    new_router.insert("routes".to_string(), RuntimeValue::List(routes));
    Ok(RuntimeValue::Record(new_router))
}

/// Extract the routes list from a router Record.
fn extract_routes<'a>(router: &RuntimeValue<'a>) -> Result<Vec<RuntimeValue<'a>>, String> {
    match router {
        RuntimeValue::Record(fields) => match fields.get("routes") {
            Some(RuntimeValue::List(routes)) => Ok(routes.clone()),
            _ => Err("Expected a Router (Record with routes field)".to_string()),
        },
        _ => Err(format!("Expected a Router, got {}", router)),
    }
}

/// Match a request path against a route pattern with `:name` parameter segments.
///
/// Returns `Some(params)` if the pattern matches, where params is a list of
/// `(name, value)` pairs for each `:name` segment. Returns `None` on mismatch.
pub fn match_path(pattern: &str, path: &str) -> Option<Vec<(String, String)>> {
    let pat_segs: Vec<&str> = pattern.split('/').filter(|s| !s.is_empty()).collect();
    let path_segs: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();

    if pat_segs.len() != path_segs.len() {
        return None;
    }

    let mut params = Vec::new();
    for (pat, actual) in pat_segs.iter().zip(path_segs.iter()) {
        if let Some(name) = pat.strip_prefix(':') {
            params.push((name.to_string(), actual.to_string()));
        } else if pat != actual {
            return None;
        }
    }

    Some(params)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_router_has_empty_routes() {
        let router = router_new(&[]).unwrap();
        let routes = router_routes(&[router]).unwrap();
        assert_eq!(routes, RuntimeValue::List(vec![]));
    }

    #[test]
    fn add_route_creates_route_entry() {
        let router = router_new(&[]).unwrap();
        let path = RuntimeValue::String("/hello".to_string());
        let handler = RuntimeValue::String("placeholder".to_string());

        let router = add_route("GET", &router, &path, &handler).unwrap();
        let routes = router_routes(&[router]).unwrap();

        if let RuntimeValue::List(routes) = routes {
            assert_eq!(routes.len(), 1);
            if let RuntimeValue::Record(fields) = &routes[0] {
                assert_eq!(
                    fields.get("method"),
                    Some(&RuntimeValue::String("GET".to_string()))
                );
                assert_eq!(
                    fields.get("path"),
                    Some(&RuntimeValue::String("/hello".to_string()))
                );
            } else {
                panic!("Expected Record route entry");
            }
        } else {
            panic!("Expected List of routes");
        }
    }

    #[test]
    fn match_path_exact() {
        let result = match_path("/hello", "/hello");
        assert_eq!(result, Some(vec![]));
    }

    #[test]
    fn match_path_no_match() {
        assert_eq!(match_path("/hello", "/world"), None);
    }

    #[test]
    fn match_path_different_lengths() {
        assert_eq!(match_path("/a/b", "/a"), None);
        assert_eq!(match_path("/a", "/a/b"), None);
    }

    #[test]
    fn match_path_single_param() {
        let result = match_path("/users/:id", "/users/123");
        assert_eq!(result, Some(vec![("id".to_string(), "123".to_string())]));
    }

    #[test]
    fn match_path_multiple_params() {
        let result = match_path("/users/:id/posts/:post_id", "/users/42/posts/99");
        assert_eq!(
            result,
            Some(vec![
                ("id".to_string(), "42".to_string()),
                ("post_id".to_string(), "99".to_string()),
            ])
        );
    }

    #[test]
    fn match_path_mixed_segments() {
        let result = match_path("/api/users/:id/profile", "/api/users/7/profile");
        assert_eq!(result, Some(vec![("id".to_string(), "7".to_string())]));
    }

    #[test]
    fn match_path_param_segment_mismatch() {
        // Fixed prefix doesn't match
        assert_eq!(match_path("/api/users/:id", "/api/posts/1"), None);
    }

    #[test]
    fn add_multiple_routes() {
        let router = router_new(&[]).unwrap();
        let handler = RuntimeValue::String("placeholder".to_string());

        let router = add_route(
            "GET",
            &router,
            &RuntimeValue::String("/a".to_string()),
            &handler,
        )
        .unwrap();
        let router = add_route(
            "POST",
            &router,
            &RuntimeValue::String("/b".to_string()),
            &handler,
        )
        .unwrap();

        let routes = router_routes(&[router]).unwrap();
        if let RuntimeValue::List(routes) = routes {
            assert_eq!(routes.len(), 2);
        } else {
            panic!("Expected List");
        }
    }
}
