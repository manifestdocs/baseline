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
    fields.insert("middleware".to_string(), RuntimeValue::List(Vec::new()));
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
    new_router.insert(
        "middleware".to_string(),
        RuntimeValue::List(extract_middleware(router)?),
    );
    Ok(RuntimeValue::Record(new_router))
}

/// Add a middleware function to a router, returning a new router.
pub fn add_middleware<'a>(
    router: &RuntimeValue<'a>,
    middleware_fn: &RuntimeValue<'a>,
) -> Result<RuntimeValue<'a>, String> {
    let routes = extract_routes(router)?;
    let mut middleware = extract_middleware(router)?;
    middleware.push(middleware_fn.clone());

    let mut new_router = HashMap::new();
    new_router.insert("routes".to_string(), RuntimeValue::List(routes));
    new_router.insert("middleware".to_string(), RuntimeValue::List(middleware));
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

/// Extract the middleware list from a router Record.
/// Returns empty list if field is missing (backwards compatibility).
pub fn extract_middleware<'a>(router: &RuntimeValue<'a>) -> Result<Vec<RuntimeValue<'a>>, String> {
    match router {
        RuntimeValue::Record(fields) => match fields.get("middleware") {
            Some(RuntimeValue::List(mw)) => Ok(mw.clone()),
            _ => Ok(Vec::new()),
        },
        _ => Err(format!("Expected a Router, got {}", router)),
    }
}

// ---------------------------------------------------------------------------
// Radix tree — segment-based trie for O(log n) route matching
// ---------------------------------------------------------------------------

/// A segment-based radix tree for fast URL routing.
///
/// Each level corresponds to one path segment. Exact segments are tried first,
/// then `:name` parameter segments (wildcard). Handlers are stored at leaf
/// nodes, keyed by HTTP method. First-registered-wins for duplicate routes.
pub struct RadixTree<'a> {
    root: RadixNode<'a>,
}

struct RadixNode<'a> {
    /// Children keyed by exact path segment.
    children: HashMap<String, RadixNode<'a>>,
    /// Parameter child: (param_name, child_node). At most one per node.
    param: Option<(String, Box<RadixNode<'a>>)>,
    /// Handlers at this path, keyed by HTTP method.
    handlers: HashMap<String, RuntimeValue<'a>>,
}

impl<'a> RadixNode<'a> {
    fn new() -> Self {
        RadixNode {
            children: HashMap::new(),
            param: None,
            handlers: HashMap::new(),
        }
    }
}

impl<'a> RadixTree<'a> {
    pub fn new() -> Self {
        RadixTree {
            root: RadixNode::new(),
        }
    }

    /// Insert a route into the tree. First-registered-wins: duplicate
    /// method+path combinations are silently ignored.
    pub fn insert(&mut self, method: &str, path: &str, handler: RuntimeValue<'a>) {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        let mut node = &mut self.root;

        for seg in &segments {
            if let Some(name) = seg.strip_prefix(':') {
                if node.param.is_none() {
                    node.param = Some((name.to_string(), Box::new(RadixNode::new())));
                }
                node = node.param.as_mut().unwrap().1.as_mut();
            } else {
                node = node
                    .children
                    .entry(seg.to_string())
                    .or_insert_with(RadixNode::new);
            }
        }

        // First registered wins
        node.handlers
            .entry(method.to_string())
            .or_insert(handler);
    }

    /// Find a handler matching the given method and path.
    /// Returns the handler and any captured path parameters.
    /// Exact segments take priority over parameter segments.
    pub fn find(
        &self,
        method: &str,
        path: &str,
    ) -> Option<(RuntimeValue<'a>, Vec<(String, String)>)> {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        let mut params = Vec::new();
        find_in_node(&self.root, &segments, method, &mut params)
    }
}

fn find_in_node<'a>(
    node: &RadixNode<'a>,
    segments: &[&str],
    method: &str,
    params: &mut Vec<(String, String)>,
) -> Option<(RuntimeValue<'a>, Vec<(String, String)>)> {
    if segments.is_empty() {
        return node
            .handlers
            .get(method)
            .map(|h| (h.clone(), params.clone()));
    }

    let seg = segments[0];
    let rest = &segments[1..];

    // Try exact match first (more specific)
    if let Some(child) = node.children.get(seg) {
        if let result @ Some(_) = find_in_node(child, rest, method, params) {
            return result;
        }
    }

    // Try parameter match
    if let Some((name, child)) = &node.param {
        params.push((name.clone(), seg.to_string()));
        if let result @ Some(_) = find_in_node(child, rest, method, params) {
            return result;
        }
        params.pop(); // backtrack
    }

    None
}

/// Compile a flat route list into a radix tree for fast lookup.
pub fn compile_routes<'a>(routes: &[RuntimeValue<'a>]) -> RadixTree<'a> {
    let mut tree = RadixTree::new();
    for route in routes {
        if let RuntimeValue::Record(fields) = route {
            let method = match fields.get("method") {
                Some(RuntimeValue::String(m)) => m.as_str(),
                _ => continue,
            };
            let path = match fields.get("path") {
                Some(RuntimeValue::String(p)) => p.as_str(),
                _ => continue,
            };
            let handler = match fields.get("handler") {
                Some(h) => h.clone(),
                None => continue,
            };
            tree.insert(method, path, handler);
        }
    }
    tree
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

    #[test]
    fn new_router_has_empty_middleware() {
        let router = router_new(&[]).unwrap();
        let mw = extract_middleware(&router).unwrap();
        assert!(mw.is_empty());
    }

    #[test]
    fn add_middleware_stores_function() {
        let router = router_new(&[]).unwrap();
        let mw_fn = RuntimeValue::String("placeholder_mw".to_string());
        let router = add_middleware(&router, &mw_fn).unwrap();
        let mw = extract_middleware(&router).unwrap();
        assert_eq!(mw.len(), 1);
        assert_eq!(mw[0], RuntimeValue::String("placeholder_mw".to_string()));
    }

    #[test]
    fn add_route_preserves_middleware() {
        let router = router_new(&[]).unwrap();
        let mw_fn = RuntimeValue::String("mw".to_string());
        let router = add_middleware(&router, &mw_fn).unwrap();
        let handler = RuntimeValue::String("handler".to_string());
        let router = add_route("GET", &router, &RuntimeValue::String("/a".to_string()), &handler).unwrap();

        // Both middleware and routes preserved
        let mw = extract_middleware(&router).unwrap();
        assert_eq!(mw.len(), 1);
        let routes = router_routes(&[router]).unwrap();
        if let RuntimeValue::List(routes) = routes {
            assert_eq!(routes.len(), 1);
        } else {
            panic!("Expected List");
        }
    }

    #[test]
    fn add_multiple_middleware() {
        let router = router_new(&[]).unwrap();
        let router = add_middleware(&router, &RuntimeValue::String("mw1".to_string())).unwrap();
        let router = add_middleware(&router, &RuntimeValue::String("mw2".to_string())).unwrap();
        let mw = extract_middleware(&router).unwrap();
        assert_eq!(mw.len(), 2);
        assert_eq!(mw[0], RuntimeValue::String("mw1".to_string()));
        assert_eq!(mw[1], RuntimeValue::String("mw2".to_string()));
    }

    // -----------------------------------------------------------------------
    // Radix tree tests
    // -----------------------------------------------------------------------

    #[test]
    fn radix_tree_exact_match() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/hello", RuntimeValue::String("h".to_string()));
        let result = tree.find("GET", "/hello");
        assert_eq!(result, Some((RuntimeValue::String("h".to_string()), vec![])));
    }

    #[test]
    fn radix_tree_no_match_wrong_path() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/hello", RuntimeValue::String("h".to_string()));
        assert_eq!(tree.find("GET", "/world"), None);
    }

    #[test]
    fn radix_tree_no_match_wrong_method() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/hello", RuntimeValue::String("h".to_string()));
        assert_eq!(tree.find("POST", "/hello"), None);
    }

    #[test]
    fn radix_tree_single_param() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/users/:id", RuntimeValue::String("u".to_string()));
        let result = tree.find("GET", "/users/42");
        assert_eq!(
            result,
            Some((
                RuntimeValue::String("u".to_string()),
                vec![("id".to_string(), "42".to_string())]
            ))
        );
    }

    #[test]
    fn radix_tree_multiple_params() {
        let mut tree = RadixTree::new();
        tree.insert(
            "GET",
            "/users/:id/posts/:post_id",
            RuntimeValue::String("p".to_string()),
        );
        let result = tree.find("GET", "/users/7/posts/99");
        assert_eq!(
            result,
            Some((
                RuntimeValue::String("p".to_string()),
                vec![
                    ("id".to_string(), "7".to_string()),
                    ("post_id".to_string(), "99".to_string()),
                ]
            ))
        );
    }

    #[test]
    fn radix_tree_exact_beats_param() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/users/:id", RuntimeValue::String("param".to_string()));
        tree.insert("GET", "/users/me", RuntimeValue::String("exact".to_string()));
        // Exact match should win over param
        let result = tree.find("GET", "/users/me");
        assert_eq!(
            result,
            Some((RuntimeValue::String("exact".to_string()), vec![]))
        );
        // Other values still match param
        let result = tree.find("GET", "/users/42");
        assert_eq!(
            result,
            Some((
                RuntimeValue::String("param".to_string()),
                vec![("id".to_string(), "42".to_string())]
            ))
        );
    }

    #[test]
    fn radix_tree_different_methods_same_path() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/api", RuntimeValue::String("get".to_string()));
        tree.insert("POST", "/api", RuntimeValue::String("post".to_string()));
        assert_eq!(
            tree.find("GET", "/api"),
            Some((RuntimeValue::String("get".to_string()), vec![]))
        );
        assert_eq!(
            tree.find("POST", "/api"),
            Some((RuntimeValue::String("post".to_string()), vec![]))
        );
    }

    #[test]
    fn radix_tree_first_registered_wins() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/hello", RuntimeValue::String("first".to_string()));
        tree.insert("GET", "/hello", RuntimeValue::String("second".to_string()));
        let result = tree.find("GET", "/hello");
        assert_eq!(
            result,
            Some((RuntimeValue::String("first".to_string()), vec![]))
        );
    }

    #[test]
    fn radix_tree_root_path() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/", RuntimeValue::String("root".to_string()));
        let result = tree.find("GET", "/");
        assert_eq!(
            result,
            Some((RuntimeValue::String("root".to_string()), vec![]))
        );
    }

    #[test]
    fn radix_tree_different_lengths_no_match() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/a/b", RuntimeValue::String("ab".to_string()));
        assert_eq!(tree.find("GET", "/a"), None);
        assert_eq!(tree.find("GET", "/a/b/c"), None);
    }

    #[test]
    fn radix_tree_compile_routes() {
        let mut route_fields = HashMap::new();
        route_fields.insert("method".to_string(), RuntimeValue::String("GET".to_string()));
        route_fields.insert("path".to_string(), RuntimeValue::String("/hello".to_string()));
        route_fields.insert("handler".to_string(), RuntimeValue::String("h".to_string()));

        let routes = vec![RuntimeValue::Record(route_fields)];
        let tree = compile_routes(&routes);
        assert_eq!(
            tree.find("GET", "/hello"),
            Some((RuntimeValue::String("h".to_string()), vec![]))
        );
    }

    #[test]
    fn radix_tree_trailing_slash_normalized() {
        let mut tree = RadixTree::new();
        tree.insert("GET", "/hello/", RuntimeValue::String("h".to_string()));
        // Both with and without trailing slash should match
        assert!(tree.find("GET", "/hello").is_some());
        assert!(tree.find("GET", "/hello/").is_some());
    }
}
