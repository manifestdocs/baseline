use std::collections::HashMap;

use tree_sitter::{Node, Tree};

use crate::analysis::refinements::Constraint;
use crate::analysis::TypeMap;
use crate::analysis::Type;

use super::model::*;
use super::schema::build_api_schemas;

/// Extract an ApiModel from parsed and type-checked Baseline source.
pub fn extract_api_model(
    tree: &Tree,
    source: &str,
    type_map: &TypeMap,
    type_defs: &HashMap<String, Type>,
    refined_types: &HashMap<String, Constraint>,
) -> ApiModel {
    let root = tree.root_node();

    // 1. Find all function definitions and their bodies
    let functions = collect_functions(root, source);

    // 2. Extract routes from Router.* calls (resolves function refs for groups)
    let raw_routes = extract_routes(root, source, &functions);

    // 3. For each route, analyze the handler body
    let routes = raw_routes
        .into_iter()
        .map(|raw| {
            let handler_info = functions
                .get(&raw.handler_name)
                .map(|body| analyze_handler(body, source, type_map))
                .unwrap_or_default();

            let mut responses = handler_info.responses;
            if responses.is_empty() {
                responses.push(ApiResponse {
                    status: 200,
                    description: "OK".to_string(),
                    schema: None,
                });
            }

            // Add error responses from handler analysis
            responses.extend(handler_info.error_responses);

            // If handler uses Request.decode/body_json with ?, add 422
            if handler_info.request_body.is_some()
                && !responses.iter().any(|r| r.status == 422)
            {
                responses.push(ApiResponse {
                    status: 422,
                    description: "Unprocessable Entity".to_string(),
                    schema: None,
                });
            }

            // Extract path parameters from the route path
            let mut parameters = extract_path_params(&raw.path);
            parameters.extend(handler_info.query_params);

            // Get summary from comment preceding handler
            let summary = functions
                .get(&raw.handler_name)
                .and_then(|body| extract_comment_summary(body.parent, source));

            ApiRoute {
                method: raw.method,
                path: raw.path,
                operation_id: raw.handler_name,
                summary,
                request_body: handler_info.request_body,
                parameters,
                responses,
            }
        })
        .collect();

    // 4. Build schemas from type definitions + refinements
    let schemas = build_api_schemas(type_defs, refined_types);

    ApiModel { routes, schemas }
}

// ---------------------------------------------------------------------------
// Route extraction
// ---------------------------------------------------------------------------

struct RawRoute {
    method: String,
    path: String,
    handler_name: String,
}

/// Walk the CST for Router.get/post/put/delete/patch calls.
/// Resolves function references via the functions map for Router.group.
/// Only enters the main!/main function body to find the router pipe chain.
fn extract_routes<'a>(
    root: Node<'a>,
    source: &str,
    functions: &HashMap<String, FunctionBody<'a>>,
) -> Vec<RawRoute> {
    let mut routes = Vec::new();

    // Find the main function body and extract routes from there
    for func_name in &["main", "main!"] {
        if let Some(func) = functions.get(*func_name) {
            extract_routes_recursive(func.body, source, "", functions, &mut routes);
        }
    }

    // If no main function found, fall back to scanning all top-level expressions
    if routes.is_empty() {
        extract_routes_recursive(root, source, "", functions, &mut routes);
    }

    routes
}

fn extract_routes_recursive<'a>(
    node: Node<'a>,
    source: &str,
    prefix: &str,
    functions: &HashMap<String, FunctionBody<'a>>,
    routes: &mut Vec<RawRoute>,
) {
    // Look for call_expression nodes
    if node.kind() == "call_expression" {
        if let Some(route) = try_extract_route(node, source, prefix) {
            routes.push(route);
            return; // Don't recurse into this call's children
        }
        // Check for Router.group
        if let Some((group_prefix, func_name)) = try_extract_group_func(node, source) {
            let full_prefix = format!("{}{}", prefix, group_prefix);
            // Resolve the function name to its body and extract routes from there
            if let Some(func) = functions.get(&func_name) {
                extract_routes_recursive(func.body, source, &full_prefix, functions, routes);
            }
            // Don't return -- continue recursing for other children (pipe chain)
        }
        // Check for Router.resources
        if let Some(resource_routes) = try_extract_resources(node, source, prefix) {
            routes.extend(resource_routes);
            return;
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        extract_routes_recursive(child, source, prefix, functions, routes);
    }
}

/// Try to extract a route from a Router.get/post/put/delete/patch/options/head call.
fn try_extract_route(node: Node, source: &str, prefix: &str) -> Option<RawRoute> {
    let (object, method_name) = extract_field_call(node, source)?;
    if object != "Router" {
        return None;
    }

    let http_method = match method_name.as_str() {
        "get" => "GET",
        "post" => "POST",
        "put" => "PUT",
        "delete" => "DELETE",
        "patch" => "PATCH",
        "options" => "OPTIONS",
        "head" => "HEAD",
        _ => return None,
    };

    // Extract arguments: Router.get(path_or_router, path_string, handler)
    // In pipe style: router |> Router.get("/path", handler)
    // The call has the router as first arg (may be piped), then path string, then handler
    let args = collect_call_args(node, source);

    // Find the string literal (path) and identifier (handler) among args
    let mut path = None;
    let mut handler = None;

    for arg in &args {
        match arg {
            CallArg::StringLiteral(s) => {
                if path.is_none() {
                    path = Some(s.clone());
                }
            }
            CallArg::Identifier(name) => {
                handler = Some(name.clone());
            }
            _ => {}
        }
    }

    let path = path?;
    let handler = handler?;

    let full_path = format!("{}{}", prefix, baseline_path_to_openapi(&path));

    Some(RawRoute {
        method: http_method.to_string(),
        path: full_path,
        handler_name: handler,
    })
}

/// Try to extract a Router.group(prefix, func()) call, returning (prefix, function_name).
fn try_extract_group_func(node: Node, source: &str) -> Option<(String, String)> {
    let (object, method_name) = extract_field_call(node, source)?;
    if object != "Router" || method_name != "group" {
        return None;
    }

    let args = collect_call_args(node, source);
    let mut prefix = None;
    let mut func_name = None;

    for arg in &args {
        match arg {
            CallArg::StringLiteral(s) if prefix.is_none() => {
                prefix = Some(s.clone());
            }
            CallArg::Node(n) => {
                // Sub-router expression: call like todo_routes()
                if n.kind() == "call_expression" {
                    let mut cursor = n.walk();
                    for child in n.children(&mut cursor) {
                        if child.kind() == "identifier" {
                            func_name = child
                                .utf8_text(source.as_bytes())
                                .ok()
                                .map(String::from);
                            break;
                        }
                    }
                }
            }
            CallArg::Identifier(name) => {
                func_name = Some(name.clone());
            }
            _ => {}
        }
    }

    Some((prefix?, func_name?))
}

/// Try to extract Router.resources(path, record) -> 5 REST routes.
fn try_extract_resources(node: Node, source: &str, prefix: &str) -> Option<Vec<RawRoute>> {
    let (object, method_name) = extract_field_call(node, source)?;
    if object != "Router" || method_name != "resources" {
        return None;
    }

    let args = collect_call_args(node, source);
    let mut path = None;
    let mut record_fields: HashMap<String, String> = HashMap::new();

    for arg in &args {
        match arg {
            CallArg::StringLiteral(s) => {
                if path.is_none() {
                    path = Some(s.clone());
                }
            }
            CallArg::Identifier(name) => {
                // Used as handler reference in simple cases
                let _ = name;
            }
            _ => {}
        }
    }

    let base_path = format!("{}{}", prefix, baseline_path_to_openapi(&path?));

    // Default REST routes
    let routes = vec![
        RawRoute {
            method: "GET".to_string(),
            path: base_path.clone(),
            handler_name: record_fields.remove("index").unwrap_or_else(|| "index".to_string()),
        },
        RawRoute {
            method: "GET".to_string(),
            path: format!("{}/{{id}}", base_path),
            handler_name: record_fields.remove("show").unwrap_or_else(|| "show".to_string()),
        },
        RawRoute {
            method: "POST".to_string(),
            path: base_path.clone(),
            handler_name: record_fields.remove("create").unwrap_or_else(|| "create".to_string()),
        },
        RawRoute {
            method: "PUT".to_string(),
            path: format!("{}/{{id}}", base_path),
            handler_name: record_fields.remove("update").unwrap_or_else(|| "update".to_string()),
        },
        RawRoute {
            method: "DELETE".to_string(),
            path: format!("{}/{{id}}", base_path),
            handler_name: record_fields.remove("destroy").unwrap_or_else(|| "destroy".to_string()),
        },
    ];

    Some(routes)
}

/// Extract the object and method name from a field_expression call.
/// e.g., `Router.get(...)` returns Some(("Router", "get"))
fn extract_field_call(node: Node, source: &str) -> Option<(String, String)> {
    // call_expression's first child should be a field_expression
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "field_expression" {
            return extract_field_parts(child, source);
        }
    }
    None
}

/// Extract object.method from a field_expression node.
fn extract_field_parts(node: Node, source: &str) -> Option<(String, String)> {
    let mut object = None;
    let mut field = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "type_identifier" | "identifier" if object.is_none() => {
                object = child.utf8_text(source.as_bytes()).ok().map(String::from);
            }
            "identifier" => {
                field = child.utf8_text(source.as_bytes()).ok().map(String::from);
            }
            _ => {}
        }
    }

    Some((object?, field?))
}

/// Convert Baseline path params (:param) to OpenAPI format ({param}).
fn baseline_path_to_openapi(path: &str) -> String {
    path.split('/')
        .map(|segment| {
            if let Some(param) = segment.strip_prefix(':') {
                format!("{{{}}}", param)
            } else {
                segment.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("/")
}

/// Extract path parameters from an OpenAPI-format path.
fn extract_path_params(path: &str) -> Vec<ApiParameter> {
    let mut params = Vec::new();
    for segment in path.split('/') {
        if segment.starts_with('{') && segment.ends_with('}') {
            let name = &segment[1..segment.len() - 1];
            params.push(ApiParameter {
                name: name.to_string(),
                location: ParamLocation::Path,
                param_type: ApiType::String,
                required: true,
            });
        }
    }
    params
}

// ---------------------------------------------------------------------------
// Call argument extraction
// ---------------------------------------------------------------------------

#[derive(Debug)]
enum CallArg<'a> {
    StringLiteral(String),
    Identifier(String),
    Node(Node<'a>),
}

/// Collect meaningful arguments from a call_expression (skipping Router/pipe args).
fn collect_call_args<'a>(node: Node<'a>, source: &str) -> Vec<CallArg<'a>> {
    let mut args = Vec::new();
    let mut cursor = node.walk();
    let mut skip_first = true; // Skip the field_expression (function name)

    for child in node.children(&mut cursor) {
        if skip_first && child.kind() == "field_expression" {
            skip_first = false;
            continue;
        }
        // Skip punctuation
        if child.kind() == "(" || child.kind() == ")" || child.kind() == "," {
            continue;
        }

        match child.kind() {
            "string_literal" => {
                if let Some(val) = extract_string_content(child, source) {
                    args.push(CallArg::StringLiteral(val));
                }
            }
            "literal" => {
                // Unwrap literal > string_literal
                if let Some(inner) = child.named_child(0) {
                    if inner.kind() == "string_literal" {
                        if let Some(val) = extract_string_content(inner, source) {
                            args.push(CallArg::StringLiteral(val));
                        }
                    }
                }
            }
            "identifier" => {
                if let Ok(name) = child.utf8_text(source.as_bytes()) {
                    args.push(CallArg::Identifier(name.to_string()));
                }
            }
            "call_expression" => {
                // Sub-expression like todo_routes()
                args.push(CallArg::Node(child));
            }
            _ => {
                args.push(CallArg::Node(child));
            }
        }
    }

    args
}

/// Extract string content from a string_literal node.
fn extract_string_content(node: Node, source: &str) -> Option<String> {
    let mut result = std::string::String::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "string_content" {
            if let Ok(text) = child.utf8_text(source.as_bytes()) {
                result.push_str(text);
            }
        }
    }
    if result.is_empty() {
        // Fallback: try the raw text minus quotes
        let text = node.utf8_text(source.as_bytes()).ok()?;
        let trimmed = text.trim_matches('"');
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
        None
    } else {
        Some(result)
    }
}

// ---------------------------------------------------------------------------
// Function body collection
// ---------------------------------------------------------------------------

struct FunctionBody<'a> {
    /// The function_def node (for extracting preceding comments)
    parent: Node<'a>,
    /// The body node to analyze for Request/Response calls
    body: Node<'a>,
}

fn collect_functions<'a>(root: Node<'a>, source: &str) -> HashMap<String, FunctionBody<'a>> {
    let mut functions = HashMap::new();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        if child.kind() == "function_def" {
            if let Some(name) = extract_function_name(child, source) {
                if let Some(body) = find_function_body(child) {
                    functions.insert(
                        name,
                        FunctionBody {
                            parent: child,
                            body,
                        },
                    );
                }
            }
        }
    }

    functions
}

fn extract_function_name(node: Node, source: &str) -> Option<String> {
    // function_def > identifier or effect_identifier (for fn main!())
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "identifier" || child.kind() == "effect_identifier" {
            return child.utf8_text(source.as_bytes()).ok().map(String::from);
        }
    }
    None
}

fn find_function_body(node: Node) -> Option<Node> {
    // The body is typically the last named child (block or expression)
    let count = node.named_child_count();
    if count > 0 {
        Some(node.named_child(count - 1)?)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Handler analysis
// ---------------------------------------------------------------------------

#[derive(Default)]
struct HandlerInfo {
    request_body: Option<String>,
    responses: Vec<ApiResponse>,
    error_responses: Vec<ApiResponse>,
    query_params: Vec<ApiParameter>,
}

/// Analyze a handler's body for Request.decode, Response.*, HttpError.* calls.
fn analyze_handler(func: &FunctionBody, source: &str, _type_map: &TypeMap) -> HandlerInfo {
    let mut info = HandlerInfo::default();
    analyze_node(func.body, source, &mut info);
    info
}

fn analyze_node(node: Node, source: &str, info: &mut HandlerInfo) {
    if node.kind() == "call_expression" {
        if let Some((object, method)) = extract_field_call(node, source) {
            match object.as_str() {
                "Request" => analyze_request_call(&method, node, source, info),
                "Response" => analyze_response_call(&method, info),
                "HttpError" => analyze_error_call(&method, info),
                _ => {}
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        analyze_node(child, source, info);
    }
}

fn analyze_request_call(method: &str, node: Node, source: &str, info: &mut HandlerInfo) {
    match method {
        "decode" | "body_json" => {
            // Extract type name from second argument (string literal)
            let args = collect_call_args(node, source);
            for arg in &args {
                if let CallArg::StringLiteral(type_name) = arg {
                    info.request_body = Some(type_name.clone());
                    break;
                }
            }
        }
        "param" => {
            // Request.param(req, "name") -> path parameter
            let args = collect_call_args(node, source);
            for arg in &args {
                if let CallArg::StringLiteral(name) = arg {
                    // Don't add if already extracted from path
                    if !info.query_params.iter().any(|p| p.name == *name) {
                        // This is a path param, already extracted from route path
                    }
                    break;
                }
            }
        }
        "param_int" => {
            // Same as param but typed as Int
            let args = collect_call_args(node, source);
            for arg in &args {
                if let CallArg::StringLiteral(_name) = arg {
                    break;
                }
            }
        }
        "query" => {
            let args = collect_call_args(node, source);
            for arg in &args {
                if let CallArg::StringLiteral(name) = arg {
                    info.query_params.push(ApiParameter {
                        name: name.clone(),
                        location: ParamLocation::Query,
                        param_type: ApiType::String,
                        required: false,
                    });
                    break;
                }
            }
        }
        _ => {}
    }
}

fn analyze_response_call(method: &str, info: &mut HandlerInfo) {
    let response = match method {
        "json" => ApiResponse {
            status: 200,
            description: "OK".to_string(),
            schema: None,
        },
        "created" => ApiResponse {
            status: 201,
            description: "Created".to_string(),
            schema: None,
        },
        "no_content" => ApiResponse {
            status: 204,
            description: "No Content".to_string(),
            schema: None,
        },
        "accepted" => ApiResponse {
            status: 202,
            description: "Accepted".to_string(),
            schema: None,
        },
        _ => return,
    };

    // Avoid duplicate status codes
    if !info.responses.iter().any(|r| r.status == response.status) {
        info.responses.push(response);
    }
}

fn analyze_error_call(method: &str, info: &mut HandlerInfo) {
    let (status, description) = match method {
        "bad_request" => (400, "Bad Request"),
        "unauthorized" => (401, "Unauthorized"),
        "forbidden" => (403, "Forbidden"),
        "not_found" => (404, "Not Found"),
        "conflict" => (409, "Conflict"),
        "unprocessable" => (422, "Unprocessable Entity"),
        "internal" => (500, "Internal Server Error"),
        _ => return,
    };

    if !info.error_responses.iter().any(|r| r.status == status) {
        info.error_responses.push(ApiResponse {
            status,
            description: description.to_string(),
            schema: None,
        });
    }
}

// ---------------------------------------------------------------------------
// Comment extraction
// ---------------------------------------------------------------------------

/// Extract a summary from the comment preceding a function definition.
fn extract_comment_summary(func_node: Node, source: &str) -> Option<String> {
    // Look at the previous sibling for a comment
    let prev = func_node.prev_sibling()?;
    if prev.kind() == "comment" {
        let text = prev.utf8_text(source.as_bytes()).ok()?;
        let trimmed = text.trim_start_matches("//").trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Public API for cross-module extraction (used by mod.rs)
// ---------------------------------------------------------------------------

/// A raw route extracted from CST (public for cross-module use).
pub struct RawRoutePublic {
    pub method: String,
    pub path: String,
    pub handler_name: String,
}

/// Handler analysis result (public for cross-module use).
#[derive(Default)]
pub struct HandlerInfoPublic {
    pub request_body: Option<String>,
    pub responses: Vec<ApiResponse>,
    pub error_responses: Vec<ApiResponse>,
    pub query_params: Vec<ApiParameter>,
}

/// Extract group mappings: function_name -> prefix.
/// e.g., Router.group("/todos", todo_routes()) yields ("todo_routes", "/todos")
pub fn extract_group_mappings(root: Node, source: &str) -> Vec<(String, String)> {
    let mut mappings = Vec::new();
    extract_group_mappings_recursive(root, source, "", &mut mappings);
    mappings
}

fn extract_group_mappings_recursive(
    node: Node,
    source: &str,
    prefix: &str,
    mappings: &mut Vec<(String, String)>,
) {
    if node.kind() == "call_expression" {
        if let Some((object, method)) = extract_field_call(node, source) {
            if object == "Router" && method == "group" {
                let args = collect_call_args(node, source);
                let mut group_prefix = None;
                let mut func_name = None;

                for arg in &args {
                    match arg {
                        CallArg::StringLiteral(s) if group_prefix.is_none() => {
                            group_prefix = Some(s.clone());
                        }
                        CallArg::Node(n) => {
                            // Look for call_expression like todo_routes()
                            if n.kind() == "call_expression" {
                                // Extract the function name from the call
                                let mut cursor = n.walk();
                                for child in n.children(&mut cursor) {
                                    if child.kind() == "identifier" {
                                        func_name = child
                                            .utf8_text(source.as_bytes())
                                            .ok()
                                            .map(String::from);
                                        break;
                                    }
                                }
                            }
                        }
                        CallArg::Identifier(name) => {
                            func_name = Some(name.clone());
                        }
                        _ => {}
                    }
                }

                if let (Some(p), Some(f)) = (group_prefix, func_name) {
                    let full_prefix = format!("{}{}", prefix, p);
                    mappings.push((f, full_prefix));
                }
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        extract_group_mappings_recursive(child, source, prefix, mappings);
    }
}

/// Collect function bodies from a module's root node.
/// Returns function_name -> body_node mapping.
pub fn collect_module_functions<'a>(root: Node<'a>, source: &str) -> HashMap<String, Node<'a>> {
    let mut functions = HashMap::new();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        if child.kind() == "function_def" {
            if let Some(name) = extract_function_name(child, source) {
                if let Some(body) = find_function_body(child) {
                    functions.insert(name, body);
                }
            }
        }
    }

    functions
}

/// Extract routes from a CST node with a given prefix (public for cross-module use).
pub fn extract_routes_from_node(node: Node, source: &str, prefix: &str) -> Vec<RawRoutePublic> {
    let mut routes = Vec::new();
    extract_routes_public_recursive(node, source, prefix, &mut routes);
    routes
}

fn extract_routes_public_recursive(
    node: Node,
    source: &str,
    prefix: &str,
    routes: &mut Vec<RawRoutePublic>,
) {
    if node.kind() == "call_expression" {
        if let Some(route) = try_extract_route(node, source, prefix) {
            routes.push(RawRoutePublic {
                method: route.method,
                path: route.path,
                handler_name: route.handler_name,
            });
            return;
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        extract_routes_public_recursive(child, source, prefix, routes);
    }
}

/// Analyze a handler node for Request/Response/HttpError calls (public for cross-module use).
pub fn analyze_handler_node(node: Node, source: &str) -> HandlerInfoPublic {
    let mut info = HandlerInfo::default();
    analyze_node(node, source, &mut info);
    HandlerInfoPublic {
        request_body: info.request_body,
        responses: info.responses,
        error_responses: info.error_responses,
        query_params: info.query_params,
    }
}

/// Extract path parameters from an OpenAPI-format path (public alias).
pub fn extract_path_params_from(path: &str) -> Vec<ApiParameter> {
    extract_path_params(path)
}

/// Extract the comment summary for a named handler from a module root.
pub fn extract_handler_comment(root: Node, source: &str, handler_name: &str) -> Option<String> {
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "function_def" {
            if let Some(name) = extract_function_name(child, source) {
                if name == handler_name {
                    return extract_comment_summary(child, source);
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_baseline_path_to_openapi() {
        assert_eq!(baseline_path_to_openapi("/users/:id"), "/users/{id}");
        assert_eq!(
            baseline_path_to_openapi("/todos/:id/items/:item_id"),
            "/todos/{id}/items/{item_id}"
        );
        assert_eq!(baseline_path_to_openapi("/health"), "/health");
    }

    #[test]
    fn test_extract_path_params() {
        let params = extract_path_params("/users/{id}/posts/{post_id}");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "id");
        assert_eq!(params[1].name, "post_id");
        assert!(params.iter().all(|p| p.location == ParamLocation::Path));
        assert!(params.iter().all(|p| p.required));
    }

    fn parse_baseline(source: &str) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        parser.parse(source, None).unwrap()
    }

    #[test]
    fn test_extract_routes_simple() {
        let source = r#"
@prelude(server)

fn handler(req: Request) -> {Http} Result<Response, HttpError> =
  Ok(Response.json({ status: "ok" }))

fn main!() -> {Http} () = {
  Router.new()
    |> Router.get("/health", handler)
    |> Server.listen!(8080)
}
"#;
        let tree = parse_baseline(source);
        let functions = collect_functions(tree.root_node(), source);
        let routes = extract_routes(tree.root_node(), source, &functions);
        assert_eq!(routes.len(), 1);
        assert_eq!(routes[0].method, "GET");
        assert_eq!(routes[0].path, "/health");
        assert_eq!(routes[0].handler_name, "handler");
    }

    #[test]
    fn test_extract_routes_multiple() {
        let source = r#"
@prelude(server)

fn list(req: Request) -> {Http} Result<Response, HttpError> = Ok(Response.json([]))
fn create(req: Request) -> {Http} Result<Response, HttpError> = Ok(Response.created({}))
fn get_one(req: Request) -> {Http} Result<Response, HttpError> = Ok(Response.json({}))

fn main!() -> {Http} () = {
  Router.new()
    |> Router.get("/todos", list)
    |> Router.post("/todos", create)
    |> Router.get("/todos/:id", get_one)
    |> Server.listen!(8080)
}
"#;
        let tree = parse_baseline(source);
        let functions = collect_functions(tree.root_node(), source);
        let routes = extract_routes(tree.root_node(), source, &functions);
        assert_eq!(routes.len(), 3);
        assert_eq!(routes[0].method, "GET");
        assert_eq!(routes[0].path, "/todos");
        assert_eq!(routes[1].method, "POST");
        assert_eq!(routes[2].path, "/todos/{id}");
    }

    #[test]
    fn test_analyze_response_calls() {
        let source = r#"
@prelude(server)

fn handler(req: Request) -> {Http} Result<Response, HttpError> = {
  let id = Request.param(req, "id")
  Ok(Response.created({ id: 1 }))
}
"#;
        let tree = parse_baseline(source);
        let functions = collect_functions(tree.root_node(), source);
        let type_map = TypeMap::new();

        let info = functions
            .get("handler")
            .map(|f| analyze_handler(f, source, &type_map))
            .unwrap();

        assert!(info.responses.iter().any(|r| r.status == 201));
    }

    #[test]
    fn test_analyze_error_calls() {
        let source = r#"
@prelude(server)

fn handler(req: Request) -> {Http} Result<Response, HttpError> = {
  Err(HttpError.not_found("Not found"))
}
"#;
        let tree = parse_baseline(source);
        let functions = collect_functions(tree.root_node(), source);
        let type_map = TypeMap::new();

        let info = functions
            .get("handler")
            .map(|f| analyze_handler(f, source, &type_map))
            .unwrap();

        assert!(info.error_responses.iter().any(|r| r.status == 404));
    }

    #[test]
    fn test_extract_routes_with_group() {
        let source = r#"
@prelude(server)

fn health(req: Request) -> {Http} Result<Response, HttpError> =
  Ok(Response.json({ status: "ok" }))

fn list_todos(req: Request) -> {Http} Result<Response, HttpError> = Ok(Response.json([]))
fn create_todo(req: Request) -> {Http} Result<Response, HttpError> = Ok(Response.created({}))

fn todo_routes() -> Unknown = Router.new()
  |> Router.get("/", list_todos)
  |> Router.post("/", create_todo)

fn main!() -> {Http} () = {
  Router.new()
    |> Router.get("/health", health)
    |> Router.group("/todos", todo_routes())
    |> Server.listen!(8080)
}
"#;
        let tree = parse_baseline(source);
        let functions = collect_functions(tree.root_node(), source);
        let routes = extract_routes(tree.root_node(), source, &functions);
        assert_eq!(routes.len(), 3, "expected 3 routes, got: {:?}",
            routes.iter().map(|r| format!("{} {}", r.method, r.path)).collect::<Vec<_>>());
        assert_eq!(routes[0].method, "GET");
        assert_eq!(routes[0].path, "/health");
        assert_eq!(routes[1].path, "/todos/");
        assert_eq!(routes[1].handler_name, "list_todos");
        assert_eq!(routes[2].path, "/todos/");
        assert_eq!(routes[2].handler_name, "create_todo");
    }

    #[test]
    fn test_full_api_model_extraction() {
        let source = r#"
@prelude(server)

type Title = String where String.length(self) >= 1 && String.length(self) <= 100
type Status = String where self == "pending" || self == "done"

fn create_todo(req: Request) -> {Http} Result<Response, HttpError> = {
  let data = Request.decode(req, "CreateTodo")?
  Ok(Response.created(data))
}

fn get_todo(req: Request) -> {Http} Result<Response, HttpError> = {
  let todo = require_todo(1)?
  Ok(Response.json(todo))
}

fn main!() -> {Http} () = {
  Router.new()
    |> Router.post("/todos", create_todo)
    |> Router.get("/todos/:id", get_todo)
    |> Server.listen!(8080)
}
"#;
        let tree = parse_baseline(source);
        let type_map = TypeMap::new();
        let type_defs = HashMap::new();
        let refined_types = crate::analysis::refinements::collect_refined_types(&tree, source);

        let model = extract_api_model(&tree, source, &type_map, &type_defs, &refined_types);

        assert_eq!(model.routes.len(), 2);

        // POST /todos
        let post_route = &model.routes[0];
        assert_eq!(post_route.method, "POST");
        assert_eq!(post_route.path, "/todos");
        assert_eq!(post_route.request_body, Some("CreateTodo".to_string()));
        assert!(post_route.responses.iter().any(|r| r.status == 201));
        assert!(post_route.responses.iter().any(|r| r.status == 422));

        // GET /todos/{id}
        let get_route = &model.routes[1];
        assert_eq!(get_route.method, "GET");
        assert_eq!(get_route.path, "/todos/{id}");
        assert_eq!(get_route.parameters.len(), 1);
        assert_eq!(get_route.parameters[0].name, "id");

        // Schemas from refinements
        assert!(model.schemas.contains_key("Title"));
        assert!(model.schemas.contains_key("Status"));
    }
}
