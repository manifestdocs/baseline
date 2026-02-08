//! Async HTTP server using Hyper + Tokio for high-performance request handling.
//!
//! This module provides an async alternative to the tiny_http-based server,
//! targeting performance comparable to Axum and Go Fiber.

use bytes::Bytes;
use http_body_util::{BodyExt, Full};
use hyper::body::Incoming;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, Response, StatusCode};
use std::collections::HashMap;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;

use crate::vm::nvalue::NValue;
use crate::vm::value::RcStr;

// ---------------------------------------------------------------------------
// Async Server Configuration
// ---------------------------------------------------------------------------

/// Configuration for the async HTTP server.
#[derive(Clone)]
pub struct ServerConfig {
    /// Number of worker tasks (defaults to CPU count)
    pub workers: usize,
    /// Enable HTTP keep-alive
    pub keep_alive: bool,
    /// Request body size limit (bytes)
    pub max_body_size: usize,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            workers: std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4),
            keep_alive: true,
            max_body_size: 1024 * 1024, // 1MB default
        }
    }
}

// ---------------------------------------------------------------------------
// Route Tree (thread-safe)
// ---------------------------------------------------------------------------

/// Thread-safe radix tree for route matching.
/// Uses Arc for shared ownership across async tasks.
#[derive(Clone)]
pub struct AsyncRouteTree {
    root: Arc<AsyncRadixNode>,
}

#[derive(Default)]
struct AsyncRadixNode {
    children: HashMap<String, AsyncRadixNode>,
    param: Option<(String, Box<AsyncRadixNode>)>,
    handlers: HashMap<String, AsyncHandler>,
}

/// Handler representation for async execution.
/// Can be either a VM function reference or a native fast path.
#[derive(Clone)]
pub enum AsyncHandler {
    /// VM function handler (uses SendableHandler for thread safety)
    Vm(crate::vm::async_executor::SendableHandler),
    /// Native fast path for common patterns (JSON response, etc.)
    Native(Arc<dyn Fn(&AsyncRequest) -> AsyncResponse + Send + Sync>),
}

// ---------------------------------------------------------------------------
// Request/Response Types (zero-copy where possible)
// ---------------------------------------------------------------------------

/// Async request wrapper with lazy parsing.
pub struct AsyncRequest {
    /// HTTP method
    pub method: Method,
    /// Request path (without query string)
    pub path: String,
    /// Query string parameters (lazily parsed)
    query: Option<HashMap<String, String>>,
    /// Raw query string
    raw_query: Option<String>,
    /// Request headers (reference to avoid copy)
    pub headers: hyper::HeaderMap,
    /// Request body (Bytes for zero-copy)
    pub body: Bytes,
    /// Route parameters extracted during matching
    pub params: HashMap<String, String>,
}

impl AsyncRequest {
    /// Create from Hyper request.
    pub async fn from_hyper(req: Request<Incoming>) -> Result<Self, hyper::Error> {
        let method = req.method().clone();
        let uri = req.uri();
        let path = uri.path().to_string();
        let raw_query = uri.query().map(|q| q.to_string());
        let headers = req.headers().clone();

        // Collect body into Bytes (zero-copy if already contiguous)
        let body = req.collect().await?.to_bytes();

        Ok(Self {
            method,
            path,
            query: None,
            raw_query,
            headers,
            body,
            params: HashMap::new(),
        })
    }

    /// Create a test request (for unit testing).
    #[cfg(test)]
    pub fn test_request(
        method: Method,
        path: &str,
        query: Option<&str>,
        body: &str,
        params: HashMap<String, String>,
    ) -> Self {
        Self {
            method,
            path: path.to_string(),
            query: None,
            raw_query: query.map(|s| s.to_string()),
            headers: hyper::HeaderMap::new(),
            body: Bytes::from(body.to_string()),
            params,
        }
    }

    /// Get query parameters (lazily parsed).
    pub fn query(&mut self) -> &HashMap<String, String> {
        if self.query.is_none() {
            self.query = Some(parse_query_string(self.raw_query.as_deref()));
        }
        self.query.as_ref().unwrap()
    }

    /// Get body as string (UTF-8).
    pub fn body_str(&self) -> Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(&self.body)
    }

    /// Convert to NValue record for VM execution.
    pub fn to_nvalue(&self) -> NValue {
        let headers: Vec<NValue> = self
            .headers
            .iter()
            .map(|(k, v)| {
                NValue::tuple(vec![
                    NValue::string(RcStr::from(k.to_string())),
                    NValue::string(RcStr::from(v.to_str().unwrap_or("").to_string())),
                ])
            })
            .collect();

        let params: Vec<(RcStr, NValue)> = self
            .params
            .iter()
            .map(|(k, v)| {
                (
                    RcStr::from(k.as_str()),
                    NValue::string(RcStr::from(v.as_str())),
                )
            })
            .collect();

        let query_params: Vec<(RcStr, NValue)> = self
            .raw_query
            .as_ref()
            .map(|q| {
                parse_query_string(Some(q))
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            RcStr::from(k.as_str()),
                            NValue::string(RcStr::from(v.as_str())),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();

        NValue::record(vec![
            (
                "method".into(),
                NValue::string(RcStr::from(self.method.to_string())),
            ),
            ("url".into(), NValue::string(RcStr::from(self.path.clone()))),
            ("headers".into(), NValue::list(headers)),
            (
                "body".into(),
                NValue::string(RcStr::from(self.body_str().unwrap_or("").to_string())),
            ),
            ("params".into(), NValue::record(params)),
            ("query".into(), NValue::record(query_params)),
        ])
    }

    /// Convert to SendableValue record for cross-thread transfer.
    /// This avoids intermediate NValue/RcStr allocations.
    pub fn to_sendable(&self) -> crate::vm::async_executor::SendableValue {
        use crate::vm::async_executor::SendableValue;

        let headers: Vec<SendableValue> = self
            .headers
            .iter()
            .map(|(k, v)| {
                SendableValue::Tuple(vec![
                    SendableValue::String(k.to_string()),
                    SendableValue::String(v.to_str().unwrap_or("").to_string()),
                ])
            })
            .collect();

        // Convert params map to vec of (String, SendableValue)
        let params_vec: Vec<(String, SendableValue)> = self
            .params
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    SendableValue::String(v.clone()),
                )
            })
            .collect();
        let params_sv = SendableValue::Record(params_vec);

        // Convert query if present
        let query_vec: Vec<(String, SendableValue)> = self
            .raw_query
            .as_ref()
            .map(|q| {
                parse_query_string(Some(q))
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            k,
                            SendableValue::String(v),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();
        let query_sv = SendableValue::Record(query_vec);

        SendableValue::Record(vec![
            (
                "method".to_string(),
                SendableValue::String(self.method.to_string()),
            ),
            ("url".to_string(), SendableValue::String(self.path.clone())),
            ("headers".to_string(), SendableValue::List(headers)),
            (
                "body".to_string(),
                SendableValue::String(self.body_str().unwrap_or("").to_string()),
            ),
            ("params".to_string(), params_sv),
            ("query".to_string(), query_sv),
        ])
    }
}

/// Async response builder with pre-allocated buffers.
pub struct AsyncResponse {
    pub status: StatusCode,
    pub headers: Vec<(String, String)>,
    pub body: Bytes,
}

impl AsyncResponse {
    /// Create a simple text response.
    pub fn text(status: u16, body: impl Into<Bytes>) -> Self {
        Self {
            status: StatusCode::from_u16(status).unwrap_or(StatusCode::OK),
            headers: vec![("Content-Type".into(), "text/plain".into())],
            body: body.into(),
        }
    }

    /// Create a JSON response.
    pub fn json(body: impl Into<Bytes>) -> Self {
        Self {
            status: StatusCode::OK,
            headers: vec![("Content-Type".into(), "application/json".into())],
            body: body.into(),
        }
    }

    /// Create from NValue response record.
    pub fn from_nvalue(val: &NValue) -> Self {
        if let Some(fields) = val.as_record() {
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
                .and_then(|(_, v)| v.as_str())
                .map(|s| Bytes::from(s.to_string()))
                .unwrap_or_default();

            let headers: Vec<(String, String)> = fields
                .iter()
                .find(|(k, _)| &**k == "headers")
                .and_then(|(_, v)| v.as_list())
                .map(|list| {
                    list.iter()
                        .filter_map(|item| {
                            if let Some(tuple) = item.as_tuple() {
                                if tuple.len() == 2 {
                                    let k = tuple[0].as_str()?;
                                    let v = tuple[1].as_str()?;
                                    return Some((k.to_string(), v.to_string()));
                                }
                            }
                            None
                        })
                        .collect()
                })
                .unwrap_or_default();

            Self {
                status: StatusCode::from_u16(status).unwrap_or(StatusCode::OK),
                headers,
                body,
            }
        } else {
            // Fallback: treat as string body
            let body = val.as_str().map(|s| s.to_string()).unwrap_or_default();
            Self::text(200, body)
        }
    }


    /// Create from SendableValue response.
    /// Avoids converting back to NValue on the IO thread.
    pub fn from_sendable_val(val: &crate::vm::async_executor::SendableValue) -> Self {
        use crate::vm::async_executor::SendableValue;

        if let SendableValue::Record(fields) = val {
            let status = fields
                .iter()
                .find(|(k, _)| k == "status")
                .and_then(|(_, v)| {
                    // Check for Int or AnyInt (usually Int in SendableValue)
                    if let SendableValue::Int(i) = v {
                        Some(*i as u16)
                    } else {
                        None
                    }
                })
                .unwrap_or(200);

            let body = fields
                .iter()
                .find(|(k, _)| k == "body")
                .and_then(|(_, v)| {
                    if let SendableValue::String(s) = v {
                        Some(Bytes::from(s.clone()))
                    } else {
                        None
                    }
                })
                .unwrap_or_default();

            let headers: Vec<(String, String)> = fields
                .iter()
                .find(|(k, _)| k == "headers")
                .and_then(|(_, v)| {
                    if let SendableValue::List(list) = v {
                        Some(list.iter()
                            .filter_map(|item| {
                                if let SendableValue::Tuple(tuple) = item {
                                    if tuple.len() == 2 {
                                        let k = if let SendableValue::String(s) = &tuple[0] { s } else { return None; };
                                        let v = if let SendableValue::String(s) = &tuple[1] { s } else { return None; };
                                        return Some((k.clone(), v.clone()));
                                    }
                                }
                                None
                            })
                            .collect())
                    } else {
                        None
                    }
                })
                .unwrap_or_default();

            Self {
                status: StatusCode::from_u16(status).unwrap_or(StatusCode::OK),
                headers,
                body,
            }
        } else {
             // Fallback: treat as string body
             let body = if let SendableValue::String(s) = val {
                 Bytes::from(s.clone())
             } else {
                 Bytes::new()
             };
             Self::text(200, body)
        }
    }

    /// Convert to Hyper response.
    pub fn into_hyper(self) -> Response<Full<Bytes>> {
        let mut builder = Response::builder().status(self.status);

        for (name, value) in &self.headers {
            builder = builder.header(name.as_str(), value.as_str());
        }

        builder.body(Full::new(self.body)).unwrap()
    }
}

// ---------------------------------------------------------------------------
// Query String Parser
// ---------------------------------------------------------------------------

fn parse_query_string(query: Option<&str>) -> HashMap<String, String> {
    let mut map = HashMap::new();
    if let Some(q) = query {
        for pair in q.split('&') {
            if let Some((key, value)) = pair.split_once('=') {
                // URL decode (basic)
                let key = urlencoding_decode(key);
                let value = urlencoding_decode(value);
                map.insert(key, value);
            }
        }
    }
    map
}

fn urlencoding_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            let hex: String = chars.by_ref().take(2).collect();
            if hex.len() == 2 {
                if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                    result.push(byte as char);
                    continue;
                }
            }
            result.push('%');
            result.push_str(&hex);
        } else if c == '+' {
            result.push(' ');
        } else {
            result.push(c);
        }
    }
    result
}

// ---------------------------------------------------------------------------
// Route Matching
// ---------------------------------------------------------------------------

/// Builder for constructing AsyncRouteTree.
pub struct AsyncRouteTreeBuilder {
    root: AsyncRadixNode,
}

impl AsyncRouteTreeBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            root: AsyncRadixNode::default(),
        }
    }

    /// Add a route with the given method, path, and handler.
    pub fn route(mut self, method: &str, path: &str, handler: AsyncHandler) -> Self {
        let segments: Vec<&str> = path
            .trim_start_matches('/')
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        self.insert_route(&segments, method, handler);
        self
    }

    /// Add a native GET route.
    pub fn get<F>(self, path: &str, handler: F) -> Self
    where
        F: Fn(&AsyncRequest) -> AsyncResponse + Send + Sync + 'static,
    {
        self.route("GET", path, AsyncHandler::Native(Arc::new(handler)))
    }

    /// Add a native POST route.
    pub fn post<F>(self, path: &str, handler: F) -> Self
    where
        F: Fn(&AsyncRequest) -> AsyncResponse + Send + Sync + 'static,
    {
        self.route("POST", path, AsyncHandler::Native(Arc::new(handler)))
    }

    /// Build the immutable route tree.
    pub fn build(self) -> AsyncRouteTree {
        AsyncRouteTree {
            root: Arc::new(self.root),
        }
    }

    fn insert_route(&mut self, segments: &[&str], method: &str, handler: AsyncHandler) {
        let mut current = &mut self.root;

        for &segment in segments {
            if segment.starts_with(':') {
                // Parameter segment
                let param_name = segment[1..].to_string();
                if current.param.is_none() {
                    current.param = Some((param_name.clone(), Box::new(AsyncRadixNode::default())));
                }
                let (_, child) = current.param.as_mut().unwrap();
                current = child.as_mut();
            } else {
                // Exact segment
                current = current
                    .children
                    .entry(segment.to_string())
                    .or_insert_with(AsyncRadixNode::default);
            }
        }

        current.handlers.insert(method.to_string(), handler);
    }
}

impl Default for AsyncRouteTreeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl AsyncRouteTree {
    /// Create empty route tree.
    pub fn new() -> Self {
        Self {
            root: Arc::new(AsyncRadixNode::default()),
        }
    }

    /// Create a builder for constructing route trees.
    pub fn builder() -> AsyncRouteTreeBuilder {
        AsyncRouteTreeBuilder::new()
    }

    /// Find a handler for the given method and path.
    /// Returns the handler and extracted path parameters.
    pub fn find(
        &self,
        method: &str,
        path: &str,
    ) -> Option<(AsyncHandler, HashMap<String, String>)> {
        let segments: Vec<&str> = path
            .trim_start_matches('/')
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        let mut params = HashMap::new();
        let node = self.find_node(&self.root, &segments, &mut params)?;

        let handler = node.handlers.get(method)?;
        Some((handler.clone(), params))
    }

    fn find_node<'a>(
        &self,
        node: &'a AsyncRadixNode,
        segments: &[&str],
        params: &mut HashMap<String, String>,
    ) -> Option<&'a AsyncRadixNode> {
        if segments.is_empty() {
            return Some(node);
        }

        let segment = segments[0];
        let rest = &segments[1..];

        // Try exact match first
        if let Some(child) = node.children.get(segment) {
            if let Some(found) = self.find_node(child, rest, params) {
                return Some(found);
            }
        }

        // Try parameter match
        if let Some((param_name, child)) = &node.param {
            params.insert(param_name.clone(), segment.to_string());
            if let Some(found) = self.find_node(child, rest, params) {
                return Some(found);
            }
            params.remove(param_name);
        }

        None
    }

    /// Create from NValue router record.
    pub fn from_nvalue(router: &NValue) -> Result<Self, String> {
        let fields = router.as_record().ok_or("Router must be a record")?;
        let routes = fields
            .iter()
            .find(|(k, _)| &**k == "routes")
            .and_then(|(_, v)| v.as_list())
            .ok_or("Router must have 'routes' list")?;

        let mut builder = Self::builder();

        for route in routes {
            if let Some(route_fields) = route.as_record() {
                let method = route_fields
                    .iter()
                    .find(|(k, _)| &**k == "method")
                    .and_then(|(_, v)| v.as_str())
                    .ok_or("Route must have 'method' string")?;
                let path = route_fields
                    .iter()
                    .find(|(k, _)| &**k == "path")
                    .and_then(|(_, v)| v.as_str())
                    .ok_or("Route must have 'path' string")?;
                let handler_val = route_fields
                    .iter()
                    .find(|(k, _)| &**k == "handler")
                    .map(|(_, v)| v)
                    .ok_or("Route must have 'handler'")?;

                if let Some(sendable) = crate::vm::async_executor::SendableHandler::from_nvalue(handler_val) {
                    builder = builder.route(method, path, AsyncHandler::Vm(sendable));
                } else {
                     return Err(format!("Invalid handler for route {} {}", method, path));
                }
            }
        }
        Ok(builder.build())
    }
}

impl Default for AsyncRouteTree {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Async Server Entry Point
// ---------------------------------------------------------------------------

use crate::vm::async_executor::{AsyncExecutorConfig, AsyncVmExecutor, SendableHandler};
use crate::vm::chunk::Chunk;

/// Context for the async server, holding shared state.
pub struct AsyncServerContext {
    /// Route tree for path matching
    pub routes: Arc<AsyncRouteTree>,
    /// VM executor for running handlers (None for native-only mode)
    pub executor: Option<Arc<AsyncVmExecutor>>,
    /// Middleware chain (VM handlers)
    pub middleware: Vec<SendableHandler>,
}

impl AsyncServerContext {
    /// Create a native-only server context (no VM execution).
    pub fn native_only(routes: AsyncRouteTree) -> Self {
        Self {
            routes: Arc::new(routes),
            executor: None,
            middleware: Vec::new(),
        }
    }

    /// Create a full server context with VM executor.
    pub fn with_executor(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
    ) -> Self {
        let executor = AsyncVmExecutor::new(chunks, AsyncExecutorConfig::default());
        Self {
            routes: Arc::new(routes),
            executor: Some(Arc::new(executor)),
            middleware,
        }
    }
}

/// Start the async HTTP server.
/// This is the main entry point called from the VM.
pub async fn run_server(
    addr: SocketAddr,
    route_tree: AsyncRouteTree,
    _config: ServerConfig,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let ctx = Arc::new(AsyncServerContext::native_only(route_tree));
    run_server_with_context(addr, ctx).await
}

/// Start the async HTTP server with full context (including VM executor).
pub async fn run_server_with_context(
    addr: SocketAddr,
    ctx: Arc<AsyncServerContext>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let listener = TcpListener::bind(addr).await?;

    eprintln!("[server] Listening on http://{}", addr);

    loop {
        let (stream, _) = listener.accept().await?;
        let io = hyper_util::rt::TokioIo::new(stream);
        let ctx = Arc::clone(&ctx);

        tokio::task::spawn(async move {
            let service = service_fn(move |req| {
                let ctx = Arc::clone(&ctx);
                async move { handle_request_with_context(req, &ctx).await }
            });

            if let Err(err) = http1::Builder::new().serve_connection(io, service).await {
                eprintln!("[server] Connection error: {:?}", err);
            }
        });
    }
}

async fn handle_request_with_context(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
) -> Result<Response<Full<Bytes>>, Infallible> {
    let method = req.method().to_string();
    let path = req.uri().path().to_string();

    // Parse request
    let mut async_req = match AsyncRequest::from_hyper(req).await {
        Ok(r) => r,
        Err(_) => {
            return Ok(AsyncResponse::text(500, "Failed to read request").into_hyper());
        }
    };

    // Find handler
    match ctx.routes.find(&method, &path) {
        Some((handler, params)) => {
            async_req.params = params;

            let response = match handler {
                AsyncHandler::Native(f) => f(&async_req),
                AsyncHandler::Vm(vm_handler) => {
                    // Execute VM handler via async executor
                    if let Some(executor) = &ctx.executor {
                        use crate::vm::async_executor::SendableValue;

                        // Convert request to sendable format DIRECTLY
                        let request_sv = async_req.to_sendable();

                        match executor.execute_handler(vm_handler, request_sv).await {
                            Ok(result_sv) => {
                                // Convert result from sendable format DIRECTLY
                                AsyncResponse::from_sendable_val(&result_sv)
                            }
                            Err(e) => {
                                eprintln!("[server] Handler error: {}", e);
                                AsyncResponse::text(500, "Internal Server Error")
                            }
                        }
                    } else {
                        AsyncResponse::text(501, "VM execution not configured")
                    }
                }
            };

            Ok(response.into_hyper())
        }
        None => Ok(AsyncResponse::text(404, "Not Found").into_hyper()),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_string_parsing() {
        let query = parse_query_string(Some("foo=bar&baz=qux"));
        assert_eq!(query.get("foo"), Some(&"bar".to_string()));
        assert_eq!(query.get("baz"), Some(&"qux".to_string()));
    }

    #[test]
    fn test_query_string_url_decode() {
        let query = parse_query_string(Some("name=hello%20world&value=a%2Bb"));
        assert_eq!(query.get("name"), Some(&"hello world".to_string()));
        assert_eq!(query.get("value"), Some(&"a+b".to_string()));
    }

    #[test]
    fn test_async_response_text() {
        let resp = AsyncResponse::text(200, "Hello");
        assert_eq!(resp.status, StatusCode::OK);
        assert_eq!(resp.body, Bytes::from("Hello"));
    }

    #[test]
    fn test_async_response_json() {
        let resp = AsyncResponse::json(r#"{"ok":true}"#);
        assert_eq!(resp.status, StatusCode::OK);
        assert!(
            resp.headers
                .iter()
                .any(|(k, v)| k == "Content-Type" && v == "application/json")
        );
    }

    #[test]
    fn test_urlencoding_decode() {
        assert_eq!(urlencoding_decode("hello%20world"), "hello world");
        assert_eq!(urlencoding_decode("a+b"), "a b");
        assert_eq!(urlencoding_decode("100%25"), "100%");
    }
}
