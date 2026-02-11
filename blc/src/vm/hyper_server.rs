//! Async HTTP server using Hyper + Tokio for high-performance request handling.
//!
//! This module provides an async alternative to the tiny_http-based server,
//! targeting performance comparable to Axum and Go Fiber.
//!
//! Phase 2-3 features: connection limits, keep-alive tuning, graceful shutdown,
//! request timeouts, body size enforcement, HTTP/2 support.

use bytes::Bytes;
use http_body_util::Full;
use hyper::body::Incoming;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, Response, StatusCode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Infallible;
use std::net::{IpAddr, SocketAddr};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::net::TcpListener;
use tokio::sync::Semaphore;

use crate::vm::nvalue::NValue;
use crate::vm::radix::{ParamCollector, RadixNode, SmallParams};
use crate::vm::value::RcStr;
use crate::vm::vm::Vm;

// Thread-local VM for inline handler execution on Tokio worker threads.
thread_local! {
    static HANDLER_VM: RefCell<Vm> = RefCell::new(Vm::new());
}

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
    /// Keep-alive timeout (how long to hold idle connections open)
    pub keep_alive_timeout: Duration,
    /// Request body size limit (bytes)
    pub max_body_size: usize,
    /// Maximum total concurrent connections
    pub max_connections: usize,
    /// Maximum connections from a single IP address
    pub max_connections_per_ip: usize,
    /// Request processing timeout (time for handler to complete)
    pub request_timeout: Duration,
    /// Graceful shutdown timeout (time to drain existing connections)
    pub shutdown_timeout: Duration,
    /// Enable HTTP/2 support (auto-detect HTTP/1.1 or HTTP/2)
    pub enable_http2: bool,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            workers: std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4),
            keep_alive: true,
            keep_alive_timeout: Duration::from_secs(75),
            max_body_size: 1024 * 1024, // 1MB default
            max_connections: 10_000,
            max_connections_per_ip: 256,
            request_timeout: Duration::from_secs(30),
            shutdown_timeout: Duration::from_secs(30),
            enable_http2: false,
        }
    }
}

// ---------------------------------------------------------------------------
// Connection Limiter
// ---------------------------------------------------------------------------

/// Tracks and limits connections globally and per-IP.
struct ConnectionLimiter {
    /// Global connection semaphore
    global: Arc<Semaphore>,
    /// Per-IP connection counts
    per_ip: Arc<std::sync::Mutex<HashMap<IpAddr, usize>>>,
    /// Max connections per IP
    max_per_ip: usize,
}

/// RAII guard that decrements counts when a connection closes.
struct ConnectionGuard {
    ip: IpAddr,
    per_ip: Arc<std::sync::Mutex<HashMap<IpAddr, usize>>>,
    _permit: tokio::sync::OwnedSemaphorePermit,
}

impl Drop for ConnectionGuard {
    fn drop(&mut self) {
        let mut map = self.per_ip.lock().unwrap();
        if let Some(count) = map.get_mut(&self.ip) {
            *count -= 1;
            if *count == 0 {
                map.remove(&self.ip);
            }
        }
    }
}

impl ConnectionLimiter {
    fn new(max_connections: usize, max_per_ip: usize) -> Self {
        Self {
            global: Arc::new(Semaphore::new(max_connections)),
            per_ip: Arc::new(std::sync::Mutex::new(HashMap::new())),
            max_per_ip: max_per_ip,
        }
    }

    /// Try to acquire a connection slot. Returns a guard on success, None if at capacity.
    fn try_acquire(&self, ip: IpAddr) -> Option<ConnectionGuard> {
        // Try global permit first
        let permit = self.global.clone().try_acquire_owned().ok()?;

        // Check per-IP limit
        let mut map = self.per_ip.lock().unwrap();
        let count = map.entry(ip).or_insert(0);
        if *count >= self.max_per_ip {
            // Drop permit (returned to semaphore)
            drop(permit);
            return None;
        }
        *count += 1;
        drop(map);

        Some(ConnectionGuard {
            ip,
            per_ip: Arc::clone(&self.per_ip),
            _permit: permit,
        })
    }

    /// Get the number of available global connection slots.
    #[cfg(test)]
    fn available(&self) -> usize {
        self.global.available_permits()
    }

    /// Get the current connection count for an IP.
    #[cfg(test)]
    fn ip_count(&self, ip: &IpAddr) -> usize {
        self.per_ip
            .lock()
            .unwrap()
            .get(ip)
            .copied()
            .unwrap_or(0)
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

type AsyncRadixNode = RadixNode<AsyncHandler>;

/// Handler representation for async execution.
/// Can be either a VM function reference or a native fast path.
#[derive(Clone)]
pub enum AsyncHandler {
    /// VM function handler (uses SendableHandler for thread safety)
    Vm(crate::vm::sendable::SendableHandler),
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
    pub params: SmallParams,
}

impl AsyncRequest {
    /// Create from Hyper request with body size enforcement.
    pub async fn from_hyper(
        req: Request<Incoming>,
        max_body_size: usize,
    ) -> Result<Self, BodyError> {
        let method = req.method().clone();
        let uri = req.uri();
        let path = uri.path().to_string();
        let raw_query = uri.query().map(|q| q.to_string());
        let headers = req.headers().clone();

        // Pre-check Content-Length header if present
        if let Some(content_length) = headers
            .get(hyper::header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| s.parse::<usize>().ok())
        {
            if content_length > max_body_size {
                return Err(BodyError::TooLarge);
            }
        }

        // Collect body with size limit
        let body = read_body_limited(req.into_body(), max_body_size).await?;

        Ok(Self {
            method,
            path,
            query: None,
            raw_query,
            headers,
            body,
            params: SmallParams::new(),
        })
    }

    /// Create a test request (for unit testing).
    #[cfg(test)]
    pub fn test_request(
        method: Method,
        path: &str,
        query: Option<&str>,
        body: &str,
        params: SmallParams,
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
            .as_slice()
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
    pub fn to_sendable(&self) -> crate::vm::sendable::SendableValue {
        use crate::vm::sendable::SendableValue;

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

        // Convert params to vec of (String, SendableValue)
        let params_vec: Vec<(String, SendableValue)> = self
            .params
            .as_slice()
            .iter()
            .map(|(k, v)| (k.clone(), SendableValue::String(v.clone())))
            .collect();
        let params_sv = SendableValue::Record(params_vec);

        // Convert query if present
        let query_vec: Vec<(String, SendableValue)> = self
            .raw_query
            .as_ref()
            .map(|q| {
                parse_query_string(Some(q))
                    .into_iter()
                    .map(|(k, v)| (k, SendableValue::String(v)))
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
                SendableValue::Bytes(self.body.clone()),
            ),
            ("params".to_string(), params_sv),
            ("query".to_string(), query_sv),
        ])
    }
}

/// Errors that can occur when reading a request body.
pub enum BodyError {
    /// Body exceeded the configured max_body_size
    TooLarge,
    /// Transport-level error reading the body
    Hyper(String),
}

/// Read request body with a size limit. Returns BodyError::TooLarge if exceeded.
async fn read_body_limited(body: Incoming, max_size: usize) -> Result<Bytes, BodyError> {
    use http_body_util::BodyExt;

    // Use http_body_util::Limited to cap body collection.
    // When the limit is exceeded, collect() returns an error whose Display
    // contains "length limit" (from LengthLimitError).
    let limited = http_body_util::Limited::new(body, max_size);
    match BodyExt::collect(limited).await {
        Ok(collected) => Ok(collected.to_bytes()),
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("length limit") {
                Err(BodyError::TooLarge)
            } else {
                Err(BodyError::Hyper(msg))
            }
        }
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
        // Unwrap Result enum (Ok/Err) — VM handlers return Ok(response)
        if val.is_heap() {
            if let super::nvalue::HeapObject::Enum { tag, payload, .. } = val.as_heap_ref() {
                if &**tag == "Ok" {
                    return Self::from_nvalue(payload);
                } else if &**tag == "Err" {
                    let msg = payload.as_str().unwrap_or("Internal Server Error");
                    return Self::text(500, msg.to_string());
                }
            }
        }

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
    pub fn from_sendable_val(val: &crate::vm::sendable::SendableValue) -> Self {
        use crate::vm::sendable::SendableValue;

        // Unwrap Result enum (Ok/Err) — VM handlers return Ok(response)
        if let SendableValue::Enum { tag, payload } = val {
            if tag == "Ok" {
                return Self::from_sendable_val(payload);
            } else if tag == "Err" {
                let msg = match payload.as_ref() {
                    SendableValue::String(s) => s.clone(),
                    _ => "Internal Server Error".to_string(),
                };
                return Self::text(500, msg);
            }
        }

        if let SendableValue::Record(fields) = val {
            let status = fields
                .iter()
                .find(|(k, _)| k == "status")
                .and_then(|(_, v)| {
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
                .and_then(|(_, v)| match v {
                    SendableValue::String(s) => Some(Bytes::from(s.clone())),
                    #[cfg(feature = "async-server")]
                    SendableValue::Bytes(b) => Some(b.clone()),
                    _ => None,
                })
                .unwrap_or_default();

            let headers: Vec<(String, String)> = fields
                .iter()
                .find(|(k, _)| k == "headers")
                .and_then(|(_, v)| {
                    if let SendableValue::List(list) = v {
                        Some(
                            list.iter()
                                .filter_map(|item| {
                                    if let SendableValue::Tuple(tuple) = item {
                                        if tuple.len() == 2 {
                                            let k = if let SendableValue::String(s) = &tuple[0] {
                                                s
                                            } else {
                                                return None;
                                            };
                                            let v = if let SendableValue::String(s) = &tuple[1] {
                                                s
                                            } else {
                                                return None;
                                            };
                                            return Some((k.clone(), v.clone()));
                                        }
                                    }
                                    None
                                })
                                .collect(),
                        )
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
            let body = match val {
                SendableValue::String(s) => Bytes::from(s.clone()),
                #[cfg(feature = "async-server")]
                SendableValue::Bytes(b) => b.clone(),
                _ => Bytes::new(),
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

/// Build a hyper Response directly from a SendableValue, skipping the
/// AsyncResponse intermediate allocation. Used for VM handler results.
pub fn build_hyper_response_from_sendable(
    val: &crate::vm::sendable::SendableValue,
) -> Response<Full<Bytes>> {
    use crate::vm::sendable::SendableValue;

    // Unwrap Result enum (Ok/Err) — VM handlers return Ok(response)
    if let SendableValue::Enum { tag, payload } = val {
        if tag == "Ok" {
            return build_hyper_response_from_sendable(payload);
        } else if tag == "Err" {
            let msg = match payload.as_ref() {
                SendableValue::String(s) => s.clone(),
                _ => "Internal Server Error".to_string(),
            };
            return AsyncResponse::text(500, msg).into_hyper();
        }
    }

    if let SendableValue::Record(fields) = val {
        let status = fields
            .iter()
            .find(|(k, _)| k == "status")
            .and_then(|(_, v)| {
                if let SendableValue::Int(i) = v {
                    StatusCode::from_u16(*i as u16).ok()
                } else {
                    None
                }
            })
            .unwrap_or(StatusCode::OK);

        let body = fields
            .iter()
            .find(|(k, _)| k == "body")
            .map(|(_, v)| match v {
                SendableValue::Bytes(b) => b.clone(),
                SendableValue::String(s) => Bytes::from(s.clone()),
                _ => Bytes::new(),
            })
            .unwrap_or_default();

        let mut builder = Response::builder().status(status);

        if let Some((_, SendableValue::List(list))) =
            fields.iter().find(|(k, _)| k == "headers")
        {
            for item in list {
                if let SendableValue::Tuple(tuple) = item {
                    if tuple.len() == 2 {
                        if let (SendableValue::String(k), SendableValue::String(v)) =
                            (&tuple[0], &tuple[1])
                        {
                            builder = builder.header(k.as_str(), v.as_str());
                        }
                    }
                }
            }
        }

        builder.body(Full::new(body)).unwrap()
    } else {
        // Fallback: treat as string body
        let body = match val {
            SendableValue::String(s) => Bytes::from(s.clone()),
            SendableValue::Bytes(b) => b.clone(),
            _ => Bytes::new(),
        };
        Response::builder()
            .status(StatusCode::OK)
            .header("Content-Type", "text/plain")
            .body(Full::new(body))
            .unwrap()
    }
}

/// Build a hyper Response directly from a VM handler's NValue result.
/// Avoids the intermediate SendableValue conversion for better performance.
pub fn build_hyper_response_from_nvalue(val: &NValue) -> Response<Full<Bytes>> {
    use crate::vm::nvalue::HeapObject;

    // Unwrap Result enum (Ok/Err) — VM handlers return Ok(response)
    if val.is_heap() {
        match val.as_heap_ref() {
            HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                return build_hyper_response_from_nvalue(payload);
            }
            HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                let msg: String = payload
                    .as_string()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "Internal Server Error".to_string());
                return AsyncResponse::text(500, msg).into_hyper();
            }
            _ => {}
        }
    }

    if let Some(fields) = val.as_record() {
        let status = fields
            .iter()
            .find(|(k, _)| &**k == "status")
            .and_then(|(_, v)| {
                if v.is_any_int() {
                    StatusCode::from_u16(v.as_any_int() as u16).ok()
                } else {
                    None
                }
            })
            .unwrap_or(StatusCode::OK);

        let body = fields
            .iter()
            .find(|(k, _)| &**k == "body")
            .map(|(_, v)| match v.as_string() {
                Some(s) => Bytes::from(s.to_string()),
                None => Bytes::new(),
            })
            .unwrap_or_default();

        let mut builder = Response::builder().status(status);

        if let Some((_, headers_val)) = fields.iter().find(|(k, _)| &**k == "headers") {
            if let Some(items) = headers_val.as_list() {
                for item in items {
                    if item.is_heap() {
                        if let HeapObject::Tuple(pair) = item.as_heap_ref() {
                            if pair.len() == 2 {
                                if let (Some(k), Some(v)) =
                                    (pair[0].as_string(), pair[1].as_string())
                                {
                                    builder = builder.header(&**k, &**v);
                                }
                            }
                        }
                    }
                }
            }
        }

        return builder.body(Full::new(body)).unwrap();
    }

    // Fallback: treat as string body
    let body = val
        .as_string()
        .map(|s| Bytes::from(s.to_string()))
        .unwrap_or_else(|| Bytes::from(format!("{}", val)));
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "text/plain")
        .body(Full::new(body))
        .unwrap()
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
                    current.param =
                        Some((param_name.clone(), Box::new(AsyncRadixNode::default())));
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
    ) -> Option<(AsyncHandler, SmallParams)> {
        let segments: Vec<&str> = path
            .trim_start_matches('/')
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        let mut params = SmallParams::new();
        let node = self.find_node(&self.root, &segments, &mut params)?;

        let handler = node.handlers.get(method)?;
        Some((handler.clone(), params))
    }

    fn find_node<'a>(
        &self,
        node: &'a AsyncRadixNode,
        segments: &[&str],
        params: &mut SmallParams,
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
            params.push(param_name.clone(), segment.to_string());
            if let Some(found) = self.find_node(child, rest, params) {
                return Some(found);
            }
            params.pop();
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

                if let Some(sendable) =
                    crate::vm::sendable::SendableHandler::from_nvalue(handler_val)
                {
                    builder = builder.route(method, path, AsyncHandler::Vm(sendable));
                } else {
                    return Err(format!(
                        "Invalid handler for route {} {}",
                        method, path
                    ));
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

use crate::vm::async_executor::{AsyncExecutorConfig, AsyncVmExecutor};
use crate::vm::sendable::SendableHandler;
use crate::vm::chunk::Chunk;

/// Context for the async server, holding shared state.
pub struct AsyncServerContext {
    /// Route tree for path matching
    pub routes: Arc<AsyncRouteTree>,
    /// VM executor for running handlers (None for native-only mode)
    pub executor: Option<Arc<AsyncVmExecutor>>,
    /// Shared bytecode for inline VM execution (avoids executor overhead)
    pub chunks: Option<Arc<Vec<Chunk>>>,
    /// Middleware chain (VM handlers)
    pub middleware: Vec<SendableHandler>,
}

impl AsyncServerContext {
    /// Create a native-only server context (no VM execution).
    pub fn native_only(routes: AsyncRouteTree) -> Self {
        Self {
            routes: Arc::new(routes),
            executor: None,
            chunks: None,
            middleware: Vec::new(),
        }
    }

    /// Create a full server context with VM executor and inline chunks.
    pub fn with_executor(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
    ) -> Self {
        let chunks_arc = Arc::new(chunks);
        let executor = AsyncVmExecutor::new(
            (*chunks_arc).clone(),
            AsyncExecutorConfig::default(),
        );
        Self {
            routes: Arc::new(routes),
            executor: Some(Arc::new(executor)),
            chunks: Some(chunks_arc),
            middleware,
        }
    }
}

/// Start the async HTTP server.
/// This is the main entry point called from the VM.
pub async fn run_server(
    addr: SocketAddr,
    route_tree: AsyncRouteTree,
    config: ServerConfig,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let ctx = Arc::new(AsyncServerContext::native_only(route_tree));
    run_server_with_context(addr, ctx, config).await
}

/// Start the async HTTP server with full context (including VM executor).
///
/// Features:
/// - Connection limiting (global + per-IP)
/// - Keep-alive tuning
/// - Request timeouts
/// - Body size enforcement
/// - Graceful shutdown on SIGTERM/SIGINT
/// - Optional HTTP/2 support
pub async fn run_server_with_context(
    addr: SocketAddr,
    ctx: Arc<AsyncServerContext>,
    config: ServerConfig,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let listener = TcpListener::bind(addr).await?;
    let limiter = Arc::new(ConnectionLimiter::new(
        config.max_connections,
        config.max_connections_per_ip,
    ));
    let config = Arc::new(config);

    eprintln!("[server] Listening on http://{}", addr);

    // Set up graceful shutdown signal
    let shutdown = shutdown_signal();
    tokio::pin!(shutdown);

    // Track active connections for graceful drain
    let active_connections = Arc::new(AtomicUsize::new(0));

    loop {
        tokio::select! {
            result = listener.accept() => {
                let (stream, remote_addr) = match result {
                    Ok(conn) => conn,
                    Err(e) => {
                        eprintln!("[server] Accept error: {:?}", e);
                        continue;
                    }
                };

                let remote_ip = remote_addr.ip();

                // Try to acquire connection slot
                let guard = match limiter.try_acquire(remote_ip) {
                    Some(g) => g,
                    None => {
                        // At capacity — drop the connection silently
                        drop(stream);
                        continue;
                    }
                };

                let io = hyper_util::rt::TokioIo::new(stream);
                let ctx = Arc::clone(&ctx);
                let config = Arc::clone(&config);
                let active = Arc::clone(&active_connections);

                active.fetch_add(1, Ordering::Relaxed);

                tokio::task::spawn(async move {
                    // guard is moved into this task — dropped when connection closes
                    let _guard = guard;

                    if config.enable_http2 {
                        serve_auto_connection(io, ctx, &config).await;
                    } else {
                        serve_http1_connection(io, ctx, &config).await;
                    }

                    active.fetch_sub(1, Ordering::Relaxed);
                });
            }
            _ = &mut shutdown => {
                eprintln!("[server] Shutdown signal received, draining connections...");
                break;
            }
        }
    }

    // Graceful drain: wait for active connections to finish
    let drain_deadline = tokio::time::Instant::now() + config.shutdown_timeout;
    while active_connections.load(Ordering::Relaxed) > 0 {
        if tokio::time::Instant::now() >= drain_deadline {
            eprintln!(
                "[server] Shutdown timeout reached, {} connections still active",
                active_connections.load(Ordering::Relaxed)
            );
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    eprintln!("[server] Shutdown complete");
    Ok(())
}

/// Serve a connection using HTTP/1.1 only.
pub(crate) async fn serve_http1_connection(
    io: hyper_util::rt::TokioIo<tokio::net::TcpStream>,
    ctx: Arc<AsyncServerContext>,
    config: &ServerConfig,
) {
    let max_body_size = config.max_body_size;
    let request_timeout = config.request_timeout;

    let service = service_fn(move |req| {
        let ctx = Arc::clone(&ctx);
        async move {
            handle_request_with_timeout(req, &ctx, max_body_size, request_timeout).await
        }
    });

    let mut conn = http1::Builder::new()
        .keep_alive(config.keep_alive)
        .serve_connection(io, service);

    // Use pin for potential graceful shutdown
    let conn = std::pin::Pin::new(&mut conn);
    if let Err(err) = conn.await {
        if !err.to_string().contains("connection closed") {
            eprintln!("[server] Connection error: {:?}", err);
        }
    }
}

/// Serve a connection using HTTP/1.1 or HTTP/2 auto-detection.
pub(crate) async fn serve_auto_connection(
    io: hyper_util::rt::TokioIo<tokio::net::TcpStream>,
    ctx: Arc<AsyncServerContext>,
    config: &ServerConfig,
) {
    use hyper_util::server::conn::auto;

    let max_body_size = config.max_body_size;
    let request_timeout = config.request_timeout;

    let service = service_fn(move |req| {
        let ctx = Arc::clone(&ctx);
        async move {
            handle_request_with_timeout(req, &ctx, max_body_size, request_timeout).await
        }
    });

    let builder = auto::Builder::new(hyper_util::rt::TokioExecutor::new());

    if let Err(err) = builder.serve_connection(io, service).await {
        if !err.to_string().contains("connection closed") {
            eprintln!("[server] Connection error: {:?}", err);
        }
    }
}

/// Handle a request with timeout and body size enforcement.
async fn handle_request_with_timeout(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    max_body_size: usize,
    request_timeout: Duration,
) -> Result<Response<Full<Bytes>>, Infallible> {
    match tokio::time::timeout(
        request_timeout,
        handle_request_with_context(req, ctx, max_body_size),
    )
    .await
    {
        Ok(result) => result,
        Err(_elapsed) => {
            // Request timed out
            Ok(AsyncResponse::text(408, "Request Timeout").into_hyper())
        }
    }
}

async fn handle_request_with_context(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    max_body_size: usize,
) -> Result<Response<Full<Bytes>>, Infallible> {
    // Parse request with body size enforcement
    let mut async_req = match AsyncRequest::from_hyper(req, max_body_size).await {
        Ok(r) => r,
        Err(BodyError::TooLarge) => {
            return Ok(AsyncResponse::text(413, "Payload Too Large").into_hyper());
        }
        Err(BodyError::Hyper(_)) => {
            return Ok(AsyncResponse::text(500, "Failed to read request").into_hyper());
        }
    };

    // Find handler — borrow method/path from async_req to avoid duplicate allocations
    match ctx.routes.find(async_req.method.as_str(), &async_req.path) {
        Some((handler, params)) => {
            async_req.params = params;

            let response = match handler {
                AsyncHandler::Native(f) => f(&async_req),
                AsyncHandler::Vm(vm_handler) => {
                    if let Some(chunks) = &ctx.chunks {
                        // Inline VM execution: no spawn_blocking, no semaphore.
                        // Runs on Tokio worker thread using thread-local VM.
                        let chunks = Arc::clone(chunks);
                        let resp = HANDLER_VM.with(|cell| {
                            let mut vm = cell.borrow_mut();
                            let request_nv = async_req.to_nvalue();
                            let handler_nv = vm_handler.to_nvalue();
                            match vm.call_nvalue(
                                &handler_nv,
                                &[request_nv],
                                &chunks,
                                0,
                                0,
                            ) {
                                Ok(result) => build_hyper_response_from_nvalue(&result),
                                Err(e) => {
                                    vm.reset();
                                    eprintln!("[server] Handler error: {}", e);
                                    AsyncResponse::text(500, "Internal Server Error")
                                        .into_hyper()
                                }
                            }
                        });
                        return Ok(resp);
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

/// Wait for a shutdown signal (SIGTERM or SIGINT).
async fn shutdown_signal() {
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install SIGTERM handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
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
        assert!(resp
            .headers
            .iter()
            .any(|(k, v)| k == "Content-Type" && v == "application/json"));
    }

    #[test]
    fn test_urlencoding_decode() {
        assert_eq!(urlencoding_decode("hello%20world"), "hello world");
        assert_eq!(urlencoding_decode("a+b"), "a b");
        assert_eq!(urlencoding_decode("100%25"), "100%");
    }

    // -----------------------------------------------------------------------
    // Connection Limiter Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_connection_limiter_global_limit() {
        let limiter = ConnectionLimiter::new(2, 256);
        let ip: IpAddr = "127.0.0.1".parse().unwrap();

        let g1 = limiter.try_acquire(ip);
        assert!(g1.is_some());
        assert_eq!(limiter.available(), 1);

        let g2 = limiter.try_acquire(ip);
        assert!(g2.is_some());
        assert_eq!(limiter.available(), 0);

        // Third should fail — at capacity
        let g3 = limiter.try_acquire(ip);
        assert!(g3.is_none());

        // Drop one guard — slot opens
        drop(g1);
        assert_eq!(limiter.available(), 1);

        let g4 = limiter.try_acquire(ip);
        assert!(g4.is_some());
    }

    #[test]
    fn test_connection_limiter_per_ip_limit() {
        let limiter = ConnectionLimiter::new(100, 2);
        let ip1: IpAddr = "10.0.0.1".parse().unwrap();
        let ip2: IpAddr = "10.0.0.2".parse().unwrap();

        let _g1 = limiter.try_acquire(ip1);
        let _g2 = limiter.try_acquire(ip1);

        // Third from same IP should fail
        let g3 = limiter.try_acquire(ip1);
        assert!(g3.is_none());

        // Different IP should succeed
        let g4 = limiter.try_acquire(ip2);
        assert!(g4.is_some());
    }

    #[test]
    fn test_connection_limiter_guard_cleanup() {
        let limiter = ConnectionLimiter::new(10, 10);
        let ip: IpAddr = "192.168.1.1".parse().unwrap();

        {
            let _g = limiter.try_acquire(ip);
            assert_eq!(limiter.ip_count(&ip), 1);
        }
        // Guard dropped — count should be 0
        assert_eq!(limiter.ip_count(&ip), 0);
    }
}
