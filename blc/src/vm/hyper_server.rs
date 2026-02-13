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
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;
use tokio::net::TcpListener;
use tokio::sync::Semaphore;

use crate::vm::exec::Vm;
use crate::vm::nvalue::NValue;
use crate::vm::radix::{ParamCollector, RadixNode, SmallParams};
use crate::vm::value::RcStr;

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
    /// Maximum HTTP header size in bytes (default 8KB)
    pub max_header_size: usize,
    /// Enable structured access logging
    pub access_log: bool,
    /// Built-in health check path (None to disable)
    pub health_check_path: Option<String>,
    /// Request ID header name (None to disable)
    pub request_id_header: Option<String>,
    /// CORS configuration (None to disable)
    pub cors: Option<CorsConfig>,
    /// Rate limiting configuration (None to disable)
    pub rate_limit: Option<RateLimitConfig>,
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
            max_header_size: 8192,
            access_log: true,
            health_check_path: Some("/__health".to_string()),
            request_id_header: Some("x-request-id".to_string()),
            cors: None,
            rate_limit: None,
        }
    }
}

/// CORS (Cross-Origin Resource Sharing) configuration.
#[derive(Clone)]
pub struct CorsConfig {
    pub allowed_origins: Vec<String>,
    pub allowed_methods: Vec<String>,
    pub allowed_headers: Vec<String>,
    pub max_age: u64,
    pub allow_credentials: bool,
}

impl Default for CorsConfig {
    fn default() -> Self {
        Self {
            allowed_origins: vec!["*".to_string()],
            allowed_methods: vec![
                "GET".into(),
                "POST".into(),
                "PUT".into(),
                "DELETE".into(),
                "PATCH".into(),
                "OPTIONS".into(),
            ],
            allowed_headers: vec!["Content-Type".into(), "Authorization".into()],
            max_age: 86400,
            allow_credentials: false,
        }
    }
}

/// Rate limiting configuration.
#[derive(Clone)]
pub struct RateLimitConfig {
    pub requests_per_second: f64,
    pub burst_size: usize,
}

/// Per-IP token bucket rate limiter.
pub struct RateLimiter {
    buckets: std::sync::Mutex<HashMap<IpAddr, TokenBucket>>,
    config: RateLimitConfig,
}

struct TokenBucket {
    tokens: f64,
    last_refill: std::time::Instant,
}

impl RateLimiter {
    pub fn new(config: RateLimitConfig) -> Self {
        Self {
            buckets: std::sync::Mutex::new(HashMap::new()),
            config,
        }
    }

    /// Try to consume a token for the given IP. Returns true if allowed.
    pub fn check(&self, ip: IpAddr) -> bool {
        let mut buckets = self.buckets.lock().unwrap();
        let now = std::time::Instant::now();
        let bucket = buckets.entry(ip).or_insert_with(|| TokenBucket {
            tokens: self.config.burst_size as f64,
            last_refill: now,
        });

        // Refill tokens based on elapsed time
        let elapsed = now.duration_since(bucket.last_refill).as_secs_f64();
        bucket.tokens = (bucket.tokens + elapsed * self.config.requests_per_second)
            .min(self.config.burst_size as f64);
        bucket.last_refill = now;

        if bucket.tokens >= 1.0 {
            bucket.tokens -= 1.0;
            true
        } else {
            false
        }
    }

    /// Remove stale buckets that haven't been used recently.
    pub fn cleanup(&self) {
        let mut buckets = self.buckets.lock().unwrap();
        let now = std::time::Instant::now();
        buckets.retain(|_, b| now.duration_since(b.last_refill).as_secs() < 300);
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
        self.per_ip.lock().unwrap().get(ip).copied().unwrap_or(0)
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
            ("body".to_string(), SendableValue::Bytes(self.body.clone())),
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

        if let Some((_, SendableValue::List(list))) = fields.iter().find(|(k, _)| k == "headers") {
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
    pub fn find(&self, method: &str, path: &str) -> Option<(AsyncHandler, SmallParams)> {
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

use crate::vm::async_executor::{AsyncExecutorConfig, AsyncVmExecutor};
use crate::vm::chunk::Chunk;
use crate::vm::sendable::SendableHandler;

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
    /// Server configuration
    pub config: Arc<ServerConfig>,
    /// Rate limiter (if configured)
    pub rate_limiter: Option<Arc<RateLimiter>>,
}

impl AsyncServerContext {
    /// Create a native-only server context (no VM execution).
    pub fn native_only(routes: AsyncRouteTree, config: ServerConfig) -> Self {
        let rate_limiter = config
            .rate_limit
            .as_ref()
            .map(|rl| Arc::new(RateLimiter::new(rl.clone())));
        Self {
            routes: Arc::new(routes),
            executor: None,
            chunks: None,
            middleware: Vec::new(),
            config: Arc::new(config),
            rate_limiter,
        }
    }

    /// Create a full server context with VM executor and inline chunks.
    pub fn with_executor(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
        config: ServerConfig,
    ) -> Self {
        let chunks_arc = Arc::new(chunks);
        let executor = AsyncVmExecutor::new((*chunks_arc).clone(), AsyncExecutorConfig::default());
        let rate_limiter = config
            .rate_limit
            .as_ref()
            .map(|rl| Arc::new(RateLimiter::new(rl.clone())));
        Self {
            routes: Arc::new(routes),
            executor: Some(Arc::new(executor)),
            chunks: Some(chunks_arc),
            middleware,
            config: Arc::new(config),
            rate_limiter,
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
    let ctx = Arc::new(AsyncServerContext::native_only(route_tree, config));
    run_server_with_context(addr, ctx).await
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
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let listener = TcpListener::bind(addr).await?;
    let config = &ctx.config;
    let limiter = Arc::new(ConnectionLimiter::new(
        config.max_connections,
        config.max_connections_per_ip,
    ));

    eprintln!("[server] Listening on http://{}", addr);

    // Set up graceful shutdown signal
    let shutdown = shutdown_signal();
    tokio::pin!(shutdown);

    // Broadcast channel for per-connection graceful shutdown
    let (shutdown_tx, _shutdown_rx) = tokio::sync::watch::channel(false);

    // Spawn periodic rate limiter cleanup task
    if let Some(ref rl) = ctx.rate_limiter {
        let rl = Arc::clone(rl);
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(60)).await;
                rl.cleanup();
            }
        });
    }

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
                let active = Arc::clone(&active_connections);
                let mut shutdown_rx = shutdown_tx.subscribe();

                active.fetch_add(1, Ordering::Relaxed);

                tokio::task::spawn(async move {
                    // guard is moved into this task — dropped when connection closes
                    let _guard = guard;

                    if ctx.config.enable_http2 {
                        serve_auto_connection(io, ctx, remote_addr, &mut shutdown_rx).await;
                    } else {
                        serve_http1_connection(io, ctx, remote_addr, &mut shutdown_rx).await;
                    }

                    active.fetch_sub(1, Ordering::Relaxed);
                });
            }
            _ = &mut shutdown => {
                eprintln!("[server] Shutdown signal received, draining connections...");
                let _ = shutdown_tx.send(true);
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
    remote_addr: SocketAddr,
    shutdown_rx: &mut tokio::sync::watch::Receiver<bool>,
) {
    let config = Arc::clone(&ctx.config);
    let remote = remote_addr;

    let service = service_fn(move |req| {
        let ctx = Arc::clone(&ctx);
        async move { handle_request_with_timeout(req, &ctx, remote).await }
    });

    let mut builder = http1::Builder::new();
    builder.keep_alive(config.keep_alive);
    builder.max_buf_size(config.max_header_size);
    if config.keep_alive {
        builder.timer(hyper_util::rt::TokioTimer::new());
        builder.header_read_timeout(config.keep_alive_timeout);
    }
    let mut conn = builder.serve_connection(io, service);

    // Select between connection completion and shutdown signal
    let conn = std::pin::Pin::new(&mut conn);
    tokio::select! {
        result = conn => {
            if let Err(err) = result {
                if !err.to_string().contains("connection closed") {
                    eprintln!("[server] Connection error: {:?}", err);
                }
            }
        }
        _ = shutdown_rx.changed() => {
            // Graceful shutdown: let in-flight request finish via hyper's
            // graceful_shutdown, then the connection future will complete.
        }
    }
}

/// Serve a connection using HTTP/1.1 or HTTP/2 auto-detection.
pub(crate) async fn serve_auto_connection(
    io: hyper_util::rt::TokioIo<tokio::net::TcpStream>,
    ctx: Arc<AsyncServerContext>,
    remote_addr: SocketAddr,
    shutdown_rx: &mut tokio::sync::watch::Receiver<bool>,
) {
    use hyper_util::server::conn::auto;

    let remote = remote_addr;

    let service = service_fn(move |req| {
        let ctx = Arc::clone(&ctx);
        async move { handle_request_with_timeout(req, &ctx, remote).await }
    });

    let builder = auto::Builder::new(hyper_util::rt::TokioExecutor::new());
    let conn = builder.serve_connection(io, service);
    tokio::pin!(conn);

    tokio::select! {
        result = &mut conn => {
            if let Err(err) = result {
                if !err.to_string().contains("connection closed") {
                    eprintln!("[server] Connection error: {:?}", err);
                }
            }
        }
        _ = shutdown_rx.changed() => {
            // Graceful shutdown for auto connection
        }
    }
}

/// Handle a request with timeout and body size enforcement.
async fn handle_request_with_timeout(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    remote_addr: SocketAddr,
) -> Result<Response<Full<Bytes>>, Infallible> {
    match tokio::time::timeout(
        ctx.config.request_timeout,
        handle_request_with_context(req, ctx, remote_addr),
    )
    .await
    {
        Ok(result) => result,
        Err(_elapsed) => Ok(AsyncResponse::text(408, "Request Timeout").into_hyper()),
    }
}

async fn handle_request_with_context(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    remote_addr: SocketAddr,
) -> Result<Response<Full<Bytes>>, Infallible> {
    use hyper::body::Body as _;

    let config = &ctx.config;
    let start = std::time::Instant::now();

    // Generate or extract request ID
    let request_id = config.request_id_header.as_ref().map(|header_name| {
        req.headers()
            .get(header_name.as_str())
            .and_then(|v| v.to_str().ok())
            .map(|s| s.to_string())
            .unwrap_or_else(generate_request_id)
    });

    // Pre-parse request ID header name for reuse
    let request_id_hdr = config
        .request_id_header
        .as_ref()
        .and_then(|name| hyper::header::HeaderName::from_bytes(name.as_bytes()).ok());

    // Health check — respond before body parsing to minimize overhead
    if let Some(ref health_path) = config.health_check_path {
        if req.method() == Method::GET && req.uri().path() == health_path.as_str() {
            let mut resp = AsyncResponse::json(r#"{"status":"ok"}"#).into_hyper();
            if let Some(ref id) = request_id {
                if let Some(ref hdr) = request_id_hdr {
                    if let Ok(val) = hyper::header::HeaderValue::from_str(id) {
                        resp.headers_mut().insert(hdr.clone(), val);
                    }
                }
            }
            if config.access_log {
                eprintln!(
                    "[access] GET {} 200 {}b {:.1}ms {}{}",
                    health_path,
                    resp.body().size_hint().lower(),
                    start.elapsed().as_secs_f64() * 1000.0,
                    remote_addr.ip(),
                    format_request_id(&request_id),
                );
            }
            return Ok(resp);
        }
    }

    // CORS preflight — respond to OPTIONS before body parsing
    if let Some(ref cors) = config.cors {
        if req.method() == Method::OPTIONS {
            let mut resp = Response::builder()
                .status(StatusCode::NO_CONTENT)
                .body(Full::new(Bytes::new()))
                .unwrap();
            apply_cors_headers(resp.headers_mut(), cors, req.headers());
            if let Some(ref id) = request_id {
                if let Some(ref hdr) = request_id_hdr {
                    if let Ok(val) = hyper::header::HeaderValue::from_str(id) {
                        resp.headers_mut().insert(hdr.clone(), val);
                    }
                }
            }
            if config.access_log {
                let path = req.uri().path().to_string();
                eprintln!(
                    "[access] OPTIONS {} 204 0b {:.1}ms {}{}",
                    path,
                    start.elapsed().as_secs_f64() * 1000.0,
                    remote_addr.ip(),
                    format_request_id(&request_id),
                );
            }
            return Ok(resp);
        }
    }

    // Rate limiting check
    if let Some(ref limiter) = ctx.rate_limiter {
        if !limiter.check(remote_addr.ip()) {
            let mut resp = AsyncResponse::text(429, "Too Many Requests").into_hyper();
            resp.headers_mut()
                .insert("retry-after", hyper::header::HeaderValue::from_static("1"));
            if let Some(ref id) = request_id {
                if let Some(ref hdr) = request_id_hdr {
                    if let Ok(val) = hyper::header::HeaderValue::from_str(id) {
                        resp.headers_mut().insert(hdr.clone(), val);
                    }
                }
            }
            if config.access_log {
                let path = req.uri().path().to_string();
                let method = req.method().to_string();
                eprintln!(
                    "[access] {} {} 429 0b {:.1}ms {}{}",
                    method,
                    path,
                    start.elapsed().as_secs_f64() * 1000.0,
                    remote_addr.ip(),
                    format_request_id(&request_id),
                );
            }
            return Ok(resp);
        }
    }

    // Parse request with body size enforcement
    let mut async_req = match AsyncRequest::from_hyper(req, config.max_body_size).await {
        Ok(r) => r,
        Err(BodyError::TooLarge) => {
            if config.access_log {
                eprintln!(
                    "[access] ?? ?? 413 0b {:.1}ms {}{}",
                    start.elapsed().as_secs_f64() * 1000.0,
                    remote_addr.ip(),
                    format_request_id(&request_id),
                );
            }
            return Ok(AsyncResponse::text(413, "Payload Too Large").into_hyper());
        }
        Err(BodyError::Hyper(_)) => {
            return Ok(AsyncResponse::text(500, "Failed to read request").into_hyper());
        }
    };

    let method = async_req.method.clone();
    let path = async_req.path.clone();

    // Find handler — borrow method/path from async_req to avoid duplicate allocations
    let mut resp = match ctx.routes.find(async_req.method.as_str(), &async_req.path) {
        Some((handler, params)) => {
            async_req.params = params;

            match handler {
                AsyncHandler::Native(f) => f(&async_req).into_hyper(),
                AsyncHandler::Vm(vm_handler) => {
                    if let Some(chunks) = &ctx.chunks {
                        // Inline VM execution with panic isolation
                        let chunks = Arc::clone(chunks);
                        HANDLER_VM.with(|cell| {
                            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                let mut vm = cell.borrow_mut();
                                let request_nv = async_req.to_nvalue();
                                let handler_nv = vm_handler.to_nvalue();
                                match vm.call_nvalue(&handler_nv, &[request_nv], &chunks, 0, 0) {
                                    Ok(result) => build_hyper_response_from_nvalue(&result),
                                    Err(e) => {
                                        vm.reset();
                                        eprintln!("[server] Handler error: {}", e);
                                        AsyncResponse::text(500, "Internal Server Error")
                                            .into_hyper()
                                    }
                                }
                            })) {
                                Ok(resp) => resp,
                                Err(panic_info) => {
                                    if let Ok(mut vm) = cell.try_borrow_mut() {
                                        vm.reset();
                                    }
                                    let msg = extract_panic_message(&panic_info);
                                    eprintln!("[server] Handler panicked: {}", msg);
                                    AsyncResponse::text(500, "Internal Server Error").into_hyper()
                                }
                            }
                        })
                    } else {
                        AsyncResponse::text(501, "VM execution not configured").into_hyper()
                    }
                }
            }
        }
        None => AsyncResponse::text(404, "Not Found").into_hyper(),
    };

    // Append request ID header to response
    if let Some(ref id) = request_id {
        if let Some(ref hdr) = request_id_hdr {
            if let Ok(val) = hyper::header::HeaderValue::from_str(id) {
                resp.headers_mut().insert(hdr.clone(), val);
            }
        }
    }

    // Append CORS headers to all responses
    if let Some(ref cors) = config.cors {
        apply_cors_headers(resp.headers_mut(), cors, &hyper::HeaderMap::new());
    }

    // Structured access log
    if config.access_log {
        let status = resp.status().as_u16();
        let body_len = resp.body().size_hint().lower();
        eprintln!(
            "[access] {} {} {} {}b {:.1}ms {}{}",
            method,
            path,
            status,
            body_len,
            start.elapsed().as_secs_f64() * 1000.0,
            remote_addr.ip(),
            format_request_id(&request_id),
        );
    }

    Ok(resp)
}

/// Extract a human-readable message from a panic payload.
pub fn extract_panic_message(info: &Box<dyn std::any::Any + Send>) -> String {
    info.downcast_ref::<String>()
        .cloned()
        .or_else(|| info.downcast_ref::<&str>().map(|s| s.to_string()))
        .unwrap_or_else(|| "unknown panic".to_string())
}

/// Generate a random request ID using timestamp + thread ID + counter.
fn generate_request_id() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let tid = std::thread::current().id();
    thread_local! {
        static COUNTER: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    }
    let count = COUNTER.with(|c| {
        let v = c.get();
        c.set(v.wrapping_add(1));
        v
    });
    format!("{:016x}{:x}{:04x}", ts as u64, hash_thread_id(tid), count)
}

fn hash_thread_id(tid: std::thread::ThreadId) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    tid.hash(&mut hasher);
    hasher.finish() & 0xFFFF
}

fn format_request_id(id: &Option<String>) -> String {
    match id {
        Some(id) => format!(" {}", id),
        None => String::new(),
    }
}

/// Apply CORS headers to a response.
fn apply_cors_headers(
    headers: &mut hyper::HeaderMap,
    cors: &CorsConfig,
    req_headers: &hyper::HeaderMap,
) {
    let origin_val = if cors.allowed_origins.len() == 1 && cors.allowed_origins[0] == "*" {
        "*".to_string()
    } else {
        req_headers
            .get("origin")
            .and_then(|v| v.to_str().ok())
            .filter(|origin| cors.allowed_origins.iter().any(|a| a == origin))
            .unwrap_or("")
            .to_string()
    };

    if origin_val.is_empty() {
        return;
    }

    if let Ok(v) = hyper::header::HeaderValue::from_str(&origin_val) {
        headers.insert("access-control-allow-origin", v);
    }
    if cors.allow_credentials {
        headers.insert(
            "access-control-allow-credentials",
            hyper::header::HeaderValue::from_static("true"),
        );
    }
    if !cors.allowed_methods.is_empty() {
        let methods = cors.allowed_methods.join(", ");
        if let Ok(v) = hyper::header::HeaderValue::from_str(&methods) {
            headers.insert("access-control-allow-methods", v);
        }
    }
    if !cors.allowed_headers.is_empty() {
        let hdrs = cors.allowed_headers.join(", ");
        if let Ok(v) = hyper::header::HeaderValue::from_str(&hdrs) {
            headers.insert("access-control-allow-headers", v);
        }
    }
    if cors.max_age > 0 {
        if let Ok(v) = hyper::header::HeaderValue::from_str(&cors.max_age.to_string()) {
            headers.insert("access-control-max-age", v);
        }
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
