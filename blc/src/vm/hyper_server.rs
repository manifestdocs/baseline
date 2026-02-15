//! Async HTTP server using Hyper + Tokio for high-performance request handling.
//!
//! This module provides an async alternative to the tiny_http-based server,
//! targeting performance comparable to Axum and Go Fiber.
//!
//! Phase 2-3 features: connection limits, keep-alive tuning, graceful shutdown,
//! request timeouts, body size enforcement, HTTP/2 support.

use bytes::Bytes;
use http_body_util::{BodyExt, Full, StreamBody};
use hyper::body::{Frame, Incoming};
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

/// Unified response body type supporting both buffered and streaming responses.
pub type ServerBody = http_body_util::combinators::BoxBody<Bytes, Infallible>;

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
    /// HTTP/2 initial stream-level flow-control window size (bytes).
    /// Default: 65,535 (HTTP/2 spec minimum). Set higher for large responses.
    pub http2_initial_stream_window_size: Option<u32>,
    /// HTTP/2 initial connection-level flow-control window size (bytes).
    /// Default: 65,535. Increase for high-throughput connections with many streams.
    pub http2_initial_connection_window_size: Option<u32>,
    /// HTTP/2 maximum concurrent streams per connection.
    /// Default: 200. Limits how many requests can be multiplexed on one connection.
    pub http2_max_concurrent_streams: Option<u32>,
    /// HTTP/2 maximum frame size (bytes). Default: 16,384. Max: 16,777,215.
    pub http2_max_frame_size: Option<u32>,
    /// HTTP/2 keep-alive ping interval. None disables pings.
    pub http2_keep_alive_interval: Option<Duration>,
    /// HTTP/2 keep-alive ping timeout. Connection closed if no response within this duration.
    pub http2_keep_alive_timeout: Option<Duration>,
    /// Enable HTTP/2 adaptive flow control (overrides window size settings).
    pub http2_adaptive_window: bool,
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
    /// TLS certificate file path (PEM format). Requires `tls` feature.
    pub tls_cert_path: Option<String>,
    /// TLS private key file path (PEM format). Requires `tls` feature.
    pub tls_key_path: Option<String>,
    /// Enable response compression (gzip for responses >1KB with compressible content types)
    pub compression: bool,
    /// Static file directory mappings: (url_prefix, filesystem_path)
    pub static_dirs: Vec<(String, String)>,
    /// Auto-generated API docs path (None to disable, e.g. Some("/docs"))
    pub docs_path: Option<String>,
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
            http2_initial_stream_window_size: None,
            http2_initial_connection_window_size: None,
            http2_max_concurrent_streams: None,
            http2_max_frame_size: None,
            http2_keep_alive_interval: None,
            http2_keep_alive_timeout: None,
            http2_adaptive_window: false,
            max_header_size: 8192,
            access_log: true,
            health_check_path: Some("/__health".to_string()),
            request_id_header: Some("x-request-id".to_string()),
            cors: None,
            rate_limit: None,
            tls_cert_path: None,
            tls_key_path: None,
            compression: false,
            static_dirs: Vec::new(),
            docs_path: None,
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
    /// SSE handler: receives request + sender channel, runs as a long-lived task.
    NativeSse(Arc<dyn Fn(AsyncRequest, tokio::sync::mpsc::Sender<SseEvent>) + Send + Sync>),
    /// WebSocket handler: VM function invoked after HTTP upgrade completes.
    VmWs(crate::vm::sendable::SendableHandler),
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

    /// Get cookies from the request (parsed from Cookie header).
    pub fn cookies(&self) -> HashMap<String, String> {
        parse_cookies(&self.headers)
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

        let cookies: Vec<(RcStr, NValue)> = self
            .cookies()
            .into_iter()
            .map(|(k, v)| (RcStr::from(k), NValue::string(RcStr::from(v))))
            .collect();

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
            ("cookies".into(), NValue::record(cookies)),
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
            (
                "cookies".to_string(),
                SendableValue::Record(
                    self.cookies()
                        .into_iter()
                        .map(|(k, v)| (k, SendableValue::String(v)))
                        .collect(),
                ),
            ),
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
                    // If the Err payload is a Response record (has status field),
                    // extract it as a proper HTTP response instead of returning 500.
                    if let Some(fields) = payload.as_record() {
                        if fields.iter().any(|(k, _)| &**k == "status") {
                            return Self::from_nvalue(payload);
                        }
                    }
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
                // If the Err payload is a Response record (has status field),
                // extract it as a proper HTTP response instead of returning 500.
                if let SendableValue::Record(fields) = payload.as_ref() {
                    if fields.iter().any(|(k, _)| k == "status") {
                        return Self::from_sendable_val(payload);
                    }
                }
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

    /// Convert to Hyper response with a boxed body.
    pub fn into_hyper(self) -> Response<ServerBody> {
        let mut builder = Response::builder().status(self.status);

        for (name, value) in &self.headers {
            builder = builder.header(name.as_str(), value.as_str());
        }

        builder.body(Full::new(self.body).boxed()).unwrap()
    }

    /// Create an SSE streaming response from an mpsc::Receiver.
    pub fn sse(rx: tokio::sync::mpsc::Receiver<SseEvent>) -> Response<ServerBody> {
        use tokio_stream::wrappers::ReceiverStream;
        use tokio_stream::StreamExt;

        let stream = ReceiverStream::new(rx);
        let body_stream = stream.map(|event| Ok(Frame::data(event.encode())));
        let body = StreamBody::new(body_stream);

        Response::builder()
            .status(200)
            .header("Content-Type", "text/event-stream")
            .header("Cache-Control", "no-cache")
            .header("Connection", "keep-alive")
            .body(body.boxed())
            .unwrap()
    }
}

// ---------------------------------------------------------------------------
// SSE (Server-Sent Events)
// ---------------------------------------------------------------------------

/// A Server-Sent Event that can be sent over an SSE connection.
pub struct SseEvent {
    /// Event type (optional; maps to the `event:` field).
    pub event: Option<String>,
    /// Event data payload (maps to the `data:` field).
    pub data: String,
    /// Event ID (optional; maps to the `id:` field).
    pub id: Option<String>,
    /// Reconnection time in milliseconds (optional; maps to the `retry:` field).
    pub retry: Option<u32>,
}

impl SseEvent {
    /// Create a new SSE event with the given data.
    pub fn new(data: impl Into<String>) -> Self {
        Self {
            event: None,
            data: data.into(),
            id: None,
            retry: None,
        }
    }

    /// Set the event type.
    pub fn with_event(mut self, event: impl Into<String>) -> Self {
        self.event = Some(event.into());
        self
    }

    /// Set the event ID.
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Set the retry interval in milliseconds.
    pub fn with_retry(mut self, retry: u32) -> Self {
        self.retry = Some(retry);
        self
    }

    /// Encode the event into the SSE wire format.
    pub fn encode(&self) -> Bytes {
        let mut buf = String::new();
        if let Some(ref event) = self.event {
            buf.push_str("event: ");
            buf.push_str(event);
            buf.push('\n');
        }
        if let Some(ref id) = self.id {
            buf.push_str("id: ");
            buf.push_str(id);
            buf.push('\n');
        }
        if let Some(retry) = self.retry {
            buf.push_str("retry: ");
            buf.push_str(&retry.to_string());
            buf.push('\n');
        }
        for line in self.data.lines() {
            buf.push_str("data: ");
            buf.push_str(line);
            buf.push('\n');
        }
        // Handle empty data (still need "data: \n")
        if self.data.is_empty() {
            buf.push_str("data: \n");
        }
        buf.push('\n');
        Bytes::from(buf)
    }
}

/// Build a hyper Response directly from a SendableValue.
/// Delegates to `AsyncResponse::from_sendable_val` to avoid code duplication.
pub fn build_hyper_response_from_sendable(
    val: &crate::vm::sendable::SendableValue,
) -> Response<ServerBody> {
    AsyncResponse::from_sendable_val(val).into_hyper()
}

/// Build a hyper Response directly from a VM handler's NValue result.
/// Delegates to `AsyncResponse::from_nvalue` to avoid code duplication.
pub fn build_hyper_response_from_nvalue(val: &NValue) -> Response<ServerBody> {
    AsyncResponse::from_nvalue(val).into_hyper()
}

/// Create an empty boxed body for WebSocket upgrade responses.
fn empty_body() -> ServerBody {
    use http_body_util::Empty;
    Empty::new().map_err(|never| match never {}).boxed()
}

/// Bridge between async WebSocket stream and sync VM handler.
///
/// Architecture:
///   - WS reader task: reads from WebSocket → sends WsEvents to VM thread
///   - WS writer task: receives WsCommands from VM thread → sends to WebSocket
///   - VM thread: runs handler with thread-local WS channels installed
async fn run_ws_handler<S>(
    ws: tokio_tungstenite::WebSocketStream<S>,
    handler: crate::vm::sendable::SendableHandler,
    params: SmallParams,
    chunks: Option<Arc<Vec<Chunk>>>,
    _middleware: Vec<crate::vm::sendable::SendableHandler>,
    state: crate::vm::sendable::SendableValue,
    path: String,
) where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin + Send + 'static,
{
    use crate::vm::natives::ws::{WsCommand, WsEvent};
    use futures_util::{SinkExt, StreamExt};
    use tokio_tungstenite::tungstenite::Message;

    let (mut ws_sink, mut ws_stream) = ws.split();

    // Channels between async WS tasks and sync VM handler thread
    let (cmd_tx, mut cmd_rx) = tokio::sync::mpsc::channel::<WsCommand>(32);
    let (evt_tx, evt_rx) = tokio::sync::mpsc::channel::<WsEvent>(32);

    // Writer task: cmd_rx → ws_sink
    let write_task = tokio::spawn(async move {
        while let Some(cmd) = cmd_rx.recv().await {
            let msg = match cmd {
                WsCommand::SendText(text) => Message::Text(text),
                WsCommand::Close => {
                    let _ = ws_sink.send(Message::Close(None)).await;
                    break;
                }
            };
            if ws_sink.send(msg).await.is_err() {
                break;
            }
        }
        // Ensure sink is closed
        let _ = ws_sink.close().await;
    });

    // Reader task: ws_stream → evt_tx
    let evt_tx_clone = evt_tx.clone();
    let read_task = tokio::spawn(async move {
        while let Some(result) = ws_stream.next().await {
            match result {
                Ok(Message::Text(text)) => {
                    if evt_tx_clone.send(WsEvent::Text(text)).await.is_err() {
                        break;
                    }
                }
                Ok(Message::Binary(data)) => {
                    if evt_tx_clone.send(WsEvent::Binary(data.to_vec())).await.is_err() {
                        break;
                    }
                }
                Ok(Message::Ping(_)) | Ok(Message::Pong(_)) => {
                    // Handled automatically by tungstenite
                }
                Ok(Message::Close(frame)) => {
                    let reason = frame.map(|f| f.reason.to_string());
                    let _ = evt_tx_clone.send(WsEvent::Closed(reason)).await;
                    break;
                }
                Ok(Message::Frame(_)) => {
                    // Raw frames — ignore
                }
                Err(e) => {
                    let _ = evt_tx_clone.send(WsEvent::Closed(Some(e.to_string()))).await;
                    break;
                }
            }
        }
    });

    // VM handler thread (sync, blocking)
    let vm_handle = tokio::task::spawn_blocking(move || {
        // Install WS channels into thread-local storage
        crate::vm::natives::ws::set_ws_channels(cmd_tx, evt_rx);

        if let Some(chunks) = chunks {
            HANDLER_VM.with(|cell| {
                let mut vm = cell.borrow_mut();
                // Build a minimal request-like record with path and params
                let mut fields = vec![
                    ("path".into(), NValue::string(path.into())),
                    ("method".into(), NValue::string("WS".into())),
                ];
                // Add params
                for (k, v) in params.as_slice().iter() {
                    fields.push((k.as_str().into(), NValue::string(v.as_str().into())));
                }
                let request_nv = NValue::record(fields);
                let state_nv = state.to_nvalue();
                let request_nv = crate::vm::exec::http_helpers::inject_state_nv(&request_nv, &state_nv);
                let handler_nv = handler.to_nvalue();

                if let Err(e) = vm.call_nvalue(
                    &handler_nv,
                    &[request_nv],
                    &chunks,
                    0, 0,
                ) {
                    eprintln!("[server] WebSocket handler error: {}", e);
                }
            });
        }

        // Clean up WS channels
        crate::vm::natives::ws::clear_ws_channels();
    });

    // Wait for VM handler to finish, then clean up tasks
    let _ = vm_handle.await;
    read_task.abort();
    write_task.abort();
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

    /// Add a native SSE (Server-Sent Events) route.
    pub fn sse<F>(self, path: &str, handler: F) -> Self
    where
        F: Fn(AsyncRequest, tokio::sync::mpsc::Sender<SseEvent>) + Send + Sync + 'static,
    {
        self.route("GET", path, AsyncHandler::NativeSse(Arc::new(handler)))
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

                if method == "WS" {
                    // WebSocket routes use method "WS" internally; register as GET for HTTP upgrade
                    if let Some(sendable) =
                        crate::vm::sendable::SendableHandler::from_nvalue(handler_val)
                    {
                        builder = builder.route("GET", path, AsyncHandler::VmWs(sendable));
                    } else {
                        return Err(format!("Invalid handler for WS route {}", path));
                    }
                } else if let Some(sendable) =
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
    /// Router-level shared state (injected into every request)
    pub state: SendableValue,
    /// Route metadata for API docs: (method, path) pairs
    pub route_meta: Vec<(String, String)>,
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
            state: SendableValue::Record(Vec::new()),
            route_meta: Vec::new(),
        }
    }

    /// Create a full server context with VM executor and inline chunks.
    pub fn with_executor(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
        config: ServerConfig,
    ) -> Self {
        Self::with_executor_and_state(routes, chunks, middleware, config, SendableValue::Record(Vec::new()))
    }

    /// Create a full server context with VM executor, inline chunks, and shared state.
    pub fn with_executor_and_state(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
        config: ServerConfig,
        state: SendableValue,
    ) -> Self {
        Self::with_executor_state_and_meta(routes, chunks, middleware, config, state, Vec::new())
    }

    /// Create a full server context with VM executor, inline chunks, shared state, and route metadata.
    pub fn with_executor_state_and_meta(
        routes: AsyncRouteTree,
        chunks: Vec<Chunk>,
        middleware: Vec<SendableHandler>,
        config: ServerConfig,
        state: SendableValue,
        route_meta: Vec<(String, String)>,
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
            state,
            route_meta,
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

    // Set up TLS acceptor if configured
    #[cfg(feature = "tls")]
    let tls_acceptor = match (&config.tls_cert_path, &config.tls_key_path) {
        (Some(cert), Some(key)) => {
            let tls_config = load_tls_config(cert, key)?;
            Some(tokio_rustls::TlsAcceptor::from(tls_config))
        }
        _ => None,
    };

    let scheme = {
        #[cfg(feature = "tls")]
        {
            if tls_acceptor.is_some() {
                "https"
            } else {
                "http"
            }
        }
        #[cfg(not(feature = "tls"))]
        {
            "http"
        }
    };
    eprintln!("[server] Listening on {}://{}", scheme, addr);

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

                let ctx = Arc::clone(&ctx);
                let active = Arc::clone(&active_connections);
                let mut shutdown_rx = shutdown_tx.subscribe();

                active.fetch_add(1, Ordering::Relaxed);

                // Optionally wrap with TLS
                #[cfg(feature = "tls")]
                let tls_acceptor = tls_acceptor.clone();

                tokio::task::spawn(async move {
                    // guard is moved into this task — dropped when connection closes
                    let _guard = guard;

                    #[cfg(feature = "tls")]
                    if let Some(acceptor) = tls_acceptor {
                        match acceptor.accept(stream).await {
                            Ok(tls_stream) => {
                                let io = hyper_util::rt::TokioIo::new(tls_stream);
                                if ctx.config.enable_http2 {
                                    serve_auto_connection(io, ctx, remote_addr, &mut shutdown_rx).await;
                                } else {
                                    serve_http1_connection(io, ctx, remote_addr, &mut shutdown_rx).await;
                                }
                            }
                            Err(e) => {
                                eprintln!("[server] TLS handshake failed: {:?}", e);
                            }
                        }
                        active.fetch_sub(1, Ordering::Relaxed);
                        return;
                    }

                    let io = hyper_util::rt::TokioIo::new(stream);
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
pub(crate) async fn serve_http1_connection<I>(
    io: hyper_util::rt::TokioIo<I>,
    ctx: Arc<AsyncServerContext>,
    remote_addr: SocketAddr,
    shutdown_rx: &mut tokio::sync::watch::Receiver<bool>,
) where
    I: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin + Send + 'static,
{
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
    let conn = builder.serve_connection(io, service).with_upgrades();
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
            // Signal hyper to stop accepting new requests and drain in-flight ones.
            conn.as_mut().graceful_shutdown();
        }
    }
}

/// Serve a connection using HTTP/1.1 or HTTP/2 auto-detection.
pub(crate) async fn serve_auto_connection<I>(
    io: hyper_util::rt::TokioIo<I>,
    ctx: Arc<AsyncServerContext>,
    remote_addr: SocketAddr,
    shutdown_rx: &mut tokio::sync::watch::Receiver<bool>,
) where
    I: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin + Send + 'static,
{
    use hyper_util::server::conn::auto;

    let config = Arc::clone(&ctx.config);
    let remote = remote_addr;

    let service = service_fn(move |req| {
        let ctx = Arc::clone(&ctx);
        async move { handle_request_with_timeout(req, &ctx, remote).await }
    });

    let mut builder = auto::Builder::new(hyper_util::rt::TokioExecutor::new());

    // HTTP/1.1 settings
    builder.http1().keep_alive(config.keep_alive);
    builder.http1().max_buf_size(config.max_header_size);
    if config.keep_alive {
        builder
            .http1()
            .timer(hyper_util::rt::TokioTimer::new())
            .header_read_timeout(config.keep_alive_timeout);
    }

    // HTTP/2 stream configuration
    if let Some(sz) = config.http2_initial_stream_window_size {
        builder.http2().initial_stream_window_size(sz);
    }
    if let Some(sz) = config.http2_initial_connection_window_size {
        builder.http2().initial_connection_window_size(sz);
    }
    if let Some(max) = config.http2_max_concurrent_streams {
        builder.http2().max_concurrent_streams(max);
    }
    if let Some(sz) = config.http2_max_frame_size {
        builder.http2().max_frame_size(sz);
    }
    if let Some(interval) = config.http2_keep_alive_interval {
        builder.http2().keep_alive_interval(interval);
    }
    if let Some(timeout) = config.http2_keep_alive_timeout {
        builder.http2().keep_alive_timeout(timeout);
    }
    if config.http2_adaptive_window {
        builder.http2().adaptive_window(true);
    }
    builder
        .http2()
        .timer(hyper_util::rt::TokioTimer::new());

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
            // Signal hyper to stop accepting new requests and drain in-flight ones.
            // For HTTP/2 this sends a GOAWAY frame.
            conn.as_mut().graceful_shutdown();
        }
    }
}

/// Handle a request with timeout and body size enforcement.
async fn handle_request_with_timeout(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    remote_addr: SocketAddr,
) -> Result<Response<ServerBody>, Infallible> {
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

// ---------------------------------------------------------------------------
// Request metadata and response helpers (extracted from dispatch pipeline)
// ---------------------------------------------------------------------------

/// Per-request metadata computed once and threaded through the pipeline.
struct RequestMeta {
    request_id: Option<String>,
    request_id_hdr: Option<hyper::header::HeaderName>,
    accepts_gzip: bool,
    start: std::time::Instant,
}

impl RequestMeta {
    /// Build from an incoming request and server config.
    fn from_request(req: &Request<Incoming>, config: &ServerConfig) -> Self {
        let accepts_gzip = config.compression && accepts_gzip(req.headers());

        let request_id = config.request_id_header.as_ref().map(|header_name| {
            req.headers()
                .get(header_name.as_str())
                .and_then(|v| v.to_str().ok())
                .map(|s| s.to_string())
                .unwrap_or_else(generate_request_id)
        });

        let request_id_hdr = config
            .request_id_header
            .as_ref()
            .and_then(|name| hyper::header::HeaderName::from_bytes(name.as_bytes()).ok());

        Self {
            request_id,
            request_id_hdr,
            accepts_gzip,
            start: std::time::Instant::now(),
        }
    }
}

/// Insert the request ID header into a response, if configured.
fn insert_request_id(resp: &mut Response<ServerBody>, meta: &RequestMeta) {
    if let Some(ref id) = meta.request_id {
        if let Some(ref hdr) = meta.request_id_hdr {
            if let Ok(val) = hyper::header::HeaderValue::from_str(id) {
                resp.headers_mut().insert(hdr.clone(), val);
            }
        }
    }
}

/// Apply all post-processing to a response: request ID, CORS, and compression.
fn finalize_response(
    mut resp: Response<ServerBody>,
    meta: &RequestMeta,
    config: &ServerConfig,
) -> Response<ServerBody> {
    insert_request_id(&mut resp, meta);
    if let Some(ref cors) = config.cors {
        apply_cors_headers(resp.headers_mut(), cors, &hyper::HeaderMap::new());
    }
    // Skip compression for SSE streams
    let is_event_stream = resp
        .headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .map(|ct| ct.contains("text/event-stream"))
        .unwrap_or(false);
    if !is_event_stream {
        resp = try_compress_response(resp, meta.accepts_gzip);
    }
    resp
}

/// Emit a structured access log line.
fn log_access(
    config: &ServerConfig,
    method: &str,
    path: &str,
    resp: &Response<ServerBody>,
    meta: &RequestMeta,
    remote_addr: SocketAddr,
) {
    use hyper::body::Body as _;
    if config.access_log {
        eprintln!(
            "[access] {} {} {} {}b {:.1}ms {}{}",
            method,
            path,
            resp.status().as_u16(),
            resp.body().size_hint().lower(),
            meta.start.elapsed().as_secs_f64() * 1000.0,
            remote_addr.ip(),
            format_request_id(&meta.request_id),
        );
    }
}

async fn handle_request_with_context(
    req: Request<Incoming>,
    ctx: &AsyncServerContext,
    remote_addr: SocketAddr,
) -> Result<Response<ServerBody>, Infallible> {
    use hyper::body::Body as _;

    let config = &ctx.config;
    let meta = RequestMeta::from_request(&req, config);

    // Health check — respond before body parsing to minimize overhead
    if let Some(ref health_path) = config.health_check_path {
        if req.method() == Method::GET && req.uri().path() == health_path.as_str() {
            let mut resp = AsyncResponse::json(r#"{"status":"ok"}"#).into_hyper();
            insert_request_id(&mut resp, &meta);
            log_access(config, "GET", health_path, &resp, &meta, remote_addr);
            return Ok(resp);
        }
    }

    // Auto-generated API docs
    if let Some(ref docs_path) = config.docs_path {
        let req_path = req.uri().path();
        if req.method() == Method::GET && req_path == docs_path.as_str() {
            let html = generate_docs_html(&ctx.route_meta, docs_path);
            let mut resp = Response::builder()
                .status(StatusCode::OK)
                .header("content-type", "text/html; charset=utf-8")
                .body(Full::new(Bytes::from(html)).boxed())
                .unwrap();
            insert_request_id(&mut resp, &meta);
            log_access(config, "GET", docs_path, &resp, &meta, remote_addr);
            return Ok(resp);
        }

        let openapi_path = format!("{}/openapi.json", docs_path.trim_end_matches('/'));
        if req.method() == Method::GET && req_path == openapi_path {
            let json = generate_openapi_json(&ctx.route_meta);
            let mut resp = AsyncResponse::json(&json).into_hyper();
            insert_request_id(&mut resp, &meta);
            log_access(config, "GET", &openapi_path, &resp, &meta, remote_addr);
            return Ok(resp);
        }
    }

    // CORS preflight — respond to OPTIONS before body parsing
    if let Some(ref cors) = config.cors {
        if req.method() == Method::OPTIONS {
            let path = req.uri().path().to_string();
            let mut resp = Response::builder()
                .status(StatusCode::NO_CONTENT)
                .body(Full::new(Bytes::new()).boxed())
                .unwrap();
            apply_cors_headers(resp.headers_mut(), cors, req.headers());
            insert_request_id(&mut resp, &meta);
            log_access(config, "OPTIONS", &path, &resp, &meta, remote_addr);
            return Ok(resp);
        }
    }

    // Rate limiting check
    if let Some(ref limiter) = ctx.rate_limiter {
        if !limiter.check(remote_addr.ip()) {
            let path = req.uri().path().to_string();
            let method = req.method().to_string();
            let mut resp = AsyncResponse::text(429, "Too Many Requests").into_hyper();
            resp.headers_mut()
                .insert("retry-after", hyper::header::HeaderValue::from_static("1"));
            insert_request_id(&mut resp, &meta);
            log_access(config, &method, &path, &resp, &meta, remote_addr);
            return Ok(resp);
        }
    }

    // Static file serving — check before body parsing (GET only)
    if req.method() == Method::GET {
        for (prefix, dir) in &config.static_dirs {
            let req_path = req.uri().path();
            if req_path.starts_with(prefix.as_str()) {
                if let Some((mut file_resp, compressible)) = serve_static_file(dir, prefix, req_path).await {
                    insert_request_id(&mut file_resp, &meta);
                    if compressible {
                        file_resp = try_compress_response(file_resp, meta.accepts_gzip);
                    }
                    log_access(config, "GET", req_path, &file_resp, &meta, remote_addr);
                    return Ok(file_resp);
                }
            }
        }
    }

    // -------------------------------------------------------------------
    // WebSocket upgrade: check BEFORE body parsing (body is not consumed)
    // -------------------------------------------------------------------
    let is_ws_upgrade = req.method() == hyper::Method::GET
        && req.headers().get("upgrade")
            .and_then(|v| v.to_str().ok())
            .map(|v| v.eq_ignore_ascii_case("websocket"))
            .unwrap_or(false);

    if is_ws_upgrade {
        // Try to find a VmWs route for this path
        let req_path = req.uri().path().to_string();
        if let Some((handler, params)) = ctx.routes.find("GET", &req_path) {
            if let AsyncHandler::VmWs(vm_handler) = handler {
                // Perform the WebSocket upgrade
                use tokio_tungstenite::tungstenite::protocol::Role;
                use tokio_tungstenite::WebSocketStream;

                let (response, on_upgrade) = {
                    // Build 101 Switching Protocols response
                    let key = req.headers().get("sec-websocket-key")
                        .and_then(|v| v.to_str().ok())
                        .unwrap_or_default();
                    let accept = tokio_tungstenite::tungstenite::handshake::derive_accept_key(key.as_bytes());

                    let upgrade = hyper::upgrade::on(req);

                    let resp = Response::builder()
                        .status(101)
                        .header("Upgrade", "websocket")
                        .header("Connection", "Upgrade")
                        .header("Sec-WebSocket-Accept", accept)
                        .body(empty_body())
                        .unwrap();

                    (resp, upgrade)
                };

                // Spawn the WS handler task
                let chunks = ctx.chunks.clone();
                let middleware: Vec<_> = ctx.middleware.iter().cloned().collect();
                let state = ctx.state.clone();
                let handler_clone = vm_handler.clone();
                let params_owned = params.clone_data();
                let path_clone = req_path.clone();

                tokio::spawn(async move {
                    match on_upgrade.await {
                        Ok(upgraded) => {
                            let ws = WebSocketStream::from_raw_socket(
                                hyper_util::rt::TokioIo::new(upgraded),
                                Role::Server,
                                None,
                            ).await;

                            run_ws_handler(ws, handler_clone, params_owned, chunks, middleware, state, path_clone).await;
                        }
                        Err(e) => {
                            eprintln!("[server] WebSocket upgrade failed: {}", e);
                        }
                    }
                });

                if config.access_log {
                    eprintln!(
                        "[access] GET {} WebSocket upgrade {}{}",
                        req_path,
                        remote_addr.ip(),
                        format_request_id(&meta.request_id),
                    );
                }
                return Ok(response);
            }
        }
    }

    // Parse request with body size enforcement
    let mut async_req = match AsyncRequest::from_hyper(req, config.max_body_size).await {
        Ok(r) => r,
        Err(BodyError::TooLarge) => {
            let resp = AsyncResponse::text(413, "Payload Too Large").into_hyper();
            log_access(config, "??", "??", &resp, &meta, remote_addr);
            return Ok(resp);
        }
        Err(BodyError::Hyper(_)) => {
            return Ok(AsyncResponse::text(500, "Failed to read request").into_hyper());
        }
    };

    let method = async_req.method.clone();
    let path = async_req.path.clone();

    // Find handler — borrow method/path from async_req to avoid duplicate allocations
    let resp = match ctx.routes.find(async_req.method.as_str(), &async_req.path) {
        Some((handler, params)) => {
            async_req.params = params;

            match handler {
                AsyncHandler::Native(f) => f(&async_req).into_hyper(),
                AsyncHandler::Vm(vm_handler) => {
                    if let Some(chunks) = &ctx.chunks {
                        // Inline VM execution with panic isolation
                        let chunks = Arc::clone(chunks);
                        let middleware: Vec<_> = ctx.middleware.iter().collect();
                        HANDLER_VM.with(|cell| {
                            let state_nv = ctx.state.to_nvalue();
                            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                let mut vm = cell.borrow_mut();
                                let request_nv = async_req.to_nvalue();
                                let request_nv = crate::vm::exec::http_helpers::inject_state_nv(&request_nv, &state_nv);
                                let handler_nv = vm_handler.to_nvalue();
                                let result = if middleware.is_empty() {
                                    vm.call_nvalue(
                                        &handler_nv,
                                        &[request_nv],
                                        &chunks,
                                        0, 0,
                                    )
                                } else {
                                    let mw_nvalues: Vec<NValue> =
                                        middleware.iter().map(|m| m.to_nvalue()).collect();
                                    vm.apply_mw_chain(
                                        &mw_nvalues,
                                        &handler_nv,
                                        &request_nv,
                                        &chunks,
                                        0, 0,
                                    )
                                };
                                match result {
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
                AsyncHandler::NativeSse(f) => {
                    let (tx, rx) = tokio::sync::mpsc::channel::<SseEvent>(32);
                    let handler = Arc::clone(&f);
                    tokio::spawn(async move {
                        handler(async_req, tx);
                    });
                    let mut resp = AsyncResponse::sse(rx);
                    // SSE: add request ID and CORS but skip compression
                    insert_request_id(&mut resp, &meta);
                    if let Some(ref cors) = config.cors {
                        apply_cors_headers(resp.headers_mut(), cors, &hyper::HeaderMap::new());
                    }
                    if config.access_log {
                        eprintln!(
                            "[access] {} {} SSE stream started {}{}",
                            method,
                            path,
                            remote_addr.ip(),
                            format_request_id(&meta.request_id),
                        );
                    }
                    // Return SSE response directly — skip finalize_response
                    return Ok(resp);
                }
            }
        }
        None => AsyncResponse::text(404, "Not Found").into_hyper(),
    };

    // Apply request ID, CORS, and compression
    let resp = finalize_response(resp, &meta, config);
    log_access(config, &method, &path, &resp, &meta, remote_addr);

    Ok(resp)
}

// ---------------------------------------------------------------------------
// Auto-generated API documentation
// ---------------------------------------------------------------------------

/// Generate an HTML documentation page listing all API routes.
fn generate_docs_html(routes: &[(String, String)], docs_path: &str) -> String {
    // Group routes by path for cleaner display
    let mut path_methods: std::collections::BTreeMap<&str, Vec<&str>> = std::collections::BTreeMap::new();
    for (method, path) in routes {
        path_methods.entry(path.as_str()).or_default().push(method.as_str());
    }

    let openapi_path = format!("{}/openapi.json", docs_path.trim_end_matches('/'));

    let mut rows = String::new();
    for (path, methods) in &path_methods {
        let method_badges: Vec<String> = methods
            .iter()
            .map(|m| {
                let color = match *m {
                    "GET" => "#61affe",
                    "POST" => "#49cc90",
                    "PUT" => "#fca130",
                    "DELETE" => "#f93e3e",
                    "PATCH" => "#50e3c2",
                    "OPTIONS" => "#0d5aa7",
                    "HEAD" => "#9012fe",
                    _ => "#999",
                };
                format!(
                    r#"<span class="method" style="background:{};">{}</span>"#,
                    color, m
                )
            })
            .collect();
        rows.push_str(&format!(
            r#"<tr><td class="path">{}</td><td>{}</td></tr>"#,
            path,
            method_badges.join(" ")
        ));
    }

    format!(
        r##"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>API Documentation</title>
<style>
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #fafafa; color: #333; padding: 2rem; }}
  h1 {{ font-size: 1.5rem; margin-bottom: 0.25rem; }}
  .subtitle {{ color: #666; margin-bottom: 1.5rem; font-size: 0.9rem; }}
  .subtitle a {{ color: #4a90d9; text-decoration: none; }}
  .subtitle a:hover {{ text-decoration: underline; }}
  table {{ width: 100%; border-collapse: collapse; background: #fff; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
  th {{ text-align: left; padding: 0.75rem 1rem; background: #f5f5f5; font-size: 0.85rem; text-transform: uppercase; color: #666; border-bottom: 2px solid #eee; }}
  td {{ padding: 0.6rem 1rem; border-bottom: 1px solid #f0f0f0; }}
  .path {{ font-family: 'SF Mono', Monaco, monospace; font-size: 0.9rem; }}
  .method {{ display: inline-block; padding: 2px 8px; border-radius: 3px; color: #fff; font-size: 0.75rem; font-weight: 600; margin-right: 4px; font-family: 'SF Mono', Monaco, monospace; }}
  .footer {{ margin-top: 1.5rem; font-size: 0.8rem; color: #999; }}
</style>
</head>
<body>
<h1>API Documentation</h1>
<p class="subtitle">{} route(s) &middot; <a href="{}">OpenAPI JSON</a></p>
<table>
<thead><tr><th>Path</th><th>Methods</th></tr></thead>
<tbody>{}</tbody>
</table>
<p class="footer">Generated by Baseline</p>
</body>
</html>"##,
        path_methods.len(),
        openapi_path,
        rows
    )
}

/// Generate an OpenAPI 3.0 JSON spec from route metadata.
fn generate_openapi_json(routes: &[(String, String)]) -> String {
    let mut paths: std::collections::BTreeMap<String, Vec<&str>> = std::collections::BTreeMap::new();
    for (method, path) in routes {
        // Convert :param to {param} for OpenAPI spec
        let openapi_path = path
            .split('/')
            .map(|seg| {
                if let Some(name) = seg.strip_prefix(':') {
                    format!("{{{}}}", name)
                } else {
                    seg.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("/");
        paths.entry(openapi_path).or_default().push(method.as_str());
    }

    let mut path_entries = Vec::new();
    for (path, methods) in &paths {
        // Extract path parameters from {param} segments
        let params: Vec<&str> = path
            .split('/')
            .filter_map(|seg| {
                seg.strip_prefix('{').and_then(|s| s.strip_suffix('}'))
            })
            .collect();

        let params_json = if params.is_empty() {
            String::new()
        } else {
            let param_entries: Vec<String> = params
                .iter()
                .map(|name| {
                    format!(
                        r#"{{"name":"{}","in":"path","required":true,"schema":{{"type":"string"}}}}"#,
                        name
                    )
                })
                .collect();
            format!(r#","parameters":[{}]"#, param_entries.join(","))
        };

        let mut method_entries = Vec::new();
        for method in methods {
            method_entries.push(format!(
                r#""{}":{{"responses":{{"200":{{"description":"Success"}}}}{}}}"#,
                method.to_lowercase(),
                params_json,
            ));
        }
        path_entries.push(format!(
            r#""{}":{{{}}}"#,
            path,
            method_entries.join(",")
        ));
    }

    format!(
        r#"{{"openapi":"3.0.3","info":{{"title":"Baseline API","version":"1.0.0"}},"paths":{{{}}}}}"#,
        path_entries.join(",")
    )
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

// ---------------------------------------------------------------------------
// TLS Support (behind `tls` feature flag)
// ---------------------------------------------------------------------------

/// Load TLS configuration from PEM certificate and key files.
#[cfg(feature = "tls")]
pub fn load_tls_config(
    cert_path: &str,
    key_path: &str,
) -> Result<Arc<tokio_rustls::rustls::ServerConfig>, Box<dyn std::error::Error + Send + Sync>> {
    use tokio_rustls::rustls;

    let cert_file = std::fs::File::open(cert_path)
        .map_err(|e| format!("Failed to open TLS cert {}: {}", cert_path, e))?;
    let key_file = std::fs::File::open(key_path)
        .map_err(|e| format!("Failed to open TLS key {}: {}", key_path, e))?;

    let certs: Vec<rustls::pki_types::CertificateDer<'static>> =
        rustls_pemfile::certs(&mut std::io::BufReader::new(cert_file))
            .filter_map(|r| r.ok())
            .collect();

    if certs.is_empty() {
        return Err("No certificates found in PEM file".into());
    }

    let key = rustls_pemfile::private_key(&mut std::io::BufReader::new(key_file))
        .map_err(|e| format!("Failed to parse TLS key: {}", e))?
        .ok_or("No private key found in PEM file")?;

    let config = rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| format!("TLS config error: {}", e))?;

    Ok(Arc::new(config))
}

// ---------------------------------------------------------------------------
// Response Compression
// ---------------------------------------------------------------------------

/// Check if a content type is compressible.
fn is_compressible_content_type(headers: &hyper::HeaderMap) -> bool {
    headers
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .map(|ct| {
            ct.starts_with("text/")
                || ct.contains("json")
                || ct.contains("xml")
                || ct.contains("javascript")
                || ct.contains("css")
                || ct.contains("svg")
        })
        .unwrap_or(false)
}

/// Check if the client accepts gzip encoding.
fn accepts_gzip(req_headers: &hyper::HeaderMap) -> bool {
    req_headers
        .get("accept-encoding")
        .and_then(|v| v.to_str().ok())
        .map(|ae| ae.contains("gzip"))
        .unwrap_or(false)
}

/// Compress raw bytes with gzip. Returns None if compression fails or isn't smaller.
fn gzip_compress(data: &[u8]) -> Option<Bytes> {
    use flate2::Compression;
    use flate2::write::GzEncoder;
    use std::io::Write;

    let mut encoder = GzEncoder::new(Vec::with_capacity(data.len() / 2), Compression::fast());
    encoder.write_all(data).ok()?;
    let compressed = encoder.finish().ok()?;

    if compressed.len() < data.len() {
        Some(Bytes::from(compressed))
    } else {
        None
    }
}

/// Try to compress a hyper response body with gzip.
/// Decomposes and rebuilds the response if compression is beneficial.
/// Only compresses buffered (non-streaming) responses.
fn try_compress_response(
    resp: Response<ServerBody>,
    req_accepted_gzip: bool,
) -> Response<ServerBody> {
    use hyper::body::Body as _;

    if !req_accepted_gzip {
        return resp;
    }

    let body_len = resp.body().size_hint().lower() as usize;
    if body_len < 1024 {
        return resp;
    }

    if !is_compressible_content_type(resp.headers()) {
        return resp;
    }

    if resp.headers().contains_key("content-encoding") {
        return resp;
    }

    // Decompose response to get at the body bytes
    let (mut parts, mut body) = resp.into_parts();

    // Synchronously poll the single frame from the boxed body.
    // This works for Full<Bytes>-backed bodies which resolve in one poll.
    let waker = noop_waker();
    let mut cx = std::task::Context::from_waker(&waker);
    let raw_bytes = match std::pin::Pin::new(&mut body).poll_frame(&mut cx) {
        std::task::Poll::Ready(Some(Ok(frame))) => frame.into_data().unwrap_or_default(),
        // Streaming body or empty — return as-is
        _ => return Response::from_parts(parts, body),
    };

    match gzip_compress(&raw_bytes) {
        Some(compressed) => {
            parts.headers.insert(
                "content-encoding",
                hyper::header::HeaderValue::from_static("gzip"),
            );
            parts.headers.insert(
                "vary",
                hyper::header::HeaderValue::from_static("Accept-Encoding"),
            );
            Response::from_parts(parts, Full::new(compressed).boxed())
        }
        None => Response::from_parts(parts, Full::new(raw_bytes).boxed()),
    }
}

/// No-op waker for synchronous polling of a single-frame body.
fn noop_waker() -> std::task::Waker {
    use std::task::{RawWaker, RawWakerVTable};
    fn noop(_: *const ()) {}
    fn clone_fn(p: *const ()) -> RawWaker {
        RawWaker::new(p, &VTABLE)
    }
    const VTABLE: RawWakerVTable = RawWakerVTable::new(clone_fn, noop, noop, noop);
    unsafe { std::task::Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}

// ---------------------------------------------------------------------------
// Static File Serving
// ---------------------------------------------------------------------------

/// Maximum file size (in bytes) to buffer in memory for compression.
/// Files larger than this are streamed directly.
const STATIC_FILE_BUFFER_THRESHOLD: u64 = 64 * 1024;

/// Serve a static file from the filesystem with security checks.
/// Files under 64KB are read into memory (suitable for compression).
/// Larger files are streamed directly via tokio::fs.
async fn serve_static_file(
    base_dir: &str,
    url_prefix: &str,
    request_path: &str,
) -> Option<(Response<ServerBody>, bool)> {
    // Strip the URL prefix to get the relative file path
    let relative = request_path.strip_prefix(url_prefix)?;
    let relative = relative.trim_start_matches('/');

    if relative.is_empty() {
        return None;
    }

    // Path traversal prevention
    if relative.contains("..") || relative.contains('\0') {
        return Some((
            Response::builder()
                .status(StatusCode::FORBIDDEN)
                .body(Full::new(Bytes::from("Forbidden")).boxed())
                .unwrap(),
            false,
        ));
    }

    let file_path = std::path::Path::new(base_dir).join(relative);

    // Verify the resolved path is within the base directory (symlink-safe)
    let canonical_base = match std::fs::canonicalize(base_dir) {
        Ok(p) => p,
        Err(_) => {
            return Some((
                Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(Full::new(Bytes::from("Not Found")).boxed())
                    .unwrap(),
                false,
            ));
        }
    };
    let canonical_file = match std::fs::canonicalize(&file_path) {
        Ok(p) => p,
        Err(_) => {
            return Some((
                Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(Full::new(Bytes::from("Not Found")).boxed())
                    .unwrap(),
                false,
            ));
        }
    };
    if !canonical_file.starts_with(&canonical_base) {
        return Some((
            Response::builder()
                .status(StatusCode::FORBIDDEN)
                .body(Full::new(Bytes::from("Forbidden")).boxed())
                .unwrap(),
            false,
        ));
    }

    let content_type = guess_content_type(&canonical_file);

    // ETag based on file modification time + size
    let metadata = std::fs::metadata(&canonical_file).ok();
    let file_size = metadata.as_ref().map(|m| m.len()).unwrap_or(0);
    let etag = metadata.as_ref().and_then(|m| {
        let modified = m.modified().ok()?;
        let duration = modified.duration_since(std::time::UNIX_EPOCH).ok()?;
        Some(format!("\"{:x}-{:x}\"", duration.as_secs(), m.len()))
    });

    // Decide: buffer small files (compressible), stream large files
    if file_size <= STATIC_FILE_BUFFER_THRESHOLD {
        let content = match std::fs::read(&canonical_file) {
            Ok(c) => c,
            Err(_) => {
                return Some((
                    Response::builder()
                        .status(StatusCode::NOT_FOUND)
                        .body(Full::new(Bytes::from("Not Found")).boxed())
                        .unwrap(),
                    false,
                ));
            }
        };

        let mut builder = Response::builder()
            .status(StatusCode::OK)
            .header("content-type", content_type);

        if let Some(ref tag) = etag {
            builder = builder.header("etag", tag.as_str());
            builder = builder.header("cache-control", "public, max-age=3600");
        }

        Some((
            builder
                .body(Full::new(Bytes::from(content)).boxed())
                .unwrap(),
            true, // compressible
        ))
    } else {
        // Stream large files via tokio::fs
        let file = match tokio::fs::File::open(&canonical_file).await {
            Ok(f) => f,
            Err(_) => {
                return Some((
                    Response::builder()
                        .status(StatusCode::NOT_FOUND)
                        .body(Full::new(Bytes::from("Not Found")).boxed())
                        .unwrap(),
                    false,
                ));
            }
        };

        let stream = tokio_util::io::ReaderStream::new(file);
        use tokio_stream::StreamExt;
        let body = http_body_util::StreamBody::new(
            stream.map(|result| {
                result
                    .map(hyper::body::Frame::data)
                    .map_err(|e| Box::new(e) as Box<dyn std::error::Error + Send + Sync>)
            }),
        );

        let mut builder = Response::builder()
            .status(StatusCode::OK)
            .header("content-type", content_type)
            .header("content-length", file_size.to_string());

        if let Some(ref tag) = etag {
            builder = builder.header("etag", tag.as_str());
            builder = builder.header("cache-control", "public, max-age=3600");
        }

        Some((
            builder
                .body(body.boxed())
                .unwrap(),
            false, // not compressible (streaming)
        ))
    }
}

/// Guess MIME content type from file extension.
fn guess_content_type(path: &std::path::Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some("html") | Some("htm") => "text/html; charset=utf-8",
        Some("css") => "text/css; charset=utf-8",
        Some("js") | Some("mjs") => "application/javascript; charset=utf-8",
        Some("json") => "application/json; charset=utf-8",
        Some("xml") => "application/xml; charset=utf-8",
        Some("svg") => "image/svg+xml",
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("gif") => "image/gif",
        Some("webp") => "image/webp",
        Some("ico") => "image/x-icon",
        Some("woff") => "font/woff",
        Some("woff2") => "font/woff2",
        Some("ttf") => "font/ttf",
        Some("otf") => "font/otf",
        Some("pdf") => "application/pdf",
        Some("txt") => "text/plain; charset=utf-8",
        Some("md") => "text/markdown; charset=utf-8",
        Some("wasm") => "application/wasm",
        Some("map") => "application/json",
        _ => "application/octet-stream",
    }
}

// ---------------------------------------------------------------------------
// Cookie Helpers
// ---------------------------------------------------------------------------

/// Parse cookies from a request's Cookie header.
pub fn parse_cookies(headers: &hyper::HeaderMap) -> HashMap<String, String> {
    let mut cookies = HashMap::new();
    if let Some(cookie_header) = headers.get("cookie").and_then(|v| v.to_str().ok()) {
        for pair in cookie_header.split(';') {
            let pair = pair.trim();
            if let Some(eq_pos) = pair.find('=') {
                let name = pair[..eq_pos].trim().to_string();
                let value = pair[eq_pos + 1..].trim().to_string();
                cookies.insert(name, value);
            }
        }
    }
    cookies
}

/// Build a Set-Cookie header value.
pub fn build_set_cookie(
    name: &str,
    value: &str,
    path: Option<&str>,
    max_age: Option<u64>,
    http_only: bool,
    secure: bool,
    same_site: Option<&str>,
) -> String {
    let mut cookie = format!("{}={}", name, value);
    if let Some(p) = path {
        cookie.push_str(&format!("; Path={}", p));
    }
    if let Some(age) = max_age {
        cookie.push_str(&format!("; Max-Age={}", age));
    }
    if http_only {
        cookie.push_str("; HttpOnly");
    }
    if secure {
        cookie.push_str("; Secure");
    }
    if let Some(ss) = same_site {
        cookie.push_str(&format!("; SameSite={}", ss));
    }
    cookie
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

    // -----------------------------------------------------------------------
    // Compression Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_gzip_compress_small_data() {
        // Small data should still compress if possible
        let data = b"hello world";
        // gzip_compress returns None if compressed >= original
        let _ = gzip_compress(data);
    }

    #[test]
    fn test_gzip_compress_large_compressible() {
        let data = "a".repeat(2000);
        let result = gzip_compress(data.as_bytes());
        assert!(result.is_some());
        assert!(result.unwrap().len() < data.len());
    }

    #[test]
    fn test_try_compress_response_skips_small() {
        let resp = Response::builder()
            .header("content-type", "text/plain")
            .body(Full::new(Bytes::from("small")).boxed())
            .unwrap();
        let result = try_compress_response(resp, true);
        assert!(!result.headers().contains_key("content-encoding"));
    }

    #[test]
    fn test_try_compress_response_skips_binary() {
        let resp = Response::builder()
            .header("content-type", "image/png")
            .body(Full::new(Bytes::from("a".repeat(2000))).boxed())
            .unwrap();
        let result = try_compress_response(resp, true);
        assert!(!result.headers().contains_key("content-encoding"));
    }

    #[test]
    fn test_try_compress_response_compresses_text() {
        let resp = Response::builder()
            .header("content-type", "text/html")
            .body(Full::new(Bytes::from("a".repeat(2000))).boxed())
            .unwrap();
        let result = try_compress_response(resp, true);
        assert_eq!(result.headers().get("content-encoding").unwrap(), "gzip");
        assert!(result.headers().contains_key("vary"));
    }

    #[test]
    fn test_try_compress_response_skips_no_gzip() {
        let resp = Response::builder()
            .header("content-type", "text/html")
            .body(Full::new(Bytes::from("a".repeat(2000))).boxed())
            .unwrap();
        let result = try_compress_response(resp, false);
        assert!(!result.headers().contains_key("content-encoding"));
    }

    // -----------------------------------------------------------------------
    // Content Type Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_guess_content_type() {
        use std::path::Path;
        assert_eq!(
            guess_content_type(Path::new("file.html")),
            "text/html; charset=utf-8"
        );
        assert_eq!(
            guess_content_type(Path::new("file.css")),
            "text/css; charset=utf-8"
        );
        assert_eq!(
            guess_content_type(Path::new("file.js")),
            "application/javascript; charset=utf-8"
        );
        assert_eq!(
            guess_content_type(Path::new("file.json")),
            "application/json; charset=utf-8"
        );
        assert_eq!(guess_content_type(Path::new("file.png")), "image/png");
        assert_eq!(
            guess_content_type(Path::new("file.wasm")),
            "application/wasm"
        );
        assert_eq!(
            guess_content_type(Path::new("file.xyz")),
            "application/octet-stream"
        );
    }

    // -----------------------------------------------------------------------
    // Cookie Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_cookies() {
        let mut headers = hyper::HeaderMap::new();
        headers.insert(
            "cookie",
            hyper::header::HeaderValue::from_static("session=abc123; theme=dark; lang=en"),
        );
        let cookies = parse_cookies(&headers);
        assert_eq!(cookies.get("session"), Some(&"abc123".to_string()));
        assert_eq!(cookies.get("theme"), Some(&"dark".to_string()));
        assert_eq!(cookies.get("lang"), Some(&"en".to_string()));
    }

    #[test]
    fn test_parse_cookies_empty() {
        let headers = hyper::HeaderMap::new();
        let cookies = parse_cookies(&headers);
        assert!(cookies.is_empty());
    }

    #[test]
    fn test_build_set_cookie() {
        let cookie = build_set_cookie(
            "session",
            "abc123",
            Some("/"),
            Some(3600),
            true,
            true,
            Some("Strict"),
        );
        assert_eq!(
            cookie,
            "session=abc123; Path=/; Max-Age=3600; HttpOnly; Secure; SameSite=Strict"
        );
    }

    #[test]
    fn test_build_set_cookie_minimal() {
        let cookie = build_set_cookie("name", "value", None, None, false, false, None);
        assert_eq!(cookie, "name=value");
    }

    // -----------------------------------------------------------------------
    // Static File Serving Tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_static_file_path_traversal() {
        // Should return Forbidden for path traversal
        let result = serve_static_file("/tmp", "/static", "/static/../etc/passwd").await;
        assert!(result.is_some());
        let (resp, _) = result.unwrap();
        assert_eq!(resp.status(), StatusCode::FORBIDDEN);
    }

    #[tokio::test]
    async fn test_static_file_not_found() {
        let result = serve_static_file("/tmp", "/static", "/static/nonexistent_file_xyz.txt").await;
        assert!(result.is_some());
        let (resp, _) = result.unwrap();
        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_static_file_empty_relative() {
        let result = serve_static_file("/tmp", "/static", "/static/").await;
        assert!(result.is_none());
    }

    // -----------------------------------------------------------------------
    // Compressible Content Type Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_is_compressible() {
        let mut headers = hyper::HeaderMap::new();
        headers.insert(
            "content-type",
            hyper::header::HeaderValue::from_static("text/html"),
        );
        assert!(is_compressible_content_type(&headers));

        headers.insert(
            "content-type",
            hyper::header::HeaderValue::from_static("application/json"),
        );
        assert!(is_compressible_content_type(&headers));

        headers.insert(
            "content-type",
            hyper::header::HeaderValue::from_static("image/png"),
        );
        assert!(!is_compressible_content_type(&headers));
    }

    #[test]
    fn test_accepts_gzip_check() {
        let mut headers = hyper::HeaderMap::new();
        headers.insert(
            "accept-encoding",
            hyper::header::HeaderValue::from_static("gzip, deflate, br"),
        );
        assert!(accepts_gzip(&headers));

        let empty = hyper::HeaderMap::new();
        assert!(!accepts_gzip(&empty));
    }

    // -----------------------------------------------------------------------
    // SSE Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_sse_event_encode_data_only() {
        let event = SseEvent::new("hello");
        assert_eq!(event.encode(), Bytes::from("data: hello\n\n"));
    }

    #[test]
    fn test_sse_event_encode_with_event_type() {
        let event = SseEvent::new("world").with_event("greeting");
        assert_eq!(
            event.encode(),
            Bytes::from("event: greeting\ndata: world\n\n")
        );
    }

    #[test]
    fn test_sse_event_encode_with_id() {
        let event = SseEvent::new("data").with_id("42");
        assert_eq!(event.encode(), Bytes::from("id: 42\ndata: data\n\n"));
    }

    #[test]
    fn test_sse_event_encode_with_retry() {
        let event = SseEvent::new("data").with_retry(5000);
        assert_eq!(
            event.encode(),
            Bytes::from("retry: 5000\ndata: data\n\n")
        );
    }

    #[test]
    fn test_sse_event_encode_full() {
        let event = SseEvent::new("payload")
            .with_event("update")
            .with_id("7")
            .with_retry(3000);
        assert_eq!(
            event.encode(),
            Bytes::from("event: update\nid: 7\nretry: 3000\ndata: payload\n\n")
        );
    }

    #[test]
    fn test_sse_event_encode_multiline() {
        let event = SseEvent::new("line1\nline2\nline3");
        assert_eq!(
            event.encode(),
            Bytes::from("data: line1\ndata: line2\ndata: line3\n\n")
        );
    }

    #[test]
    fn test_sse_event_encode_empty_data() {
        let event = SseEvent::new("");
        assert_eq!(event.encode(), Bytes::from("data: \n\n"));
    }

    #[test]
    fn test_sse_route_builder() {
        let tree = AsyncRouteTree::builder()
            .sse("/events", |_req, _tx| {
                // Handler body not called in this test
            })
            .build();

        let result = tree.find("GET", "/events");
        assert!(result.is_some());
        let (handler, _params) = result.unwrap();
        assert!(matches!(handler, AsyncHandler::NativeSse(_)));

        // SSE routes should not match other methods
        assert!(tree.find("POST", "/events").is_none());
    }

    #[test]
    fn test_insert_request_id_adds_header() {
        let meta = RequestMeta {
            request_id: Some("req-abc-123".to_string()),
            request_id_hdr: Some(
                hyper::header::HeaderName::from_static("x-request-id"),
            ),
            accepts_gzip: false,
            start: std::time::Instant::now(),
        };
        let mut resp = AsyncResponse::text(200, "ok").into_hyper();
        insert_request_id(&mut resp, &meta);
        assert_eq!(
            resp.headers().get("x-request-id").unwrap().to_str().unwrap(),
            "req-abc-123"
        );
    }

    #[test]
    fn test_insert_request_id_noop_when_not_configured() {
        let meta = RequestMeta {
            request_id: None,
            request_id_hdr: None,
            accepts_gzip: false,
            start: std::time::Instant::now(),
        };
        let mut resp = AsyncResponse::text(200, "ok").into_hyper();
        insert_request_id(&mut resp, &meta);
        assert!(resp.headers().get("x-request-id").is_none());
    }

    #[test]
    fn test_finalize_response_applies_request_id() {
        let meta = RequestMeta {
            request_id: Some("fin-001".to_string()),
            request_id_hdr: Some(
                hyper::header::HeaderName::from_static("x-req-id"),
            ),
            accepts_gzip: false,
            start: std::time::Instant::now(),
        };
        let config = ServerConfig::default();
        let resp = AsyncResponse::text(200, "hello").into_hyper();
        let resp = finalize_response(resp, &meta, &config);
        assert_eq!(
            resp.headers().get("x-req-id").unwrap().to_str().unwrap(),
            "fin-001"
        );
    }

    #[test]
    fn test_openapi_no_params() {
        let routes = vec![
            ("GET".to_string(), "/health".to_string()),
        ];
        let json = generate_openapi_json(&routes);
        assert!(json.contains(r#""/health""#));
        assert!(!json.contains("parameters"));
    }

    #[test]
    fn test_openapi_extracts_path_params() {
        let routes = vec![
            ("GET".to_string(), "/users/:id".to_string()),
        ];
        let json = generate_openapi_json(&routes);
        // :id should become {id} in the path
        assert!(json.contains(r#"/users/{id}"#), "path param not converted: {}", json);
        // Should include parameters array
        assert!(json.contains(r#""name":"id""#), "param name missing: {}", json);
        assert!(json.contains(r#""in":"path""#), "param in missing: {}", json);
        assert!(json.contains(r#""required":true"#), "required missing: {}", json);
    }

    #[test]
    fn test_openapi_multiple_params() {
        let routes = vec![
            ("GET".to_string(), "/orgs/:org_id/users/:user_id".to_string()),
        ];
        let json = generate_openapi_json(&routes);
        assert!(json.contains(r#"/orgs/{org_id}/users/{user_id}"#));
        assert!(json.contains(r#""name":"org_id""#));
        assert!(json.contains(r#""name":"user_id""#));
    }
}
