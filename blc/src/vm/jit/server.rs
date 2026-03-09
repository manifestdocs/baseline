//! JIT HTTP server runtime using Hyper + Tokio.
//!
//! Replaces the old VM-based hyper_server.rs with JIT-native handler dispatch.
//! Handlers are called via Cranelift trampolines (platform CC → Tail CC).
//!
//! Architecture:
//! - Single-threaded Tokio runtime (NValue uses Rc, not Send)
//! - JIT fn_table + trampolines set once at startup
//! - Per-request: parse → route match → call handler via trampoline → respond

use bytes::Bytes;
use http_body_util::{BodyExt, Full};
use hyper::body::Incoming;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::net::{IpAddr, SocketAddr};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tokio::net::TcpListener;

/// Global request counter for generating unique request IDs.
static REQUEST_COUNTER: AtomicU64 = AtomicU64::new(1);

use crate::vm::natives::http_error::{http_error_status_code, http_error_to_response_record};
use crate::vm::nvalue::NValue;
use crate::vm::radix::{RadixTree, SmallParams};

use baseline_rt::helpers;
use baseline_rt::nvalue::HeapObject;

// ---------------------------------------------------------------------------
// Handler info extracted from Router NValue at startup
// ---------------------------------------------------------------------------

/// Information needed to call a JIT-compiled handler function.
#[derive(Clone)]
struct JitHandlerInfo {
    /// Function index in the JIT dispatch table (chunk_idx).
    chunk_idx: usize,
    /// Whether this is a closure (needs closure value as first arg).
    is_closure: bool,
    /// Raw NValue bits for closure value (passed as first arg if is_closure).
    closure_bits: u64,
}

// ---------------------------------------------------------------------------
// Route extraction from Router NValue
// ---------------------------------------------------------------------------

/// Extract routes from a Router NValue record and build a RadixTree.
///
/// Router NValue structure:
/// ```
/// { routes: [{ method: String, path: String, handler: Function|Closure }, ...],
///   middleware: [...], state: {...} }
/// ```
fn extract_routes(
    router: &NValue,
) -> Result<
    (
        RadixTree<JitHandlerInfo>,
        Vec<JitHandlerInfo>,
        Option<NValue>,
    ),
    String,
> {
    let fields = router.as_record().ok_or("Router must be a record")?;

    let routes = fields
        .iter()
        .find(|(k, _)| &**k == "routes")
        .and_then(|(_, v)| v.as_list())
        .ok_or("Router must have 'routes' list")?;

    // Extract middleware handlers
    let middleware_list = fields
        .iter()
        .find(|(k, _)| &**k == "middleware")
        .and_then(|(_, v)| v.as_list());

    let mut middleware = Vec::new();
    if let Some(mw_list) = middleware_list {
        for mw in mw_list {
            if let Some(info) = extract_handler_info(mw) {
                middleware.push(info);
            }
        }
    }

    // Extract shared state
    let state = fields
        .iter()
        .find(|(k, _)| &**k == "state")
        .map(|(_, v)| v.clone());

    let mut tree = RadixTree::new();

    for route in routes {
        let route_fields = route.as_record().ok_or("Route must be a record")?;

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

        let info = extract_handler_info(handler_val)
            .ok_or_else(|| format!("Invalid handler for route {} {}", method, path))?;

        tree.insert(method, path, info);
    }

    Ok((tree, middleware, state))
}

/// Extract JIT handler info from an NValue (Function or Closure).
fn extract_handler_info(val: &NValue) -> Option<JitHandlerInfo> {
    if val.is_function() {
        Some(JitHandlerInfo {
            chunk_idx: val.as_function(),
            is_closure: false,
            closure_bits: 0,
        })
    } else if val.is_heap() {
        if let HeapObject::Closure { chunk_idx, .. } = val.as_heap_ref() {
            Some(JitHandlerInfo {
                chunk_idx: *chunk_idx,
                is_closure: true,
                closure_bits: val.raw(),
            })
        } else {
            None
        }
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// NValue ↔ HTTP conversion
// ---------------------------------------------------------------------------

/// Build an NValue Request record from HTTP request components.
///
/// Record fields (alphabetical order per NValue convention):
/// body, headers, method, params, path, query
fn build_request_nvalue(
    method: &str,
    path: &str,
    query: Option<&str>,
    headers: &hyper::HeaderMap,
    body: &[u8],
    params: &SmallParams,
    state: &Option<NValue>,
) -> NValue {
    let mut fields: Vec<(baseline_rt::value::RcStr, NValue)> = Vec::with_capacity(8);

    // body
    let body_str = String::from_utf8_lossy(body);
    fields.push(("body".into(), NValue::string(body_str.into_owned().into())));

    // headers as list of (name, value) tuples
    let header_list: Vec<NValue> = headers
        .iter()
        .map(|(k, v)| {
            NValue::tuple(vec![
                NValue::string(k.as_str().into()),
                NValue::string(v.to_str().unwrap_or("").into()),
            ])
        })
        .collect();
    fields.push(("headers".into(), NValue::list(header_list)));

    // method
    fields.push(("method".into(), NValue::string(method.into())));

    // params as record
    let param_fields: Vec<(baseline_rt::value::RcStr, NValue)> = params
        .as_slice()
        .iter()
        .map(|(k, v)| (k.as_str().into(), NValue::string(v.as_str().into())))
        .collect();
    fields.push(("params".into(), NValue::record(param_fields)));

    // path
    fields.push(("path".into(), NValue::string(path.into())));

    // query as record (parsed from query string)
    let query_fields: Vec<(baseline_rt::value::RcStr, NValue)> = match query {
        Some(q) => q
            .split('&')
            .filter_map(|pair| {
                let (key, value) = pair.split_once('=')?;
                Some((
                    urlencoding_decode(key).into(),
                    NValue::string(urlencoding_decode(value).into()),
                ))
            })
            .collect(),
        None => Vec::new(),
    };
    fields.push(("query".into(), NValue::record(query_fields)));

    // Inject shared state if present
    if let Some(state_val) = state {
        fields.push(("state".into(), state_val.clone()));
    }

    // NValue::record sorts fields alphabetically
    NValue::record(fields)
}

/// Convert a handler return NValue to a hyper Response.
///
/// Handlers return `Result<Response, HttpError>`:
/// - Ok variant: Response record with { status, body, headers }
/// - Err variant: HttpError enum, converted via http_error_to_response_record
fn nvalue_to_response(val: &NValue, accept_encoding: Option<&str>) -> Response<Full<Bytes>> {
    // Check if it's a Result enum (Ok/Err)
    if let Some((tag, payload)) = val.as_enum() {
        match &**tag {
            "Ok" => return response_record_to_hyper(payload, accept_encoding),
            "Err" => {
                // Check if the payload is an HttpError enum
                if let Some((err_tag, _err_payload)) = payload.as_enum() {
                    if http_error_status_code(&err_tag).is_some() {
                        let resp_record = http_error_to_response_record(&err_tag, _err_payload);
                        return response_record_to_hyper(&resp_record, accept_encoding);
                    }
                }
                // Generic error: return 500
                let msg = format!("{}", payload);
                return Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .header("Content-Type", "text/plain")
                    .body(Full::new(Bytes::from(msg)))
                    .unwrap();
            }
            _ => {}
        }
    }

    // If not a Result, try treating it as a Response record directly
    if val.as_record().is_some() {
        return response_record_to_hyper(val, accept_encoding);
    }

    // Fallback: return the value as text
    let text = format!("{}", val);
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "text/plain")
        .body(Full::new(Bytes::from(text)))
        .unwrap()
}

/// Convert a Response record NValue to a hyper Response.
/// If `accept_encoding` contains "gzip", compresses eligible bodies.
fn response_record_to_hyper(val: &NValue, accept_encoding: Option<&str>) -> Response<Full<Bytes>> {
    let fields = match val.as_record() {
        Some(f) => f,
        None => {
            return Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(Full::new(Bytes::from("Handler returned non-record")))
                .unwrap();
        }
    };

    let status = fields
        .iter()
        .find(|(k, _)| &**k == "status")
        .map(|(_, v)| v.as_any_int() as u16)
        .unwrap_or(200);

    let body = fields
        .iter()
        .find(|(k, _)| &**k == "body")
        .and_then(|(_, v)| v.as_str())
        .unwrap_or("");

    let mut builder = Response::builder().status(status);

    // Extract headers list: [(name, value), ...]
    if let Some(header_list) = fields
        .iter()
        .find(|(k, _)| &**k == "headers")
        .and_then(|(_, v)| v.as_list())
    {
        for header in header_list {
            if let Some(items) = header.as_tuple() {
                if items.len() == 2 {
                    if let (Some(name), Some(value)) = (items[0].as_str(), items[1].as_str()) {
                        builder = builder.header(name, value);
                    }
                }
            }
        }
    }

    // Determine content type for compression eligibility
    let content_type = fields
        .iter()
        .find(|(k, _)| &**k == "headers")
        .and_then(|(_, v)| v.as_list())
        .and_then(|headers| {
            headers.iter().find_map(|h| {
                let items = h.as_tuple()?;
                if items.len() == 2 && items[0].as_str()?.eq_ignore_ascii_case("content-type") {
                    items[1].as_str().map(|s| s.to_string())
                } else {
                    None
                }
            })
        });

    let body_bytes = body.as_bytes();
    let accepts_gzip = accept_encoding
        .map(|ae| ae.contains("gzip"))
        .unwrap_or(false);

    if accepts_gzip && should_compress(content_type.as_deref(), body_bytes.len()) {
        if let Some(compressed) = gzip_compress(body_bytes) {
            return builder
                .header("Content-Encoding", "gzip")
                .body(Full::new(Bytes::from(compressed)))
                .unwrap();
        }
    }

    builder
        .body(Full::new(Bytes::from(body.to_string())))
        .unwrap()
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
// JIT handler invocation
// ---------------------------------------------------------------------------

/// Call a JIT-compiled handler function with a Request NValue argument.
///
/// Sets up RC lifecycle: the return value is borrowed (incref'd) and then
/// the JIT's ownership is released (decref'd).
fn call_handler(info: &JitHandlerInfo, request_raw: u64, rc_enabled: bool) -> Option<NValue> {
    let fn_ptr = helpers::get_fn_ptr_from_table(info.chunk_idx);
    if fn_ptr.is_null() {
        helpers::jit_set_error(format!(
            "Handler function not found in JIT table (chunk_idx={})",
            info.chunk_idx
        ));
        return None;
    }

    let raw = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        helpers::call_jit_fn(fn_ptr, info.closure_bits, info.is_closure, &[request_raw])
    })) {
        Ok(val) => val,
        Err(payload) => {
            let msg = if let Some(s) = payload.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = payload.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic in JIT handler".to_string()
            };
            eprintln!("[server] Handler panicked: {}", msg);
            return None;
        }
    };

    // Extract the return value before cleanup
    let result = unsafe { NValue::borrow_from_raw(raw) };

    // RC mode: release JIT's ownership (borrow_from_raw bumped refcount)
    if rc_enabled {
        helpers::jit_rc_decref(raw);
    }

    Some(result)
}

/// Call middleware chain then handler.
///
/// Middleware functions take (request, next) where next is a function that
/// continues the chain. The chain is managed via thread-local state:
/// 1. Set up MwChainState with all middleware + handler
/// 2. Call the first middleware with (request, next) where next = __mw_next
/// 3. When middleware calls next(req), __mw_next calls jit_middleware_dispatch
///    which advances the chain and calls the next middleware or handler
fn call_with_middleware(
    middleware: &[JitHandlerInfo],
    handler: &JitHandlerInfo,
    request_raw: u64,
    rc_enabled: bool,
    mw_next_idx: usize,
) -> Option<NValue> {
    if middleware.is_empty() {
        return call_handler(handler, request_raw, rc_enabled);
    }

    // Set up the middleware chain in thread-local state
    let mw_infos: Vec<(usize, bool, u64)> = middleware
        .iter()
        .map(|m| (m.chunk_idx, m.is_closure, m.closure_bits))
        .collect();
    let handler_info = (handler.chunk_idx, handler.is_closure, handler.closure_bits);

    // Start at index 1 since we'll call middleware[0] directly
    helpers::mw_chain_setup(
        mw_infos[1..].to_vec(),
        handler_info,
        mw_next_idx,
        rc_enabled,
    );

    // Call the first middleware with (request, next)
    let first = &middleware[0];
    let next_nv = crate::vm::nvalue::NValue::function(mw_next_idx);
    let fn_ptr = helpers::get_fn_ptr_from_table(first.chunk_idx);
    if fn_ptr.is_null() {
        helpers::mw_chain_clear();
        helpers::jit_set_error(format!(
            "First middleware function not found (chunk_idx={})",
            first.chunk_idx
        ));
        return None;
    }

    let raw = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        helpers::call_jit_fn(
            fn_ptr,
            first.closure_bits,
            first.is_closure,
            &[request_raw, next_nv.raw()],
        )
    })) {
        Ok(val) => val,
        Err(payload) => {
            helpers::mw_chain_clear();
            let msg = if let Some(s) = payload.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = payload.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic in middleware".to_string()
            };
            eprintln!("[server] Middleware panicked: {}", msg);
            return None;
        }
    };

    helpers::mw_chain_clear();

    // Extract return value
    let result = unsafe { crate::vm::nvalue::NValue::borrow_from_raw(raw) };
    if rc_enabled {
        helpers::jit_rc_decref(raw);
    }

    Some(result)
}

// ---------------------------------------------------------------------------
// Accept loop with graceful shutdown signal handling
// ---------------------------------------------------------------------------

/// Spawn a connection task with graceful shutdown support.
///
/// Each connection listens for a shutdown signal via `watch` channel.
/// On signal, calls `graceful_shutdown()` on the HTTP connection to finish
/// the current request without accepting new requests on keep-alive.
fn spawn_connection(
    stream: tokio::net::TcpStream,
    peer_addr: SocketAddr,
    tree: &std::rc::Rc<RadixTree<JitHandlerInfo>>,
    middleware: &std::rc::Rc<Vec<JitHandlerInfo>>,
    state: &std::rc::Rc<Option<NValue>>,
    rate_limiter: &std::rc::Rc<RateLimiter>,
    shutdown_rx: &tokio::sync::watch::Receiver<bool>,
    active: &std::rc::Rc<Cell<u32>>,
    rc_enabled: bool,
    mw_next_idx: usize,
) {
    let tree = tree.clone();
    let middleware = middleware.clone();
    let state = state.clone();
    let rate_limiter = rate_limiter.clone();
    let active = active.clone();
    let mut shutdown_rx = shutdown_rx.clone();
    let peer_ip = peer_addr.ip();

    let service = service_fn(move |req: Request<Incoming>| {
        let tree = tree.clone();
        let middleware = middleware.clone();
        let state = state.clone();
        let rate_limiter = rate_limiter.clone();
        async move {
            handle_request(
                req,
                &tree,
                &middleware,
                &state,
                &rate_limiter,
                peer_ip,
                rc_enabled,
                mw_next_idx,
            )
            .await
        }
    });

    active.set(active.get() + 1);

    tokio::task::spawn_local(async move {
        let io = hyper_util::rt::TokioIo::new(stream);
        let conn = http1::Builder::new()
            .keep_alive(true)
            .header_read_timeout(KEEP_ALIVE_TIMEOUT)
            .timer(hyper_util::rt::TokioTimer::new())
            .max_buf_size(MAX_HEADER_BUF_SIZE)
            .max_headers(MAX_HEADERS)
            .serve_connection(io, service);
        tokio::pin!(conn);

        loop {
            tokio::select! {
                result = conn.as_mut() => {
                    if let Err(e) = result {
                        if !e.to_string().contains("connection closed") {
                            eprintln!("[server] Connection error: {}", e);
                        }
                    }
                    break;
                }
                _ = shutdown_rx.changed() => {
                    // Finish current request, then close (no more keep-alive)
                    conn.as_mut().graceful_shutdown();
                    if let Err(e) = conn.as_mut().await {
                        if !e.to_string().contains("connection closed") {
                            eprintln!("[server] Connection error during shutdown: {}", e);
                        }
                    }
                    break;
                }
            }
        }

        active.set(active.get() - 1);
    });
}

/// Accept loop that breaks on SIGINT or SIGTERM.
#[allow(clippy::too_many_arguments)]
async fn accept_loop(
    listener: &TcpListener,
    tree: &std::rc::Rc<RadixTree<JitHandlerInfo>>,
    middleware: &std::rc::Rc<Vec<JitHandlerInfo>>,
    state: &std::rc::Rc<Option<NValue>>,
    rate_limiter: &std::rc::Rc<RateLimiter>,
    shutdown_rx: &tokio::sync::watch::Receiver<bool>,
    active: &std::rc::Rc<Cell<u32>>,
    rc_enabled: bool,
    mw_next_idx: usize,
) {
    #[cfg(unix)]
    let mut sigterm_signal =
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("Failed to register SIGTERM handler");

    let sigterm = async {
        #[cfg(unix)]
        {
            sigterm_signal.recv().await;
        }
        #[cfg(not(unix))]
        {
            std::future::pending::<()>().await;
        }
    };
    tokio::pin!(sigterm);

    loop {
        // Wait for connection or shutdown signal
        let accept_result = tokio::select! {
            result = listener.accept() => result,
            _ = tokio::signal::ctrl_c() => {
                eprintln!("\n[server] Graceful shutdown initiated (SIGINT)...");
                return;
            }
            _ = &mut sigterm => {
                eprintln!("[server] Graceful shutdown initiated (SIGTERM)...");
                return;
            }
        };

        match accept_result {
            Ok((stream, peer_addr)) => {
                spawn_connection(
                    stream,
                    peer_addr,
                    tree,
                    middleware,
                    state,
                    rate_limiter,
                    shutdown_rx,
                    active,
                    rc_enabled,
                    mw_next_idx,
                );
            }
            Err(e) => {
                eprintln!("[server] Accept error: {}", e);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Server entry point
// ---------------------------------------------------------------------------

/// Default graceful shutdown timeout in seconds.
const DEFAULT_SHUTDOWN_TIMEOUT_SECS: u64 = 30;

/// Maximum header buffer size (8 KB). Prevents memory exhaustion from oversized headers.
const MAX_HEADER_BUF_SIZE: usize = 8 * 1024;

/// Maximum number of headers per request (100). Prevents abuse via header flooding.
const MAX_HEADERS: usize = 100;

/// Maximum request body size (1 MB). Prevents memory exhaustion from large payloads.
const MAX_BODY_SIZE: u64 = 1024 * 1024;

/// Keep-alive timeout (30 seconds). Idle connections close after this duration.
const KEEP_ALIVE_TIMEOUT: Duration = Duration::from_secs(30);

/// Default rate limit: requests per second per IP.
const RATE_LIMIT_RPS: u32 = 100;

/// Rate limit window duration.
const RATE_LIMIT_WINDOW: Duration = Duration::from_secs(1);

// ---------------------------------------------------------------------------
// Per-IP token bucket rate limiter
// ---------------------------------------------------------------------------

/// Token bucket entry for a single IP address.
struct TokenBucket {
    tokens: u32,
    last_refill: Instant,
}

/// Simple per-IP rate limiter using token bucket algorithm.
///
/// Single-threaded (Rc<RefCell>) — safe because Tokio current_thread runtime.
/// Stale entries are cleaned up periodically during `check()`.
struct RateLimiter {
    buckets: RefCell<HashMap<IpAddr, TokenBucket>>,
    max_tokens: u32,
    window: Duration,
    check_count: Cell<u32>,
}

impl RateLimiter {
    fn new(max_rps: u32) -> Self {
        Self {
            buckets: RefCell::new(HashMap::new()),
            max_tokens: max_rps,
            window: RATE_LIMIT_WINDOW,
            check_count: Cell::new(0),
        }
    }

    /// Returns true if the request is allowed, false if rate limited.
    fn check(&self, ip: IpAddr) -> bool {
        let now = Instant::now();
        let mut buckets = self.buckets.borrow_mut();

        // Periodic cleanup: every 1000 checks, remove stale entries (>60s idle)
        let count = self.check_count.get().wrapping_add(1);
        self.check_count.set(count);
        if count % 1000 == 0 {
            buckets.retain(|_, b| now.duration_since(b.last_refill) < Duration::from_secs(60));
        }

        let bucket = buckets.entry(ip).or_insert(TokenBucket {
            tokens: self.max_tokens,
            last_refill: now,
        });

        // Refill tokens based on elapsed time
        let elapsed = now.duration_since(bucket.last_refill);
        if elapsed >= self.window {
            let refills = (elapsed.as_secs_f64() / self.window.as_secs_f64()) as u32;
            bucket.tokens = (bucket.tokens + refills * self.max_tokens).min(self.max_tokens);
            bucket.last_refill = now;
        }

        if bucket.tokens > 0 {
            bucket.tokens -= 1;
            true
        } else {
            false
        }
    }
}

/// Start the JIT HTTP server with graceful shutdown support.
///
/// On SIGINT (ctrl-c) or SIGTERM, the server:
/// 1. Stops accepting new connections
/// 2. Signals existing connections to finish their current request (no keep-alive)
/// 3. Waits up to `shutdown_timeout` seconds for in-flight requests to complete
/// 4. Exits with code 0 (clean) or 1 (forced timeout)
///
/// Called from main.rs after detecting a Server.listen! stash.
pub fn run_server(
    router: NValue,
    port: u16,
    fn_table: &[*const u8],
    trampolines: [*const u8; 5],
    rc_enabled: bool,
    mw_next_idx: usize,
) -> ! {
    // Set up JIT context for this thread (single-threaded runtime)
    helpers::set_fn_table(fn_table);
    helpers::set_trampolines(trampolines);
    if rc_enabled {
        helpers::jit_set_rc_mode(true);
    }

    // Extract routes from Router NValue
    let (tree, middleware, state) = match extract_routes(&router) {
        Ok(result) => result,
        Err(e) => {
            eprintln!("[server] Failed to extract routes: {}", e);
            std::process::exit(1);
        }
    };

    // Count routes for logging
    let route_count = count_routes(&tree.root);

    // Build the Tokio runtime (single-threaded for NValue Rc safety)
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to create Tokio runtime");

    // LocalSet is required for spawn_local on current_thread runtime
    let local = tokio::task::LocalSet::new();

    let exit_code = rt.block_on(local.run_until(async move {
        let addr = SocketAddr::from(([0, 0, 0, 0], port));
        let listener = match TcpListener::bind(addr).await {
            Ok(l) => l,
            Err(e) => {
                eprintln!("[server] Failed to bind to {}: {}", addr, e);
                std::process::exit(1);
            }
        };

        eprintln!(
            "[server] Listening on http://{} ({} routes)",
            addr, route_count
        );

        // Shared state for the service closure
        let tree = std::rc::Rc::new(tree);
        let middleware = std::rc::Rc::new(middleware);
        let state = std::rc::Rc::new(state);
        let rate_limiter = std::rc::Rc::new(RateLimiter::new(RATE_LIMIT_RPS));

        // Graceful shutdown: watch channel signals connections to drain
        let (shutdown_tx, shutdown_rx) = tokio::sync::watch::channel(false);
        let active_connections = std::rc::Rc::new(Cell::new(0u32));

        // Accept loop — breaks on shutdown signal
        accept_loop(
            &listener,
            &tree,
            &middleware,
            &state,
            &rate_limiter,
            &shutdown_rx,
            &active_connections,
            rc_enabled,
            mw_next_idx,
        )
        .await;

        // Phase 2: Signal all connections to drain
        shutdown_tx.send(true).ok();

        let remaining = active_connections.get();
        if remaining == 0 {
            eprintln!("[server] No active connections, shutting down immediately");
            return 0;
        }

        eprintln!(
            "[server] Waiting for {} connection(s) to drain (timeout: {}s)...",
            remaining, DEFAULT_SHUTDOWN_TIMEOUT_SECS
        );

        // Phase 3: Wait for connections to finish with timeout
        let deadline =
            tokio::time::Instant::now() + Duration::from_secs(DEFAULT_SHUTDOWN_TIMEOUT_SECS);

        while active_connections.get() > 0 {
            if tokio::time::Instant::now() >= deadline {
                eprintln!(
                    "[server] Shutdown timeout! {} connection(s) forcefully closed",
                    active_connections.get()
                );
                return 1;
            }
            tokio::time::sleep(Duration::from_millis(50)).await;
        }

        eprintln!("[server] All connections drained, shutdown complete");
        0
    }));

    std::process::exit(exit_code);
}

/// Handle a single HTTP request with logging, request ID, and health check.
async fn handle_request(
    req: Request<Incoming>,
    tree: &RadixTree<JitHandlerInfo>,
    middleware: &[JitHandlerInfo],
    state: &Option<NValue>,
    rate_limiter: &RateLimiter,
    peer_ip: IpAddr,
    rc_enabled: bool,
    mw_next_idx: usize,
) -> Result<Response<Full<Bytes>>, std::convert::Infallible> {
    let start = std::time::Instant::now();
    let method = req.method().clone();
    let uri = req.uri().clone();
    let path = uri.path().to_string();
    let query = uri.query().map(|q| q.to_string());
    let headers = req.headers().clone();

    // Capture Accept-Encoding for response compression
    let accept_encoding = headers
        .get("accept-encoding")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string());

    // Generate or extract request ID
    let request_id = match req.headers().get("x-request-id") {
        Some(id) => id.to_str().unwrap_or("").to_string(),
        None => {
            let id = REQUEST_COUNTER.fetch_add(1, Ordering::Relaxed);
            format!("req-{}", id)
        }
    };

    // Per-IP rate limiting (token bucket)
    if !rate_limiter.check(peer_ip) {
        let resp = Response::builder()
            .status(StatusCode::TOO_MANY_REQUESTS)
            .header("Content-Type", "application/json")
            .header("Retry-After", "1")
            .header("X-Request-Id", &request_id)
            .body(Full::new(Bytes::from(
                "{\"error\":\"Too Many Requests\",\"retry_after\":1}",
            )))
            .unwrap();
        log_request(
            &method,
            &path,
            resp.status().as_u16(),
            start.elapsed(),
            &request_id,
        );
        return Ok(resp);
    }

    // Built-in health check endpoint (bypasses route tree and JIT)
    if path == "/__health" && method == hyper::Method::GET {
        let resp = Response::builder()
            .status(StatusCode::OK)
            .header("Content-Type", "application/json")
            .header("X-Request-Id", &request_id)
            .body(Full::new(Bytes::from("{\"status\":\"ok\"}")))
            .unwrap();
        log_request(
            &method,
            &path,
            resp.status().as_u16(),
            start.elapsed(),
            &request_id,
        );
        return Ok(resp);
    }

    // Built-in CORS preflight handling (OPTIONS requests)
    if method == hyper::Method::OPTIONS {
        let origin = headers
            .get("origin")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("*");
        let request_method = headers
            .get("access-control-request-method")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("GET, POST, PUT, DELETE, PATCH");
        let request_headers = headers
            .get("access-control-request-headers")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("Content-Type");

        let resp = Response::builder()
            .status(StatusCode::NO_CONTENT)
            .header("Access-Control-Allow-Origin", origin)
            .header("Access-Control-Allow-Methods", request_method)
            .header("Access-Control-Allow-Headers", request_headers)
            .header("Access-Control-Max-Age", "86400")
            .header("X-Request-Id", &request_id)
            .body(Full::new(Bytes::new()))
            .unwrap();
        log_request(
            &method,
            &path,
            resp.status().as_u16(),
            start.elapsed(),
            &request_id,
        );
        return Ok(resp);
    }

    // Check Content-Length before reading (fast reject)
    if let Some(cl) = req.headers().get(hyper::header::CONTENT_LENGTH) {
        if let Ok(len) = cl.to_str().unwrap_or("0").parse::<u64>() {
            if len > MAX_BODY_SIZE {
                let resp = Response::builder()
                    .status(StatusCode::PAYLOAD_TOO_LARGE)
                    .header("Content-Type", "application/json")
                    .header("X-Request-Id", &request_id)
                    .body(Full::new(Bytes::from(format!(
                        "{{\"error\":\"Payload Too Large\",\"max_bytes\":{}}}",
                        MAX_BODY_SIZE
                    ))))
                    .unwrap();
                log_request(
                    &method,
                    &path,
                    resp.status().as_u16(),
                    start.elapsed(),
                    &request_id,
                );
                return Ok(resp);
            }
        }
    }

    // Read body with size limit enforced via http_body_util::Limited
    let limited = http_body_util::Limited::new(req.into_body(), MAX_BODY_SIZE as usize);
    let body = match limited.collect().await {
        Ok(collected) => collected.to_bytes(),
        Err(e) => {
            let msg = e.to_string();
            let resp = if msg.contains("length limit exceeded") {
                Response::builder()
                    .status(StatusCode::PAYLOAD_TOO_LARGE)
                    .header("Content-Type", "application/json")
                    .header("X-Request-Id", &request_id)
                    .body(Full::new(Bytes::from(format!(
                        "{{\"error\":\"Payload Too Large\",\"max_bytes\":{}}}",
                        MAX_BODY_SIZE
                    ))))
                    .unwrap()
            } else {
                Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .header("X-Request-Id", &request_id)
                    .body(Full::new(Bytes::from("Failed to read request body")))
                    .unwrap()
            };
            log_request(
                &method,
                &path,
                resp.status().as_u16(),
                start.elapsed(),
                &request_id,
            );
            return Ok(resp);
        }
    };

    // Route matching
    let mut params = SmallParams::new();
    let handler = tree.find(method.as_str(), &path, &mut params);

    let handler = match handler {
        Some(h) => h,
        None => {
            let body = format!("{{\"error\":\"Not Found\",\"path\":\"{}\"}}", path);
            let resp = Response::builder()
                .status(StatusCode::NOT_FOUND)
                .header("Content-Type", "application/json")
                .header("X-Request-Id", &request_id)
                .body(Full::new(Bytes::from(body)))
                .unwrap();
            log_request(
                &method,
                &path,
                resp.status().as_u16(),
                start.elapsed(),
                &request_id,
            );
            return Ok(resp);
        }
    };

    // Build Request NValue
    let request_nv = build_request_nvalue(
        method.as_str(),
        &path,
        query.as_deref(),
        &headers,
        &body,
        &params,
        state,
    );

    // Call handler via JIT trampoline.
    // In RC mode, the JIT function takes ownership of the request parameter
    // (scope exit decrefs all tracked params). We must forget our NValue to
    // avoid a double-free when Rust's Drop also tries to decref.
    let request_raw = request_nv.raw();
    if rc_enabled {
        std::mem::forget(request_nv);
    }
    let response = call_with_middleware(middleware, handler, request_raw, rc_enabled, mw_next_idx);

    // Drain arena if not RC mode (free intermediate heap values)
    if !rc_enabled {
        drop(helpers::jit_arena_drain());
    }

    // Check for runtime errors
    if let Some(err) = helpers::jit_take_error() {
        eprintln!("[server] Runtime error: {}", err);
        let resp = Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .header("Content-Type", "application/json")
            .header("X-Request-Id", &request_id)
            .body(Full::new(Bytes::from(
                "{\"error\":\"Internal Server Error\",\"message\":\"Handler runtime error\"}",
            )))
            .unwrap();
        log_request(
            &method,
            &path,
            resp.status().as_u16(),
            start.elapsed(),
            &request_id,
        );
        return Ok(resp);
    }

    let resp = match response {
        Some(val) => {
            let mut resp = nvalue_to_response(&val, accept_encoding.as_deref());
            if let Ok(val) = hyper::header::HeaderValue::from_str(&request_id) {
                resp.headers_mut().insert("x-request-id", val);
            }
            resp
        }
        None => Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .header("Content-Type", "application/json")
            .header("X-Request-Id", &request_id)
            .body(Full::new(Bytes::from(
                "{\"error\":\"Internal Server Error\",\"message\":\"Handler failed\"}",
            )))
            .unwrap(),
    };

    log_request(
        &method,
        &path,
        resp.status().as_u16(),
        start.elapsed(),
        &request_id,
    );
    Ok(resp)
}

/// Minimum response body size to apply compression (1 KB).
const COMPRESSION_MIN_SIZE: usize = 1024;

/// Compress body bytes with gzip if beneficial.
/// Returns (possibly compressed bytes, true if compressed).
fn gzip_compress(body: &[u8]) -> Option<Vec<u8>> {
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use std::io::Write;

    let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
    encoder.write_all(body).ok()?;
    let compressed = encoder.finish().ok()?;

    // Only use compressed version if it's actually smaller
    if compressed.len() < body.len() {
        Some(compressed)
    } else {
        None
    }
}

/// Check if a content type should be compressed.
fn should_compress(content_type: Option<&str>, body_len: usize) -> bool {
    if body_len < COMPRESSION_MIN_SIZE {
        return false;
    }
    match content_type {
        Some(ct)
            if ct.starts_with("image/")
                || ct.starts_with("video/")
                || ct.starts_with("audio/")
                || ct.contains("gzip")
                || ct.contains("zip") =>
        {
            false
        }
        _ => true,
    }
}

/// Log a structured access log line for a request.
fn log_request(
    method: &hyper::Method,
    path: &str,
    status: u16,
    duration: Duration,
    request_id: &str,
) {
    eprintln!(
        "[server] {} {} {} {}ms id={}",
        method,
        path,
        status,
        duration.as_millis(),
        request_id,
    );
}

/// Count total routes in the radix tree (for logging).
fn count_routes(node: &crate::vm::radix::RadixNode<JitHandlerInfo>) -> usize {
    let mut count = node.handlers.len();
    for child in node.children.values() {
        count += count_routes(child);
    }
    if let Some((_, child)) = &node.param {
        count += count_routes(child);
    }
    count
}
