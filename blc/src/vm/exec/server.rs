use std::collections::HashMap;
use std::sync::Arc;

use crate::vm::chunk::{Chunk, CompileError, Op};
use crate::vm::nvalue::{HeapObject, NValue};
use crate::vm::radix::{RadixNode, RadixTree, SmallParams};
use crate::vm::sendable::{SendableHandler, SendableValue};

use super::http_helpers::{extract_response_nv, inject_params_nv, parse_url_query_nv};

/// Thread-safe route tree node.
struct SendableNode {
    children: HashMap<String, SendableNode>,
    param: Option<(String, Box<SendableNode>)>,
    handlers: HashMap<String, SendableHandler>,
}

/// Thread-safe route tree built from NValue routes.
struct SendableRouteTree {
    root: SendableNode,
}

// SAFETY: SendableNode/SendableRouteTree contain only owned Strings and SendableHandlers.
unsafe impl Send for SendableRouteTree {}
unsafe impl Sync for SendableRouteTree {}

impl SendableNode {
    fn new() -> Self {
        SendableNode {
            children: HashMap::new(),
            param: None,
            handlers: HashMap::new(),
        }
    }

    fn to_nv_node(&self) -> RadixNode<NValue> {
        let mut node = RadixNode::default();
        for (seg, child) in &self.children {
            node.children.insert(seg.clone(), child.to_nv_node());
        }
        if let Some((name, child)) = &self.param {
            node.param = Some((name.clone(), Box::new(child.to_nv_node())));
        }
        for (method, handler) in &self.handlers {
            node.handlers.insert(method.clone(), handler.to_nvalue());
        }
        node
    }
}

impl SendableRouteTree {
    fn from_nv_routes(routes: &[NValue]) -> Self {
        let mut tree = SendableRouteTree {
            root: SendableNode::new(),
        };
        for route in routes {
            if let Some(fields) = route.as_record() {
                let method = fields
                    .iter()
                    .find(|(k, _)| &**k == "method")
                    .and_then(|(_, v)| v.as_string())
                    .map(|s| s.to_string());
                let path = fields
                    .iter()
                    .find(|(k, _)| &**k == "path")
                    .and_then(|(_, v)| v.as_string())
                    .map(|s| s.to_string());
                let handler = fields
                    .iter()
                    .find(|(k, _)| &**k == "handler")
                    .and_then(|(_, v)| SendableHandler::from_nvalue(v));

                if let (Some(m), Some(p), Some(h)) = (method, path, handler) {
                    tree.insert(&m, &p, h);
                }
            }
        }
        tree
    }

    fn insert(&mut self, method: &str, path: &str, handler: SendableHandler) {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        let mut node = &mut self.root;
        for seg in &segments {
            if let Some(name) = seg.strip_prefix(':') {
                if node.param.is_none() {
                    node.param = Some((name.to_string(), Box::new(SendableNode::new())));
                }
                node = node.param.as_mut().unwrap().1.as_mut();
            } else {
                node = node
                    .children
                    .entry(seg.to_string())
                    .or_insert_with(SendableNode::new);
            }
        }
        node.handlers.entry(method.to_string()).or_insert(handler);
    }

    fn to_nv_radix_tree(&self) -> NvRadixTree {
        NvRadixTree {
            root: self.root.to_nv_node(),
        }
    }
}

/// Thread-safe chunk representation using owned Strings instead of Rc<str>.
struct SendableChunks {
    chunks: Vec<SendableChunk>,
}

struct SendableChunk {
    code: Vec<Op>,
    constants: Vec<SendableValue>,
    source_map: Vec<(usize, usize)>,
}

// SAFETY: SendableChunks contains only owned data (Vec<Op>, Vec<SendableValue>).
unsafe impl Send for SendableChunks {}
unsafe impl Sync for SendableChunks {}

impl SendableChunks {
    fn from_chunks(chunks: &[Chunk]) -> Self {
        SendableChunks {
            chunks: chunks
                .iter()
                .map(|c| SendableChunk {
                    code: c.code.clone(),
                    constants: c
                        .constants
                        .iter()
                        .map(|v| SendableValue::from_nvalue(v))
                        .collect(),
                    source_map: c.source_map.clone(),
                })
                .collect(),
        }
    }

    fn to_chunks(&self) -> Vec<Chunk> {
        self.chunks
            .iter()
            .map(|sc| {
                let constants: Vec<NValue> = sc.constants.iter().map(|sv| sv.to_nvalue()).collect();
                Chunk::from_parts(sc.code.clone(), constants, sc.source_map.clone())
            })
            .collect()
    }
}

// NvRadixTree is now RadixTree<NValue> from the radix module.
// Type alias for convenience.
type NvRadixTree = RadixTree<NValue>;

#[allow(dead_code)]
fn nv_radix_from_routes(routes: &[NValue]) -> NvRadixTree {
    let mut tree = NvRadixTree::new();
    for route in routes {
        if let Some(fields) = route.as_record() {
            let method = fields
                .iter()
                .find(|(k, _)| &**k == "method")
                .and_then(|(_, v)| v.as_string())
                .map(|s| s.to_string());
            let path = fields
                .iter()
                .find(|(k, _)| &**k == "path")
                .and_then(|(_, v)| v.as_string())
                .map(|s| s.to_string());
            let handler = fields
                .iter()
                .find(|(k, _)| &**k == "handler")
                .map(|(_, v)| v.clone());

            if let (Some(m), Some(p), Some(h)) = (method, path, handler) {
                tree.insert(&m, &p, h);
            }
        }
    }
    tree
}

// ---------------------------------------------------------------------------
// Middleware chain helper
// ---------------------------------------------------------------------------

/// Build a NativeMwNext NValue representing the continuation of a middleware chain.
pub(crate) fn build_mw_next_nvalue(remaining_mw: &[NValue], handler: &NValue) -> NValue {
    NValue::from_heap_obj(HeapObject::NativeMwNext {
        handler: handler.clone(),
        remaining_mw: remaining_mw.to_vec(),
    })
}

// ---------------------------------------------------------------------------
// Thread-pool server: handle a single request
// ---------------------------------------------------------------------------

fn handle_request(
    vm: &mut super::Vm,
    mut request: tiny_http::Request,
    route_tree: &NvRadixTree,
    middleware: &[NValue],
    chunks: &[Chunk],
) {
    let req_method = request.method().to_string();
    let raw_url = request.url().to_string();
    let mut req_body = String::new();
    if let Err(e) = request.as_reader().read_to_string(&mut req_body) {
        eprintln!("[server] Failed to read request body: {}", e);
        let response = tiny_http::Response::from_string("Internal Server Error")
            .with_status_code(tiny_http::StatusCode(500));
        let _ = request.respond(response);
        return;
    }

    let (req_path, query_record) = parse_url_query_nv(&raw_url);

    let req_headers: Vec<NValue> = request
        .headers()
        .iter()
        .map(|h| {
            NValue::tuple(vec![
                NValue::string(h.field.to_string().into()),
                NValue::string(h.value.to_string().into()),
            ])
        })
        .collect();

    let req_record = NValue::record(vec![
        ("body".into(), NValue::string(req_body.into())),
        ("headers".into(), NValue::list(req_headers)),
        ("method".into(), NValue::string(req_method.clone().into())),
        ("params".into(), NValue::record(Vec::new())),
        ("query".into(), query_record),
        ("url".into(), NValue::string(raw_url.into())),
    ]);

    let mut params = SmallParams::new();
    let (status, resp_headers, body) = match route_tree.find(&req_method, &req_path, &mut params) {
        Some(handler) => {
            let enriched = inject_params_nv(&req_record, params.as_slice());
            let result = if middleware.is_empty() {
                vm.call_nvalue(&handler, &[enriched], chunks, 0, 0)
            } else {
                vm.apply_mw_chain(middleware, &handler, &enriched, chunks, 0, 0)
            };
            match result {
                Ok(val) => extract_response_nv(&val),
                Err(e) => {
                    eprintln!("[server] Handler error: {}", e);
                    (500, Vec::new(), "Internal Server Error".to_string())
                }
            }
        }
        None => (404, Vec::new(), "Not Found".to_string()),
    };

    let mut response =
        tiny_http::Response::from_string(&body).with_status_code(tiny_http::StatusCode(status));
    for (name, value) in &resp_headers {
        if let Ok(header) = tiny_http::Header::from_bytes(name.as_bytes(), value.as_bytes()) {
            response = response.with_header(header);
        }
    }
    let _ = request.respond(response);
}

// ---------------------------------------------------------------------------
// Vm server dispatch methods
// ---------------------------------------------------------------------------

impl super::Vm {
    /// Dispatch Server.listen!(router, port) via Hyper+Tokio async server.
    /// Handles both Server.listen! and Server.listen_async! when async-server feature is enabled.
    #[cfg(feature = "async-server")]
    pub(crate) fn dispatch_server_listen_async(
        &mut self,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        if arg_count < 2 || arg_count > 3 {
            return Err(self.error(
                format!(
                    "Server.listen! expects 2-3 arguments (router, port[, config]), got {}",
                    arg_count
                ),
                line,
                col,
            ));
        }

        let config_val = if arg_count == 3 {
            Some(self.pop(line, col)?)
        } else {
            None
        };
        let port_val = self.pop(line, col)?;
        let router_val = self.pop(line, col)?;

        if !port_val.is_any_int() {
            return Err(self.error(
                format!("Server.listen! port must be Int, got {}", port_val),
                line,
                col,
            ));
        }
        let port = port_val.as_any_int() as u16;

        let router_fields = match router_val.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(self.error(
                    format!(
                        "Server.listen! first argument must be a Router, got {}",
                        router_val
                    ),
                    line,
                    col,
                ));
            }
        };

        let middleware: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "middleware")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        let sendable_mw: Vec<SendableHandler> = middleware
            .iter()
            .filter_map(SendableHandler::from_nvalue)
            .collect();

        let route_tree = crate::vm::hyper_server::AsyncRouteTree::from_nvalue(&router_val)
            .map_err(|e| self.error(format!("Failed to build route tree: {}", e), line, col))?;

        let chunks_vec = chunks.to_vec();

        let addr = std::net::SocketAddr::from(([0, 0, 0, 0], port));
        let config = match config_val {
            Some(ref cv) => parse_server_config(cv),
            None => crate::vm::hyper_server::ServerConfig::default(),
        };
        let ctx = std::sync::Arc::new(crate::vm::hyper_server::AsyncServerContext::with_executor(
            route_tree,
            chunks_vec,
            sendable_mw,
            config,
        ));

        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|e| self.error(format!("Failed to create runtime: {}", e), line, col))?;

        rt.block_on(crate::vm::hyper_server::run_server_with_context(addr, ctx))
            .map_err(|e| self.error(format!("Server error: {}", e), line, col))?;

        self.stack.push(NValue::unit());
        Ok(())
    }

    /// Dispatch Server.listen!(router, port) — blocks in accept loop.
    /// Spawns a thread pool for concurrent request handling.
    pub(crate) fn dispatch_server_listen(
        &mut self,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        if arg_count != 2 {
            return Err(self.error(
                format!(
                    "Server.listen! expects 2 arguments (router, port), got {}",
                    arg_count
                ),
                line,
                col,
            ));
        }
        let port_val = self.pop(line, col)?;
        let router_val = self.pop(line, col)?;

        if !port_val.is_any_int() {
            return Err(self.error(
                format!("Server.listen! port must be Int, got {}", port_val),
                line,
                col,
            ));
        }
        let port = port_val.as_any_int() as u16;

        let router_fields = match router_val.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(self.error(
                    format!(
                        "Server.listen! first argument must be a Router, got {}",
                        router_val
                    ),
                    line,
                    col,
                ));
            }
        };

        let routes: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "routes")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        let middleware: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "middleware")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        // Serialize route tree + middleware + chunks to thread-safe form
        let sendable_routes = SendableRouteTree::from_nv_routes(&routes);
        let sendable_mw: Vec<SendableHandler> = middleware
            .iter()
            .filter_map(SendableHandler::from_nvalue)
            .collect();
        let sendable_chunks = SendableChunks::from_chunks(chunks);

        let addr = format!("0.0.0.0:{}", port);
        let server = tiny_http::Server::http(&addr).map_err(|e| {
            self.error(
                format!("Failed to start server on {}: {}", addr, e),
                line,
                col,
            )
        })?;

        let num_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);
        eprintln!(
            "[server] Listening on http://0.0.0.0:{} ({} workers)",
            port, num_workers
        );

        let server = Arc::new(server);
        let sendable_routes = Arc::new(sendable_routes);
        let sendable_mw = Arc::new(sendable_mw);
        let sendable_chunks = Arc::new(sendable_chunks);

        let mut handles = Vec::with_capacity(num_workers);
        for _ in 0..num_workers {
            let server = Arc::clone(&server);
            let s_routes = Arc::clone(&sendable_routes);
            let s_mw = Arc::clone(&sendable_mw);
            let s_chunks = Arc::clone(&sendable_chunks);

            handles.push(std::thread::spawn(move || {
                // Each thread gets its own VM + reconstructed NValue data
                let mut vm = super::Vm::new();
                let chunks = s_chunks.to_chunks();
                let route_tree = s_routes.to_nv_radix_tree();
                let middleware: Vec<NValue> = s_mw.iter().map(|h| h.to_nvalue()).collect();

                for request in server.incoming_requests() {
                    handle_request(&mut vm, request, &route_tree, &middleware, &chunks);
                }
            }));
        }

        // Block until all workers finish (server dropped = workers exit)
        for handle in handles {
            let _ = handle.join();
        }

        self.stack.push(NValue::unit());
        Ok(())
    }

    /// Apply middleware chain for VM server dispatch.
    /// Builds the chain inside-out: each middleware receives (request, next) where
    /// next is a closure wrapping the remaining middleware + handler.
    ///
    /// This is the main entry point for invoking handlers with middleware from Rust.
    pub fn apply_mw_chain(
        &mut self,
        middleware: &[NValue],
        handler: &NValue,
        request: &NValue,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if middleware.is_empty() {
            return self.call_nvalue(handler, &[request.clone()], chunks, line, col);
        }

        // Build the "next" chain inside-out: innermost = handler
        // Each layer wraps: fn(req) => middleware[i](req, next_inner)
        //
        // We use NativeMwNext heap objects to carry the chain. When the
        // middleware calls next(req), the VM intercepts it.
        let current_mw = &middleware[0];
        let remaining = &middleware[1..];

        // Build next as a closure that, when called with (req), invokes the
        // remaining middleware chain. We represent this as a NativeMwNext
        // heap object that the VM call_nvalue can intercept.
        let next = build_mw_next_nvalue(remaining, handler);

        self.call_nvalue(current_mw, &[request.clone(), next], chunks, line, col)
    }

    /// Handle a call to a NativeMwNext value (middleware chain continuation).
    pub(crate) fn call_mw_next(
        &mut self,
        handler: &NValue,
        remaining_mw: &[NValue],
        args: &[NValue],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if args.is_empty() {
            return Err(self.error(
                "Middleware next() requires a request argument".into(),
                line,
                col,
            ));
        }
        let request = &args[0];
        self.apply_mw_chain(remaining_mw, handler, request, chunks, line, col)
    }
}

// ---------------------------------------------------------------------------
// Server config parsing from Baseline values
// ---------------------------------------------------------------------------

/// Parse a Baseline record value into a ServerConfig.
/// Unknown fields are silently ignored.
#[cfg(feature = "async-server")]
fn parse_server_config(val: &NValue) -> crate::vm::hyper_server::ServerConfig {
    use crate::vm::hyper_server::{CorsConfig, ServerConfig};
    use std::time::Duration;

    let mut config = ServerConfig::default();
    let fields = match val.as_record() {
        Some(f) => f,
        None => return config,
    };

    for (key, v) in fields.iter() {
        match &**key {
            "max_body_size" if v.is_any_int() => {
                config.max_body_size = v.as_any_int() as usize;
            }
            "request_timeout" if v.is_any_int() => {
                config.request_timeout = Duration::from_secs(v.as_any_int() as u64);
            }
            "enable_http2" if v.is_bool() => {
                config.enable_http2 = v.as_bool();
            }
            "keep_alive" if v.is_bool() => {
                config.keep_alive = v.as_bool();
            }
            "keep_alive_timeout" if v.is_any_int() => {
                config.keep_alive_timeout = Duration::from_secs(v.as_any_int() as u64);
            }
            "max_connections" if v.is_any_int() => {
                config.max_connections = v.as_any_int() as usize;
            }
            "max_connections_per_ip" if v.is_any_int() => {
                config.max_connections_per_ip = v.as_any_int() as usize;
            }
            "max_header_size" if v.is_any_int() => {
                config.max_header_size = v.as_any_int() as usize;
            }
            "shutdown_timeout" if v.is_any_int() => {
                config.shutdown_timeout = Duration::from_secs(v.as_any_int() as u64);
            }
            "access_log" if v.is_bool() => {
                config.access_log = v.as_bool();
            }
            "health_check_path" => {
                if let Some(s) = v.as_string() {
                    config.health_check_path = Some(s.to_string());
                } else if v.is_unit() {
                    config.health_check_path = None;
                }
            }
            "request_id_header" => {
                if let Some(s) = v.as_string() {
                    config.request_id_header = Some(s.to_string());
                } else if v.is_unit() {
                    config.request_id_header = None;
                }
            }
            "cors" => {
                if let Some(cors_fields) = v.as_record() {
                    config.cors = Some(parse_cors_config(cors_fields));
                } else if v.is_bool() && v.as_bool() {
                    config.cors = Some(CorsConfig::default());
                }
            }
            "rate_limit" => {
                if let Some(rl_fields) = v.as_record() {
                    config.rate_limit = Some(parse_rate_limit_config(rl_fields));
                }
            }
            "tls_cert" | "tls_cert_path" => {
                if let Some(s) = v.as_string() {
                    config.tls_cert_path = Some(s.to_string());
                }
            }
            "tls_key" | "tls_key_path" => {
                if let Some(s) = v.as_string() {
                    config.tls_key_path = Some(s.to_string());
                }
            }
            "compression" if v.is_bool() => {
                config.compression = v.as_bool();
            }
            "static_dirs" => {
                if let Some(list) = v.as_list() {
                    config.static_dirs = list
                        .iter()
                        .filter_map(|item| {
                            let tuple = item.as_tuple()?;
                            if tuple.len() == 2 {
                                let prefix = tuple[0].as_string()?.to_string();
                                let dir = tuple[1].as_string()?.to_string();
                                Some((prefix, dir))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
            }
            _ => {}
        }
    }
    config
}

#[cfg(feature = "async-server")]
fn parse_cors_config(
    fields: &[(crate::vm::value::RcStr, NValue)],
) -> crate::vm::hyper_server::CorsConfig {
    let mut cors = crate::vm::hyper_server::CorsConfig::default();
    for (key, v) in fields.iter() {
        match &**key {
            "allowed_origins" => {
                if let Some(list) = v.as_list() {
                    cors.allowed_origins = list
                        .iter()
                        .filter_map(|item| item.as_string().map(|s| s.to_string()))
                        .collect();
                }
            }
            "allowed_methods" => {
                if let Some(list) = v.as_list() {
                    cors.allowed_methods = list
                        .iter()
                        .filter_map(|item| item.as_string().map(|s| s.to_string()))
                        .collect();
                }
            }
            "allowed_headers" => {
                if let Some(list) = v.as_list() {
                    cors.allowed_headers = list
                        .iter()
                        .filter_map(|item| item.as_string().map(|s| s.to_string()))
                        .collect();
                }
            }
            "max_age" if v.is_any_int() => {
                cors.max_age = v.as_any_int() as u64;
            }
            "allow_credentials" if v.is_bool() => {
                cors.allow_credentials = v.as_bool();
            }
            _ => {}
        }
    }
    cors
}

#[cfg(feature = "async-server")]
fn parse_rate_limit_config(
    fields: &[(crate::vm::value::RcStr, NValue)],
) -> crate::vm::hyper_server::RateLimitConfig {
    let mut rl = crate::vm::hyper_server::RateLimitConfig {
        requests_per_second: 100.0,
        burst_size: 200,
    };
    for (key, v) in fields.iter() {
        match &**key {
            "requests_per_second" if v.is_any_int() => {
                rl.requests_per_second = v.as_any_int() as f64;
            }
            "burst_size" if v.is_any_int() => {
                rl.burst_size = v.as_any_int() as usize;
            }
            _ => {}
        }
    }
    rl
}
