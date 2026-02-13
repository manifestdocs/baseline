//! HTTP Server Tests
//!
//! Tests for the async HTTP server implementation including Phase 2-3 features:
//! connection management, timeouts, body size enforcement, and graceful shutdown.

use std::time::{Duration, Instant};

// Re-export from hyper_server for testing
use crate::vm::hyper_server::{
    AsyncRequest, AsyncResponse, AsyncRouteTree, AsyncRouteTreeBuilder, ServerConfig,
};
use crate::vm::radix::{ParamCollector, SmallParams};

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::Bytes;
    use hyper::Method;

    // -----------------------------------------------------------------------
    // AsyncResponse Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_response_text_creates_correct_status() {
        let resp = AsyncResponse::text(200, "OK");
        assert_eq!(resp.status.as_u16(), 200);

        let resp = AsyncResponse::text(404, "Not Found");
        assert_eq!(resp.status.as_u16(), 404);

        let resp = AsyncResponse::text(500, "Error");
        assert_eq!(resp.status.as_u16(), 500);
    }

    #[test]
    fn test_response_text_sets_content_type() {
        let resp = AsyncResponse::text(200, "Hello");
        assert!(
            resp.headers
                .iter()
                .any(|(k, v)| k == "Content-Type" && v == "text/plain")
        );
    }

    #[test]
    fn test_response_json_sets_content_type() {
        let resp = AsyncResponse::json(r#"{"ok":true}"#);
        assert!(
            resp.headers
                .iter()
                .any(|(k, v)| k == "Content-Type" && v == "application/json")
        );
    }

    #[test]
    fn test_response_body_preserved() {
        let body = "Hello, World!";
        let resp = AsyncResponse::text(200, body);
        assert_eq!(resp.body, Bytes::from(body));
    }

    #[test]
    fn test_response_json_body_preserved() {
        let json = r#"{"status":"ok","count":42}"#;
        let resp = AsyncResponse::json(json);
        assert_eq!(resp.body, Bytes::from(json));
    }

    // -----------------------------------------------------------------------
    // AsyncRouteTree Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_empty_route_tree_returns_none() {
        let tree = AsyncRouteTree::new();
        assert!(tree.find("GET", "/").is_none());
        assert!(tree.find("GET", "/foo").is_none());
    }

    #[test]
    fn test_route_tree_exact_match() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/health", |_| AsyncResponse::text(200, "OK"))
            .get("/users", |_| AsyncResponse::json("[]"))
            .build();

        assert!(tree.find("GET", "/health").is_some());
        assert!(tree.find("GET", "/users").is_some());
        assert!(tree.find("GET", "/foo").is_none());
        assert!(tree.find("POST", "/health").is_none());
    }

    #[test]
    fn test_route_tree_param_extraction() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/users/:id", |_| AsyncResponse::text(200, "user"))
            .get("/users/:id/posts/:post_id", |_| {
                AsyncResponse::text(200, "post")
            })
            .build();

        let (_, params) = tree.find("GET", "/users/123").unwrap();
        assert_eq!(params.get("id"), Some("123"));

        let (_, params) = tree.find("GET", "/users/456/posts/789").unwrap();
        assert_eq!(params.get("id"), Some("456"));
        assert_eq!(params.get("post_id"), Some("789"));
    }

    #[test]
    fn test_route_tree_exact_preferred_over_param() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/users/me", |_| AsyncResponse::text(200, "current user"))
            .get("/users/:id", |_| AsyncResponse::text(200, "user by id"))
            .build();

        let (_, params) = tree.find("GET", "/users/me").unwrap();
        assert!(params.is_empty());

        let (_, params) = tree.find("GET", "/users/123").unwrap();
        assert_eq!(params.get("id"), Some("123"));
    }

    #[test]
    fn test_route_tree_different_methods() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/resource", |_| AsyncResponse::text(200, "get"))
            .post("/resource", |_| AsyncResponse::text(201, "created"))
            .build();

        assert!(tree.find("GET", "/resource").is_some());
        assert!(tree.find("POST", "/resource").is_some());
        assert!(tree.find("DELETE", "/resource").is_none());
    }

    // -----------------------------------------------------------------------
    // ServerConfig Tests (Phase 2-3)
    // -----------------------------------------------------------------------

    #[test]
    fn test_server_config_defaults() {
        let config = ServerConfig::default();
        assert!(config.workers > 0);
        assert!(config.keep_alive);
        assert_eq!(config.keep_alive_timeout, Duration::from_secs(75));
        assert_eq!(config.max_body_size, 1024 * 1024);
        assert_eq!(config.max_connections, 10_000);
        assert_eq!(config.max_connections_per_ip, 256);
        assert_eq!(config.request_timeout, Duration::from_secs(30));
        assert_eq!(config.shutdown_timeout, Duration::from_secs(30));
        assert!(!config.enable_http2);
    }

    #[test]
    fn test_server_config_custom() {
        let config = ServerConfig {
            workers: 8,
            keep_alive: false,
            keep_alive_timeout: Duration::from_secs(120),
            max_body_size: 2 * 1024 * 1024,
            max_connections: 5_000,
            max_connections_per_ip: 100,
            request_timeout: Duration::from_secs(60),
            shutdown_timeout: Duration::from_secs(15),
            enable_http2: true,
            ..ServerConfig::default()
        };
        assert_eq!(config.workers, 8);
        assert!(!config.keep_alive);
        assert_eq!(config.keep_alive_timeout, Duration::from_secs(120));
        assert_eq!(config.max_body_size, 2 * 1024 * 1024);
        assert_eq!(config.max_connections, 5_000);
        assert_eq!(config.max_connections_per_ip, 100);
        assert_eq!(config.request_timeout, Duration::from_secs(60));
        assert_eq!(config.shutdown_timeout, Duration::from_secs(15));
        assert!(config.enable_http2);
    }

    #[test]
    fn test_server_config_clone() {
        let config = ServerConfig::default();
        let cloned = config.clone();
        assert_eq!(config.max_connections, cloned.max_connections);
        assert_eq!(config.enable_http2, cloned.enable_http2);
    }

    // -----------------------------------------------------------------------
    // NValue Conversion Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_async_request_to_nvalue() {
        let mut params = SmallParams::new();
        params.push("id".to_string(), "123".to_string());

        let req = AsyncRequest::test_request(
            Method::GET,
            "/users/123",
            Some("page=1&limit=10"),
            "",
            params,
        );

        let nvalue = req.to_nvalue();
        assert!(nvalue.as_record().is_some());

        let fields = nvalue.as_record().unwrap();

        let method = fields
            .iter()
            .find(|(k, _)| &**k == "method")
            .map(|(_, v)| v);
        assert_eq!(method.unwrap().as_str(), Some("GET"));

        let url = fields.iter().find(|(k, _)| &**k == "url").map(|(_, v)| v);
        assert_eq!(url.unwrap().as_str(), Some("/users/123"));
    }

    #[test]
    fn test_async_response_from_nvalue() {
        use crate::vm::nvalue::NValue;
        use crate::vm::value::RcStr;

        let headers = vec![NValue::tuple(vec![
            NValue::string(RcStr::from("Content-Type")),
            NValue::string(RcStr::from("application/json")),
        ])];

        let resp_nvalue = NValue::record(vec![
            ("status".into(), NValue::int(201)),
            ("headers".into(), NValue::list(headers)),
            ("body".into(), NValue::string(RcStr::from(r#"{"id":1}"#))),
        ]);

        let resp = AsyncResponse::from_nvalue(&resp_nvalue);

        assert_eq!(resp.status.as_u16(), 201);
        assert_eq!(resp.body, Bytes::from(r#"{"id":1}"#));
        assert!(
            resp.headers
                .iter()
                .any(|(k, v)| k == "Content-Type" && v == "application/json")
        );
    }

    // -----------------------------------------------------------------------
    // Zero-Copy Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_async_request_to_sendable() {
        use crate::vm::sendable::SendableValue;

        let mut params = SmallParams::new();
        params.push("id".to_string(), "123".to_string());
        let req = AsyncRequest::test_request(
            Method::GET,
            "/users/123",
            Some("q=test"),
            "body content",
            params,
        );

        let sv = req.to_sendable();

        if let SendableValue::Record(fields) = sv {
            let get_field = |name: &str| fields.iter().find(|(k, _)| k == name).map(|(_, v)| v);

            assert_eq!(
                get_field("method").and_then(|v| if let SendableValue::String(s) = v {
                    Some(s.as_str())
                } else {
                    None
                }),
                Some("GET")
            );
            assert_eq!(
                get_field("url").and_then(|v| if let SendableValue::String(s) = v {
                    Some(s.as_str())
                } else {
                    None
                }),
                Some("/users/123")
            );

            // Body is now SendableValue::Bytes (zero-copy)
            let body_str = get_field("body").and_then(|v| match v {
                SendableValue::Bytes(b) => std::str::from_utf8(b).ok(),
                SendableValue::String(s) => Some(s.as_str()),
                _ => None,
            });
            assert_eq!(body_str, Some("body content"));

            if let Some(SendableValue::Record(params)) = get_field("params") {
                let id = params.iter().find(|(k, _)| k == "id").map(|(_, v)| v);
                assert_eq!(
                    id.and_then(|v| if let SendableValue::String(s) = v {
                        Some(s.as_str())
                    } else {
                        None
                    }),
                    Some("123")
                );
            } else {
                panic!("params field missing or not a record");
            }
        } else {
            panic!("Expected Record");
        }
    }

    #[test]
    fn test_async_response_from_sendable_val() {
        use crate::vm::sendable::SendableValue;

        let sv = SendableValue::Record(vec![
            ("status".to_string(), SendableValue::Int(201)),
            (
                "body".to_string(),
                SendableValue::String(r#"{"created":true}"#.to_string()),
            ),
            (
                "headers".to_string(),
                SendableValue::List(vec![SendableValue::Tuple(vec![
                    SendableValue::String("Content-Type".to_string()),
                    SendableValue::String("application/json".to_string()),
                ])]),
            ),
        ]);

        let resp = AsyncResponse::from_sendable_val(&sv);

        assert_eq!(resp.status.as_u16(), 201);
        assert_eq!(resp.body, Bytes::from(r#"{"created":true}"#));
        assert!(
            resp.headers
                .iter()
                .any(|(k, v)| k == "Content-Type" && v == "application/json")
        );
    }

    // -----------------------------------------------------------------------
    // SmallParams Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_small_params_get() {
        let mut params = SmallParams::new();
        assert!(params.is_empty());
        assert_eq!(params.get("id"), None);

        params.push("id".to_string(), "42".to_string());
        assert!(!params.is_empty());
        assert_eq!(params.get("id"), Some("42"));
        assert_eq!(params.get("name"), None);

        params.push("name".to_string(), "alice".to_string());
        assert_eq!(params.get("name"), Some("alice"));
        assert_eq!(params.get("id"), Some("42"));
    }

    #[test]
    fn test_small_params_pop() {
        let mut params = SmallParams::new();
        params.push("a".to_string(), "1".to_string());
        params.push("b".to_string(), "2".to_string());
        assert_eq!(params.as_slice().len(), 2);

        params.pop();
        assert_eq!(params.as_slice().len(), 1);
        assert_eq!(params.get("a"), Some("1"));
        assert_eq!(params.get("b"), None);
    }

    // -----------------------------------------------------------------------
    // build_hyper_response_from_sendable Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_build_hyper_response_from_sendable_record() {
        use crate::vm::hyper_server::build_hyper_response_from_sendable;
        use crate::vm::sendable::SendableValue;

        let sv = SendableValue::Record(vec![
            ("status".to_string(), SendableValue::Int(201)),
            (
                "body".to_string(),
                SendableValue::Bytes(Bytes::from("created")),
            ),
            (
                "headers".to_string(),
                SendableValue::List(vec![SendableValue::Tuple(vec![
                    SendableValue::String("X-Custom".to_string()),
                    SendableValue::String("val".to_string()),
                ])]),
            ),
        ]);

        let resp = build_hyper_response_from_sendable(&sv);
        assert_eq!(resp.status().as_u16(), 201);
        assert_eq!(resp.headers().get("X-Custom").unwrap(), "val");
    }

    #[test]
    fn test_build_hyper_response_from_sendable_string_body() {
        use crate::vm::hyper_server::build_hyper_response_from_sendable;
        use crate::vm::sendable::SendableValue;

        let sv = SendableValue::Record(vec![
            ("status".to_string(), SendableValue::Int(200)),
            (
                "body".to_string(),
                SendableValue::String("hello".to_string()),
            ),
        ]);

        let resp = build_hyper_response_from_sendable(&sv);
        assert_eq!(resp.status().as_u16(), 200);
    }

    #[test]
    fn test_build_hyper_response_from_sendable_fallback() {
        use crate::vm::hyper_server::build_hyper_response_from_sendable;
        use crate::vm::sendable::SendableValue;

        let sv = SendableValue::String("plain text".to_string());
        let resp = build_hyper_response_from_sendable(&sv);
        assert_eq!(resp.status().as_u16(), 200);
    }

    // -----------------------------------------------------------------------
    // Phase 2-3: Body Size Enforcement Tests
    // -----------------------------------------------------------------------

    /// Test body size enforcement by sending raw HTTP requests over TCP.
    /// Uses raw TCP to avoid needing hyper-util client features.
    #[tokio::test]
    async fn test_body_size_enforcement_small_body_ok() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};

        let tree = AsyncRouteTreeBuilder::new()
            .post("/echo", |req| AsyncResponse::text(200, req.body.clone()))
            .build();

        let config = ServerConfig {
            max_body_size: 64,
            access_log: false,
            health_check_path: None,
            request_id_header: None,
            ..ServerConfig::default()
        };
        let ctx = std::sync::Arc::new(crate::vm::hyper_server::AsyncServerContext::native_only(
            tree, config,
        ));

        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        let server_ctx = std::sync::Arc::clone(&ctx);
        let (_shutdown_tx, mut shutdown_rx) = tokio::sync::watch::channel(false);

        let server = tokio::spawn(async move {
            let (stream, remote_addr) = listener.accept().await.unwrap();
            let io = hyper_util::rt::TokioIo::new(stream);
            crate::vm::hyper_server::serve_http1_connection(
                io,
                server_ctx,
                remote_addr,
                &mut shutdown_rx,
            )
            .await;
        });

        // Send a small body via raw TCP
        let mut stream = tokio::net::TcpStream::connect(addr).await.unwrap();
        let body = "hi";
        let req = format!(
            "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
            body.len(),
            body
        );
        stream.write_all(req.as_bytes()).await.unwrap();

        let mut buf = Vec::new();
        stream.read_to_end(&mut buf).await.unwrap();
        let response = String::from_utf8_lossy(&buf);

        assert!(
            response.contains("200 OK"),
            "Expected 200 OK, got: {}",
            response
        );
        server.await.unwrap();
    }

    #[tokio::test]
    async fn test_oversized_body_returns_413() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};

        let tree = AsyncRouteTreeBuilder::new()
            .post("/upload", |_| AsyncResponse::text(200, "ok"))
            .build();

        let config = ServerConfig {
            max_body_size: 8, // Very small limit
            access_log: false,
            health_check_path: None,
            request_id_header: None,
            ..ServerConfig::default()
        };
        let ctx = std::sync::Arc::new(crate::vm::hyper_server::AsyncServerContext::native_only(
            tree, config,
        ));

        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        let server_ctx = std::sync::Arc::clone(&ctx);
        let (_shutdown_tx, mut shutdown_rx) = tokio::sync::watch::channel(false);

        let server = tokio::spawn(async move {
            let (stream, remote_addr) = listener.accept().await.unwrap();
            let io = hyper_util::rt::TokioIo::new(stream);
            crate::vm::hyper_server::serve_http1_connection(
                io,
                server_ctx,
                remote_addr,
                &mut shutdown_rx,
            )
            .await;
        });

        // Send oversized body via raw TCP
        let big_body = "this body is definitely larger than 8 bytes";
        let req = format!(
            "POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
            big_body.len(),
            big_body
        );

        let mut stream = tokio::net::TcpStream::connect(addr).await.unwrap();
        stream.write_all(req.as_bytes()).await.unwrap();

        let mut buf = Vec::new();
        stream.read_to_end(&mut buf).await.unwrap();
        let response = String::from_utf8_lossy(&buf);

        assert!(
            response.contains("413"),
            "Expected 413 Payload Too Large, got: {}",
            response
        );
        server.await.unwrap();
    }

    // -----------------------------------------------------------------------
    // Phase 2-3: Request Timeout Tests
    // -----------------------------------------------------------------------

    #[tokio::test]
    async fn test_request_timeout_mechanism() {
        // Test the timeout wrapper directly — a slow async operation should timeout
        let request_timeout = Duration::from_millis(10);

        let result = tokio::time::timeout(request_timeout, async {
            // Simulate a slow handler
            tokio::time::sleep(Duration::from_millis(100)).await;
            AsyncResponse::text(200, "ok")
        })
        .await;

        assert!(result.is_err()); // Should have timed out
    }

    #[tokio::test]
    async fn test_request_timeout_fast_handler_succeeds() {
        let request_timeout = Duration::from_secs(5);

        let result =
            tokio::time::timeout(request_timeout, async { AsyncResponse::text(200, "fast") }).await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap().status.as_u16(), 200);
    }

    // -----------------------------------------------------------------------
    // Phase 2-3: Graceful Shutdown Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_graceful_shutdown_config() {
        let config = ServerConfig {
            shutdown_timeout: Duration::from_secs(10),
            ..ServerConfig::default()
        };
        assert_eq!(config.shutdown_timeout, Duration::from_secs(10));
    }

    // -----------------------------------------------------------------------
    // Phase 2-3: HTTP/2 Config Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_http2_config_default_disabled() {
        let config = ServerConfig::default();
        assert!(!config.enable_http2);
    }

    #[test]
    fn test_http2_config_enabled() {
        let config = ServerConfig {
            enable_http2: true,
            ..ServerConfig::default()
        };
        assert!(config.enable_http2);
    }

    // -----------------------------------------------------------------------
    // Performance Baseline Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_response_creation_performance() {
        let start = Instant::now();
        for _ in 0..10_000 {
            let _ = AsyncResponse::json(r#"{"status":"ok"}"#);
        }
        let elapsed = start.elapsed();

        assert!(
            elapsed < Duration::from_millis(100),
            "Response creation took {:?}, expected < 100ms",
            elapsed
        );
    }

    #[test]
    fn test_route_matching_performance() {
        let tree = AsyncRouteTree::new();

        let start = Instant::now();
        for _ in 0..10_000 {
            let _ = tree.find("GET", "/users/123/posts/456");
        }
        let elapsed = start.elapsed();

        assert!(
            elapsed < Duration::from_millis(50),
            "Route matching took {:?}, expected < 50ms",
            elapsed
        );
    }

    #[test]
    fn test_route_tree_with_native_handlers_performance() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/", |_| AsyncResponse::text(200, "root"))
            .get("/health", |_| AsyncResponse::json(r#"{"status":"ok"}"#))
            .get("/users", |_| AsyncResponse::json("[]"))
            .get("/users/:id", |_| AsyncResponse::text(200, "user"))
            .get("/users/:id/posts", |_| AsyncResponse::json("[]"))
            .get("/users/:id/posts/:post_id", |_| {
                AsyncResponse::text(200, "post")
            })
            .post("/users", |_| AsyncResponse::text(201, "created"))
            .build();

        let paths = [
            ("GET", "/"),
            ("GET", "/health"),
            ("GET", "/users"),
            ("GET", "/users/123"),
            ("GET", "/users/123/posts"),
            ("GET", "/users/123/posts/456"),
            ("POST", "/users"),
        ];

        let start = Instant::now();
        for _ in 0..10_000 {
            for (method, path) in &paths {
                let _ = tree.find(method, path);
            }
        }
        let elapsed = start.elapsed();

        assert!(
            elapsed < Duration::from_millis(200),
            "Route matching took {:?}, expected < 200ms for 70k lookups",
            elapsed
        );
    }
}
