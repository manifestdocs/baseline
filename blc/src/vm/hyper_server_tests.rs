//! HTTP Server Benchmark Tests
//!
//! TDD tests for the high-performance async HTTP server implementation.
//! These tests validate correctness before performance optimization.

use std::collections::HashMap;
use std::time::{Duration, Instant};

// Re-export from hyper_server for testing
use crate::vm::hyper_server::{
    AsyncRequest, AsyncResponse, AsyncRouteTree, AsyncRouteTreeBuilder, ServerConfig,
};

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

        // Should find registered routes
        assert!(tree.find("GET", "/health").is_some());
        assert!(tree.find("GET", "/users").is_some());

        // Should not find unregistered routes
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

        // Single param
        let (_, params) = tree.find("GET", "/users/123").unwrap();
        assert_eq!(params.get("id"), Some(&"123".to_string()));

        // Multiple params
        let (_, params) = tree.find("GET", "/users/456/posts/789").unwrap();
        assert_eq!(params.get("id"), Some(&"456".to_string()));
        assert_eq!(params.get("post_id"), Some(&"789".to_string()));
    }

    #[test]
    fn test_route_tree_exact_preferred_over_param() {
        let tree = AsyncRouteTreeBuilder::new()
            .get("/users/me", |_| AsyncResponse::text(200, "current user"))
            .get("/users/:id", |_| AsyncResponse::text(200, "user by id"))
            .build();

        // Exact match should be preferred
        let (_, params) = tree.find("GET", "/users/me").unwrap();
        assert!(params.is_empty()); // No params for exact match

        // Param match for other values
        let (_, params) = tree.find("GET", "/users/123").unwrap();
        assert_eq!(params.get("id"), Some(&"123".to_string()));
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
    // ServerConfig Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_server_config_defaults() {
        let config = ServerConfig::default();
        assert!(config.workers > 0);
        assert!(config.keep_alive);
        assert_eq!(config.max_body_size, 1024 * 1024);
    }

    #[test]
    fn test_server_config_custom() {
        let config = ServerConfig {
            workers: 8,
            keep_alive: false,
            max_body_size: 2 * 1024 * 1024,
        };
        assert_eq!(config.workers, 8);
        assert!(!config.keep_alive);
        assert_eq!(config.max_body_size, 2 * 1024 * 1024);
    }

    // -----------------------------------------------------------------------
    // Query String Parsing (via module tests)
    // -----------------------------------------------------------------------
    // Note: Query string tests are in hyper_server.rs

    // -----------------------------------------------------------------------
    // NValue Conversion Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_async_request_to_nvalue() {
        // Create a mock AsyncRequest using test constructor
        let mut params = HashMap::new();
        params.insert("id".to_string(), "123".to_string());

        let req = AsyncRequest::test_request(
            Method::GET,
            "/users/123",
            Some("page=1&limit=10"),
            "",
            params,
        );

        let nvalue = req.to_nvalue();

        // Verify it's a record
        assert!(nvalue.as_record().is_some());

        let fields = nvalue.as_record().unwrap();

        // Check method
        let method = fields
            .iter()
            .find(|(k, _)| &**k == "method")
            .map(|(_, v)| v);
        assert!(method.is_some());
        assert_eq!(method.unwrap().as_str(), Some("GET"));

        // Check url
        let url = fields.iter().find(|(k, _)| &**k == "url").map(|(_, v)| v);
        assert!(url.is_some());
        assert_eq!(url.unwrap().as_str(), Some("/users/123"));
    }

    #[test]
    fn test_async_response_from_nvalue() {
        use crate::vm::nvalue::NValue;
        use crate::vm::value::RcStr;

        // Build a response NValue
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
        use crate::vm::async_executor::SendableValue;

        let mut params = HashMap::new();
        params.insert("id".to_string(), "123".to_string());
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
                get_field("method").and_then(|v| if let SendableValue::String(s) = v { Some(s.as_str()) } else { None }),
                Some("GET")
            );
            assert_eq!(
                get_field("url").and_then(|v| if let SendableValue::String(s) = v { Some(s.as_str()) } else { None }),
                Some("/users/123")
            );
             assert_eq!(
                get_field("body").and_then(|v| if let SendableValue::String(s) = v { Some(s.as_str()) } else { None }),
                Some("body content")
            );
            
            // Check params
            if let Some(SendableValue::Record(params)) = get_field("params") {
                let id = params.iter().find(|(k, _)| k == "id").map(|(_, v)| v);
                assert_eq!(
                    id.and_then(|v| if let SendableValue::String(s) = v { Some(s.as_str()) } else { None }),
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
        use crate::vm::async_executor::SendableValue;

        let sv = SendableValue::Record(vec![
            ("status".to_string(), SendableValue::Int(201)),
            ("body".to_string(), SendableValue::String(r#"{"created":true}"#.to_string())),
            ("headers".to_string(), SendableValue::List(vec![
                SendableValue::Tuple(vec![
                    SendableValue::String("Content-Type".to_string()),
                    SendableValue::String("application/json".to_string()),
                ])
            ])),
        ]);

        let resp = AsyncResponse::from_sendable_val(&sv);

        assert_eq!(resp.status.as_u16(), 201);
        assert_eq!(resp.body, Bytes::from(r#"{"created":true}"#));
        assert!(resp.headers.iter().any(|(k, v)| k == "Content-Type" && v == "application/json"));
    }

    // -----------------------------------------------------------------------
    // Performance Baseline Tests (to be expanded)
    // -----------------------------------------------------------------------

    #[test]
    fn test_response_creation_performance() {
        // Measure time to create 10,000 responses
        let start = Instant::now();
        for _ in 0..10_000 {
            let _ = AsyncResponse::json(r#"{"status":"ok"}"#);
        }
        let elapsed = start.elapsed();

        // Should complete in under 100ms (100µs per response is very conservative)
        assert!(
            elapsed < Duration::from_millis(100),
            "Response creation took {:?}, expected < 100ms",
            elapsed
        );
    }

    #[test]
    fn test_route_matching_performance() {
        let tree = AsyncRouteTree::new();

        // Measure time to do 10,000 route lookups (even on empty tree)
        let start = Instant::now();
        for _ in 0..10_000 {
            let _ = tree.find("GET", "/users/123/posts/456");
        }
        let elapsed = start.elapsed();

        // Should complete in under 50ms
        assert!(
            elapsed < Duration::from_millis(50),
            "Route matching took {:?}, expected < 50ms",
            elapsed
        );
    }

    #[test]
    fn test_route_tree_with_native_handlers_performance() {
        // Build a tree with multiple routes
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

        // Measure time for 10,000 mixed route lookups
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

        // 70,000 lookups should complete in under 200ms
        assert!(
            elapsed < Duration::from_millis(200),
            "Route matching took {:?}, expected < 200ms for 70k lookups",
            elapsed
        );
    }
}
