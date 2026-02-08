//! Native async server benchmark - tests Hyper server without VM overhead
//!
//! Run with: cargo run --example async_bench_server -p blc --release
//!
//! Then benchmark with: wrk -t4 -c100 -d10s http://localhost:8080/hello

use blc::stdlib::hyper_server::{AsyncResponse, AsyncRouteTreeBuilder, ServerConfig, run_server};
use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Build native route tree (no VM handlers)
    let routes = AsyncRouteTreeBuilder::new()
        // Plain text hello world
        .get("/hello", |_| AsyncResponse::text(200, "Hello, World!"))
        // JSON response
        .get("/json", |_| {
            AsyncResponse::json(r#"{"message":"Hello, World!"}"#)
        })
        // Health check
        .get("/health", |_| AsyncResponse::json(r#"{"status":"ok"}"#))
        // User lookup with params
        .get("/users/:id", |req| {
            let id = req.params.get("id").map(|s| s.as_str()).unwrap_or("?");
            let body = format!(r#"{{"id":"{}","name":"User {}"}}"#, id, id);
            AsyncResponse::json(body)
        })
        // Echo endpoint
        .post("/echo", |req| match req.body_str() {
            Ok(body) => AsyncResponse::text(200, body.to_string()),
            Err(_) => AsyncResponse::text(400, "Invalid UTF-8 body"),
        })
        .build();

    let addr: SocketAddr = "0.0.0.0:8080".parse()?;

    println!("Native async server (Hyper) listening on http://{}", addr);
    println!();
    println!("Endpoints:");
    println!("  GET  /hello     - plain text hello world");
    println!("  GET  /json      - JSON hello world");
    println!("  GET  /health    - health check");
    println!("  GET  /users/:id - user lookup with path param");
    println!("  POST /echo      - echo request body");
    println!();
    println!("Benchmark with:");
    println!("  wrk -t4 -c100 -d10s http://localhost:8080/hello");
    println!();

    run_server(addr, routes, ServerConfig::default()).await
}
