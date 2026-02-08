//! Axum benchmark server - baseline for performance comparison
//!
//! Implements the same endpoints as Baseline's bench_server.bl

use axum::{
    extract::Path,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;

#[derive(Serialize)]
struct Message {
    message: &'static str,
}

#[derive(Serialize)]
struct Health {
    status: &'static str,
}

#[derive(Serialize)]
struct User {
    id: String,
    name: String,
}

// GET /hello - plain text
async fn hello() -> &'static str {
    "Hello, World!"
}

// GET /json - JSON response
async fn json() -> Json<Message> {
    Json(Message {
        message: "Hello, World!",
    })
}

// POST /echo - echo body
async fn echo(body: String) -> String {
    body
}

// GET /health - health check
async fn health() -> Json<Health> {
    Json(Health { status: "ok" })
}

// GET /users/:id - user lookup
async fn get_user(Path(id): Path<String>) -> Json<User> {
    Json(User {
        name: format!("User {}", id),
        id,
    })
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/hello", get(hello))
        .route("/json", get(json))
        .route("/echo", post(echo))
        .route("/health", get(health))
        .route("/users/:id", get(get_user));

    let addr = SocketAddr::from(([0, 0, 0, 0], 8081));
    println!("Axum server listening on http://{}", addr);
    println!("Endpoints:");
    println!("  GET  /hello     - plain text hello world");
    println!("  GET  /json      - JSON hello world");
    println!("  POST /echo      - echo request body");
    println!("  GET  /health    - health check");
    println!("  GET  /users/:id - user lookup");

    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
