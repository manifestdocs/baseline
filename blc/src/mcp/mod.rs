mod rosetta;
mod tools;

pub async fn run_server() {
    use rmcp::ServiceExt;
    use tokio::io::{stdin, stdout};

    let service = tools::BaselineMcp::new();
    let server = service
        .serve((stdin(), stdout()))
        .await
        .expect("Failed to start MCP server");
    server.waiting().await.expect("MCP server error");
}
