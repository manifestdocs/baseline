mod rosetta;
mod tools;

pub async fn run_server() -> Result<(), String> {
    use rmcp::ServiceExt;
    use tokio::io::{stdin, stdout};

    let service = tools::BaselineMcp::new();
    let server = service
        .serve((stdin(), stdout()))
        .await
        .map_err(|e| format!("Failed to start MCP server: {}", e))?;
    server
        .waiting()
        .await
        .map_err(|e| format!("MCP server error: {}", e))?;
    Ok(())
}
