#![cfg(feature = "mcp")]

use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

/// Send a JSON-RPC message as a newline-delimited JSON line.
fn send_jsonrpc(stdin: &mut impl Write, msg: &Value) {
    let line = serde_json::to_string(msg).unwrap();
    writeln!(stdin, "{}", line).unwrap();
    stdin.flush().unwrap();
}

/// Read a JSON-RPC response (one JSON line from stdout).
fn read_jsonrpc(reader: &mut BufReader<impl std::io::Read>) -> Value {
    let mut line = String::new();
    reader.read_line(&mut line).unwrap();
    serde_json::from_str(line.trim()).expect("Invalid JSON response")
}

fn spawn_mcp() -> (
    impl Write,
    BufReader<impl std::io::Read>,
    std::process::Child,
) {
    let blc = env!("CARGO_BIN_EXE_blc");
    let mut child = Command::new(blc)
        .arg("mcp")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("Failed to spawn blc mcp");

    let stdin = child.stdin.take().unwrap();
    let stdout = BufReader::new(child.stdout.take().unwrap());
    (stdin, stdout, child)
}

fn initialize(stdin: &mut impl Write, reader: &mut BufReader<impl std::io::Read>) {
    send_jsonrpc(
        stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": { "name": "test", "version": "0.1.0" }
            }
        }),
    );

    // Send initialized notification (no response expected)
    send_jsonrpc(
        stdin,
        &json!({
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }),
    );

    let resp = read_jsonrpc(reader);
    assert_eq!(resp["id"], 1);
    assert_eq!(resp["result"]["serverInfo"]["name"], "baseline");
}

#[test]
fn test_initialize_and_list_tools() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    // List tools
    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list"
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    assert_eq!(resp["id"], 2);
    let tools = resp["result"]["tools"].as_array().unwrap();
    assert_eq!(tools.len(), 5);

    let names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
    assert!(names.contains(&"baseline/check"));
    assert!(names.contains(&"baseline/test"));
    assert!(names.contains(&"baseline/docs"));
    assert!(names.contains(&"baseline/reference"));
    assert!(names.contains(&"baseline/rosetta"));

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_check_inline_source() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/check",
                "arguments": {
                    "source": "fn add(a: Int, b: Int) -> Int = a + b"
                }
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    assert_eq!(resp["id"], 2);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    let check_result: Value = serde_json::from_str(content).unwrap();
    assert_eq!(check_result["status"], "success");

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_check_inline_with_error() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/check",
                "arguments": {
                    "source": "fn bad(x: Int) -> String = x + 1"
                }
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    let check_result: Value = serde_json::from_str(content).unwrap();
    assert_eq!(check_result["status"], "failure");
    assert!(!check_result["diagnostics"].as_array().unwrap().is_empty());

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_reference_full() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/reference",
                "arguments": {}
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    assert!(content.contains("Baseline"));
    assert!(content.contains("## Rosetta Stone"));

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_reference_section() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/reference",
                "arguments": { "section": "mistakes" }
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    assert!(content.contains("Common Agent Mistakes"));
    assert!(!content.contains("## Syntax"));

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_rosetta_search() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/rosetta",
                "arguments": { "concept": "error handling" }
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    let entries: Value = serde_json::from_str(content).unwrap();
    assert!(!entries.as_array().unwrap().is_empty());
    assert!(content.contains("Result"));

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_docs_search() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "baseline/docs",
                "arguments": { "query": "List.map" }
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    let content = resp["result"]["content"][0]["text"].as_str().unwrap();
    let docs: Value = serde_json::from_str(content).unwrap();
    assert!(!docs["modules"].as_array().unwrap().is_empty());

    drop(stdin);
    let _ = child.wait();
}

#[test]
fn test_resources_list_and_read() {
    let (mut stdin, mut stdout, mut child) = spawn_mcp();
    initialize(&mut stdin, &mut stdout);

    // List resources
    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "resources/list"
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    assert_eq!(resp["id"], 2);
    let resources = resp["result"]["resources"].as_array().unwrap();
    assert_eq!(resources.len(), 1);
    assert_eq!(resources[0]["uri"], "baseline://reference/llms.txt");
    assert_eq!(resources[0]["name"], "llms.txt");
    assert_eq!(resources[0]["mimeType"], "text/plain");

    // Read the resource
    send_jsonrpc(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "resources/read",
            "params": {
                "uri": "baseline://reference/llms.txt"
            }
        }),
    );

    let resp = read_jsonrpc(&mut stdout);
    assert_eq!(resp["id"], 3);
    let contents = resp["result"]["contents"].as_array().unwrap();
    assert_eq!(contents.len(), 1);
    assert_eq!(contents[0]["uri"], "baseline://reference/llms.txt");
    let text = contents[0]["text"].as_str().unwrap();
    assert!(text.contains("Baseline"));
    assert!(text.contains("## Rosetta Stone"));

    drop(stdin);
    let _ = child.wait();
}
