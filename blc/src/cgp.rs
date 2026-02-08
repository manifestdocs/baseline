//! Constrained Generation Protocol (CGP) server.
//!
//! Provides token-level type-constrained guidance for LLM inference.
//! The server accepts partial programs and returns valid next tokens
//! based on grammar analysis and type/effect scope checking.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::Read;
use std::sync::Mutex;
use tree_sitter::Parser;

use crate::prelude::Prelude;

// --- Request/Response types ---

#[derive(Deserialize)]
struct StartRequest {
    #[serde(rename = "sessionId")]
    session_id: String,
    #[serde(default)]
    context: SessionContext,
}

#[derive(Deserialize, Default)]
#[allow(dead_code)]
struct SessionContext {
    #[serde(default)]
    file: String,
    #[serde(default)]
    bindings: Vec<Binding>,
    #[serde(default)]
    effects: Vec<String>,
    #[serde(default)]
    expected_type: String,
    #[serde(default)]
    prelude: String,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct Binding {
    name: String,
    #[serde(rename = "type")]
    binding_type: String,
}

#[derive(Deserialize)]
struct AdvanceRequest {
    #[serde(rename = "sessionId")]
    session_id: String,
    tokens: Vec<String>,
}

#[derive(Deserialize)]
struct CompleteRequest {
    #[serde(rename = "sessionId")]
    session_id: String,
}

#[derive(Serialize)]
struct StartResponse {
    #[serde(rename = "sessionId")]
    session_id: String,
    valid_tokens: Vec<String>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    invalid_tokens: HashMap<String, String>,
}

#[derive(Serialize)]
struct AdvanceResponse {
    #[serde(rename = "sessionId")]
    session_id: String,
    valid_tokens: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    partial_type: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    errors: Vec<String>,
}

#[derive(Serialize)]
struct CompleteResponse {
    #[serde(rename = "sessionId")]
    session_id: String,
    status: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    diagnostics: Vec<String>,
}

#[derive(Serialize)]
struct ErrorResponse {
    error: String,
}

// --- Session state ---

struct Session {
    source: String,
    context: SessionContext,
    parser: Parser,
}

// --- Keyword/token sets ---

const KEYWORDS: &[&str] = &[
    "fn", "let", "type", "effect", "if", "then", "else", "match", "for", "in",
    "do", "with", "handle", "import", "export", "where", "test", "not", "true",
    "false", "describe", "context", "it", "expect", "before_each", "after_each",
];

const EXPRESSION_STARTERS: &[&str] = &[
    "if", "match", "for", "with", "handle", "not", "true", "false",
    "(", "[", "{", "|", "??", "#{",
];

const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=",
    "&&", "||", "|>", "..", "++", "?",
];

const TYPES: &[&str] = &[
    "Int", "String", "Boolean", "Float", "Unit", "List", "Option", "Result",
    "Map", "Set",
];

const EFFECTS: &[&str] = &[
    "Console", "Http", "Fs", "Log", "Time", "Db", "Net",
];

// --- Server ---

pub fn run_server(port: u16) {
    let addr = format!("0.0.0.0:{}", port);
    let server = tiny_http::Server::http(&addr).unwrap_or_else(|e| {
        eprintln!("Failed to start CGP server on {}: {}", addr, e);
        std::process::exit(1);
    });

    eprintln!("CGP server listening on http://{}", addr);
    eprintln!("Endpoints:");
    eprintln!("  POST /cgp/start    — Initialize session");
    eprintln!("  POST /cgp/advance  — Advance with tokens");
    eprintln!("  POST /cgp/complete — Finalize session");

    let sessions: Mutex<HashMap<String, Session>> = Mutex::new(HashMap::new());

    for mut request in server.incoming_requests() {
        let path = request.url().to_string();
        let method = request.method().to_string();

        if method != "POST" {
            let resp = tiny_http::Response::from_string(
                serde_json::to_string(&ErrorResponse {
                    error: "Method not allowed".into(),
                })
                .unwrap(),
            )
            .with_status_code(405)
            .with_header(
                tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"application/json"[..])
                    .unwrap(),
            );
            let _ = request.respond(resp);
            continue;
        }

        let mut body = String::new();
        let _ = request.as_reader().read_to_string(&mut body);

        let (status, response_body) = match path.as_str() {
            "/cgp/start" => handle_start(&body, &sessions),
            "/cgp/advance" => handle_advance(&body, &sessions),
            "/cgp/complete" => handle_complete(&body, &sessions),
            _ => (
                404,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Not found: {}", path),
                })
                .unwrap(),
            ),
        };

        let resp = tiny_http::Response::from_string(response_body)
            .with_status_code(status)
            .with_header(
                tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"application/json"[..])
                    .unwrap(),
            );
        let _ = request.respond(resp);
    }
}

fn handle_start(body: &str, sessions: &Mutex<HashMap<String, Session>>) -> (u16, String) {
    let req: StartRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => {
            return (
                400,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Invalid JSON: {}", e),
                })
                .unwrap(),
            )
        }
    };

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .expect("Failed to load grammar");

    let session_id = req.session_id.clone();

    // Compute initial valid tokens based on context
    let mut valid_tokens: Vec<String> = Vec::new();
    let mut invalid_tokens: HashMap<String, String> = HashMap::new();

    // All keywords are initially valid
    for kw in KEYWORDS {
        valid_tokens.push(kw.to_string());
    }

    // Add context bindings
    for binding in &req.context.bindings {
        valid_tokens.push(binding.name.clone());
    }

    // Add in-scope effects
    for eff in &req.context.effects {
        valid_tokens.push(eff.clone());
    }

    // Add prelude types
    for ty in TYPES {
        valid_tokens.push(ty.to_string());
    }

    // Mark out-of-scope effects as invalid
    let in_scope: std::collections::HashSet<&str> =
        req.context.effects.iter().map(|s| s.as_str()).collect();
    for eff in EFFECTS {
        if !in_scope.contains(eff) && !in_scope.is_empty() {
            invalid_tokens.insert(
                eff.to_string(),
                format!("Effect {} not in scope", eff),
            );
        }
    }

    // Add prelude module names as valid tokens
    let prelude_level = match req.context.prelude.as_str() {
        "none" => Prelude::None,
        "minimal" => Prelude::Minimal,
        "pure" => Prelude::Pure,
        "core" => Prelude::Core,
        "script" => Prelude::Script,
        "server" => Prelude::Server,
        _ => Prelude::Core,
    };
    for module in prelude_level.type_modules() {
        let s = module.to_string();
        if !valid_tokens.contains(&s) {
            valid_tokens.push(s);
        }
    }

    valid_tokens.sort();
    valid_tokens.dedup();

    let session = Session {
        source: String::new(),
        context: req.context,
        parser,
    };

    sessions.lock().unwrap().insert(session_id.clone(), session);

    (
        200,
        serde_json::to_string(&StartResponse {
            session_id,
            valid_tokens,
            invalid_tokens,
        })
        .unwrap(),
    )
}

fn handle_advance(body: &str, sessions: &Mutex<HashMap<String, Session>>) -> (u16, String) {
    let req: AdvanceRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => {
            return (
                400,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Invalid JSON: {}", e),
                })
                .unwrap(),
            )
        }
    };

    let mut sessions = sessions.lock().unwrap();
    let session = match sessions.get_mut(&req.session_id) {
        Some(s) => s,
        None => {
            return (
                404,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Session not found: {}", req.session_id),
                })
                .unwrap(),
            )
        }
    };

    // Append tokens to source
    for token in &req.tokens {
        session.source.push_str(token);
    }

    // Parse the current source to determine state
    let tree = session.parser.parse(&session.source, None);

    let mut errors = Vec::new();
    let mut valid_tokens = Vec::new();
    let partial_type: Option<String> = None;

    if let Some(tree) = tree {
        let root = tree.root_node();

        // Check for syntax errors
        if root.has_error() {
            collect_errors_from_tree(root, &session.source, &mut errors);
        }
        // Determine valid next tokens based on parse state
        valid_tokens = compute_valid_next_tokens(
            &session.source,
            &root,
            &session.context,
        );
    } else {
        errors.push("Failed to parse source".into());
    }

    // If we couldn't determine specific valid tokens, provide defaults
    if valid_tokens.is_empty() {
        valid_tokens = default_expression_tokens(&session.context);
    }

    (
        200,
        serde_json::to_string(&AdvanceResponse {
            session_id: req.session_id,
            valid_tokens,
            partial_type,
            errors,
        })
        .unwrap(),
    )
}

fn handle_complete(body: &str, sessions: &Mutex<HashMap<String, Session>>) -> (u16, String) {
    let req: CompleteRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => {
            return (
                400,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Invalid JSON: {}", e),
                })
                .unwrap(),
            )
        }
    };

    let mut sessions = sessions.lock().unwrap();
    let session = match sessions.remove(&req.session_id) {
        Some(s) => s,
        None => {
            return (
                404,
                serde_json::to_string(&ErrorResponse {
                    error: format!("Session not found: {}", req.session_id),
                })
                .unwrap(),
            )
        }
    };

    // Final type check
    let check_result = crate::parse::parse_source(&session.source, "<cgp>");
    let diagnostics: Vec<String> = check_result
        .diagnostics
        .iter()
        .map(|d| format!("{}: {}", d.code, d.message))
        .collect();

    let status = if check_result.status == "success" {
        "valid"
    } else {
        "invalid"
    };

    (
        200,
        serde_json::to_string(&CompleteResponse {
            session_id: req.session_id,
            status: status.into(),
            diagnostics,
        })
        .unwrap(),
    )
}

// --- Token computation ---

fn compute_valid_next_tokens(
    source: &str,
    root: &tree_sitter::Node,
    context: &SessionContext,
) -> Vec<String> {
    let mut tokens = Vec::new();

    // Analyze the last few characters to determine context
    let trimmed = source.trim_end();

    if trimmed.is_empty() {
        // At start of file — top-level items
        tokens.extend(["fn", "@prelude", "@module", "import", "type", "effect", "export",
                       "test", "describe", "context", "@spec", "@requires", "@ensures",
                       "@pure", "@total", "@given", "@returns", "@assume"]
            .iter().map(|s| s.to_string()));
        return tokens;
    }

    // Determine parse context from the last node
    let last_byte = source.len().saturating_sub(1);
    let last_node = deepest_node_at(root, last_byte);
    let parent = last_node.parent();
    let _parent_kind = parent.map(|p| p.kind()).unwrap_or("");

    // Check what kind of position we're in
    let last_char = trimmed.chars().last().unwrap_or(' ');

    match last_char {
        '=' => {
            // After '=' — expression expected
            tokens.extend(expression_tokens(context));
        }
        '(' => {
            // After '(' — expression or pattern expected
            tokens.extend(expression_tokens(context));
            tokens.push(")".into());
        }
        ')' => {
            // After ')' — operator, '->', '=', etc.
            tokens.extend(OPERATORS.iter().map(|s| s.to_string()));
            tokens.extend(["->", "=", ")", ",", "]", "}"].iter().map(|s| s.to_string()));
        }
        ',' => {
            // After ',' — next arg/element
            tokens.extend(expression_tokens(context));
        }
        '.' => {
            // After '.' — field/method access
            // This is context-dependent; provide common fields
            tokens.extend(["map", "filter", "fold", "len", "head", "tail",
                          "get", "insert", "remove", "contains", "keys", "values",
                          "println!", "readln!", "get!", "post!"]
                .iter().map(|s| s.to_string()));
        }
        '{' => {
            // After '{' — block body, record fields, handler clauses
            tokens.extend(expression_tokens(context));
            tokens.push("}".into());
        }
        '}' => {
            // After '}' — end of block
            tokens.extend(OPERATORS.iter().map(|s| s.to_string()));
            tokens.extend(["\n", "where", "|>"].iter().map(|s| s.to_string()));
        }
        _ if last_char.is_alphanumeric() || last_char == '_' || last_char == '!' => {
            // After identifier/number — operator or continuation
            tokens.extend(OPERATORS.iter().map(|s| s.to_string()));
            tokens.extend(["(", ")", ",", ".", "->", "=", "|>", "where", "\n",
                          "then", "else", "do", "in", "with"]
                .iter().map(|s| s.to_string()));
        }
        _ => {
            tokens.extend(expression_tokens(context));
        }
    }

    // Filter out-of-scope effects
    if !context.effects.is_empty() {
        let in_scope: std::collections::HashSet<&str> =
            context.effects.iter().map(|s| s.as_str()).collect();
        tokens.retain(|t| {
            // Keep if it's not an effect name, or if it's in scope
            !EFFECTS.contains(&t.as_str()) || in_scope.contains(t.as_str())
        });
    }

    // Add type constructors
    tokens.extend(["Some", "None", "Ok", "Err"].iter().map(|s| s.to_string()));

    tokens.sort();
    tokens.dedup();
    tokens
}

fn expression_tokens(context: &SessionContext) -> Vec<String> {
    let mut tokens: Vec<String> = EXPRESSION_STARTERS.iter().map(|s| s.to_string()).collect();

    // Add in-scope bindings
    for binding in &context.bindings {
        tokens.push(binding.name.clone());
    }

    // Add in-scope effects
    for eff in &context.effects {
        tokens.push(eff.clone());
    }

    // Add types
    tokens.extend(TYPES.iter().map(|s| s.to_string()));

    // Add basic literals
    tokens.extend(["0", "1", "\""].iter().map(|s| s.to_string()));

    tokens
}

fn default_expression_tokens(context: &SessionContext) -> Vec<String> {
    let mut tokens = expression_tokens(context);
    tokens.extend(KEYWORDS.iter().map(|s| s.to_string()));
    tokens.sort();
    tokens.dedup();
    tokens
}

fn deepest_node_at<'a>(node: &tree_sitter::Node<'a>, byte: usize) -> tree_sitter::Node<'a> {
    let mut current = *node;

    loop {
        let mut cursor = current.walk();
        let mut found_child = false;
        for child in current.children(&mut cursor) {
            if child.start_byte() <= byte && child.end_byte() > byte {
                current = child;
                found_child = true;
                break;
            }
        }
        if !found_child {
            break;
        }
    }

    current
}

fn collect_errors_from_tree(
    node: tree_sitter::Node,
    source: &str,
    errors: &mut Vec<String>,
) {
    if node.is_error() || node.is_missing() {
        let start = node.start_position();
        let text = node
            .utf8_text(source.as_bytes())
            .unwrap_or("<unknown>");
        errors.push(format!(
            "Syntax error at {}:{}: {}",
            start.row + 1,
            start.column + 1,
            if text.len() > 30 { &text[..30] } else { text }
        ));
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_errors_from_tree(child, source, errors);
    }
}
