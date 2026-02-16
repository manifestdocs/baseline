//! Constrained Generation Protocol (CGP) server.
//!
//! Provides token-level type-constrained guidance for LLM inference.
//! The server accepts partial programs and returns valid next tokens
//! based on incremental parsing, type checking, and effect scope analysis.
//!
//! Key features:
//! - Incremental tree-sitter parsing (sub-ms reparses on token advance)
//! - Parse-tree-based cursor context analysis (not character heuristics)
//! - Type-aware completions via partial type checking
//! - Effect enforcement: tokens referencing out-of-scope effects rejected
//! - Latency tracking (<10ms per advance call target)

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;
use tree_sitter::{InputEdit, Parser, Point, Tree};

use crate::analysis::types::{self, CgpTypeInfo, Type};
use crate::prelude::Prelude;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const KEYWORDS: &[&str] = &[
    "fn", "let", "type", "effect", "if", "then", "else", "match", "for", "in",
    "do", "with", "handle", "import", "export", "where", "test", "not", "true",
    "false", "describe", "context", "it", "expect", "before_each", "after_each",
];

const TOP_LEVEL_STARTERS: &[&str] = &[
    "fn", "@prelude", "@module", "import", "type", "effect", "export",
    "test", "describe", "context", "@spec", "let",
];

const EXPRESSION_STARTERS: &[&str] = &[
    "if", "match", "for", "with", "handle", "not", "true", "false",
    "(", "[", "{", "|", "#{",
];

const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=",
    "&&", "||", "|>", "..", "++", "?",
];

const TYPE_NAMES: &[&str] = &[
    "Int", "String", "Boolean", "Float", "Unit", "List", "Option", "Result",
    "Map", "Set",
];

const BUILTIN_EFFECTS: &[&str] = &[
    "Console", "Http", "Fs", "Log", "Time", "Sqlite", "Postgres", "Mysql", "Net", "Random", "Env",
];

// ---------------------------------------------------------------------------
// Request/Response types
// ---------------------------------------------------------------------------

#[derive(Deserialize)]
struct StartRequest {
    #[serde(rename = "sessionId")]
    session_id: String,
    #[serde(default)]
    context: SessionContext,
}

#[derive(Deserialize, Default, Clone)]
struct SessionContext {
    /// Source file being generated (for diagnostics)
    #[serde(default)]
    #[allow(dead_code)]
    file: String,
    /// In-scope variable bindings with types
    #[serde(default)]
    bindings: Vec<Binding>,
    /// In-scope effect capabilities
    #[serde(default)]
    effects: Vec<String>,
    /// Expected return type of the generated expression
    #[serde(default)]
    expected_type: String,
    /// Prelude level: none, minimal, pure, core, script, server
    #[serde(default)]
    prelude: String,
    /// Generation mode: "expression" (default) or "top_level"
    #[serde(default)]
    mode: String,
}

#[derive(Deserialize, Clone)]
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
    /// Syntactic context at cursor position
    cursor_context: String,
    /// Time spent computing this response (microseconds)
    elapsed_us: u64,
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

// ---------------------------------------------------------------------------
// Session state
// ---------------------------------------------------------------------------

struct Session {
    source: String,
    context: SessionContext,
    parser: Parser,
    /// Retained parse tree for incremental reparsing
    tree: Option<Tree>,
    /// Byte offset of the wrapper prefix (for type-map adjustment)
    #[allow(dead_code)]
    wrapper_prefix_len: usize,
}

// ---------------------------------------------------------------------------
// Cursor context analysis
// ---------------------------------------------------------------------------

/// Syntactic context at the cursor position, determined by walking up the parse tree.
#[derive(Debug, Clone, PartialEq)]
enum CursorContext {
    /// Top of file or between definitions
    TopLevel,
    /// Expression position (function body, let RHS, if condition, etc.)
    Expression,
    /// Type annotation position (after `:` or `->`)
    TypeAnnotation,
    /// Pattern position (match arm, let destructuring)
    Pattern,
    /// Field/method access (after `.`)
    FieldAccess,
    /// Inside function call arguments
    FunctionArgs,
    /// Inside effect set `{Console, ...}`
    EffectSet,
    /// Inside a string literal
    StringLiteral,
    /// Inside a list literal `[...]`
    ListLiteral,
    /// Inside a record/struct literal `{field: ...}`
    RecordLiteral,
    /// After a complete expression (operator or continuation expected)
    AfterExpression,
    /// Inside match arms
    MatchArm,
    /// Function parameter list
    FunctionParams,
}

impl std::fmt::Display for CursorContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CursorContext::TopLevel => write!(f, "top_level"),
            CursorContext::Expression => write!(f, "expression"),
            CursorContext::TypeAnnotation => write!(f, "type_annotation"),
            CursorContext::Pattern => write!(f, "pattern"),
            CursorContext::FieldAccess => write!(f, "field_access"),
            CursorContext::FunctionArgs => write!(f, "function_args"),
            CursorContext::EffectSet => write!(f, "effect_set"),
            CursorContext::StringLiteral => write!(f, "string_literal"),
            CursorContext::ListLiteral => write!(f, "list_literal"),
            CursorContext::RecordLiteral => write!(f, "record_literal"),
            CursorContext::AfterExpression => write!(f, "after_expression"),
            CursorContext::MatchArm => write!(f, "match_arm"),
            CursorContext::FunctionParams => write!(f, "function_params"),
        }
    }
}

/// Analyze the parse tree at the cursor position to determine syntactic context.
fn analyze_cursor_context(root: &tree_sitter::Node, source: &str) -> CursorContext {
    let trimmed = source.trim_end();
    if trimmed.is_empty() {
        return CursorContext::TopLevel;
    }

    let cursor_byte = source.len().saturating_sub(1);
    let deepest = deepest_node_at(root, cursor_byte);

    // Check if we're inside a string
    if deepest.kind() == "string_content" || deepest.kind() == "string" {
        return CursorContext::StringLiteral;
    }

    // Walk up from deepest node to determine context
    let mut node = deepest;
    loop {
        let kind = node.kind();
        match kind {
            "source_file" => break, // fall through to character heuristic
            "function_def" => {
                // Are we in the body (after `=`) or in parameters/return type?
                if let Some(body) = node.child_by_field_name("body")
                    && cursor_byte >= body.start_byte()
                {
                    return CursorContext::Expression;
                }
                if let Some(ret) = node.child_by_field_name("return_type")
                    && cursor_byte >= ret.start_byte()
                {
                    return CursorContext::TypeAnnotation;
                }
                if let Some(params) = node.child_by_field_name("params")
                    && cursor_byte >= params.start_byte()
                    && cursor_byte <= params.end_byte()
                {
                    return CursorContext::FunctionParams;
                }
                return CursorContext::TopLevel;
            }
            "let_binding" => {
                // After `=` is expression, after `:` is type
                if let Some(value) = node.child_by_field_name("value")
                    && cursor_byte >= value.start_byte()
                {
                    return CursorContext::Expression;
                }
                if let Some(ty) = node.child_by_field_name("type")
                    && cursor_byte >= ty.start_byte()
                {
                    return CursorContext::TypeAnnotation;
                }
                return CursorContext::Expression;
            }
            "match_expression" => return CursorContext::MatchArm,
            "match_arm" => {
                // Pattern before `=>`, expression after
                if let Some(body) = node.child_by_field_name("body")
                    && cursor_byte >= body.start_byte()
                {
                    return CursorContext::Expression;
                }
                return CursorContext::Pattern;
            }
            "call_expression" => {
                if let Some(args) = node.child_by_field_name("arguments")
                    && cursor_byte >= args.start_byte()
                {
                    return CursorContext::FunctionArgs;
                }
                return CursorContext::Expression;
            }
            "field_expression" => return CursorContext::FieldAccess,
            "effect_set" => return CursorContext::EffectSet,
            "list_expression" => return CursorContext::ListLiteral,
            "record_expression" | "struct_expression" => return CursorContext::RecordLiteral,
            "type_def" | "type_identifier" | "generic_type" => {
                return CursorContext::TypeAnnotation;
            }
            "if_expression" | "for_expression" | "binary_expression"
            | "unary_expression" | "pipe_expression" | "block"
            | "lambda_expression" | "try_expression" | "with_expression"
            | "handle_expression" | "restrict_expression" | "parenthesized_expression" => {
                return CursorContext::Expression;
            }
            _ => {}
        }

        if let Some(parent) = node.parent() {
            node = parent;
        } else {
            break;
        }
    }

    // Fallback: use last character heuristic
    let last_char = trimmed.chars().last().unwrap_or(' ');
    match last_char {
        '=' | '(' | ',' | '{' | '[' => CursorContext::Expression,
        '.' => CursorContext::FieldAccess,
        ')' | '}' | ']' | '!' => CursorContext::AfterExpression,
        ':' | '>' => CursorContext::TypeAnnotation,
        _ if last_char.is_alphanumeric() || last_char == '_' => CursorContext::AfterExpression,
        _ => CursorContext::Expression,
    }
}

// ---------------------------------------------------------------------------
// Type-aware token computation
// ---------------------------------------------------------------------------

/// Compute valid next tokens based on cursor context, type info, and effects.
fn compute_valid_tokens(
    context: &CursorContext,
    session_ctx: &SessionContext,
    type_info: Option<&CgpTypeInfo>,
    source: &str,
) -> (Vec<String>, HashMap<String, String>) {
    let mut tokens = Vec::new();
    let mut invalid = HashMap::new();

    let in_scope_effects: HashSet<&str> =
        session_ctx.effects.iter().map(|s| s.as_str()).collect();

    match context {
        CursorContext::TopLevel => {
            tokens.extend(TOP_LEVEL_STARTERS.iter().map(|s| s.to_string()));
        }
        CursorContext::Expression => {
            tokens.extend(EXPRESSION_STARTERS.iter().map(|s| s.to_string()));
            tokens.extend(["let", "fn"].iter().map(|s| s.to_string()));

            // Add in-scope bindings
            add_bindings(&mut tokens, session_ctx, type_info);

            // Add type constructors
            tokens.extend(
                ["Some", "None", "Ok", "Err"].iter().map(|s| s.to_string()),
            );

            // Add literal starters
            tokens.extend(["0", "1", "\""].iter().map(|s| s.to_string()));

            // Add in-scope module names for qualified calls
            add_modules(&mut tokens, session_ctx);
        }
        CursorContext::TypeAnnotation => {
            tokens.extend(TYPE_NAMES.iter().map(|s| s.to_string()));
            // Add user-defined type names from type_info
            if let Some(info) = type_info {
                for (name, ty) in &info.bindings {
                    if matches!(ty, Type::Struct(_, _) | Type::Enum(_, _)) {
                        tokens.push(name.clone());
                    }
                }
            }
        }
        CursorContext::Pattern => {
            // Variable names, constructors, wildcards, literals
            tokens.extend(
                ["_", "Some", "None", "Ok", "Err", "true", "false", "0", "1", "\""]
                    .iter()
                    .map(|s| s.to_string()),
            );
            // Add user-defined enum constructors
            if let Some(info) = type_info {
                for (name, ty) in &info.bindings {
                    if matches!(ty, Type::Enum(_, _)) {
                        tokens.push(name.clone());
                    }
                }
            }
        }
        CursorContext::FieldAccess => {
            // Methods and fields based on receiver type
            add_field_completions(&mut tokens, source, session_ctx, type_info);
        }
        CursorContext::FunctionArgs => {
            tokens.extend(EXPRESSION_STARTERS.iter().map(|s| s.to_string()));
            add_bindings(&mut tokens, session_ctx, type_info);
            tokens.extend(
                ["Some", "None", "Ok", "Err", ")", ","]
                    .iter()
                    .map(|s| s.to_string()),
            );
            tokens.extend(["0", "1", "\""].iter().map(|s| s.to_string()));
        }
        CursorContext::EffectSet => {
            // Only in-scope effects
            for eff in &session_ctx.effects {
                tokens.push(eff.clone());
            }
            tokens.push("}".into());
            tokens.push(",".into());
        }
        CursorContext::StringLiteral => {
            // Inside string — no grammar tokens, just content + close
            tokens.push("\"".into());
            tokens.push("${".into()); // interpolation
            return (tokens, invalid);
        }
        CursorContext::ListLiteral => {
            tokens.extend(EXPRESSION_STARTERS.iter().map(|s| s.to_string()));
            add_bindings(&mut tokens, session_ctx, type_info);
            tokens.extend(["]", ","].iter().map(|s| s.to_string()));
            tokens.extend(["0", "1", "\""].iter().map(|s| s.to_string()));
        }
        CursorContext::RecordLiteral => {
            // Field names + values
            tokens.extend(EXPRESSION_STARTERS.iter().map(|s| s.to_string()));
            add_bindings(&mut tokens, session_ctx, type_info);
            tokens.extend(["}", ",", ":"].iter().map(|s| s.to_string()));
        }
        CursorContext::AfterExpression => {
            tokens.extend(OPERATORS.iter().map(|s| s.to_string()));
            tokens.extend(
                ["(", ")", ",", ".", "->", "|>", "where", "\n", "then", "else",
                 "do", "in", "with", "]", "}"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        CursorContext::MatchArm => {
            // Pattern or expression depending on position
            tokens.extend(
                ["_", "Some", "None", "Ok", "Err", "true", "false", "|", "=>"]
                    .iter()
                    .map(|s| s.to_string()),
            );
            tokens.extend(EXPRESSION_STARTERS.iter().map(|s| s.to_string()));
            add_bindings(&mut tokens, session_ctx, type_info);
        }
        CursorContext::FunctionParams => {
            // Parameter names and types
            tokens.extend(TYPE_NAMES.iter().map(|s| s.to_string()));
            tokens.extend([")", ",", ":"].iter().map(|s| s.to_string()));
        }
    }

    // Effect enforcement: mark out-of-scope effects as invalid
    if !in_scope_effects.is_empty() {
        for eff in BUILTIN_EFFECTS {
            if !in_scope_effects.contains(eff) {
                invalid.insert(eff.to_string(), format!("Effect {} not in scope", eff));
            }
        }
    }

    // Filter effect-requiring tokens
    tokens.retain(|t| !invalid.contains_key(t));

    tokens.sort();
    tokens.dedup();
    (tokens, invalid)
}

/// Add in-scope variable bindings to the token list.
fn add_bindings(
    tokens: &mut Vec<String>,
    session_ctx: &SessionContext,
    type_info: Option<&CgpTypeInfo>,
) {
    // Add client-provided bindings
    for binding in &session_ctx.bindings {
        tokens.push(binding.name.clone());
    }
    // Add type-checker-discovered bindings
    if let Some(info) = type_info {
        for (name, _ty) in &info.bindings {
            if !name.starts_with('_') {
                tokens.push(name.clone());
            }
        }
    }
}

/// Add available module names for qualified calls.
fn add_modules(tokens: &mut Vec<String>, session_ctx: &SessionContext) {
    let prelude = parse_prelude(&session_ctx.prelude);
    for module in prelude.type_modules() {
        tokens.push(module.to_string());
    }
}

/// Add field/method completions based on receiver type context.
fn add_field_completions(
    tokens: &mut Vec<String>,
    source: &str,
    session_ctx: &SessionContext,
    type_info: Option<&CgpTypeInfo>,
) {
    // Extract the receiver (text before the last `.`)
    let trimmed = source.trim_end();
    let receiver = trimmed.rsplit_once('.').map(|(_, _)| {
        // Walk back to find the receiver identifier
        let before_dot = &trimmed[..trimmed.len() - 1].trim_end();
        before_dot
            .rsplit(|c: char| !c.is_alphanumeric() && c != '_')
            .next()
            .unwrap_or("")
    });

    if let Some(receiver_name) = receiver {
        // Check if receiver is a module name — provide module methods
        if let Some(info) = type_info {
            let prefix = format!("{}.", receiver_name);
            for (method_name, _ty) in &info.module_methods {
                if let Some(method) = method_name.strip_prefix(&prefix) {
                    tokens.push(method.to_string());
                }
            }
        }

        // Check prelude modules
        let prelude = parse_prelude(&session_ctx.prelude);
        let is_module = prelude
            .type_modules()
            .iter()
            .any(|m| m.to_string() == *receiver_name);

        if is_module {
            // Module method access — add common methods
            add_module_methods(tokens, receiver_name, session_ctx);
            return;
        }
    }

    // Generic field/method completions
    tokens.extend(
        [
            "map", "filter", "fold", "len", "head", "tail", "get", "insert",
            "remove", "contains", "keys", "values", "unwrap", "is_some",
            "is_none", "is_ok", "is_err",
        ]
        .iter()
        .map(|s| s.to_string()),
    );
}

/// Add module-specific method completions.
fn add_module_methods(tokens: &mut Vec<String>, module: &str, ctx: &SessionContext) {
    let in_scope: HashSet<&str> = ctx.effects.iter().map(|s| s.as_str()).collect();

    match module {
        "Console" if in_scope.contains("Console") => {
            tokens.extend(
                ["println!", "print!", "error!", "readln!"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "List" => {
            tokens.extend(
                ["map", "filter", "fold", "find", "head", "tail", "len", "reverse",
                 "contains", "zip", "take", "drop", "flatten", "sort", "range"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "String" => {
            tokens.extend(
                ["len", "slice", "contains", "split", "trim", "upper", "lower",
                 "starts_with", "ends_with", "replace", "chars", "to_int", "join"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Option" => {
            tokens.extend(
                ["map", "unwrap", "unwrap_or", "is_some", "is_none", "flat_map"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Result" => {
            tokens.extend(
                ["map", "unwrap", "unwrap_or", "is_ok", "is_err", "map_err", "context", "and_then"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Math" => {
            tokens.extend(
                ["abs", "min", "max", "floor", "ceil", "round", "sqrt", "pow"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Map" => {
            tokens.extend(
                ["get", "insert", "remove", "contains", "keys", "values", "len",
                 "entries", "from_list", "merge"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Set" => {
            tokens.extend(
                ["insert", "remove", "contains", "len", "union", "intersect",
                 "difference", "from_list"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Http" if in_scope.contains("Http") => {
            tokens.extend(
                ["get!", "post!", "put!", "delete!", "request!"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Fs" if in_scope.contains("Fs") => {
            tokens.extend(
                ["read!", "write!", "exists!", "delete!", "list!"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Json" => {
            tokens.extend(
                ["parse", "stringify", "get", "from_string"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        "Int" => {
            tokens.extend(
                ["from_string", "to_string", "abs", "min", "max"]
                    .iter()
                    .map(|s| s.to_string()),
            );
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Partial type computation
// ---------------------------------------------------------------------------

/// Build a type-checkable wrapper around partial source code.
/// Returns (wrapped_source, prefix_byte_length).
fn build_type_check_wrapper(source: &str, ctx: &SessionContext) -> (String, usize) {
    let prelude_name = if ctx.prelude.is_empty() {
        "core"
    } else {
        &ctx.prelude
    };

    let mode = if ctx.mode.is_empty() {
        "expression"
    } else {
        &ctx.mode
    };

    if mode == "top_level" || source.trim_start().starts_with("fn ")
        || source.trim_start().starts_with("type ")
        || source.trim_start().starts_with("@")
        || source.trim_start().starts_with("import ")
    {
        // Top-level code — add prelude only
        let prefix = format!("@prelude({})\n", prelude_name);
        let prefix_len = prefix.len();
        let wrapped = format!("{}{}", prefix, source);
        return (wrapped, prefix_len);
    }

    // Expression mode — wrap in a function body with bindings as parameters
    let params: Vec<String> = ctx
        .bindings
        .iter()
        .map(|b| format!("{}: {}", b.name, b.binding_type))
        .collect();
    let params_str = params.join(", ");

    let ret_type = if ctx.expected_type.is_empty() {
        "Unit"
    } else {
        &ctx.expected_type
    };

    let prefix = format!(
        "@prelude({})\nfn _cgp({}) -> {} =\n  ",
        prelude_name, params_str, ret_type
    );
    let prefix_len = prefix.len();
    let wrapped = format!("{}{}\n", prefix, source);
    (wrapped, prefix_len)
}

/// Run the type checker on wrapped partial source and extract type info.
fn compute_type_info(source: &str, ctx: &SessionContext) -> Option<(CgpTypeInfo, usize)> {
    let (wrapped, prefix_len) = build_type_check_wrapper(source, ctx);

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .ok()?;

    let tree = parser.parse(&wrapped, None)?;
    let root = tree.root_node();

    let info = types::check_types_for_cgp(&root, &wrapped, "<cgp>");
    Some((info, prefix_len))
}

/// Extract the partial type at the cursor position from the TypeMap.
fn extract_partial_type(
    type_info: &CgpTypeInfo,
    source_len: usize,
    prefix_len: usize,
) -> Option<String> {
    // Look for the type at or near the end of the user's source
    let target_byte = prefix_len + source_len;

    // Search backwards from cursor for the nearest typed node
    let mut best: Option<(usize, &Type)> = None;
    for (byte_offset, ty) in &type_info.type_map {
        if *byte_offset <= target_byte && *byte_offset >= prefix_len {
            if matches!(ty, Type::Unknown | Type::Unit) {
                continue;
            }
            match best {
                None => best = Some((*byte_offset, ty)),
                Some((prev_offset, _)) if *byte_offset > prev_offset => {
                    best = Some((*byte_offset, ty));
                }
                _ => {}
            }
        }
    }

    best.map(|(_, ty)| ty.to_string())
}

// ---------------------------------------------------------------------------
// Server
// ---------------------------------------------------------------------------

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
    eprintln!("  POST /cgp/complete — Finalize and validate");

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
        let _ = std::io::Read::read_to_string(&mut request.as_reader(), &mut body);

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

// ---------------------------------------------------------------------------
// Handlers
// ---------------------------------------------------------------------------

fn handle_start(body: &str, sessions: &Mutex<HashMap<String, Session>>) -> (u16, String) {
    let req: StartRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => return json_error(400, &format!("Invalid JSON: {}", e)),
    };

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .expect("Failed to load grammar");

    let session_id = req.session_id.clone();
    let prelude = parse_prelude(&req.context.prelude);

    // Compute initial valid tokens
    let mut valid_tokens: Vec<String> = Vec::new();
    let mut invalid_tokens: HashMap<String, String> = HashMap::new();

    // Top-level starters
    valid_tokens.extend(TOP_LEVEL_STARTERS.iter().map(|s| s.to_string()));

    // Keywords
    valid_tokens.extend(KEYWORDS.iter().map(|s| s.to_string()));

    // Context bindings
    for binding in &req.context.bindings {
        valid_tokens.push(binding.name.clone());
    }

    // In-scope effects
    for eff in &req.context.effects {
        valid_tokens.push(eff.clone());
    }

    // Prelude types and modules
    valid_tokens.extend(TYPE_NAMES.iter().map(|s| s.to_string()));
    for module in prelude.type_modules() {
        valid_tokens.push(module.to_string());
    }

    // Mark out-of-scope effects as invalid
    let in_scope: HashSet<&str> = req.context.effects.iter().map(|s| s.as_str()).collect();
    if !in_scope.is_empty() {
        for eff in BUILTIN_EFFECTS {
            if !in_scope.contains(eff) {
                invalid_tokens.insert(eff.to_string(), format!("Effect {} not in scope", eff));
            }
        }
    }

    valid_tokens.sort();
    valid_tokens.dedup();

    // Compute wrapper prefix length for this context
    let (_, prefix_len) = build_type_check_wrapper("", &req.context);

    let session = Session {
        source: String::new(),
        context: req.context,
        parser,
        tree: None,
        wrapper_prefix_len: prefix_len,
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
    let start_time = std::time::Instant::now();

    let req: AdvanceRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => return json_error(400, &format!("Invalid JSON: {}", e)),
    };

    let mut sessions = sessions.lock().unwrap();
    let session = match sessions.get_mut(&req.session_id) {
        Some(s) => s,
        None => return json_error(404, &format!("Session not found: {}", req.session_id)),
    };

    // Track old source length for incremental edit
    let old_len = session.source.len();

    // Append tokens to source
    for token in &req.tokens {
        session.source.push_str(token);
    }
    let new_len = session.source.len();

    // Incremental parse: edit the old tree, then reparse with it as reference
    if old_len != new_len
        && let Some(ref mut tree) = session.tree
    {
        let edit = InputEdit {
            start_byte: old_len,
            old_end_byte: old_len,
            new_end_byte: new_len,
            start_position: byte_to_point(&session.source, old_len),
            old_end_position: byte_to_point(&session.source, old_len),
            new_end_position: byte_to_point(&session.source, new_len),
        };
        tree.edit(&edit);
    }

    // Parse (incremental if we have an old tree)
    let new_tree = session
        .parser
        .parse(&session.source, session.tree.as_ref());
    session.tree = new_tree;

    let mut errors = Vec::new();
    let mut cursor_context = CursorContext::TopLevel;
    let mut partial_type: Option<String> = None;

    // Analyze parse tree
    if let Some(ref tree) = session.tree {
        let root = tree.root_node();

        // Collect syntax errors
        if root.has_error() {
            collect_errors_from_tree(root, &session.source, &mut errors);
        }

        // Determine cursor context from parse tree
        cursor_context = analyze_cursor_context(&root, &session.source);
    } else {
        errors.push("Failed to parse source".into());
    }

    // Run type checker for partial_type and type-aware completions
    let type_info = compute_type_info(&session.source, &session.context);

    if let Some((ref info, prefix_len)) = type_info {
        // Extract partial type at cursor
        partial_type = extract_partial_type(info, session.source.len(), prefix_len);

        // Collect type errors (non-syntax)
        for diag in &info.diagnostics {
            if diag.code.starts_with("TYP_") || diag.code.starts_with("CAP_") {
                errors.push(format!("{}: {}", diag.code, diag.message));
            }
        }
    }

    // Compute valid tokens based on cursor context + type info
    let (valid_tokens, _invalid) = compute_valid_tokens(
        &cursor_context,
        &session.context,
        type_info.as_ref().map(|(info, _)| info),
        &session.source,
    );

    let elapsed = start_time.elapsed();

    (
        200,
        serde_json::to_string(&AdvanceResponse {
            session_id: req.session_id,
            valid_tokens,
            partial_type,
            errors,
            cursor_context: cursor_context.to_string(),
            elapsed_us: elapsed.as_micros() as u64,
        })
        .unwrap(),
    )
}

fn handle_complete(body: &str, sessions: &Mutex<HashMap<String, Session>>) -> (u16, String) {
    let req: CompleteRequest = match serde_json::from_str(body) {
        Ok(r) => r,
        Err(e) => return json_error(400, &format!("Invalid JSON: {}", e)),
    };

    let mut sessions = sessions.lock().unwrap();
    let session = match sessions.remove(&req.session_id) {
        Some(s) => s,
        None => return json_error(404, &format!("Session not found: {}", req.session_id)),
    };

    // Final validation: full parse + type check + effect check
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

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn json_error(status: u16, msg: &str) -> (u16, String) {
    (
        status,
        serde_json::to_string(&ErrorResponse {
            error: msg.to_string(),
        })
        .unwrap(),
    )
}

fn parse_prelude(name: &str) -> Prelude {
    match name {
        "none" => Prelude::None,
        "minimal" => Prelude::Minimal,
        "pure" => Prelude::Pure,
        "core" => Prelude::Core,
        "script" => Prelude::Script,
        "server" => Prelude::Server,
        _ => Prelude::Core,
    }
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

fn byte_to_point(source: &str, byte_offset: usize) -> Point {
    let prefix = &source[..byte_offset.min(source.len())];
    let row = prefix.matches('\n').count();
    let col = prefix.len() - prefix.rfind('\n').map(|i| i + 1).unwrap_or(0);
    Point::new(row, col)
}

fn collect_errors_from_tree(
    node: tree_sitter::Node,
    source: &str,
    errors: &mut Vec<String>,
) {
    if node.is_error() || node.is_missing() {
        let start = node.start_position();
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<unknown>");
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_context(prelude: &str, bindings: Vec<(&str, &str)>, effects: Vec<&str>) -> SessionContext {
        SessionContext {
            file: "test.bl".into(),
            bindings: bindings
                .into_iter()
                .map(|(n, t)| Binding {
                    name: n.into(),
                    binding_type: t.into(),
                })
                .collect(),
            effects: effects.into_iter().map(String::from).collect(),
            expected_type: "Int".into(),
            prelude: prelude.into(),
            mode: "expression".into(),
        }
    }

    #[test]
    fn test_cursor_context_empty_source() {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse("", None).unwrap();
        let ctx = analyze_cursor_context(&tree.root_node(), "");
        assert_eq!(ctx, CursorContext::TopLevel);
    }

    #[test]
    fn test_cursor_context_after_fn() {
        let source = "fn add(x: Int, y: Int) -> Int =\n  ";
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let ctx = analyze_cursor_context(&tree.root_node(), source);
        // Should be expression context (in function body)
        assert!(
            matches!(ctx, CursorContext::Expression | CursorContext::TopLevel),
            "Expected Expression or TopLevel, got {:?}",
            ctx
        );
    }

    #[test]
    fn test_cursor_context_after_dot() {
        let source = "List.";
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let ctx = analyze_cursor_context(&tree.root_node(), source);
        assert_eq!(ctx, CursorContext::FieldAccess);
    }

    #[test]
    fn test_incremental_parse() {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();

        // Initial parse
        let source1 = "fn f() -> Int = ";
        let tree1 = parser.parse(source1, None).unwrap();

        // Incremental: append "42"
        let source2 = "fn f() -> Int = 42";
        let mut tree = tree1;
        let edit = InputEdit {
            start_byte: source1.len(),
            old_end_byte: source1.len(),
            new_end_byte: source2.len(),
            start_position: byte_to_point(source2, source1.len()),
            old_end_position: byte_to_point(source2, source1.len()),
            new_end_position: byte_to_point(source2, source2.len()),
        };
        tree.edit(&edit);

        let tree2 = parser.parse(source2, Some(&tree)).unwrap();
        assert!(!tree2.root_node().has_error());
    }

    #[test]
    fn test_type_check_wrapper_expression() {
        let ctx = make_context("core", vec![("x", "Int"), ("y", "String")], vec!["Console"]);
        let (wrapped, prefix_len) = build_type_check_wrapper("x + 1", &ctx);

        assert!(wrapped.contains("@prelude(core)"));
        assert!(wrapped.contains("fn _cgp(x: Int, y: String) -> Int ="));
        assert!(wrapped.contains("x + 1"));
        assert!(prefix_len > 0);
    }

    #[test]
    fn test_type_check_wrapper_top_level() {
        let ctx = SessionContext {
            mode: "top_level".into(),
            prelude: "script".into(),
            ..Default::default()
        };
        let source = "fn greet(name: String) -> String =\n  \"Hello\"";
        let (wrapped, prefix_len) = build_type_check_wrapper(source, &ctx);

        assert!(wrapped.starts_with("@prelude(script)\n"));
        assert!(wrapped.contains("fn greet"));
        // Top-level mode: prefix is just the prelude line
        assert_eq!(prefix_len, "@prelude(script)\n".len());
    }

    #[test]
    fn test_partial_type_extraction() {
        let ctx = make_context("core", vec![], vec![]);
        let source = "42";
        let result = compute_type_info(source, &ctx);
        assert!(result.is_some());

        if let Some((info, prefix_len)) = result {
            let pt = extract_partial_type(&info, source.len(), prefix_len);
            // Should detect Int from the literal 42
            assert_eq!(pt, Some("Int".to_string()));
        }
    }

    #[test]
    fn test_valid_tokens_expression_context() {
        let ctx = make_context("core", vec![("x", "Int")], vec!["Console"]);
        let (tokens, invalid) = compute_valid_tokens(
            &CursorContext::Expression,
            &ctx,
            None,
            "",
        );

        assert!(tokens.contains(&"x".to_string()), "Should include binding 'x'");
        assert!(tokens.contains(&"if".to_string()), "Should include keyword 'if'");
        assert!(tokens.contains(&"Some".to_string()), "Should include constructor 'Some'");
        assert!(!invalid.contains_key("Console"), "Console should be in scope");
        assert!(invalid.contains_key("Http"), "Http should NOT be in scope");
    }

    #[test]
    fn test_valid_tokens_type_context() {
        let ctx = make_context("core", vec![], vec![]);
        let (tokens, _) = compute_valid_tokens(
            &CursorContext::TypeAnnotation,
            &ctx,
            None,
            "",
        );

        assert!(tokens.contains(&"Int".to_string()));
        assert!(tokens.contains(&"String".to_string()));
        assert!(tokens.contains(&"List".to_string()));
        assert!(!tokens.contains(&"if".to_string()), "Keywords not valid in type position");
    }

    #[test]
    fn test_valid_tokens_effect_set() {
        let ctx = make_context("script", vec![], vec!["Console", "Http"]);
        let (tokens, _) = compute_valid_tokens(
            &CursorContext::EffectSet,
            &ctx,
            None,
            "",
        );

        assert!(tokens.contains(&"Console".to_string()));
        assert!(tokens.contains(&"Http".to_string()));
        assert!(tokens.contains(&"}".to_string()));
    }

    #[test]
    fn test_byte_to_point() {
        let source = "line1\nline2\nline3";
        let p = byte_to_point(source, 6); // start of "line2"
        assert_eq!(p.row, 1);
        assert_eq!(p.column, 0);

        let p2 = byte_to_point(source, 8); // "ne2"
        assert_eq!(p2.row, 1);
        assert_eq!(p2.column, 2);
    }

    #[test]
    fn test_effect_enforcement() {
        let ctx = make_context("script", vec![], vec!["Console"]);
        let (tokens, invalid) = compute_valid_tokens(
            &CursorContext::Expression,
            &ctx,
            None,
            "",
        );

        // Console is in scope — should not be in invalid
        assert!(!invalid.contains_key("Console"));
        // Http is NOT in scope — should be in invalid
        assert!(invalid.contains_key("Http"));
        // Fs is NOT in scope — should be in invalid
        assert!(invalid.contains_key("Fs"));
        // Tokens should not contain invalid effect names
        assert!(!tokens.contains(&"Http".to_string()));
    }

    #[test]
    fn test_advance_performance() {
        // Verify that a single advance call completes in <10ms
        let ctx = make_context("core", vec![("x", "Int")], vec!["Console"]);

        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();

        let source = "let y = x + 1\nlet z = y * 2\n";
        let tree = parser.parse(source, None);

        let start = std::time::Instant::now();

        // Simulate what handle_advance does
        if let Some(ref t) = tree {
            let _ctx = analyze_cursor_context(&t.root_node(), source);
        }
        let _type_info = compute_type_info(source, &ctx);
        let (_tokens, _invalid) = compute_valid_tokens(
            &CursorContext::Expression,
            &ctx,
            None,
            source,
        );

        let elapsed = start.elapsed();
        assert!(
            elapsed.as_millis() < 10,
            "Advance took {}ms, expected <10ms",
            elapsed.as_millis()
        );
    }
}
