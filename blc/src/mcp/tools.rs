use rmcp::{
    handler::server::{tool::ToolRouter, wrapper::Parameters},
    model::{CallToolResult, Content, ServerInfo},
    tool, tool_handler, tool_router, ErrorData as McpError, ServerHandler,
};
use schemars::JsonSchema;
use serde::Deserialize;
use std::path::PathBuf;

use super::rosetta;

const LLMS_TXT: &str = include_str!("../../../llms.txt");

// ============================================================
// Request types
// ============================================================

#[derive(Deserialize, JsonSchema)]
pub struct CheckRequest {
    #[schemars(
        description = "Absolute path to a .bl file to type-check. Mutually exclusive with `source` — provide one or the other. Use this for files already on disk."
    )]
    pub file: Option<String>,

    #[schemars(
        description = "Inline Baseline source code to type-check. Mutually exclusive with `file` — provide one or the other. Use this to validate code snippets without writing to disk. Example: \"fn add(a: Int, b: Int) -> Int = a + b\""
    )]
    pub source: Option<String>,

    #[serde(default = "default_level")]
    #[schemars(
        description = "Verification depth. 'types' — type and effect checking only (fastest). 'refinements' (default) — types + refinement type constraints. 'full' — types + refinements + SMT spec checking (slowest, may timeout on complex files)."
    )]
    pub level: String,
}

fn default_level() -> String {
    "refinements".to_string()
}

#[derive(Deserialize, JsonSchema)]
pub struct TestRequest {
    #[schemars(
        description = "Absolute path to a .bl file containing @test blocks. The file must have at least one `@test` section with `test \"name\" = expr` assertions. Returns pass/fail/skip for each test. If the file has analysis errors, all tests fail with an 'analysis' error."
    )]
    pub file: String,
}

#[derive(Deserialize, JsonSchema)]
pub struct DocsRequest {
    #[schemars(
        description = "Search query to find functions in the standard library. Matches against function names, module names, and descriptions. Examples: 'List.map', 'filter', 'Http', 'read'. Omit to return all modules."
    )]
    pub query: Option<String>,

    #[schemars(
        description = "Filter results to a single module. Module names are PascalCase: 'List', 'String', 'Console', 'Http', 'Fs', 'Math', 'Option', 'Result', 'Random', 'Log', 'Time', 'Env'. Can be combined with `query` to search within a module."
    )]
    pub module: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
pub struct ReferenceRequest {
    #[schemars(
        description = "Return a specific section of the language reference. Available sections: 'rosetta' (Python→Baseline translations), 'syntax' (grammar reference), 'key_rules' (7 essential rules), 'not_supported' (features that don't exist), 'mistakes' (common agent errors with corrections), 'patterns' (idiomatic code examples), 'http_server' (web server setup), 'preludes' (import levels), 'compiler' (CLI commands). Omit to return the full document. Start with 'mistakes' or 'key_rules' when learning the language."
    )]
    pub section: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
pub struct RosettaRequest {
    #[schemars(
        description = "Programming concept to search for in the translation table. Use natural language: 'error handling', 'for loop', 'class', 'null check', 'print', 'string concat', 'import'. Returns the top matches with Python, TypeScript, and Baseline equivalents."
    )]
    pub concept: Option<String>,

    #[schemars(
        description = "Source language code snippet to find the Baseline equivalent for. Example: 'try: val = risky() except: val = default'. The search matches against Python and TypeScript snippets in the translation table."
    )]
    pub snippet: Option<String>,

    #[schemars(
        description = "Source language for snippet matching: 'python' (default), 'typescript', 'javascript'. When set, snippet matches are scored higher against that language's column."
    )]
    pub source_language: Option<String>,
}

// ============================================================
// Server struct
// ============================================================

pub struct BaselineMcp {
    tool_router: ToolRouter<Self>,
}

impl BaselineMcp {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }
}

// ============================================================
// Tool implementations
// ============================================================

#[tool_router]
impl BaselineMcp {
    #[tool(
        name = "baseline/check",
        description = "VERIFY: Type-check a Baseline source file or inline code snippet. Returns a JSON object with `status` ('success' or 'failure'), `diagnostics` array (each with severity, code, message, location), and `verification_level`. Use `source` for quick validation of code you're writing. Use `file` for files already on disk. Call this BEFORE writing code to disk — catch type errors, effect errors, and refinement violations early. If status is 'failure', read the diagnostic messages carefully — they explain exactly what's wrong and often suggest fixes."
    )]
    async fn check(&self, params: Parameters<CheckRequest>) -> Result<CallToolResult, McpError> {
        let req = params.0;

        // Validate level early
        let ver_level: crate::diagnostics::VerificationLevel =
            req.level.parse().map_err(|e: String| {
                McpError::invalid_params(e, None)
            })?;

        // Resolve path: either from file param or write inline source to tempfile
        let (_tmpdir, path) = match (&req.file, &req.source) {
            (Some(file), None) => (None, PathBuf::from(file)),
            (None, Some(source)) => {
                let dir = tempfile::tempdir().map_err(|e| {
                    McpError::internal_error(format!("Failed to create temp dir: {}", e), None)
                })?;
                let p = dir.path().join("check.bl");
                std::fs::write(&p, source).map_err(|e| {
                    McpError::internal_error(format!("Failed to write temp file: {}", e), None)
                })?;
                (Some(dir), p)
            }
            (Some(_), Some(_)) => {
                return Err(McpError::invalid_params(
                    "Provide either `file` or `source`, not both",
                    None,
                ));
            }
            (None, None) => {
                return Err(McpError::invalid_params(
                    "Provide either `file` or `source`",
                    None,
                ));
            }
        };

        let mut result = crate::parse::parse_file(&path).map_err(|e| {
            McpError::internal_error(format!("Failed to read file: {}", e), None)
        })?;

        // Set verification level metadata on the result
        result.verification_level = ver_level;

        let json = serde_json::to_string_pretty(&result)
            .map_err(|e| McpError::internal_error(format!("JSON error: {}", e), None))?;

        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    #[tool(
        name = "baseline/test",
        description = "VERIFY: Run @test blocks in a Baseline source file. Returns JSON with `summary` (total, passed, failed, skipped) and `tests` array (each with name, status, optional error message). The file must contain at least one `@test` section. If the file has analysis errors (type/effect/parse), ALL tests fail with an 'analysis' error — fix those first using baseline/check. Use this after baseline/check passes to verify runtime behavior."
    )]
    async fn test(&self, params: Parameters<TestRequest>) -> Result<CallToolResult, McpError> {
        let path = PathBuf::from(&params.0.file);
        let result = crate::vm::test_runner::run_test_file(&path);
        let json = serde_json::to_string_pretty(&result)
            .map_err(|e| McpError::internal_error(format!("JSON error: {}", e), None))?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    #[tool(
        name = "baseline/docs",
        description = "LEARN: Search the Baseline standard library documentation. Returns JSON with `modules` array, each containing `name`, `functions` (with signature, description, examples). Use `query` to search across all modules (e.g., 'map', 'filter', 'read'). Use `module` to browse a specific module (e.g., 'List', 'Http', 'Console'). Combine both to search within a module. Call this when you need to know what functions are available, what arguments they take, or how to use a specific API."
    )]
    async fn docs(&self, params: Parameters<DocsRequest>) -> Result<CallToolResult, McpError> {
        let docs = crate::docs::generate_docs();

        // Apply query filter
        let docs = match &params.0.query {
            Some(q) => crate::docs::filter_docs(&docs, q),
            None => docs,
        };

        // Apply module filter
        let docs = match &params.0.module {
            Some(module) => {
                let module_lower = module.to_lowercase();
                crate::docs::DocsOutput {
                    modules: docs
                        .modules
                        .into_iter()
                        .filter(|m| m.name.to_lowercase() == module_lower)
                        .collect(),
                }
            }
            None => docs,
        };

        let json = serde_json::to_string_pretty(&docs)
            .map_err(|e| McpError::internal_error(format!("JSON error: {}", e), None))?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    #[tool(
        name = "baseline/reference",
        description = "LEARN: Return the Baseline language quick reference (llms.txt). This is the authoritative guide to Baseline syntax, rules, and patterns. Use `section` to fetch a specific part: 'key_rules' (start here — 7 essential rules), 'mistakes' (common agent errors with corrections), 'syntax' (grammar reference), 'patterns' (idiomatic code examples), 'rosetta' (Python/TS→Baseline translations), 'not_supported' (features that don't exist in Baseline), 'http_server' (web server setup), 'preludes' (import levels), 'compiler' (CLI commands). Omit `section` for the full document. When first learning Baseline, start with 'key_rules' then 'mistakes'."
    )]
    async fn reference(
        &self,
        params: Parameters<ReferenceRequest>,
    ) -> Result<CallToolResult, McpError> {
        let text = match &params.0.section {
            Some(section) => extract_section(LLMS_TXT, section).ok_or_else(|| {
                McpError::invalid_params(
                    format!(
                        "Unknown section '{}'. Available: rosetta, syntax, key_rules, not_supported, mistakes, patterns, http_server, preludes, compiler",
                        section
                    ),
                    None,
                )
            })?,
            None => LLMS_TXT.to_string(),
        };
        Ok(CallToolResult::success(vec![Content::text(text)]))
    }

    #[tool(
        name = "baseline/rosetta",
        description = "LEARN: Translate a programming concept or code snippet from Python/TypeScript to Baseline. Searches a curated table of ~80 common patterns and returns matches with side-by-side Python, TypeScript, and Baseline code. Use `concept` for natural language queries ('error handling', 'for loop', 'class', 'null check'). Use `snippet` to paste source code and find the Baseline equivalent. Set `source_language` to 'typescript' if translating from TS. Returns up to 10 ranked matches. Use this when you know how to do something in Python/TS and need the Baseline way."
    )]
    async fn rosetta(
        &self,
        params: Parameters<RosettaRequest>,
    ) -> Result<CallToolResult, McpError> {
        let req = params.0;
        let query = req
            .concept
            .as_deref()
            .or(req.snippet.as_deref())
            .unwrap_or("");

        if query.is_empty() {
            return Err(McpError::invalid_params(
                "Provide `concept` or `snippet` to search",
                None,
            ));
        }

        let results = rosetta::search(query, req.source_language.as_deref(), 10);

        if results.is_empty() {
            return Ok(CallToolResult::success(vec![Content::text(
                "No matching patterns found. Try broader terms like 'error', 'loop', 'class', 'print'.",
            )]));
        }

        let json = serde_json::to_string_pretty(&results)
            .map_err(|e| McpError::internal_error(format!("JSON error: {}", e), None))?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}

#[tool_handler]
impl ServerHandler for BaselineMcp {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            server_info: rmcp::model::Implementation {
                name: "baseline".into(),
                version: env!("CARGO_PKG_VERSION").into(),
                title: None,
                icons: None,
                website_url: None,
            },
            capabilities: rmcp::model::ServerCapabilities::builder()
                .enable_tools()
                .build(),
            instructions: Some(INSTRUCTIONS.into()),
            ..Default::default()
        }
    }
}

// ============================================================
// Section parser for llms.txt
// ============================================================

fn extract_section(text: &str, section: &str) -> Option<String> {
    let heading = match section.to_lowercase().as_str() {
        "rosetta" => "## Rosetta Stone",
        "syntax" => "## Syntax",
        "key_rules" => "## Key Rules",
        "not_supported" => "## NOT Supported",
        "mistakes" => "## Common Agent Mistakes",
        "patterns" => "## Common Patterns",
        "http_server" => "## HTTP Server",
        "preludes" => "## Preludes",
        "compiler" => "## Compiler",
        _ => return None,
    };

    let start = text.find(heading)?;
    let after_heading = start + heading.len();
    let end = text[after_heading..]
        .find("\n## ")
        .map(|pos| after_heading + pos)
        .unwrap_or(text.len());

    Some(text[start..end].trim_end().to_string())
}

// ============================================================
// Instructions for AI agents
// ============================================================

const INSTRUCTIONS: &str = r#"<baseline_instructions>
<overview>
Baseline is a strongly typed, effect-tracked functional programming language. Extension: `.bl`, compiler: `blc`.

Design philosophy: "The type is the spec" — types encode correctness using refinements. "Effects are data" — side effects are explicit capabilities tracked by the compiler. "LLM-native" — designed for machine generation with unambiguous syntax.

This MCP server provides tools to type-check code, run tests, search standard library docs, look up language syntax, and translate patterns from Python/TypeScript. Use these tools to write correct Baseline code without guessing.
</overview>

<tool_selection>
Choose the right tool for what you need:

LEARNING BASELINE (do this first):
- baseline/reference section="key_rules" — 7 essential rules every agent must know
- baseline/reference section="mistakes" — common errors with corrections (read this early!)
- baseline/rosetta — translate a concept you know from Python/TS to Baseline
- baseline/docs — look up available standard library functions

WRITING CODE:
- baseline/check with source="..." — validate code BEFORE writing to disk (fast feedback loop)
- baseline/check with file="/path/to/file.bl" — validate a file already on disk
- baseline/test with file="/path/to/file.bl" — run @test blocks after check passes

REFERENCE:
- baseline/reference — full language reference or specific sections
- baseline/docs with module="List" — browse a specific module's functions
- baseline/docs with query="map" — search across all modules

Recommended workflow: reference(key_rules) → reference(mistakes) → rosetta(concept) → write code → check(source) → fix errors → write to disk → test(file)
</tool_selection>

<key_rules>
These 7 rules prevent the most common errors:

1. EFFECTS: Effectful functions end with `!` — Console.println!("hello"), Http.get!(url). Pure functions (no `!`) CANNOT call effectful functions. The compiler enforces this.

2. NEGATION: Use `not` keyword for boolean negation — `if not valid then ...`. NEVER use `!` for negation (that's the effect marker).

3. CALL STYLE: Always Module.function(value) — String.to_upper(name), List.map(items, f). NEVER value.method() style. There are no methods in Baseline.

4. STRINGS: Use interpolation `"Hello, ${name}!"` for building strings. NEVER use `+` for string concatenation.

5. NO SEMICOLONS: Statements are separated by newlines. Semicolons are a parse error.

6. IMMUTABLE: All `let` bindings are immutable. No `var`, no mutation, no reassignment. Use recursion or higher-order functions instead of loops.

7. EXHAUSTIVE MATCH: Pattern matching must cover all cases. For sum types, every variant must have an arm. Use `_ ->` as a catch-all when needed.
</key_rules>

<common_mistakes>
Mistakes agents make most often — check your code against these:

WRONG: `!condition` or `!valid`
RIGHT: `not condition` or `not valid`

WRONG: `name.to_upper()` or `items.map(f)`
RIGHT: `String.to_upper(name)` or `List.map(items, f)`

WRONG: `"Hello, " + name + "!"`
RIGHT: `"Hello, ${name}!"`

WRONG: `let x = 1; let y = 2;`
RIGHT: `let x = 1` (newline) `let y = 2`

WRONG: `fn greet(name: String) -> String { return "Hi " + name }`
RIGHT: `fn greet(name: String) -> String = "Hi, ${name}"`

WRONG: `if x != null` or `if x is not None`
RIGHT: `match x { Some(v) -> use(v), None -> fallback }`

WRONG: `try { risky() } catch { default }`
RIGHT: `match risky() { Ok(v) -> v, Err(_) -> default }`

WRONG: Writing code without checking — use baseline/check FIRST
</common_mistakes>

<syntax_quick_ref>
Functions:     fn add(a: Int, b: Int) -> Int = a + b
Effectful:     fn greet!(name: String) -> () = Console.println!("Hi ${name}")
Let binding:   let x = 42
Records:       type User = { name: String, age: Int }
Sum types:     type Status = | Active | Inactive | Error(String)
Refinements:   type Port = Int where self > 0 && self <= 65535
Lambdas:       |x| x + 1
Pipes:         items |> List.filter(|x| x > 0) |> List.map(|x| x * 2)
Match:         match value { Some(x) -> x, None -> 0 }
If/else:       if condition then a else b
Tests:         @test { test "name" = assert(expr) }
Imports:       @prelude(core) or import ModuleName
</syntax_quick_ref>

<output_formats>
baseline/check returns:
  { "status": "success"|"failure", "diagnostics": [...], "verification_level": "..." }
  Each diagnostic: { "severity": "error"|"warning"|"info", "code": "TYP_001", "message": "...", "location": { "line": 1, "col": 5 } }

baseline/test returns:
  { "summary": { "total": 3, "passed": 2, "failed": 1, "skipped": 0 }, "tests": [...] }
  Each test: { "name": "...", "status": "pass"|"fail"|"skip", "error": "..." }

baseline/docs returns:
  { "modules": [{ "name": "List", "functions": [{ "name": "map", "signature": "...", "description": "..." }] }] }

baseline/reference returns: plain text (markdown)

baseline/rosetta returns:
  [{ "concept": "...", "category": "...", "python": "...", "typescript": "...", "baseline": "...", "explanation": "..." }]
</output_formats>

<critical_rules>
ALWAYS call baseline/check before considering code complete — the compiler catches errors you will miss.
ALWAYS use baseline/reference section="key_rules" when first writing Baseline — it prevents the most common mistakes.
NEVER use `!` for boolean negation — use `not`. The `!` suffix is ONLY for effectful function names.
NEVER use method syntax (value.method()) — Baseline uses Module.function(value) exclusively.
NEVER use `+` for string concatenation — use string interpolation "${expr}".
NEVER use semicolons — they are parse errors in Baseline.
If baseline/check returns errors, read the diagnostic messages — they explain what's wrong and often suggest the fix.
If you're unsure how to express something, use baseline/rosetta to find the Baseline equivalent of a Python/TS pattern.
</critical_rules>
</baseline_instructions>"#;
