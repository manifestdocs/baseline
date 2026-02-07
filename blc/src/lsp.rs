//! Language Server Protocol implementation for Baseline.
//!
//! Provides diagnostics, hover, go-to-definition, and completion
//! using tower-lsp over stdio.

use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::diagnostics;
use crate::parse;

/// Per-file state cached after each parse.
struct FileState {
    source: String,
    symbols: Vec<SymbolInfo>,
}

/// A symbol extracted from the CST.
#[derive(Clone)]
struct SymbolInfo {
    name: String,
    kind: SymbolKind,
    type_sig: Option<String>,
    range: Range,
    selection_range: Range,
}

/// The LSP backend holding a client handle and per-file state.
pub struct BaselineLanguageServer {
    client: Client,
    files: Mutex<HashMap<Url, FileState>>,
}

impl BaselineLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            files: Mutex::new(HashMap::new()),
        }
    }

    /// Re-parse a file, publish diagnostics, and update cached symbols.
    async fn on_change(&self, uri: Url, text: String) {
        let file_name = uri.path().to_string();
        let result = parse::parse_source(&text, &file_name);

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = result
            .diagnostics
            .iter()
            .map(convert_diagnostic)
            .collect();

        self.client
            .publish_diagnostics(uri.clone(), lsp_diagnostics, None)
            .await;

        let symbols = extract_symbols(&text);

        let mut files = self.files.lock().unwrap();
        files.insert(
            uri,
            FileState {
                source: text,
                symbols,
            },
        );
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for BaselineLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "baseline-ls".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Baseline language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.on_change(params.text_document.uri, change.text).await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let files = self.files.lock().unwrap();
        let Some(state) = files.get(&uri) else {
            return Ok(None);
        };

        // Find a symbol whose range contains the cursor position
        for sym in &state.symbols {
            if contains_position(&sym.range, &pos) {
                let contents = match &sym.type_sig {
                    Some(sig) => format!("**{}**: `{}`", sym.name, sig),
                    None => format!("**{}**", sym.name),
                };
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: contents,
                    }),
                    range: Some(sym.selection_range),
                }));
            }
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let files = self.files.lock().unwrap();
        let Some(state) = files.get(&uri) else {
            return Ok(None);
        };

        // Find the word at cursor position
        let word = word_at_position(&state.source, &pos);
        if word.is_empty() {
            return Ok(None);
        }

        // Find a symbol matching that word
        for sym in &state.symbols {
            if sym.name == word {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: sym.selection_range,
                })));
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;

        let files = self.files.lock().unwrap();
        let Some(state) = files.get(&uri) else {
            return Ok(None);
        };

        let items: Vec<CompletionItem> = state
            .symbols
            .iter()
            .map(|sym| CompletionItem {
                label: sym.name.clone(),
                kind: Some(match sym.kind {
                    SymbolKind::FUNCTION => CompletionItemKind::FUNCTION,
                    SymbolKind::STRUCT => CompletionItemKind::STRUCT,
                    SymbolKind::ENUM => CompletionItemKind::ENUM,
                    _ => CompletionItemKind::VARIABLE,
                }),
                detail: sym.type_sig.clone(),
                ..Default::default()
            })
            .collect();

        Ok(Some(CompletionResponse::Array(items)))
    }
}

/// Convert a Baseline diagnostic to an LSP diagnostic.
pub fn convert_diagnostic(d: &diagnostics::Diagnostic) -> tower_lsp::lsp_types::Diagnostic {
    let severity = match d.severity {
        diagnostics::Severity::Error => Some(DiagnosticSeverity::ERROR),
        diagnostics::Severity::Warning => Some(DiagnosticSeverity::WARNING),
        diagnostics::Severity::Info => Some(DiagnosticSeverity::INFORMATION),
    };

    let start_line = if d.location.line > 0 {
        d.location.line - 1
    } else {
        0
    };
    let start_col = if d.location.col > 0 {
        d.location.col - 1
    } else {
        0
    };
    let end_line = d.location.end_line.unwrap_or(d.location.line);
    let end_line = if end_line > 0 { end_line - 1 } else { 0 };
    let end_col = d.location.end_col.unwrap_or(d.location.col);
    let end_col = if end_col > 0 { end_col - 1 } else { 0 };

    tower_lsp::lsp_types::Diagnostic {
        range: Range {
            start: Position::new(start_line as u32, start_col as u32),
            end: Position::new(end_line as u32, end_col as u32),
        },
        severity,
        code: Some(NumberOrString::String(d.code.clone())),
        source: Some("baseline".to_string()),
        message: d.message.clone(),
        ..Default::default()
    }
}

/// Extract symbols from Baseline source code by walking the CST.
fn extract_symbols(source: &str) -> Vec<SymbolInfo> {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load Baseline grammar");

    let Some(tree) = parser.parse(source, None) else {
        return Vec::new();
    };

    let root = tree.root_node();
    let mut symbols = Vec::new();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        match child.kind() {
            "function_def" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = name_node
                        .utf8_text(source.as_bytes())
                        .unwrap_or("")
                        .to_string();

                    // Extract type signature
                    let type_sig = child
                        .child_by_field_name("signature")
                        .and_then(|sig| sig.utf8_text(source.as_bytes()).ok())
                        .map(|s| s.to_string());

                    let start = child.start_position();
                    let end = child.end_position();
                    let name_start = name_node.start_position();
                    let name_end = name_node.end_position();

                    symbols.push(SymbolInfo {
                        name,
                        kind: SymbolKind::FUNCTION,
                        type_sig,
                        range: Range {
                            start: Position::new(start.row as u32, start.column as u32),
                            end: Position::new(end.row as u32, end.column as u32),
                        },
                        selection_range: Range {
                            start: Position::new(
                                name_start.row as u32,
                                name_start.column as u32,
                            ),
                            end: Position::new(name_end.row as u32, name_end.column as u32),
                        },
                    });
                }
            }
            "type_def" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = name_node
                        .utf8_text(source.as_bytes())
                        .unwrap_or("")
                        .to_string();

                    // Determine if struct or enum
                    let kind = child
                        .child_by_field_name("def")
                        .map(|def| {
                            if def.kind() == "variant_list" {
                                SymbolKind::ENUM
                            } else {
                                SymbolKind::STRUCT
                            }
                        })
                        .unwrap_or(SymbolKind::STRUCT);

                    let start = child.start_position();
                    let end = child.end_position();
                    let name_start = name_node.start_position();
                    let name_end = name_node.end_position();

                    symbols.push(SymbolInfo {
                        name,
                        kind,
                        type_sig: None,
                        range: Range {
                            start: Position::new(start.row as u32, start.column as u32),
                            end: Position::new(end.row as u32, end.column as u32),
                        },
                        selection_range: Range {
                            start: Position::new(
                                name_start.row as u32,
                                name_start.column as u32,
                            ),
                            end: Position::new(name_end.row as u32, name_end.column as u32),
                        },
                    });
                }
            }
            _ => {}
        }
    }

    symbols
}

/// Check if a range contains a position.
fn contains_position(range: &Range, pos: &Position) -> bool {
    if pos.line < range.start.line || pos.line > range.end.line {
        return false;
    }
    if pos.line == range.start.line && pos.character < range.start.character {
        return false;
    }
    if pos.line == range.end.line && pos.character > range.end.character {
        return false;
    }
    true
}

/// Extract the word at a given position in source text.
fn word_at_position(source: &str, pos: &Position) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = pos.line as usize;
    if line_idx >= lines.len() {
        return String::new();
    }
    let line = lines[line_idx];
    let col = pos.character as usize;
    if col >= line.len() {
        return String::new();
    }

    let bytes = line.as_bytes();
    let mut start = col;
    let mut end = col;

    while start > 0 && is_ident_char(bytes[start - 1]) {
        start -= 1;
    }
    while end < bytes.len() && is_ident_char(bytes[end]) {
        end += 1;
    }

    line[start..end].to_string()
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'!'
}

/// Start the LSP server on stdin/stdout.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(BaselineLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_diagnostic_error() {
        let d = diagnostics::Diagnostic {
            code: "TYP_001".to_string(),
            severity: diagnostics::Severity::Error,
            location: diagnostics::Location {
                file: "test.bl".to_string(),
                line: 5,
                col: 3,
                end_line: Some(5),
                end_col: Some(10),
            },
            message: "Type mismatch".to_string(),
            context: "".to_string(),
            suggestions: vec![],
        };

        let lsp_d = convert_diagnostic(&d);
        assert_eq!(lsp_d.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(lsp_d.range.start.line, 4); // 0-indexed
        assert_eq!(lsp_d.range.start.character, 2);
        assert_eq!(lsp_d.range.end.line, 4);
        assert_eq!(lsp_d.range.end.character, 9);
        assert_eq!(lsp_d.code, Some(NumberOrString::String("TYP_001".to_string())));
        assert_eq!(lsp_d.message, "Type mismatch");
    }

    #[test]
    fn test_convert_diagnostic_warning() {
        let d = diagnostics::Diagnostic {
            code: "STY_001".to_string(),
            severity: diagnostics::Severity::Warning,
            location: diagnostics::Location {
                file: "test.bl".to_string(),
                line: 1,
                col: 1,
                end_line: None,
                end_col: None,
            },
            message: "style warning".to_string(),
            context: "".to_string(),
            suggestions: vec![],
        };

        let lsp_d = convert_diagnostic(&d);
        assert_eq!(lsp_d.severity, Some(DiagnosticSeverity::WARNING));
        assert_eq!(lsp_d.range.start.line, 0);
        assert_eq!(lsp_d.range.start.character, 0);
    }

    #[test]
    fn test_extract_symbols_function() {
        let source = r#"
greet : String -> String
greet = |name| "Hello"
"#;
        let symbols = extract_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "greet");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(
            symbols[0].type_sig.as_deref(),
            Some("String -> String")
        );
    }

    #[test]
    fn test_extract_symbols_type_def() {
        let source = r#"
type Color = | Red | Green | Blue
"#;
        let symbols = extract_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);
    }

    #[test]
    fn test_extract_symbols_struct() {
        let source = r#"
type Point = { x: Int, y: Int }
"#;
        let symbols = extract_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::STRUCT);
    }

    #[test]
    fn test_word_at_position() {
        let source = "let foo = bar + baz";
        let word = word_at_position(source, &Position::new(0, 4));
        assert_eq!(word, "foo");

        let word = word_at_position(source, &Position::new(0, 10));
        assert_eq!(word, "bar");
    }

    #[test]
    fn test_contains_position() {
        let range = Range {
            start: Position::new(2, 0),
            end: Position::new(4, 10),
        };
        assert!(contains_position(&range, &Position::new(3, 5)));
        assert!(!contains_position(&range, &Position::new(1, 5)));
        assert!(!contains_position(&range, &Position::new(5, 0)));
    }
}
