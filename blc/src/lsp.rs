//! Language Server Protocol implementation for Baseline.
//!
//! Provides diagnostics, hover, go-to-definition, completion, and
//! workspace symbol search with cross-file import resolution
//! using tower-lsp over stdio.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Mutex;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::diagnostics;
use crate::parse;
use crate::resolver::{ImportKind, ModuleLoader};

/// Info about a resolved import in the current file.
#[derive(Clone, Debug)]
struct ResolvedImportInfo {
    module_name: String,
    file_path: PathBuf,
}

/// Per-file state cached after each parse.
struct FileState {
    source: String,
    symbols: Vec<SymbolInfo>,
    imports: Vec<ResolvedImportInfo>,
    /// "Module.func" -> "(a: Int, b: Int) -> Int"
    module_methods: HashMap<String, String>,
}

/// A symbol extracted from the CST.
#[derive(Clone)]
struct SymbolInfo {
    name: String,
    kind: SymbolKind,
    type_sig: Option<String>,
    range: Range,
    selection_range: Range,
    /// Cross-file origin URI (set for imported symbols).
    defined_in: Option<Url>,
    /// Module name (e.g., "Util") for imported symbols.
    module_name: Option<String>,
}

/// The LSP backend holding a client handle and per-file state.
pub struct BaselineLanguageServer {
    client: Client,
    files: Mutex<HashMap<Url, FileState>>,
    workspace_root: Mutex<Option<PathBuf>>,
    /// Reverse dependency map: file_path -> set of URIs that import it.
    reverse_deps: Mutex<HashMap<PathBuf, HashSet<Url>>>,
    /// Guard against re-entrant analysis during dependent re-analysis.
    analyzing: Mutex<HashSet<Url>>,
}

impl BaselineLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            files: Mutex::new(HashMap::new()),
            workspace_root: Mutex::new(None),
            reverse_deps: Mutex::new(HashMap::new()),
            analyzing: Mutex::new(HashSet::new()),
        }
    }

    /// Derive base directory for import resolution.
    fn base_dir_for_uri(&self, uri: &Url) -> Option<PathBuf> {
        // Prefer workspace root, fall back to file's parent directory
        let ws = self.workspace_root.lock().unwrap();
        if let Some(root) = ws.as_ref() {
            return Some(root.clone());
        }
        uri.to_file_path()
            .ok()
            .and_then(|p| p.parent().map(|d| d.to_path_buf()))
    }

    /// Collect symbols from a module's exports based on the import kind.
    fn collect_import_symbols(
        kind: &ImportKind,
        exports: &crate::resolver::ModuleExports,
        mod_name: &str,
        target_uri: &Option<Url>,
        symbols: &mut Vec<SymbolInfo>,
    ) {
        let filter: Option<&Vec<String>> = match kind {
            ImportKind::Selective(names) => Some(names),
            ImportKind::Wildcard => None,
            ImportKind::Qualified => return, // No direct symbols injected
        };
        for (func_name, sig) in &exports.functions {
            if let Some(names) = filter
                && !names.contains(func_name)
            {
                continue;
            }
            symbols.push(SymbolInfo {
                name: func_name.clone(),
                kind: SymbolKind::FUNCTION,
                type_sig: sig.clone(),
                range: Range::default(),
                selection_range: Range::default(),
                defined_in: target_uri.clone(),
                module_name: Some(mod_name.to_string()),
            });
        }
    }

    /// Re-parse a file, publish diagnostics, and update cached symbols.
    async fn on_change(&self, uri: Url, text: String) {
        self.analyze_file(uri.clone(), text).await;

        // Re-analyze dependents (one level only — the analyzing guard prevents cycles)
        let file_path = uri.to_file_path().ok();
        if let Some(path) = file_path {
            let dependents = self.collect_dependents(&path);
            for (dep_uri, dep_source) in dependents {
                self.analyze_file(dep_uri, dep_source).await;
            }
        }
    }

    /// Core analysis: parse, resolve imports, publish diagnostics, update state.
    async fn analyze_file(&self, uri: Url, text: String) {
        // Guard against re-entrant analysis
        {
            let mut analyzing = self.analyzing.lock().unwrap();
            if analyzing.contains(&uri) {
                return;
            }
            analyzing.insert(uri.clone());
        }

        let file_path = uri.to_file_path().ok();

        // Run cross-file analysis if we have a file path, otherwise fall back
        let result = match &file_path {
            Some(path) => parse::parse_source_with_path(&text, path),
            None => {
                let file_name = uri.path().to_string();
                parse::parse_source(&text, &file_name)
            }
        };

        let lsp_diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> =
            result.diagnostics.iter().map(convert_diagnostic).collect();

        self.client
            .publish_diagnostics(uri.clone(), lsp_diagnostics, None)
            .await;

        // Extract local symbols
        let mut symbols = extract_symbols(&text);

        // Resolve imports and build module_methods map
        let mut imports = Vec::new();
        let mut module_methods = HashMap::new();

        if let Some(base_dir) = self.base_dir_for_uri(&uri) {
            let file_name = uri.path().to_string();
            let loader = ModuleLoader::with_base_dir(base_dir);

            // Parse and resolve imports from the source
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&tree_sitter_baseline::LANGUAGE.into())
                .expect("Failed to load Baseline grammar");

            if let Some(tree) = parser.parse(&text, None) {
                let root = tree.root_node();
                let parsed_imports = ModuleLoader::parse_imports(&root, &text);

                for (mut resolved, import_node) in parsed_imports {
                    let Ok(path) =
                        loader.resolve_path(&resolved.module_name, &import_node, &file_name)
                    else {
                        continue;
                    };
                    resolved.file_path = path.clone();

                    // Load and extract exports from the resolved module
                    let mut mod_loader =
                        ModuleLoader::with_base_dir(path.parent().unwrap_or(&path).to_path_buf());
                    if let Ok(idx) = mod_loader.load_module(&path, &import_node, &file_name)
                        && let Some((mod_root, mod_source, _)) = mod_loader.get_module(idx)
                    {
                        let exports = ModuleLoader::extract_exports(&mod_root, mod_source);
                        let mod_name = &resolved.module_name;

                        // Build module_methods entries
                        for (func_name, sig) in &exports.functions {
                            let key = format!("{}.{}", mod_name, func_name);
                            let sig_str = sig.clone().unwrap_or_else(|| "()".to_string());
                            module_methods.insert(key, sig_str);
                        }

                        // Add imported symbols based on import kind
                        let target_uri = Url::from_file_path(&path).ok();
                        Self::collect_import_symbols(
                            &resolved.kind,
                            &exports,
                            mod_name,
                            &target_uri,
                            &mut symbols,
                        );
                    }

                    imports.push(ResolvedImportInfo {
                        module_name: resolved.module_name,
                        file_path: path,
                    });
                }
            }
        }

        // Update reverse dependency map
        {
            let mut rev_deps = self.reverse_deps.lock().unwrap();
            // Remove old reverse deps for this URI
            for deps in rev_deps.values_mut() {
                deps.remove(&uri);
            }
            // Add new reverse deps
            for imp in &imports {
                rev_deps
                    .entry(imp.file_path.clone())
                    .or_default()
                    .insert(uri.clone());
            }
        }

        // Store file state
        {
            let mut files = self.files.lock().unwrap();
            files.insert(
                uri.clone(),
                FileState {
                    source: text,
                    symbols,
                    imports,
                    module_methods,
                },
            );
        }

        // Remove from analyzing set
        {
            let mut analyzing = self.analyzing.lock().unwrap();
            analyzing.remove(&uri);
        }
    }

    /// Collect files that import a changed module (for re-analysis).
    fn collect_dependents(&self, changed_path: &PathBuf) -> Vec<(Url, String)> {
        let canonical = changed_path
            .canonicalize()
            .unwrap_or_else(|_| changed_path.clone());

        let rev_deps = self.reverse_deps.lock().unwrap();
        let files = self.files.lock().unwrap();

        let mut deps = Vec::new();
        // Check both canonical and original path
        for check_path in [&canonical, changed_path] {
            if let Some(uris) = rev_deps.get(check_path) {
                for dep_uri in uris {
                    if let Some(state) = files.get(dep_uri) {
                        deps.push((dep_uri.clone(), state.source.clone()));
                    }
                }
            }
        }
        deps
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for BaselineLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract workspace root
        let root = params
            .root_uri
            .as_ref()
            .and_then(|u| u.to_file_path().ok())
            .or_else(|| {
                params
                    .workspace_folders
                    .as_ref()
                    .and_then(|folders| folders.first())
                    .and_then(|f| f.uri.to_file_path().ok())
            });

        if let Some(root) = root {
            *self.workspace_root.lock().unwrap() = Some(root);
        }

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
                workspace_symbol_provider: Some(OneOf::Left(true)),
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

        // Check for Module.method pattern
        if let Some((module, method)) = detect_qualified_access(&state.source, &pos) {
            let key = format!("{}.{}", module, method);
            if let Some(sig) = state.module_methods.get(&key) {
                let contents = format!(
                    "**{}.{}**: `{}`\n\n*from module {}*",
                    module, method, sig, module
                );
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: contents,
                    }),
                    range: None,
                }));
            }
        }

        // Find a symbol whose range contains the cursor position
        for sym in &state.symbols {
            if contains_position(&sym.range, &pos) {
                let mut contents = match &sym.type_sig {
                    Some(sig) => format!("**{}**: `{}`", sym.name, sig),
                    None => format!("**{}**", sym.name),
                };
                if let Some(mod_name) = &sym.module_name {
                    contents.push_str(&format!("\n\n*from module {}*", mod_name));
                }
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

        // Check for Module.method pattern
        if let Some((module, method)) = detect_qualified_access(&state.source, &pos) {
            // Find the import that matches this module
            for imp in &state.imports {
                if imp.module_name == module {
                    let target_uri = match Url::from_file_path(&imp.file_path) {
                        Ok(u) => u,
                        Err(_) => continue,
                    };

                    // Look up the method in the target file's symbols
                    if let Some(target_state) = files.get(&target_uri) {
                        for sym in &target_state.symbols {
                            if sym.name == method {
                                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: target_uri,
                                    range: sym.selection_range,
                                })));
                            }
                        }
                    }

                    // Fallback: jump to the file start
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: target_uri,
                        range: Range::default(),
                    })));
                }
            }
        }

        // Find the word at cursor position
        let word = word_at_position(&state.source, &pos);
        if word.is_empty() {
            return Ok(None);
        }

        // Find a symbol matching that word (check imported symbols with defined_in first)
        for sym in &state.symbols {
            if sym.name == word {
                if let Some(def_uri) = &sym.defined_in {
                    // Cross-file: look up definition in target file
                    if let Some(target_state) = files.get(def_uri) {
                        for target_sym in &target_state.symbols {
                            if target_sym.name == word && target_sym.defined_in.is_none() {
                                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: def_uri.clone(),
                                    range: target_sym.selection_range,
                                })));
                            }
                        }
                    }
                    // Fallback: jump to file start
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: def_uri.clone(),
                        range: Range::default(),
                    })));
                }
                // Local symbol
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
        let pos = params.text_document_position.position;

        let files = self.files.lock().unwrap();
        let Some(state) = files.get(&uri) else {
            return Ok(None);
        };

        // Check for Module. prefix — offer module method completions
        if let Some(module_prefix) = detect_module_prefix(&state.source, &pos) {
            let prefix = format!("{}.", module_prefix);
            let items: Vec<CompletionItem> = state
                .module_methods
                .iter()
                .filter(|(key, _)| key.starts_with(&prefix))
                .map(|(key, sig)| {
                    let method_name = key.strip_prefix(&prefix).unwrap_or(key);
                    CompletionItem {
                        label: method_name.to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(sig.clone()),
                        ..Default::default()
                    }
                })
                .collect();

            if !items.is_empty() {
                return Ok(Some(CompletionResponse::Array(items)));
            }
        }

        // Fall through to local symbol completions
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

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();
        let files = self.files.lock().unwrap();

        let mut results = Vec::new();
        for (file_uri, state) in files.iter() {
            for sym in &state.symbols {
                // Skip imported symbols to avoid duplicates
                if sym.defined_in.is_some() {
                    continue;
                }
                // Match against query (empty query = show all)
                if query.is_empty() || sym.name.to_lowercase().contains(&query) {
                    #[allow(deprecated)]
                    results.push(SymbolInformation {
                        name: sym.name.clone(),
                        kind: sym.kind,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: file_uri.clone(),
                            range: sym.selection_range,
                        },
                        container_name: None,
                    });
                }
            }
        }

        Ok(Some(results))
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

                    // Build type signature from params and return type
                    let type_sig = {
                        let params = child
                            .child_by_field_name("params")
                            .and_then(|p| p.utf8_text(source.as_bytes()).ok())
                            .unwrap_or("");
                        let ret = child
                            .child_by_field_name("return_type")
                            .and_then(|r| r.utf8_text(source.as_bytes()).ok())
                            .unwrap_or("()");
                        Some(format!("({}) -> {}", params, ret))
                    };

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
                            start: Position::new(name_start.row as u32, name_start.column as u32),
                            end: Position::new(name_end.row as u32, name_end.column as u32),
                        },
                        defined_in: None,
                        module_name: None,
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
                            start: Position::new(name_start.row as u32, name_start.column as u32),
                            end: Position::new(name_end.row as u32, name_end.column as u32),
                        },
                        defined_in: None,
                        module_name: None,
                    });
                }
            }
            _ => {}
        }
    }

    symbols
}

/// Detect if cursor is at a `Module.method` position. Returns `(module, method)`.
fn detect_qualified_access(source: &str, pos: &Position) -> Option<(String, String)> {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = pos.line as usize;
    if line_idx >= lines.len() {
        return None;
    }
    let line = lines[line_idx];
    let col = pos.character as usize;
    if col >= line.len() {
        return None;
    }

    let bytes = line.as_bytes();

    // Find the word at cursor
    let mut end = col;
    while end < bytes.len() && is_ident_char(bytes[end]) {
        end += 1;
    }
    let mut start = col;
    while start > 0 && is_ident_char(bytes[start - 1]) {
        start -= 1;
    }

    // Check if there's a dot before the word
    if start == 0 {
        return None;
    }
    let before = start - 1;
    if bytes[before] != b'.' {
        return None;
    }

    // Find the module name before the dot
    let mod_end = before;
    let mut mod_start = before;
    while mod_start > 0 && is_ident_char(bytes[mod_start - 1]) {
        mod_start -= 1;
    }
    if mod_start == mod_end {
        return None;
    }

    let module = &line[mod_start..mod_end];
    let method = &line[start..end];

    // Module names start with uppercase
    if !module.starts_with(|c: char| c.is_ascii_uppercase()) {
        return None;
    }

    if method.is_empty() {
        return None;
    }

    Some((module.to_string(), method.to_string()))
}

/// Detect if cursor is right after `Module.` (for completion).
/// Returns the module name if so.
fn detect_module_prefix(source: &str, pos: &Position) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = pos.line as usize;
    if line_idx >= lines.len() {
        return None;
    }
    let line = lines[line_idx];
    let col = pos.character as usize;

    // Cursor might be right after the dot or partway through typing
    // Look backwards from cursor position
    if col == 0 {
        return None;
    }

    let bytes = line.as_bytes();
    let check_col = col.min(bytes.len());

    // Walk backwards to find a dot
    let mut dot_pos = None;
    let mut i = check_col;
    // Skip any partial identifier being typed
    while i > 0 && is_ident_char(bytes[i - 1]) {
        i -= 1;
    }
    // Check if there's a dot
    if i > 0 && bytes[i - 1] == b'.' {
        dot_pos = Some(i - 1);
    }
    // Also handle cursor right after dot
    if dot_pos.is_none() && check_col > 0 && bytes[check_col - 1] == b'.' {
        dot_pos = Some(check_col - 1);
    }

    let dot_pos = dot_pos?;

    // Find the module name before the dot
    let mut mod_start = dot_pos;
    while mod_start > 0 && is_ident_char(bytes[mod_start - 1]) {
        mod_start -= 1;
    }

    if mod_start == dot_pos {
        return None;
    }

    let module = &line[mod_start..dot_pos];

    // Module names start with uppercase
    if !module.starts_with(|c: char| c.is_ascii_uppercase()) {
        return None;
    }

    Some(module.to_string())
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
        assert_eq!(
            lsp_d.code,
            Some(NumberOrString::String("TYP_001".to_string()))
        );
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
fn greet(name: String) -> String = "Hello"
"#;
        let symbols = extract_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "greet");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(
            symbols[0].type_sig.as_deref(),
            Some("(name: String) -> String")
        );
        assert!(symbols[0].defined_in.is_none());
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

    #[test]
    fn test_detect_qualified_access() {
        // Cursor on "add" in "Util.add(3, 7)"
        let source = "  let result = Util.add(3, 7)";
        let result = detect_qualified_access(source, &Position::new(0, 20));
        assert_eq!(result, Some(("Util".to_string(), "add".to_string())));

        // Cursor on "println!" in "Console.println!(...)"
        let source = r#"  Console.println!("hello")"#;
        let result = detect_qualified_access(source, &Position::new(0, 12));
        assert_eq!(
            result,
            Some(("Console".to_string(), "println!".to_string()))
        );

        // Not a qualified access (lowercase prefix)
        let source = "  foo.bar";
        let result = detect_qualified_access(source, &Position::new(0, 6));
        assert_eq!(result, None);

        // Just a plain word
        let source = "  let x = 42";
        let result = detect_qualified_access(source, &Position::new(0, 6));
        assert_eq!(result, None);
    }

    #[test]
    fn test_detect_module_prefix() {
        // Cursor right after "Util."
        let source = "  Util.";
        let result = detect_module_prefix(source, &Position::new(0, 7));
        assert_eq!(result, Some("Util".to_string()));

        // Cursor partway through typing after dot
        let source = "  Util.ad";
        let result = detect_module_prefix(source, &Position::new(0, 9));
        assert_eq!(result, Some("Util".to_string()));

        // Not a module prefix (lowercase)
        let source = "  foo.";
        let result = detect_module_prefix(source, &Position::new(0, 6));
        assert_eq!(result, None);

        // No dot
        let source = "  Util";
        let result = detect_module_prefix(source, &Position::new(0, 6));
        assert_eq!(result, None);
    }
}
