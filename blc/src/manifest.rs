//! Package manifest (`baseline.toml`) support.
//!
//! Provides:
//! - `Manifest` struct with project metadata
//! - `load_manifest()` to find and parse `baseline.toml`
//! - `create_manifest()` to generate a new manifest
//! - `hello_world_source()` for the default `src/main.bl`

use std::path::{Path, PathBuf};

/// A declared dependency in `baseline.toml`.
#[derive(Debug, Clone, PartialEq)]
pub enum Dependency {
    /// URL dependency: `Name = { url = "...", hash = "sha256-..." }`
    Url { url: String, hash: String },
    /// Path dependency: `Name = { path = "../local/pkg" }`
    Path { path: String },
}

/// Parsed baseline.toml manifest.
#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: String,
    pub version: String,
    pub prelude: String,
    pub entry: String,
    pub dependencies: Vec<(String, Dependency)>,
}

impl Default for Manifest {
    fn default() -> Self {
        Self {
            name: "my-app".to_string(),
            version: "0.1.0".to_string(),
            prelude: "script".to_string(),
            entry: "src/main.bl".to_string(),
            dependencies: Vec::new(),
        }
    }
}

/// Search for `baseline.toml` starting from `dir` and walking up.
///
/// Returns the parsed manifest and the directory containing it.
pub fn load_manifest(dir: &Path) -> Option<(Manifest, PathBuf)> {
    let mut current = dir.to_path_buf();
    loop {
        let candidate = current.join("baseline.toml");
        if candidate.is_file()
            && let Ok(content) = std::fs::read_to_string(&candidate)
        {
            let manifest = parse_manifest(&content);
            return Some((manifest, current));
        }
        if !current.pop() {
            break;
        }
    }
    None
}

/// Simple line-based TOML parser for [package] and [dependencies] sections.
///
/// Handles the minimal subset we need: key = "value" pairs and inline tables.
/// No dependency on the `toml` crate.
fn parse_manifest(content: &str) -> Manifest {
    let mut manifest = Manifest::default();
    let mut section = Section::None;

    for line in content.lines() {
        let trimmed = line.trim();

        // Section headers
        if trimmed.starts_with('[') {
            section = match trimmed {
                "[package]" => Section::Package,
                "[dependencies]" => Section::Dependencies,
                _ => Section::None,
            };
            continue;
        }

        // Skip comments and empty lines
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        match section {
            Section::Package => {
                if let Some((key, value)) = parse_key_value(trimmed) {
                    match key {
                        "name" => manifest.name = value,
                        "version" => manifest.version = value,
                        "prelude" => manifest.prelude = value,
                        "entry" => manifest.entry = value,
                        _ => {}
                    }
                }
            }
            Section::Dependencies => {
                if let Some((name, dep)) = parse_dependency_line(trimmed) {
                    manifest.dependencies.push((name.to_string(), dep));
                }
            }
            Section::None => {}
        }
    }

    manifest
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Section {
    None,
    Package,
    Dependencies,
}

/// Parse a dependency line like:
/// `Json = { url = "https://...", hash = "sha256-abc" }`
/// `Validation = { path = "../shared/validation" }`
fn parse_dependency_line(line: &str) -> Option<(&str, Dependency)> {
    let (name, rest) = line.split_once('=')?;
    let name = name.trim();
    let rest = rest.trim();

    // Must be an inline table: { ... }
    let inner = rest.strip_prefix('{')?.strip_suffix('}')?.trim();

    let mut url = None;
    let mut hash = None;
    let mut path = None;

    for field in inner.split(',') {
        let field = field.trim();
        if let Some((k, v)) = parse_key_value(field) {
            match k {
                "url" => url = Some(v),
                "hash" => hash = Some(v),
                "path" => path = Some(v),
                _ => {}
            }
        }
    }

    if let Some(path_val) = path {
        Some((name, Dependency::Path { path: path_val }))
    } else if let (Some(url_val), Some(hash_val)) = (url, hash) {
        Some((
            name,
            Dependency::Url {
                url: url_val,
                hash: hash_val,
            },
        ))
    } else {
        None
    }
}

/// Append a dependency entry to an existing manifest file content.
/// If `[dependencies]` section doesn't exist, creates it.
pub fn append_dependency(content: &str, name: &str, dep: &Dependency) -> String {
    let dep_line = match dep {
        Dependency::Url { url, hash } => {
            format!("{} = {{ url = \"{}\", hash = \"{}\" }}", name, url, hash)
        }
        Dependency::Path { path } => {
            format!("{} = {{ path = \"{}\" }}", name, path)
        }
    };

    if content.contains("[dependencies]") {
        // Find the [dependencies] section and append after it
        let mut result = String::new();
        let mut found_section = false;
        let mut inserted = false;

        for line in content.lines() {
            result.push_str(line);
            result.push('\n');

            if line.trim() == "[dependencies]" {
                found_section = true;
            }

            // Insert after the last dep line in the section (or right after header)
            if found_section && !inserted {
                let next_is_section =
                    line.trim().starts_with('[') && line.trim() != "[dependencies]";
                if next_is_section {
                    result.push_str(&dep_line);
                    result.push('\n');
                    inserted = true;
                }
            }
        }

        if found_section && !inserted {
            result.push_str(&dep_line);
            result.push('\n');
        }

        result
    } else {
        format!("{}\n[dependencies]\n{}\n", content.trim_end(), dep_line)
    }
}

/// Parse a `key = "value"` line, stripping quotes from the value.
fn parse_key_value(line: &str) -> Option<(&str, String)> {
    let mut parts = line.splitn(2, '=');
    let key = parts.next()?.trim();
    let value_raw = parts.next()?.trim();

    // Strip surrounding quotes
    let value = if (value_raw.starts_with('"') && value_raw.ends_with('"'))
        || (value_raw.starts_with('\'') && value_raw.ends_with('\''))
    {
        value_raw[1..value_raw.len() - 1].to_string()
    } else {
        value_raw.to_string()
    };

    Some((key, value))
}

/// Generate the content of a new `baseline.toml` for a project.
pub fn create_manifest(name: &str) -> String {
    format!(
        r#"[package]
name = "{}"
version = "0.1.0"
prelude = "script"
entry = "src/main.bl"
"#,
        name
    )
}

/// Generate the content of the default `src/main.bl` hello world program.
pub fn hello_world_source() -> &'static str {
    r#"@prelude(script)

fn main() -> () = {
    Console.println!("Hello, Baseline!")
}
"#
}

/// Generate the content of the default `CLAUDE.md` agent instruction file.
pub fn claude_md_template() -> &'static str {
    include_str!("templates/CLAUDE.md")
}

/// Generate the content of the GitHub Copilot instruction file.
pub fn copilot_instructions_template() -> &'static str {
    include_str!("templates/copilot-instructions.md")
}

/// Generate the content of the Cursor rules file.
pub fn cursor_rules_template() -> &'static str {
    include_str!("templates/cursor-rules.md")
}

/// Generate the content of the generic `.ai-instructions.md` fallback file.
pub fn ai_instructions_template() -> &'static str {
    include_str!("templates/ai-instructions.md")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic_manifest() {
        let content = r#"
[package]
name = "test-app"
version = "0.2.0"
prelude = "core"
entry = "src/app.bl"
"#;
        let m = parse_manifest(content);
        assert_eq!(m.name, "test-app");
        assert_eq!(m.version, "0.2.0");
        assert_eq!(m.prelude, "core");
        assert_eq!(m.entry, "src/app.bl");
    }

    #[test]
    fn parse_manifest_defaults() {
        let content = "[package]\nname = \"minimal\"\n";
        let m = parse_manifest(content);
        assert_eq!(m.name, "minimal");
        assert_eq!(m.version, "0.1.0"); // default
        assert_eq!(m.prelude, "script"); // default
        assert_eq!(m.entry, "src/main.bl"); // default
    }

    #[test]
    fn parse_deps_not_in_package() {
        let content = r#"
[package]
name = "my-app"

[dependencies]
Json = { url = "https://example.com/json.tar.gz", hash = "sha256-abc123" }
"#;
        let m = parse_manifest(content);
        assert_eq!(m.name, "my-app");
        // Dependencies should NOT leak into package name
    }

    #[test]
    fn parse_url_dependency() {
        let content = r#"
[package]
name = "my-app"

[dependencies]
Json = { url = "https://github.com/baseline-lang/json/archive/v0.2.0.tar.gz", hash = "sha256-abc123" }
"#;
        let m = parse_manifest(content);
        assert_eq!(m.dependencies.len(), 1);
        assert_eq!(m.dependencies[0].0, "Json");
        assert_eq!(
            m.dependencies[0].1,
            Dependency::Url {
                url: "https://github.com/baseline-lang/json/archive/v0.2.0.tar.gz".into(),
                hash: "sha256-abc123".into(),
            }
        );
    }

    #[test]
    fn parse_path_dependency() {
        let content = r#"
[package]
name = "my-app"

[dependencies]
Validation = { path = "../shared/validation" }
"#;
        let m = parse_manifest(content);
        assert_eq!(m.dependencies.len(), 1);
        assert_eq!(m.dependencies[0].0, "Validation");
        assert_eq!(
            m.dependencies[0].1,
            Dependency::Path {
                path: "../shared/validation".into(),
            }
        );
    }

    #[test]
    fn parse_multiple_dependencies() {
        let content = r#"
[package]
name = "my-app"

[dependencies]
Json = { url = "https://example.com/json.tar.gz", hash = "sha256-abc" }
Validation = { path = "../shared/validation" }
"#;
        let m = parse_manifest(content);
        assert_eq!(m.dependencies.len(), 2);
        assert_eq!(m.dependencies[0].0, "Json");
        assert!(matches!(m.dependencies[0].1, Dependency::Url { .. }));
        assert_eq!(m.dependencies[1].0, "Validation");
        assert!(matches!(m.dependencies[1].1, Dependency::Path { .. }));
    }

    #[test]
    fn append_dependency_creates_section() {
        let content = "[package]\nname = \"my-app\"\n";
        let dep = Dependency::Url {
            url: "https://example.com/pkg.tar.gz".into(),
            hash: "sha256-xyz".into(),
        };
        let result = append_dependency(content, "Pkg", &dep);
        assert!(result.contains("[dependencies]"));
        assert!(
            result.contains(
                "Pkg = { url = \"https://example.com/pkg.tar.gz\", hash = \"sha256-xyz\" }"
            )
        );

        // Roundtrip: parse the result
        let m = parse_manifest(&result);
        assert_eq!(m.dependencies.len(), 1);
        assert_eq!(m.dependencies[0].0, "Pkg");
    }

    #[test]
    fn append_dependency_to_existing_section() {
        let content = r#"[package]
name = "my-app"

[dependencies]
Json = { url = "https://example.com/json.tar.gz", hash = "sha256-abc" }
"#;
        let dep = Dependency::Path {
            path: "../validation".into(),
        };
        let result = append_dependency(content, "Validation", &dep);
        let m = parse_manifest(&result);
        assert_eq!(m.dependencies.len(), 2);
    }

    #[test]
    fn create_manifest_roundtrip() {
        let content = create_manifest("roundtrip");
        let m = parse_manifest(&content);
        assert_eq!(m.name, "roundtrip");
        assert_eq!(m.version, "0.1.0");
        assert_eq!(m.prelude, "script");
        assert_eq!(m.entry, "src/main.bl");
    }

    #[test]
    fn hello_world_is_valid() {
        let source = hello_world_source();
        assert!(source.contains("@prelude(script)"));
        assert!(source.contains("fn main()"));
        assert!(source.contains("Console.println!"));
    }

    #[test]
    fn agent_templates_are_valid() {
        let claude = claude_md_template();
        assert!(claude.contains("llms.txt"));
        assert!(claude.contains("blc mcp"));
        assert!(claude.contains("`!` means EFFECTS"));

        let copilot = copilot_instructions_template();
        assert!(copilot.contains("llms.txt"));
        assert!(copilot.contains("Critical Syntax Rules"));
        assert!(copilot.contains("WRONG:"));

        let cursor = cursor_rules_template();
        assert!(cursor.contains("llms.txt"));
        assert!(cursor.contains("Critical Syntax Rules"));

        let ai = ai_instructions_template();
        assert!(ai.contains("llms.txt"));
        assert!(ai.contains("\"command\": \"blc\""));
        assert!(ai.contains("Key Syntax Rules"));
    }
}
