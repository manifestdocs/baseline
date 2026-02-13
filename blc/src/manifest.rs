//! Package manifest (`baseline.toml`) support.
//!
//! Provides:
//! - `Manifest` struct with project metadata
//! - `load_manifest()` to find and parse `baseline.toml`
//! - `create_manifest()` to generate a new manifest
//! - `hello_world_source()` for the default `src/main.bl`

use std::path::{Path, PathBuf};

/// Parsed baseline.toml manifest.
#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: String,
    pub version: String,
    pub prelude: String,
    pub entry: String,
}

impl Default for Manifest {
    fn default() -> Self {
        Self {
            name: "my-app".to_string(),
            version: "0.1.0".to_string(),
            prelude: "script".to_string(),
            entry: "src/main.bl".to_string(),
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

/// Simple line-based TOML parser for the [package] section.
///
/// Handles the minimal subset we need: key = "value" pairs.
/// No dependency on the `toml` crate.
fn parse_manifest(content: &str) -> Manifest {
    let mut manifest = Manifest::default();
    let mut in_package = false;

    for line in content.lines() {
        let trimmed = line.trim();

        // Section headers
        if trimmed.starts_with('[') {
            in_package = trimmed == "[package]";
            continue;
        }

        if !in_package {
            continue;
        }

        // Skip comments and empty lines
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // Parse key = "value"
        if let Some((key, value)) = parse_key_value(trimmed) {
            match key {
                "name" => manifest.name = value,
                "version" => manifest.version = value,
                "prelude" => manifest.prelude = value,
                "entry" => manifest.entry = value,
                _ => {} // Ignore unknown keys
            }
        }
    }

    manifest
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
    fn parse_ignores_other_sections() {
        let content = r#"
[package]
name = "my-app"

[dependencies]
name = "should-be-ignored"
"#;
        let m = parse_manifest(content);
        assert_eq!(m.name, "my-app");
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
}
