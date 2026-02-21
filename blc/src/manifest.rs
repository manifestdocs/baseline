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

/// Generate the content of the default `CLAUDE.md` agent instruction file.
pub fn claude_md_template() -> &'static str {
    r#"# CLAUDE.md

This file provides guidance to Claude Code when working with this Baseline project.

## Language Overview
Baseline is a strongly typed, effect-based functional programming language.
For the full language reference, common mistakes, and standard library overview,
**ALWAYS read `llms.txt` in the project root first.**

## Key Syntax Rules
- `!` means EFFECTS, not negation. Use `not` for boolean negation.
- NO method syntax. Write `String.to_upper(name)`, never `name.to_upper()`.
- NO `+` for strings. Write `"Hello, ${name}!"`.
- NO classes, `try`/`catch`, or `null`. Use records, `Result<T,E>`, and `Option<T>`.

## MCP Tools Workflow
The `blc mcp` server provides tools to close the compile-check-repair loop.
Recommended workflow:
1. **Reference**: Fetch documentation via MCP or `llms.txt`
2. **Write**: Implement the code
3. **Check**: Run the `check` tool to type-check
4. **Fix**: Address any errors returned by the compiler
5. **Test**: Run the `test` tool
"#
}

/// Generate the content of the GitHub Copilot instruction file.
pub fn copilot_instructions_template() -> &'static str {
    r#"This project uses the Baseline programming language.

## Critical Syntax Rules
- `!` means EFFECTS, not negation. Use `not` for boolean negation. `!valid` is a parse error.
- NO method syntax. Write `String.to_upper(name)`, never `name.to_upper()`.
- NO `+` for string concatenation. Use string interpolation: `"Hello, ${name}!"`.
- NO classes. Use records and sum types.
- NO `try`/`catch`. Use `Result<T, E>` and `?`.
- NO `null` or `undefined`. Use `Option<T>`.

## Common Mistakes
WRONG: `user.to_string()`   -> RIGHT: `String.from(user)`
WRONG: `!valid`             -> RIGHT: `not valid`
WRONG: `"a" + "b"`          -> RIGHT: `"${a}${b}"`
WRONG: `return x`           -> RIGHT: `x` (last expression is the return value)

For the full language specification, **ALWAYS read `llms.txt`** in the project root.
"#
}

/// Generate the content of the Cursor rules file.
pub fn cursor_rules_template() -> &'static str {
    copilot_instructions_template()
}

/// Generate the content of the generic `.ai-instructions.md` fallback file.
pub fn ai_instructions_template() -> &'static str {
    r#"# AI Instructions for Baseline

This project uses Baseline, a strongly typed, effect-based functional programming language.

## Key Syntax Rules
- `!` means EFFECTS, not negation. Use `not` for boolean negation.
- NO method syntax. Write `String.to_upper(name)`, never `name.to_upper()`.
- NO `+` for strings. Write `"Hello, ${name}!"`.
- NO classes, `try`/`catch`, or `null`. Use records, `Result<T,E>`, and `Option<T>`.

## Full Reference
For the complete language reference, standard library, and a Rosetta Stone,
**ALWAYS read `llms.txt` in the project root** before writing code.

## MCP Server
If your environment supports Model Context Protocol (MCP), configure it to use:
`{"command": "blc", "args": ["mcp"]}`
This provides tools for type-checking (`check`) and documentation lookup (`docs`).
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
        assert_eq!(copilot, cursor);

        let ai = ai_instructions_template();
        assert!(ai.contains("llms.txt"));
        assert!(ai.contains("\"command\": \"blc\""));
        assert!(ai.contains("Key Syntax Rules"));
    }
}
