# AI Instructions for Baseline

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
