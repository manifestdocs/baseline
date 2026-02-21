# CLAUDE.md

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
