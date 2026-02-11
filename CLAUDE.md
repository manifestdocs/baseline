# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Baseline is a strongly typed, effect-based programming language currently in **v0.1 Bootstrap Phase**.

**Current Status:**
- **Compiler:** `blc` (Baseline Compiler) implemented in Rust.
- **Parser:** `tree-sitter-baseline` fully functional.
- **Version:** v0.1 (Bootstrap), focusing on core grammar and type/effect checking.

**Design Philosophy:**
- "The type is the spec" - Types encode correctness using refinements.
- "Effects are data" - Side effects are explicit capabilities.
- "LLM-native" - Designed for machine generation with unambiguous syntax.

## Repository Structure

```
baseline/
├── blc/                    # Self-contained compiler (Rust)
│   ├── src/analysis/       # Type, Effect, and Refinement checkers
│   └── src/parse.rs        # Tree-sitter integration
├── design/                 # Language specification and design docs
│   └── baseline-language-specification.md  # Authoritative spec (with status markers)
├── tree-sitter-baseline/   # Grammar definition and parser
├── examples/               # Feature demonstrations
├── extensions/             # Editor support (Zed, VS Code)
└── CLAUDE.md
```

## Implemented Features (v0.1)

### Syntax & Grammar
- **Functions:** `fn name(params) -> Type = body`
- **Tuples:** `(1, "a")`, Unit `()`
- **Records:** `{ x: Int, y: Int }`, `User { id: 1 }`, `obj.prop`
- **Control Flow:** `if/else`, `match` (pattern matching), `1..10` (ranges)
- **Modules:** `@prelude(core)`, `@module Name`
- **Pipes:** `x |> f`

### Type System
- **Core Types:** `Int`, `String`, `Boolean`, `List<T>`
- **Refinements:** Integer Intervals (`type Port = Int where self > 0`)
    - *Constraint:* Regex and custom predicates deferred to v0.2.
- **Effects:** Built-in (`Console`, `Http`, `Fs`, `Log`, `Time`, `Random`, `Env`) + user-defined effect declarations
    - Effect handlers exist (grammar + interpreter) but type checking is partial.

## Developer Workflow

### Build & Test
1. **Regenerate Grammar:**
   ```bash
   cd tree-sitter-baseline && npx tree-sitter generate && cargo build
   ```
2. **Build Compiler:**
   ```bash
   cargo build --bin blc
   ```
3. **Run Checks:**
   ```bash
   blc check examples/hello.bl --json
   ```

### Coding Guidelines
- **Rust (Compiler):** Follow standard Rust idioms. Use `tree-sitter` for parsing.
- **Grammar:** Resolve all conflicts in `grammar.js`. Prefer explicit precedence.
- **Constraints:** Refer to `design/baseline-language-specification.md` for feature status (`[IMPLEMENTED]`/`[PARTIAL]`/`[PLANNED]` markers).

## Future Work (v0.2+)

- Full Standard Library (Http, Json, Async)
- Custom Effect Handlers
- Regex/String Refinements
- Region-based Memory Management (currently using `Arc<T>`)
