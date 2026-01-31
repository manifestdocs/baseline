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
├── tree-sitter-baseline/   # Grammar definition and parser
├── examples/               # v0.1 Feature demonstrations
├── extensions/             # Editor support (Zed, VS Code)
├── SPEC_V0_1_DELTA.md      # v0.1 Implementation Constraints
└── CLAUDE.md
```

## Implemented Features (v0.1)

### Syntax & Grammar
- **Functions:** ML-style `name : Sig = body`
- **Tuples:** `(1, "a")`, Unit `()`
- **Records:** `{ x: Int, y: Int }`, `User { id: 1 }`, `obj.prop`
- **Control Flow:** `if/else`, `match` (pattern matching), `1..10` (ranges)
- **Modules:** `@prelude(core)`, `@module Name`
- **Pipes:** `x |> f`

### Type System
- **Core Types:** `Int`, `String`, `Boolean`, `List<T>`
- **Refinements:** Integer Intervals (`type Port = Int where self > 0`)
    - *Constraint:* Regex and custom predicates deferred to v0.2.
- **Effects:** Built-in only (`Console`, `Http`, `Fs`, `Log`, `Time`)
    - *Constraint:* No distinct `handle` blocks in v0.1.

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
- **Constraints:** Adhere to `SPEC_V0_1_DELTA.md`. Do not implement v0.2 features (macros, advanced refinements) yet.

## Future Work (v0.2+)

- Full Standard Library (Http, Json, Async)
- Custom Effect Handlers
- Regex/String Refinements
- Region-based Memory Management (currently using `Arc<T>`)
