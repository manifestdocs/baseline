# Large File Refactoring Plan

These four files exceed 2900 lines. This document outlines how to decompose
them into smaller, maintainable modules.

## Guiding Principles

- Each split preserves the public API via re-exports from `mod.rs`
- One conceptual concern per file (one match arm family ≈ one file)
- Shared types stay in the parent file; logic moves to sub-modules

## Proposed Splits

### `analysis/types.rs` (2952 lines)

| New Module | Contents | Approx Lines |
|---|---|---|
| `types/mod.rs` | `Type` enum, `SymbolTable`, re-exports | 400 |
| `types/check.rs` | `check_node_inner` expression checking | 800 |
| `types/patterns.rs` | Pattern matching / exhaustiveness | 300 |
| `types/definitions.rs` | Function, type, effect, struct defs | 500 |
| `types/imports.rs` | Import resolution & spec checking | 400 |
| `types/diagnostics.rs` | Style checks (STY_001 etc.) | 200 |

### `interpreter.rs` (3362 lines)

| New Module | Contents | Approx Lines |
|---|---|---|
| `interpreter/mod.rs` | `Context`, `RuntimeValue`, re-exports | 300 |
| `interpreter/eval.rs` | Core `eval` dispatch | 800 |
| `interpreter/builtins.rs` | Built-in function handling | 400 |
| `interpreter/patterns.rs` | Pattern matching evaluation | 300 |
| `interpreter/effects.rs` | Effect handler evaluation | 300 |
| `interpreter/imports.rs` | Module evaluation & injection | 200 |

### `vm/compiler.rs` (3613 lines)

| New Module | Contents | Approx Lines |
|---|---|---|
| `compiler/mod.rs` | `Compiler` struct, scope/variable mgmt | 500 |
| `compiler/expressions.rs` | `compile_expression` dispatch | 800 |
| `compiler/control_flow.rs` | if/else, match, loops | 500 |
| `compiler/functions.rs` | Function calls, closures, TCO | 400 |
| `compiler/data.rs` | Records, lists, tuples, constructors | 400 |

### `vm/vm.rs` (3580 lines)

| New Module | Contents | Approx Lines |
|---|---|---|
| `vm/runtime/mod.rs` | `Vm` struct, call stack, entry points | 400 |
| `vm/runtime/dispatch.rs` | Main `run` opcode dispatch | 600 |
| `vm/runtime/ops_arithmetic.rs` | Numeric & comparison ops | 400 |
| `vm/runtime/ops_data.rs` | Record, list, tuple, string ops | 500 |
| `vm/runtime/ops_control.rs` | Jumps, calls, returns, effects | 500 |
| `vm/runtime/ops_io.rs` | Print, debug, format ops | 200 |

## Migration Strategy

1. Create the directory structure with empty `mod.rs`
2. Move code function-by-function, keeping tests passing after each move
3. Update `mod.rs` with `pub use` re-exports to preserve the public API
4. Run `cargo test` after each file move
