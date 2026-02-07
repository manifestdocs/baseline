# Baseline

**A strongly typed, effect-based programming language designed for AI coding agents.**

Baseline is a verification-first language where types encode correctness, side effects are explicit capabilities, and syntax is unambiguous. The compiler (`blc`) provides structured JSON diagnostics that LLM agents can deterministically apply to self-correct.

## Core Principles

1. **The Type Is The Spec** — Types encode enough information (via refinements) that correctness can be verified at compile time.
2. **Effects Are Data** — Side effects (I/O, Network, DB) are managed as explicit capabilities. Pure functions are the default.
3. **LLM-Native** — Designed for machine generation. Syntax is unambiguous, errors are structured JSON, and formatting is canonical.

## Quick Start

```bash
# Build
cargo build --bin blc

# Run a program
blc run examples/hello.bl

# Check for errors (with JSON output for tooling)
blc check examples/hello.bl --json

# Run inline tests
blc test examples/inline_test.bl

# Format source code
blc fmt examples/hello.bl
```

## Language Overview

```baseline
@prelude(script)

/// Safe division that returns Result instead of crashing
safe_divide : (Int, Int) -> Result<Int, String>
safe_divide = |a, b| {
  if b == 0 then Err("Division by zero")
  else Ok(a / b)
}
  where
    test "divides correctly" = safe_divide(10, 2) == Ok(5)
    test "rejects zero" = safe_divide(10, 0) == Err("Division by zero")

/// FizzBuzz — demonstrates control flow, effects, and string interpolation
main! : () -> {Console} Unit
main! = || {
  for n in 1..101 do
    if n % 15 == 0 then Console.println!("FizzBuzz")
    else if n % 3 == 0 then Console.println!("Fizz")
    else if n % 5 == 0 then Console.println!("Buzz")
    else Console.println!("${n}")
}
```

### Key Features

- **Effect system** — Functions declare their side effects. `Console.println!` requires `{Console}`. No hidden I/O.
- **Refinement types** — Validate constraints at the type level: `type Port = Int where self > 0 && self < 65536`
- **Option and Result** — Safe error handling with `?` propagation, pattern matching, and `map`/`unwrap_or`.
- **Inline tests** — Tests live next to the code they verify, run with `blc test`.
- **Effect handlers** — `with Console.println! { body }` captures effect calls for testing.
- **Pipes** — `data |> transform |> format` for readable data pipelines.
- **Pattern matching** — Exhaustive `match` on sum types, tuples, and literals.
- **Module system** — `import Math` (qualified), `from Math import {abs, max}` (selective), `from Math import *` (wildcard).
- **String interpolation** — `"Hello, ${name}!"` with arbitrary expressions.

### Effect Handlers for Testing

Side-effecting code can be tested in isolation using `with`:

```baseline
test "greet prints hello" = {
  let (_, output) = with Console.println! {
    Console.println!("hello")
  }
  output == ["hello"]
}
```

## Toolchain

| Command | Description |
|---------|-------------|
| `blc check <file>` | Static analysis (types, effects, refinements). `--json` for structured output. |
| `blc run <file>` | Execute a program. `--vm` for bytecode VM. |
| `blc test <file>` | Run inline tests. `--json` for structured output. |
| `blc fmt <file>` | Format source code. `--check` to validate without modifying. |
| `blc lsp` | Start the Language Server Protocol server. |

## Project Structure

```
baseline/
├── blc/                        # Compiler and runtime (Rust)
│   ├── src/
│   │   ├── analysis/           # Type, effect, and refinement checkers
│   │   ├── vm/                 # Bytecode compiler and stack VM
│   │   ├── interpreter.rs      # Tree-walk interpreter
│   │   ├── format.rs           # Code formatter
│   │   ├── lsp.rs              # Language Server Protocol
│   │   └── builtins.rs         # Built-in modules and functions
│   ├── tests/                  # Unit, conformance, differential, property tests
│   └── fuzz/                   # Fuzzing targets
├── tree-sitter-baseline/       # Grammar definition and parser
├── examples/                   # Example programs and test files
├── tests/
│   ├── conformance/            # 45 conformance tests across 12 categories
│   └── programs/               # Real programs (fibonacci, sort, calculator, etc.)
├── extensions/baseline-zed/    # Zed editor extension
├── scripts/                    # Test runner and diagnostic coverage scripts
├── design/                     # Language specification and vision documents
└── .github/workflows/          # CI pipeline
```

## Available Modules

Modules are gated by prelude level (`@prelude(none|minimal|pure|core|script|server)`):

| Module | Prelude | Functions |
|--------|---------|-----------|
| `Option` | minimal+ | `map`, `unwrap`, `unwrap_or`, `is_some`, `is_none` |
| `Result` | minimal+ | `map`, `map_err`, `unwrap`, `unwrap_or`, `is_ok`, `is_err` |
| `String` | pure+ | `length`, `slice`, `contains`, `split`, `trim`, `upper`, `lower`, ... |
| `List` | pure+ | `map`, `filter`, `fold`, `find`, `head`, `tail`, `length`, `sort`, ... |
| `Math` | pure+ | `abs`, `min`, `max`, `clamp`, `pow` |
| `Json` | pure+ | `parse`, `stringify` |
| `Console` | script+ | `println!`, `print!`, `error!`, `read_line!` |
| `Log` | script+ | `info!`, `warn!`, `error!`, `debug!` |
| `Time` | script+ | `now!`, `sleep!` |
| `Random` | script+ | `int!`, `bool!` |
| `Env` | script+ | `get!`, `set!` |
| `Fs` | script+ | `read!`, `write!`, `exists!`, `delete!` |
| `Http` | script+ | `get!`, `post!`, `put!`, `delete!` |
| `Router` | server | `get`, `post`, `put`, `delete`, `group` |

## Testing

```bash
# Full test suite (575 tests)
cargo test -p blc

# Fast subset
scripts/test.sh fast

# Diagnostic coverage report
scripts/diagnostic-coverage.sh
```

The test suite includes:
- **387 unit tests** — compiler internals (types, effects, refinements, interpreter, formatter, LSP)
- **145 analysis tests** — type checker and effect checker scenarios
- **35 end-to-end tests** — full pipeline from source to execution
- **45 conformance tests** — language feature verification across 12 categories
- **4 property tests** — parse-never-panics, analysis-idempotent, soundness, valid-codes
- **2 differential tests** — interpreter vs bytecode VM consistency

## Building

### Prerequisites

- Rust (stable, edition 2024)
- Node.js (for grammar regeneration only)

```bash
# Build the compiler
cargo build --bin blc

# Regenerate grammar (only needed when changing grammar.js)
cd tree-sitter-baseline && npx tree-sitter generate && cargo build
```

## Status

Baseline is in **v0.1 (Bootstrap Phase)**. The core language, type system, effect system, interpreter, and developer tooling are functional. See the [language specification](design/baseline-language-specification.md) for the full reference.

### Implemented
- Tree-sitter grammar and fault-tolerant parser
- Type checker with generics, inference, and refinement types
- Effect system with transitive checking via call graph analysis
- Tree-walk interpreter with full language support
- Bytecode compiler and stack VM (subset of features)
- LSP server (diagnostics, hover, go-to-definition, completion)
- Code formatter
- Inline test framework with effect handlers
- Module system with qualified, selective, and wildcard imports
- Editor support (Zed)
- CI pipeline, fuzzing, conformance and property test suites

### Planned (v0.2+)
- Custom effect handlers (beyond capture-only `with`)
- Regex and string refinements
- Full standard library (Async, advanced Http)
- Region-based memory management
