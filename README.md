# Baseline

**A programming language where the types catch your bugs, the compiler explains what went wrong, and side effects are always visible.**

Baseline is a typed functional language with an effect system. Functions declare what side effects they perform — reading files, making HTTP calls, printing to the console — and the compiler enforces those declarations. The result is code where you can tell what a function does by reading its signature.

Baseline also works well with AI code generation tools. Its unambiguous syntax and structured JSON diagnostics make compiler-driven self-correction loops practical.

## Core Ideas

1. **Types prove correctness** — Refinement types like `Int where self > 0` let the compiler verify constraints that would otherwise be runtime checks.
2. **Effects are visible** — Side effects (I/O, network, filesystem) are declared in function signatures. Pure functions are the default. You see the blast radius before you call.
3. **Readable by humans and machines** — One syntax for errors, one for effects, one for data pipelines. Whether a person or an AI agent wrote it, you can read it instantly.

## Quick Start

```bash
# Build the compiler
cargo build --bin blc

# Initialize a new project
blc init my-app

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
fn safe_divide(a: Int, b: Int) -> Result<Int, String> = {
  if b == 0 then Err("Division by zero")
  else Ok(a / b)
}
  where
    test "divides correctly" = safe_divide(10, 2) == Ok(5)
    test "rejects zero" = safe_divide(10, 0) == Err("Division by zero")

/// FizzBuzz — demonstrates control flow, effects, and string interpolation
fn main!() -> {Console} Unit = {
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
- **Pipes** — `data |> List.filter(pred) |> List.map(f)` for readable data pipelines.
- **Pattern matching** — Exhaustive `match` on sum types, tuples, and literals.
- **Module system** — `import Math` (qualified), `from Math import {abs, max}` (selective), `from Math import *` (wildcard).
- **String interpolation** — `"Hello, ${name}!"` with arbitrary expressions.
- **Weak references** — `Weak<T>` with `Weak.downgrade`/`Weak.upgrade` for breaking reference cycles.
- **Scoped resources** — `Scoped<T>` with compile-time escape analysis for safe resource management.

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
| `blc check <file>` | Static analysis (types, effects, refinements). `--json` for structured output. `--level` for verification depth. |
| `blc run <file>` | Execute via bytecode VM. `--jit` for Cranelift JIT. `-- args` to pass program arguments. |
| `blc test <file>` | Run inline tests. `--json` for structured output. |
| `blc fmt <file>` | Format source code. `--check` to validate without modifying. |
| `blc build <file>` | Compile to standalone native executable (AOT via Cranelift). |
| `blc init [name]` | Initialize a new Baseline project with `baseline.toml` and `src/main.bl`. |
| `blc docs` | Generate standard library documentation. `--json` for structured output. |
| `blc lsp` | Start the Language Server Protocol server. |

**Note:** `test`, `fmt`, `build` require feature flags: `cargo build --features jit,aot`

## Runtime Backends

| Backend | Flag | Performance | Use case |
|---------|------|-------------|----------|
| **Bytecode VM** | (default) | ~CPython parity | General use, full language support |
| **Cranelift JIT** | `--jit` | 2-3x faster for numeric code | Compute-heavy workloads |
| **AOT native** | `blc build` | Native binary | Deployment, distribution |

The VM uses NaN-boxed 8-byte values, stack-based execution, and superinstructions for hot paths. The JIT adds tail-call-to-loop optimization, base-case speculation, and unboxed scalar fast paths.

## Project Structure

```
baseline/
├── blc/                        # Compiler and runtime (Rust)
│   ├── src/
│   │   ├── analysis/           # Type, effect, and refinement checkers
│   │   ├── vm/                 # Bytecode compiler, stack VM, JIT, AOT
│   │   ├── interpreter.rs      # Tree-walk interpreter (legacy)
│   │   ├── diagnostic_render.rs # Rich diagnostic output with source context
│   │   ├── docs.rs             # Stdlib documentation generator
│   │   ├── format.rs           # Code formatter
│   │   ├── lsp.rs              # Language Server Protocol
│   │   └── manifest.rs         # Project manifest (baseline.toml)
│   ├── tests/                  # Unit, conformance, differential, property tests
│   └── fuzz/                   # Fuzzing targets
├── baseline-rt/                # Static runtime library for AOT binaries
├── tree-sitter-baseline/       # Grammar definition and parser
├── selfhost/                   # Self-hosted compiler (lexer, parser, C codegen)
├── examples/                   # Example programs
├── tests/
│   ├── conformance/            # 45 conformance tests across 12 categories
│   └── programs/               # Real programs (fibonacci, sort, calculator, etc.)
├── benchmarks/cpu/             # Cross-language CPU benchmarks (C, Go, Rust, etc.)
├── docs/                       # User-facing documentation
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
| `Weak` | pure+ | `downgrade`, `upgrade` |
| `Console` | script+ | `println!`, `print!`, `error!`, `read_line!` |
| `Log` | script+ | `info!`, `warn!`, `error!`, `debug!` |
| `Time` | script+ | `now!`, `sleep!` |
| `Random` | script+ | `int!`, `bool!` |
| `Env` | script+ | `get!`, `set!`, `args!` |
| `Fs` | script+ | `read!`, `write!`, `exists!`, `delete!`, `with_file!` |
| `Http` | script+ | `get!`, `post!`, `put!`, `delete!` |
| `Router` | server | `get`, `post`, `put`, `delete`, `group` |

## Testing

```bash
# Full test suite (~450 tests)
cargo test -p blc

# With JIT and AOT tests (~550 tests)
cargo test -p blc --features jit,aot

# Fast subset
scripts/test.sh fast

# Diagnostic coverage report
scripts/diagnostic-coverage.sh
```

The test suite includes:
- **Unit tests** — compiler internals (types, effects, refinements, VM, formatter, LSP)
- **Conformance tests** — 45 tests across 12 categories verifying language features
- **Differential tests** — interpreter vs bytecode VM consistency
- **Property tests** — parse-never-panics, analysis-idempotent, soundness, valid-codes
- **Bootstrap tests** — self-hosted compiler pipeline verification

## Building

### Prerequisites

- Rust (stable, edition 2024)
- Node.js (for grammar regeneration only)

```bash
# Default build (VM only)
cargo build --bin blc

# Full build with JIT and AOT
cargo build --bin blc --features jit,aot --release

# Regenerate grammar (only needed when changing grammar.js)
cd tree-sitter-baseline && npx tree-sitter generate && cargo build
```

## Status

Baseline is in **v0.1 (Bootstrap Phase)**. The core language, type system, effect system, and developer tooling are functional.

### Implemented
- Tree-sitter grammar and fault-tolerant parser
- Type checker with generics, inference, and refinement types
- Effect system with transitive checking via call graph analysis
- Bytecode VM with NaN-boxed values and superinstructions
- Cranelift JIT with tail-call optimization and base-case speculation
- AOT compilation to standalone native binaries
- Rich diagnostics with source context, "did you mean?" suggestions
- LSP server (diagnostics, hover, go-to-definition, completion)
- Code formatter
- Inline test framework with effect handlers
- Module system with qualified, selective, and wildcard imports
- Self-hosted compiler (lexer, parser, C codegen) for bootstrap verification
- HTTP server with TLS, compression, static files, CORS, cookies
- Editor support (Zed)
- CI pipeline, fuzzing, conformance and property test suites

### Planned (v0.2+)
- Custom effect handlers (beyond capture-only `with`)
- Regex and string refinements
- Full standard library (Async, advanced Http)
- Region-based memory management

See the [language specification](design/baseline-language-specification.md) for the full reference and the [getting started guide](docs/getting-started.md) for a walkthrough.
