# Baseline

**A fast, verifiable, LLM-native programming language.**

Baseline is a verification layer designed for AI coding agents. Unlike traditional compilers optimized for binary size or execution speed, the Baseline compiler (`baselinec`) is optimized for **structured feedback**, allowing LLM agents to self-correct and human reviewers to instantly verify side effects ("Cognitive Safety").

## Core Principles

1.  **The Type Is The Spec**: Types encode enough information (via Refinements) that correctness can be verified at compile time.
2.  **Effects Are Data**: Side effects (I/O, Network, DB) are managed as capabilities. Pure functions are the default.
3.  **LLM-Native**: The language is designed for machine generation. Syntax is unambiguous, errors are structured JSON, and formatting is canonical.

## Key Features

-   **Effect Verification**: Functions must declare their capabilities. `Log.info!` requires `{Log}`. No hidden network calls.
-   **Refinement Types**: Validate data constraints at the type level. `type Port = Int where self > 0 && self < 65535`.
-   **"One Way" Principle**: A single canonical way to write every construct, eliminating style debates and maximizing LLM training efficiency.
-   **Structured Diagnostics**: The compiler emits JSON patches that Agents can deterministically apply to fix errors.

## Project Structure

This monorepo contains the entire Baseline toolchain:

-   [`baselinec`](./baselinec): The compiler core (Rust). Performs semantic analysis, effect checking, and refinement verification.
-   [`tree-sitter-baseline`](./tree-sitter-baseline): The fault-tolerant parser source of truth.
-   [`extensions/baseline-zed`](./extensions/baseline-zed): The Zed editor extension for "Safety Highlighting".
-   [`design`](./design): Technical specifications and language references.

## Getting Started

### Prerequisites

-   Rust (Stable)
-   Cargo

### Building the Compiler

```bash
# Build the compiler binary
cargo build --release --bin baselinec

# Check a file (Example)
./target/release/baselinec check examples/hello.baseline --json
```

## Example: Cognitive Safety

Baseline makes side effects visible and capabilities explicit.

```baseline
// 1. Definition: The type refines valid data
type Port = Int where self >= 1024

// 2. Capability: Explicit effect declaration
effect Net {
    listen! : Port -> {Net} Unit
}

// 3. Application: Functions declare required effects
start_server! : Port -> {Net, Log} Unit
start_server! = |port|
    Log.info!("Starting server on ${port}")
    Net.listen!(port)
```

## Roadmap

-   **Phase 1: Syntax Foundation** (Current)
    -   [x] Grammar definition (`tree-sitter`)
    -   [x] Basic parser
    -   [x] Safety Highlighting (Zed)

-   **Phase 2: The Agent Validator** (Current)
    -   [x] Compiler CLI (`baselinec`)
    -   [x] Effect System (Capability Checking)
    -   [x] Refinement Checking (Intervals)
    -   [x] Agent-Protocol JSON Output

-   **Phase 3: Integration** (Next)
    -   [ ] Interpreter & Runtime
    -   [ ] Python Bindings (`pybaseline`)
    -   [ ] Standard Library
