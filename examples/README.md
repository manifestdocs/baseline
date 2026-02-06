# Baseline Examples

A curated collection of example programs demonstrating Baseline's core language features.

## How to Run

```bash
# Build the compiler (if not already built)
cargo build --bin blc

# Run any example
cargo run --bin blc -- run examples/fizzbuzz.bl
cargo run --bin blc -- run examples/list_ops.bl
cargo run --bin blc -- run examples/option_result.bl
```

## Learning Path

Work through the examples in this order — each builds on concepts from the previous:

1. **[fizzbuzz.bl](fizzbuzz.bl)** — Control flow basics: loops, conditionals, modulo, string interpolation
2. **[list_ops.bl](list_ops.bl)** — Functional programming: map, filter, fold, pipe operator, lambdas
3. **[option_result.bl](option_result.bl)** — Error handling: Option, Result, pattern matching, the `?` operator

## What You'll Learn

| Example | Key Features |
|---------|-------------|
| FizzBuzz | `for` loops, `if/else`, `%` operator, `${...}` interpolation, `Console.println!` |
| List Ops | `List.map`, `List.filter`, `List.fold`, `\|>` pipes, lambda expressions |
| Option/Result | `Some`/`None`, `Ok`/`Err`, `match`, `?` operator, `Option.map`, `Result.map` |
