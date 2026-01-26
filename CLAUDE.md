# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rocket is a new programming language in the design phase. This repository contains the language specification only - there is no compiler implementation yet.

**Design Philosophy:**
- "The type is the spec" - Types encode correctness, enabling verification and test generation
- "Effects are data" - Side effects are explicit capabilities, pure functions are default
- "LLM-native" - Designed for machine generation with unambiguous syntax and structured errors

## Repository Structure

```
rocket-lang/
├── design/
│   └── rocket-language-specification.md   # Complete language spec (v0.1.0 Draft)
└── CLAUDE.md
```

## Key Language Features

### Syntax Patterns

Function definitions use ML-style type-first declarations:
```rocket
greet : String -> String
greet = |name| "Hello, ${name}"
```

Effectful functions are marked with `!` suffix:
```rocket
fetch_user! : Id -> {Http, Log} User?
```

### The One Way Principle

Rocket enforces single idiomatic patterns:
- String formatting: always `"${expr}"` interpolation
- Error handling: always `?` propagation or pattern matching
- Data transformation: always combinators (map/filter/fold), not loops
- Chaining: always `|>` pipes

### Core Types

- `Option<T>` (sugar: `T?`) - nullable values
- `Result<T, E>` (sugar: `T!E`) - fallible operations
- Refinement types: `type Port = Int where 1 <= self <= 65535`
- Sum types: `type Status = Active | Inactive | Pending`
- Row-polymorphic records: `{ name: String, ..rest }`

### Effect System

Effects declare capabilities required by functions:
```rocket
effect Http {
  get! : String -> Response!HttpError
  post! : (String, Body) -> Response!HttpError
}
```

Built-in effects: Console, Http, Fs, Net, Db, Time, Random, Env, Process, Log, Metrics

### Specifications

Function contracts with compile-time verification:
```rocket
@spec divide
@given numerator: Int, denominator: Int where denominator != 0
@ensures result * denominator <= numerator
```

### Testing

Inline tests co-located with code:
```rocket
add = |a, b| a + b
where
  test "basic addition" = add(1, 2) == 3
  property "commutative" = |a: Int, b: Int| add(a, b) == add(b, a)
```

## When Working on This Specification

- Focus on consistency with established patterns (ML/OCaml, Rust, Koka influences)
- The language prioritizes LLM code generation - constrained solution space, deterministic verification
- No classes or inheritance - use composition, row types, and effects
- "Parse, don't validate" - types should make illegal states unrepresentable
- All compiler output is structured JSON for machine consumption

## Future Work

The specification covers: lexical structure, types, expressions, functions, effects, modules, specifications, testing, memory model, compilation, tracing, LSP/compiler API, and standard library. No implementation exists yet.
