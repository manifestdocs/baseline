# Baseline v2 Spec — Roadmap & Acceptance Criteria

**Status**: Draft
**Date**: 2026-02-07
**Source**: `design/baseline-language-specification.md` (cherry-picked features)

## Already Implemented (from v2 spec)

These v2 features are already in the grammar/compiler — no work needed:

- `fn` keyword syntax with params-in-parens
- `export` keyword on functions and types
- `T?` sugar for `Option<T>` (grammar: `option_type`)
- `for x in iter do body` loops
- `effect Name { sig... }` declarations (grammar: `effect_def`)
- `with` expression (grammar: `with_expression`)
- Lambda syntax `|x| body`
- Pipe operator `|>`
- String interpolation `${expr}`
- `not` keyword for logical negation

---

## Phase 1: Quick Wins (v0.1.x)

Low-effort grammar and tooling additions. No architectural changes.

### 1.1 Hex, Binary, Octal Integer Literals

**Goal**: Support `0xFF`, `0b1010`, `0o755` and underscore separators `1_000_000`.

**Current**: `integer_literal: $ => /\d+/` — decimal only.

**AC**:
- [ ] Grammar: `integer_literal` matches `0x[0-9a-fA-F_]+`, `0b[01_]+`, `0o[0-7_]+`, `[0-9][0-9_]*`
- [ ] tree-sitter tests: corpus file with cases for each base, underscores, edge cases
- [ ] Interpreter: parse all bases to i64 in `eval_integer_literal`
- [ ] VM compiler: parse all bases in constant folding / literal handling
- [ ] Conformance test: `tests/conformance/01_literals/numeric_bases.bl`
- [ ] Error case: `0xGG` parses but produces runtime/analysis error (or doesn't match grammar)

### 1.2 Multi-line Strings

**Goal**: Triple-quoted strings `"""..."""` with preserved indentation and interpolation.

**Current**: Only `"..."` with `${expr}` interpolation.

**AC**:
- [ ] Grammar: `string_literal` alternative with `"""` delimiters
- [ ] Interpolation `${expr}` works inside triple-quoted strings
- [ ] Escape sequences work inside triple-quoted strings
- [ ] Newlines preserved literally (no escaping needed)
- [ ] tree-sitter test corpus cases
- [ ] Interpreter + VM handle multi-line string values
- [ ] Conformance test: `tests/conformance/01_literals/multiline_strings.bl`

### 1.3 `++` List Concatenation Operator

**Goal**: `[1, 2] ++ [3, 4]` evaluates to `[1, 2, 3, 4]`.

**Current**: `List.concat(a, b)` only.

**AC**:
- [ ] Grammar: `++` as binary operator at ADDITIVE precedence
- [ ] Type checker: both operands must be `List<T>` with same `T`, result is `List<T>`
- [ ] Interpreter: evaluate by concatenating lists
- [ ] VM: emit bytecode for list concat (can reuse `List.concat` native)
- [ ] Conformance test: `tests/conformance/06_data_types/list_concat.bl`
- [ ] Works in pipes: `xs |> |l| l ++ [0]`

### 1.4 `llms.txt` Bootstrap File

**Goal**: Ship an `llms.txt` in the repo root for inclusion in LLM system prompts.

**Current**: None.

**AC**:
- [ ] File at `llms.txt` (repo root) with syntax summary, key rules, common patterns
- [ ] Covers: function syntax, effects, types, pipes, lambdas, match, let, records, sum types, error handling, inline tests
- [ ] Includes "NOT supported" section (no classes, no null, no exceptions, no string concat with `+`)
- [ ] Under 200 lines (fits in a system prompt without dominating context)
- [ ] All examples in the file pass `blc check`

### 1.5 Raw Strings

**Goal**: `r"C:\Users\path"` — no escape processing, no interpolation.

**Current**: Not supported.

**AC**:
- [ ] Grammar: `r"..."` string literal variant (no interpolation, no escape sequences)
- [ ] Grammar: `r#"..."#` delimiter variant for strings containing `"`
- [ ] Interpreter + VM treat raw strings as literal content
- [ ] tree-sitter test corpus
- [ ] Conformance test

---

## Phase 2: v0.2 Milestone

Language-level changes that deepen the type system, module system, or runtime.

### 2.1 User-Defined Effect Handlers (`with` block semantics)

**Goal**: `with { effect: handler }` provides an effect implementation for a block, enabling testing with mocked effects.

**Current**: Grammar has `with_expression` but it's limited to `with field_expression block`. The v2 spec envisions `with { http: mock_http, console: Console.buffer() }`.

**AC**:
- [ ] Grammar: `with { ident: expr, ... } expr` — effect handler binding syntax
- [ ] Type checker: verifies handler satisfies the effect interface
- [ ] Effect checker: `with` block satisfies effect requirements (removes effects from inner block's set)
- [ ] Interpreter: handler dispatch — inner code calls effect methods through provided handler
- [ ] VM: handler dispatch mechanism (handler stack or evidence passing)
- [ ] Test: mock a `Console` effect, capture output, assert on captured strings
- [ ] Conformance test: `tests/conformance/08_effects/effect_handlers.bl`

**Open question**: Stack-based handlers (Koka-style, resumable continuations) vs evidence-passing (simpler, no continuations). Recommend evidence-passing for v0.2 — sufficient for testing/mocking without full algebraic effect semantics.

### 2.2 Named Arguments

**Goal**: `create_user(name: "Alice", age: 30)` — order-independent, self-documenting call sites.

**Current**: Positional only.

**AC**:
- [ ] Grammar: call arguments can be `ident: expr` (named) or `expr` (positional)
- [ ] Rule: named args must come after all positional args
- [ ] Rule: cannot mix named and positional for the same parameter
- [ ] Type checker: resolve named args to parameter positions, check types
- [ ] Error: duplicate named arg → diagnostic
- [ ] Error: unknown named arg → diagnostic with suggestions
- [ ] Interpreter + VM: reorder args to match param positions before call
- [ ] Conformance test: `tests/conformance/05_functions/named_args.bl`

### 2.3 `for` Loop — Full Implementation

**Goal**: `for` loops fully working in interpreter and VM for effectful iteration.

**Current**: Grammar exists (`for_expression`). Need to verify interpreter/VM support.

**AC**:
- [ ] Interpreter: `for` evaluates body for each element, returns Unit
- [ ] VM: compile `for` to iteration bytecode
- [ ] Type checker: body must return Unit, iterator must be List or Range
- [ ] Effect checker: `for` body effects propagate to enclosing function
- [ ] Compiler error if `for` used to build a list (suggest `List.map` instead) — per v2 spec One Way Principle
- [ ] Conformance test: `tests/conformance/04_control_flow/for_loops.bl`

### 2.4 Map and Set Collection Types

**Goal**: Built-in `Map<K, V>` and `Set<T>` with standard operations.

**Current**: Only `List<T>`.

**AC**:
- [ ] `Map<K, V>` type recognized by type checker
- [ ] Builtins: `Map.empty`, `Map.insert`, `Map.get`, `Map.remove`, `Map.contains`, `Map.keys`, `Map.values`, `Map.len`
- [ ] `Set<T>` type recognized by type checker
- [ ] Builtins: `Set.empty`, `Set.insert`, `Set.remove`, `Set.contains`, `Set.union`, `Set.intersection`, `Set.len`
- [ ] Map literal syntax (deferred — use `Map.empty` + `Map.insert` chains for now)
- [ ] Generic inference works: `Map.get(m, "key")` infers `Option<V>`
- [ ] Interpreter + VM implementations
- [ ] Conformance tests for both types

### 2.5 Typed Holes

**Goal**: `??` as a placeholder expression. Compiler reports the expected type and available bindings.

**Current**: Not implemented.

**AC**:
- [ ] Grammar: `??` as an expression (`hole_expression`)
- [ ] Type checker: infer expected type from context, report as structured diagnostic
- [ ] Diagnostic includes: expected type, available bindings with types, available functions matching the expected type
- [ ] Severity: Warning (allows partial compilation)
- [ ] `blc check --json` includes hole information in output
- [ ] Conformance test: `tests/conformance/07_type_system/typed_holes.bl`

---

## Phase 3: v0.3

### 3.1 Full Effect Handlers (Algebraic)

**Goal**: Resumable effect handlers with `handle`/`resume` semantics (Koka-style).

**AC**:
- [ ] `handle expr with { Effect.method!(args) -> resume(result) }` syntax
- [ ] Compiler ensures handler covers all effect methods
- [ ] `resume` keyword continues execution at the effect call site
- [ ] Enables: generators, async, exceptions as effects
- [ ] Performance: zero-cost when handler is statically known (evidence passing optimization)
- [ ] Research: evaluate Koka's evidence-passing vs delimited continuations

### 3.2 BDD Testing Framework (`describe`/`it`/`expect`)

**Goal**: Structured test blocks as a complement to inline `where test` blocks.

**AC**:
- [ ] Grammar: `describe "name" { ... }`, `it "name" { ... }`, `context "name" { ... }`
- [ ] `expect expr to_equal expr`, `expect expr to_be pattern`
- [ ] Matchers: `to_equal`, `to_be`, `to_contain`, `to_have_length`, `to_be_empty`, `to_start_with`, `to_satisfy`
- [ ] Hooks: `before_each { ... }`, `after_each { ... }`
- [ ] Focus/skip: `it.only`, `it.skip`
- [ ] `blc test --format json` structured output
- [ ] Decision: separate test files (`test/`) or colocated? Recommend separate files.

### 3.3 Map/Set Literal Syntax (if deferred from 2.4)

**AC**:
- [ ] `#{ "key": value, ... }` for Map literals (or chosen syntax)
- [ ] `#{ value1, value2, ... }` for Set literals (or chosen syntax)
- [ ] Type inference from literal contents

---

## Phase 4: v0.4+

### 4.1 Constrained Generation Protocol (CGP)

**Goal**: Real-time type-constraint feedback during LLM token generation.

**AC**:
- [ ] `baseline cgp start` initializes a session with scope context
- [ ] `baseline cgp advance` accepts tokens, returns valid next tokens
- [ ] Effect enforcement: tokens referencing out-of-scope effects are rejected
- [ ] Refinement enforcement: literal values violating refinements are flagged
- [ ] Integration: logit bias arrays for vLLM/TGI
- [ ] Latency: <10ms per token advance call
- [ ] Prerequisite: incremental type checking infrastructure

### 4.2 SMT-Based Specification Verification

**Goal**: `@spec/@requires/@ensures` checked by SMT solver at compile time.

**AC**:
- [ ] Grammar: `@spec`, `@given`, `@returns`, `@requires`, `@ensures`, `@pure`, `@total`
- [ ] SMT backend: Z3 or CVC5 integration
- [ ] Verification levels: `types` < `refinements` < `full`
- [ ] `--level=full` checks all specs; `--level=refinements` checks type refinements only
- [ ] Counter-example generation on spec violation
- [ ] `@assume` for external invariants (tracked, reported)
- [ ] Timeout handling with structured suggestions

### 4.3 Row Polymorphism

**Goal**: `{ name: String, ..r }` — functions accept any record with at least the specified fields.

**AC**:
- [ ] Grammar: `..ident` in record type position
- [ ] Type checker: row variable unification
- [ ] Bounded at module boundaries (exported functions must have concrete types)
- [ ] Inference: row variables inferred within module scope
- [ ] Conformance tests

---

## Explicitly Deferred / Rejected

| Feature | Disposition | Rationale |
|---------|-------------|-----------|
| `<\|` backward pipe | Rejected | One Way Principle — `\|>` is sufficient |
| `>>`, `<<` composition | Rejected | One Way Principle — pipes + lambdas cover this |
| `Char` type | Deferred indefinitely | `String` handles single characters fine |
| `internal` visibility | Deferred | `export` vs private is sufficient for now |
| Custom preludes via TOML | Deferred | No users yet; premature configuration |
| `**` exponentiation operator | Deferred | `Int.pow(a, b)` or `Math.pow(a, b)` sufficient |
