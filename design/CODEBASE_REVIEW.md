# Baseline Codebase Review

**Date:** 2026-01-31
**Scope:** Language spec, technical spec, compiler (`blc`), grammar (`tree-sitter-baseline`), examples, tests
**Reviewer context:** Full read of design/, blc/src/, tree-sitter-baseline/grammar.js, examples/, and test corpus

---

## 1. Spec Review Summary

### 1.1 Language Specification (`design/baseline-language-specification.md`)

The language spec is ambitious and well-thought-out. It covers a full vision for Baseline including refinement types, algebraic effects, region-based memory (Perceus/FBIP), a BDD testing framework, and LLM-native tooling. Key design decisions are sound:

- **"The type is the spec"** philosophy is well-articulated with concrete examples
- **"One Way Principle"** (Section 1.6) is a strong differentiator for LLM generation
- **"Parse, Don't Validate"** (Section 1.7.2) provides good architectural guidance
- **Effect system** design with row polymorphism and direct-style compilation is well-specified
- **Tiered refinement types** (Section 3.7) is a pragmatic approach

**Concerns with the spec:**

1. **Section numbering collisions**: Sections 6.4 and 6.5 are duplicated — there are two "6.4" sections (Effect Inference and Row Polymorphism) and two "6.5" sections (Pure Functions and Direct Style). Similarly, Section 10.3 appears three times (Region-Based Local Memory, Why Regions Are Simpler, and Persistent Data Structures).

2. **Scope ambiguity for v0.1**: The spec describes the full language vision but doesn't clearly delineate what's v0.1 vs. future. Readers must cross-reference `SPEC_V0_1_DELTA.md` to understand what's actually implemented, which creates cognitive overhead.

3. **File extension inconsistency**: The spec says `.baseline` (Section 2.1), but the actual implementation uses `.bl`. This needs to be reconciled.

4. **Testing framework (Section 9)**: The BDD testing spec (`describe`/`it`/`given`/`when`/`expect`) is extensive but none of it is implemented. The grammar only supports basic inline `test` blocks.

5. **Memory model (Section 10)**: Describes Perceus RC, FBIP, regions, and arenas — none of which are implemented. The actual runtime uses `Arc<T>` per `SPEC_V0_1_DELTA.md`. The spec should clarify the v0.1 simplification more prominently.

### 1.2 Technical Specification (`design/technical-specification.md`)

The tech spec serves as a good implementation blueprint for the dual-head architecture (Agent CLI + Human LSP). However:

1. **Naming inconsistency**: References `baselinec` as the compiler binary name, but the actual implementation is `blc`. The tech spec also still references `Rowan` and `Salsa` for AST/incremental computation — neither is used in the actual implementation. The compiler works directly on tree-sitter `Node` values.

2. **Phase alignment**: The roadmap describes 3 phases (Syntax Foundation, Agent Validator, Integration Layer). The project appears to be approximately at Phase 2 completion with partial Phase 3 (tree-walk interpreter exists).

3. **Missing components**: The tech spec calls for `json_api.rs` and `server.rs` in the compiler; neither exists. JSON output is handled inline in `main.rs` via `serde_json`, and LSP is a stub.

### 1.3 SPEC_V0_1_DELTA.md

This document is well-written and concise. It correctly constrains the implementation. The v0.1 constraints are reasonable: integer-only refinements, built-in effects only, no effect handlers, single-file modules, `Arc<T>` memory model.

---

## 2. Compiler Review (`blc/src/`)

### 2.1 Architecture Overview

The compiler is structured as:

```
main.rs          — CLI (clap), check/run/lsp commands
parse.rs         — Tree-sitter parsing + syntax error collection
diagnostics.rs   — Diagnostic types (JSON-serializable)
interpreter.rs   — Tree-walk interpreter for `run` command
analysis/
  mod.rs         — Re-exports
  types.rs       — Type checking pass
  effects.rs     — Effect checking pass
  refinements.rs — Refinement checking pass
```

This is a reasonable structure for a bootstrap compiler. The three analysis passes run independently and sequentially (types, effects, refinements), which is simple but means they can't share information.

### 2.2 Issues Found

#### Critical

1. **`diagnostics.rs:8` — Stale "Rocket" reference**: Doc comment says "result of checking a Rocket source file." This is a leftover from the rename.

2. **`analysis/mod.rs:1` — Stale "Rocket" reference**: Doc comment says "Semantic analysis passes for Rocket source files."

3. **`effects.rs:211` — Debug `eprintln!` left in production code**: Line 211 prints `DEBUG: checking field expression:` to stderr unconditionally on every field expression. This will pollute output for any program using method calls.

4. **`effects.rs:304-311` — Duplicate variable binding**: The variable `has_effect` is computed twice identically (lines 304-306 and 309-311). The first binding is unused (compiler warning confirms this).

5. **`types.rs:136-137` — Duplicate variable declaration**: `let mut name = String::new();` appears twice. The first is shadowed and never used (compiler warning confirms this).

6. **`interpreter.rs:5-21` — Dead code (`Value` enum)**: The `Value` enum (without lifetimes) is defined but never used. Only `RuntimeValue<'a>` is used. This appears to be a first attempt that was superseded.

7. **`interpreter.rs:16` — Unsound `'static` lifetime**: `Function(Vec<String>, Node<'static>)` in the dead `Value` enum uses `'static`, which would be unsound if the tree is dropped. This is dead code, but indicates the design challenge was recognized.

8. **`effects.rs` — Function name extraction fails**: The effect checker reports `<unknown>` for function names (visible in the `effects_test.bl` output). The `find_function_name` function searches for `identifier` or `effectful_identifier` children directly under `function_def`, but the grammar uses `field('name', $._name)` — the name is a field, so `child_by_field_name("name")` should be used instead (as `types.rs` does correctly).

#### Moderate

9. **No shared symbol table between passes**: The type checker builds a `SymbolTable`, the refinement checker builds a `ValueTable`, and the effect checker extracts effects independently. There's no shared semantic model. This means:
   - Type information isn't available to the refinement checker
   - The effect checker can't resolve function names through the type system
   - Diagnostics from different passes may be contradictory

10. **`types.rs` — `parse_type` for `tuple_type` returns `Unknown`**: When a tuple type has elements (e.g., `(Int, String)`), the function returns `Type::Unknown` instead of a proper tuple type. Only the empty tuple `()` → `Unit` is handled. This means multi-argument function types silently lose information.

11. **`types.rs` — No `Float` type**: The `Type` enum has `Int`, `String`, `Bool`, `Unit` but no `Float`. The spec defines `Float` as a primitive type (64-bit IEEE 754).

12. **`types.rs` — No `Option`/`Result` types**: These are spec-defined core types (`T?` for Option, `T!E` for Result) but have no representation in the type checker.

13. **`refinements.rs` — Only checks `let` bindings**: Refinement checking only validates `let x: Port = <literal>`. It doesn't check:
    - Function call arguments (passing `70000` to a function expecting `Port`)
    - Function return values
    - Struct field initialization with refined types
    - Expressions (only direct integer literals and variable lookups)

14. **`effects.rs:360-384` — Heuristic effect inference is fragile**: The `infer_required_effect` function uses a hardcoded registry (`"print" → Console`, `"now" → Time`, etc.) and falls back to capitalizing the function name. This means `foo!` is inferred as requiring effect `Foo`, which may not exist. A more robust approach would tie into the type system.

15. **`interpreter.rs:219` — Division by zero**: The interpreter does `l / r` without checking for `r == 0`. This will panic at runtime.

16. **`interpreter.rs:202` — Naive string unescaping**: `text.trim_matches('"')` doesn't handle escape sequences (`\n`, `\t`, `\"`, `\\`) or string interpolation (`${expr}`).

17. **`effects.rs:126-174` — Dead functions**: `find_function_body` and `is_expression` are defined but never called (compiler warnings confirm). These appear to be from an earlier design.

#### Minor

18. **14 compiler warnings**: The build produces 14 warnings (unused variables, dead code, unused imports, unnecessary `mut`). These should be cleaned up.

19. **`types.rs:469-470` — Debug comment left in**: `// DEBUG:` and `// println!(...)` left in block handling.

20. **`interpreter.rs` — No closure capture**: Lambda values store parameter names and a body `Node`, but don't capture the enclosing scope. This means closures won't work correctly (free variables in the lambda body won't resolve).

21. **`main.rs:82-84` — Uncertain comments in production code**: Comments like "We need to verify if it takes args?" and "For now assume no args main." indicate unresolved design questions left in the codebase.

22. **`main.rs:89` — `main` body evaluation is incorrect**: The `Run` command stores `main`'s value as a `Function(args, body_node)` and then calls `eval` on the body node. But `body_node` is the lambda body, which may reference parameters that were never bound. For a zero-arg main, this works accidentally but is structurally wrong.

### 2.3 Build Health

The project builds successfully with `cargo build --bin blc`. There are **14 warnings** but **0 errors**. Dependencies are reasonable and minimal (clap, tree-sitter, serde/serde_json).

---

## 3. Grammar Review (`tree-sitter-baseline/grammar.js`)

### 3.1 Strengths

- Clean precedence hierarchy with named constants
- Good coverage of the v0.1 language surface
- Supports string interpolation via recursive `interpolation` rule
- Effect sets, refinement clauses, and inline tests are distinct grammar nodes (enabling safety highlighting)
- Test corpus covers effects, functions, expressions, literals, types, and BDD testing

### 3.2 Issues Found

1. **`function_def` repeats `field('name', ...)`**: Line 101-102 uses `field('name', $._name)` twice:
   ```js
   field('name', $._name), ':', field('signature', $.type_signature),
   field('name', $._name), '=', field('body', $._expression),
   ```
   This defines the function name field twice, which is how the grammar encodes "name appears on both the type line and the definition line." This is correct for the syntax `foo : T -> U` / `foo = |x| ...` but means `child_by_field_name("name")` returns the first occurrence only. The effect checker's failure to find function names may be related.

2. **No semicolons in grammar**: The grammar has no semicolon token or semicolon-inference rule. The spec says "semicolons are optional between statements when separated by newlines" (Section 2.7), but the grammar doesn't support semicolons at all. The `refinement_test.bl` example uses semicolons and fails to parse.

3. **`commaSep` doesn't allow trailing commas**: The spec explicitly says "trailing commas are always allowed and recommended" (Section 2.7), but the `commaSep` helper is:
   ```js
   function commaSep(rule) {
     return optional(seq(rule, repeat(seq(',', rule))));
   }
   ```
   This doesn't match a trailing comma (`a, b,`). It should be something like `optional(seq(rule, repeat(seq(',', rule)), optional(',')))`.

4. **No sum type / enum support in grammar**: The spec defines sum types (`type Option<T> = | Some(T) | None`) but the grammar's `type_def` only supports `type Name = <type_expr>`. There's no rule for variant declarations with `|`.

5. **No `try`/`catch` or `?` operator**: The spec defines error handling with `?` (propagation) and `try`/`catch` blocks. Neither is in the grammar.

6. **No spread operator (`..`)**: Used in record updates (`{ ..user, age: 31 }`) and list patterns (`[head, ..tail]`) per the spec, but not in the grammar.

7. **No negative integer literals**: The `integer_literal` rule is `/\d+/` which only matches positive integers. Negative numbers like `-1` would need to go through unary expression `- 1`.

8. **No float literals**: The grammar only has `integer_literal`. The spec defines `Float` as a primitive with literals like `3.14`, `1.5e10`.

9. **8 declared conflicts**: The grammar has 8 explicit conflicts (ambiguities). While tree-sitter handles these via GLR parsing, this many conflicts suggests the grammar could benefit from restructuring to reduce ambiguity.

10. **Block comment regex is fragile**: The `block_comment` rule uses a regex that may not handle all edge cases with nested `/**/` correctly: `seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')`.

---

## 4. Examples & Tests Review

### 4.1 Example Files

| File | Parses? | Checks? | Notes |
|------|---------|---------|-------|
| `hello.bl` | Yes | Yes (with known false positives) | "Rocket" in comment |
| `type_test.bl` | Yes | Produces type errors as expected | Good test coverage |
| `type_fail.bl` | Yes | Produces type errors as expected | Good coverage of failure modes |
| `struct_test.bl` | Yes | Yes | Good struct/record test |
| `effects_test.bl` | Yes | Effect errors detected correctly | Function name shows `<unknown>` |
| `refinement_test.bl` | **No** | N/A (syntax errors) | Uses semicolons; grammar doesn't support them |
| `grammar_test.bl` | Yes | — | Grammar-focused |
| `run_test.bl` | Yes | — | Interpreter-focused |

### 4.2 Tree-sitter Test Corpus

The corpus in `tree-sitter-baseline/test/corpus/` has 6 test files covering core grammar features. The tests are well-structured using tree-sitter's test format. Coverage is adequate for the current grammar but should grow as features are added.

### 4.3 Gaps

- No integration tests (parse + check + run pipeline)
- No Rust unit tests in the compiler (`#[test]` functions)
- The `refinement_test.bl` example doesn't parse, so refinement checking can't be demonstrated end-to-end via this file
- No tests for the interpreter beyond manual `.bl` files

---

## 5. Cross-Cutting Concerns

### 5.1 Naming Inconsistencies

All naming inconsistencies have been resolved:
- "Rocket" references fixed in `diagnostics.rs`, `analysis/mod.rs`, `examples/hello.bl`
- `baselinec` → `blc` in tech spec
- `.baseline` → `.bl` in language spec and tech spec

### 5.2 Spec vs. Implementation Gaps

| Spec Feature | Implementation Status |
|------|------|
| Integer refinements (`where self > 0`) | Implemented (literal + variable tracking) |
| Effect checking (capability verification) | Implemented (heuristic-based) |
| Type checking (basic) | Implemented (Int, Float, String, Bool, structs, functions) |
| Tree-walk interpreter | Implemented (basic expressions, functions, match) |
| JSON diagnostic output | Implemented |
| Sum types / enums | Not in grammar |
| Float type | Implemented (grammar + type checker) |
| Option/Result types | Not in type checker |
| String refinements (regex) | Correctly deferred to v0.2 |
| Custom effect handlers | Correctly deferred to v0.2 |
| LSP server | Stub only |
| `?` error propagation | Not in grammar |
| Trailing commas | Supported in grammar |
| Semicolons | Supported (optional in blocks) |
| Spread operator (`..`) | Not in grammar |
| Closures (variable capture) | Not in interpreter |
| `for` loops (runtime) | Not in interpreter |
| BDD testing framework | Not implemented (grammar has basic `test` only) |
| Module imports | In grammar, not in compiler |

### 5.3 Architectural Observations

1. **Direct tree-sitter node walking**: The compiler operates directly on tree-sitter `Node` values without constructing a typed AST. This is fine for a bootstrap phase but will become unwieldy as the language grows. The tech spec's mention of Rowan for a typed AST is a good future direction.

2. **No intermediate representation**: There's no IR between parsing and analysis. Each analysis pass re-walks the tree. For a bootstrap compiler this is acceptable, but it means no optimization opportunities and repeated traversal costs.

3. **Analysis passes are disconnected**: Types, effects, and refinements run independently. A unified semantic model would allow richer checking (e.g., knowing a variable's type when checking its refinement).

---

## 6. Recommendations (Prioritized)

### P0/P1 — RESOLVED

All P0 and P1 items have been fixed:

1. ~~Remove the debug `eprintln!` in `effects.rs:211`~~ DONE
2. ~~Fix stale "Rocket" references~~ DONE (diagnostics.rs, analysis/mod.rs, hello.bl)
3. ~~Fix the duplicate `has_effect` binding in `effects.rs`~~ DONE
4. ~~Fix the duplicate `name` declaration in `types.rs`~~ DONE
5. ~~Remove dead `Value` enum from `interpreter.rs`~~ DONE
6. ~~Fix function name extraction in effect checker~~ DONE (uses `child_by_field_name`)
7. ~~Add trailing comma support to grammar~~ DONE
8. ~~Add semicolons to grammar~~ DONE (optional in blocks)
9. ~~Fix `refinement_test.bl` to use valid syntax~~ DONE
10. ~~Add `Float` to grammar and type checker~~ DONE
11. ~~Clean up compiler warnings~~ DONE (0 blc warnings)
12. ~~Add division-by-zero guard in interpreter~~ DONE
13. ~~Remove dead functions from `effects.rs`~~ DONE
14. ~~Fix `.baseline` → `.bl` in language spec~~ DONE
15. ~~Fix `baselinec` → `blc` and Rowan/Salsa references in tech spec~~ DONE
16. ~~Fix section numbering in language spec~~ DONE (6.4→6.6, 6.5→6.7, 10.3→10.5/10.6)

### P2 — Address in Next Phase

1. Add sum type / enum syntax to the grammar
2. Add `?` operator for error propagation
3. Add spread operator (`..`) to grammar
4. Build a shared semantic model across analysis passes
5. Add Rust `#[test]` unit tests for the compiler
6. Implement closure variable capture in interpreter
