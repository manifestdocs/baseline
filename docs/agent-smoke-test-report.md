# Agent Smoke Test Report

**Date:** 2026-02-25
**Agent:** Claude Opus 4.6 (with llms.txt + CLAUDE.md context, no MCP server)
**Compiler:** blc v0.15-dev (JIT-only)

## Test Matrix

| # | Task | First Attempt | After Fix | Mistakes |
|---|------|:---:|:---:|----------|
| 1 | Hello World | PASS | — | None |
| 2 | Data Transform (pipes, HOFs) | PASS | — | None |
| 3 | Pattern Matching (Option) | FAIL | PASS | Curly braces on match |
| 4 | Error Handling (Result) | FAIL | PASS | Curly braces + `Int.parse` ghost |
| 5 | Refinement Types | PASS | — | None |
| 6 | Effects + Testing | PASS | — | None |
| 7 | Pipes + Composition | PASS | — | None |
| 8 | Records + Interpolation | FAIL | PASS | Named record update bug |

**First-attempt score: 5/8 (62.5%)**
**After correction: 8/8 (100%)**

## Common Agent Mistakes

### 1. Match Expression Syntax (HIGH FREQUENCY)
**Wrong:** `match expr { arm1 -> ..., arm2 -> ... }`
**Right:** `match expr` followed by indented arms without braces

Agents trained on Rust/JS/TS instinctively add curly braces around match arms.
This is the #1 mistake and should be prominently warned against.

### 2. Int.parse Ghost Function (MEDIUM FREQUENCY)
`Int.parse(s)` type-checks successfully but returns `()` at runtime.
The correct function is `String.to_int(s)` which returns `Option<Int>`.

This is a type-checker/runtime mismatch — the type checker accepts functions
it knows about even if the JIT doesn't have a native binding.

### 3. Named Record Update Bug (MEDIUM FREQUENCY)
**Wrong:** `TypeName { ..base, field: val }` — fields become `()` at runtime
**Right:** `{ ..base, field: val }` — anonymous spread works correctly

This is a JIT runtime bug, not a language design issue.

## Recommendations

1. **llms.txt**: Add match syntax warning to "Common Mistakes" section
2. **llms.txt**: Add `Int.parse` → `String.to_int` note
3. **llms.txt**: Add record update guidance (use anonymous spread)
4. **Rosetta**: Verify "pattern matching" entry shows correct syntax
5. **Compiler**: Fix `Int.parse` to either work at runtime or emit a diagnostic
6. **Compiler**: Fix named record update in JIT

## Test Programs

All 8 test programs are preserved as `tests/agent_smoke/smoke_N_*.bl`.
