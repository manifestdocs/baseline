# Interpreter Deprecation Plan

The tree-walking interpreter (`interpreter.rs`) is deprecated in favor of the bytecode VM.

## Phase 1 — Soft Deprecation (Done)

- `blc run` defaults to VM (already was the case)
- `--interp` flag emits a deprecation warning
- Help text marks `--interp` as "debug only, deprecated"

## Phase 2 — Migrate Test Runner

Move `test_runner.rs` from using `interpreter::eval` to using the VM-based
test runner (`vm::test_runner`). The `blc test` command already supports both
via `--interp`, so this is a matter of removing the interpreter path.

Requires:
- Verify VM test runner covers all test features (assertions, mocking, etc.)
- Update `blc test` to only use VM

## Phase 3 — Extract & Remove

- Extract `RuntimeValue` into a shared crate or rename to `Value`
- Remove `interpreter.rs` (3362 lines)
- Remove `run_file()` and `inject_imports()` from `main.rs`
- Clean up stdlib modules that reference `interpreter::RuntimeValue`
