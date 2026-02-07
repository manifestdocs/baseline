# Baseline VM Optimization: Phases 4, 5, and 6

## Implementation Prompt for Coding Agent

This document specifies three phases of performance optimization for the Baseline bytecode VM. Each phase is independently implementable. Phases 4 and 5 are interpreter-only (no new dependencies). Phase 6 adds a Cranelift JIT compiler.

---

## Current Architecture Summary

**Pipeline:** tree-sitter CST -> Lowerer -> `IrModule` (Vec of `IrFunction`) -> `optimize_ir` passes -> Codegen -> bytecode `Program` (Vec of `Chunk`) -> VM execution

**VM:** Stack-based, NaN-boxed 8-byte `NValue` values, Rust `match` dispatch loop, `Op` enum at 6 bytes per instruction (2-byte aligned), ~49 opcodes. The dispatch loop is `run_frames()` in `vm.rs` with frame state (ip, chunk_idx, base_slot) hoisted into local variables.

**Current performance baseline (as of 2026-02-07):**
- fib(35) = 0.95s (release build, `cargo build --release`)
- ~233M dispatched operations/sec
- ~15 cycles per dispatch at 3.5 GHz
- 9.2M function calls for fib(35), ~12 ops per call

**Test counts (all must pass after each phase):**
- 494 lib tests (`cargo test --lib`)
- 145 conformance tests (`cargo test conformance`)
- 2 differential tests (`cargo test differential`)
- 42 integration tests (`cargo test --test integration_tests`)
- 4 property tests (`cargo test --test property_tests`)

**Key source files:**
- `/Users/alastair/work/baseline/blc/src/vm/vm.rs` -- VM dispatch loop, `CallFrame`, `run_frames()`
- `/Users/alastair/work/baseline/blc/src/vm/chunk.rs` -- `Op` enum (49 variants), `Chunk`, `Program`, peephole optimizer
- `/Users/alastair/work/baseline/blc/src/vm/codegen.rs` -- IR -> bytecode generation, `Codegen` struct
- `/Users/alastair/work/baseline/blc/src/vm/ir.rs` -- `IrModule`, `IrFunction`, `Expr` enum (30 variants)
- `/Users/alastair/work/baseline/blc/src/vm/optimize_ir.rs` -- IR-level constant propagation, folding, dead let elimination
- `/Users/alastair/work/baseline/blc/src/vm/lower.rs` -- CST -> IR lowering
- `/Users/alastair/work/baseline/blc/src/vm/nvalue.rs` -- NaN-boxed 8-byte value type
- `/Users/alastair/work/baseline/blc/src/vm/compiler.rs` -- legacy direct CST -> bytecode (still used for imports)
- `/Users/alastair/work/baseline/blc/src/vm/module_compiler.rs` -- multi-file compilation
- `/Users/alastair/work/baseline/blc/src/main.rs` -- CLI, `run_file_vm()` wires pipeline

**Benchmark program** (create this if it does not exist):
File: `/Users/alastair/work/baseline/tests/programs/tier1/fibonacci.bl`
```baseline
@prelude(script)

fibonacci : Int -> Int
fibonacci = |n| if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2)

main! : () -> {Console} ()
main! = || {
  Console.println!("fib(35) = ${fibonacci(35)}")
}
```

**How to benchmark:**
```bash
cargo build --release --bin blc
time ./target/release/blc run tests/programs/tier1/fibonacci.bl
```
Take the median of 5 consecutive runs. Discard first run (cold cache).

---

## Phase 4: Call Frame Optimization

### Goal

Reduce per-call overhead by 15-25% on call-heavy workloads (fib(35)). Current Call/Return handlers do unnecessary work: they push a function value onto the stack that must later be skipped during Return, they perform runtime type-checking on the function value, and they use a growable `Vec<CallFrame>` with capacity checks.

**Target: fib(35) in 0.72-0.80s** (down from 0.95s).

### Technique 4A: `CallDirect` Opcode

The current `Call(u8)` handler performs these steps every invocation:
1. Calculates `func_pos = stack.len() - n - 1`
2. Reads the function value from the stack via `get_unchecked(func_pos)`
3. Checks `is_function()` vs `is_heap()` (closure branch)
4. Extracts chunk index from the NValue
5. On Return, must `truncate(frame.base_slot - 1)` to skip past the function value slot

For direct calls to known functions (the common case), all of this is waste. The codegen already knows the chunk index at compile time.

**Add `CallDirect(u16, u8)` opcode** where the first operand is the chunk index and the second is the argument count. This opcode:
- Does NOT push a function value onto the stack
- Does NOT read or type-check any function value
- Directly uses the chunk index from the opcode
- Sets `base_slot = stack.len() - arg_count` (args start at top of stack, no function value underneath)

The corresponding `Return` handler needs a way to know whether the current frame used `CallDirect` (no function value to skip) vs `Call` (function value at `base_slot - 1`). Two approaches:

**Recommended approach:** Add a `has_func_slot: bool` field to `CallFrame` (or encode it in the high bit of `base_slot`). On Return, if the frame has no function slot, truncate to `base_slot` instead of `base_slot - 1`. The simplest encoding is to use bit 31 of the `base_slot` u32 as the flag, since maximum stack depth of ~16M slots is more than sufficient. Then:
- `CallDirect` pushes frame with `base_slot = (stack.len() - n) as u32` (bit 31 clear)
- `Call` pushes frame with `base_slot = ((func_pos + 1) as u32) | 0x8000_0000` (bit 31 set)
- Return reads: `let raw_base = frame.base_slot; let has_func = raw_base & 0x8000_0000 != 0; let base = (raw_base & 0x7FFF_FFFF) as usize;` then truncates to `base - 1` if `has_func`, else `base`.

This keeps `CallFrame` at 16 bytes (no size increase).

**Codegen changes:** In `codegen.rs`, `gen_call_direct()` currently emits:
```
LoadConst(func_value)
[args...]
Call(n)
```
Change to emit:
```
[args...]
CallDirect(chunk_idx, n)
```
This saves 1 instruction per direct call (the `LoadConst` for the function value).

`gen_var()` for function names (used in non-call contexts like `let f = fibonacci`) still uses `LoadConst(func_value)` -- only call sites change.

**Do not change `CallIndirect`**: calls through variables/closures still use `Call(u8)`.

### Technique 4B: Pre-allocated Frame Array

Currently `frames: Vec<CallFrame>` grows dynamically. For fib(35), frames are pushed/popped 9.2M times. Even though `Vec` amortizes allocations, the capacity check on every `push()` costs a branch.

**Replace `Vec<CallFrame>` with a fixed-size array:**
```rust
struct FrameStack {
    frames: Box<[CallFrame; MAX_CALL_DEPTH]>,
    len: usize,
}
```
Where `MAX_CALL_DEPTH = 1024`. This is 16 KB (1024 * 16 bytes) allocated once. Push and pop become unchecked index operations (guarded by the depth check that already exists in Call).

Initialize via `Box::new([CallFrame::default(); MAX_CALL_DEPTH])` where `CallFrame` derives `Default` (or initialize with zeroed memory via `MaybeUninit`).

The `FrameStack` must support:
- `push(frame)`: `self.frames[self.len] = frame; self.len += 1;` (unchecked -- caller already verified `len < MAX_CALL_DEPTH`)
- `pop() -> CallFrame`: `self.len -= 1; self.frames[self.len]` (Copy semantics, CallFrame is 16 bytes of plain data)
- `last() -> &CallFrame`: `&self.frames[self.len - 1]`
- `last_mut() -> &mut CallFrame`: `&mut self.frames[self.len - 1]`
- `len() -> usize`

Make `CallFrame` implement `Copy` and `Clone` (it is 4 u32 fields, trivially Copy). This avoids the `unwrap()` on `pop()` that currently exists.

### Technique 4C: Remove Redundant Function Value Push for Closures

When calling a closure via `Call(u8)`, the closure value is already on the stack from a prior `GetLocal`/`GetUpvalue`/`LoadConst`. After extracting the chunk index and upvalues during Call, the closure value sits uselessly at `base_slot - 1` until Return cleans it up. This is necessary for the current frame bookkeeping but slightly wasteful.

**Defer this to Phase 5** -- the `CallDirect` change handles the common path (known functions). Closure calls remain as-is for Phase 4.

### Files to Modify

| File | Changes |
|------|---------|
| `blc/src/vm/chunk.rs` | Add `CallDirect(u16, u8)` to `Op` enum. Add it to `offset_chunk_refs` (the u16 is a chunk index that needs offsetting during import merging). Update `has_int_operand` and `fuse_superinstructions` if they need to recognize it. |
| `blc/src/vm/vm.rs` | Add handler for `CallDirect`. Modify `CallFrame` to derive `Copy, Clone, Default`. Create `FrameStack` struct. Refactor `run_frames()` to use `FrameStack`. Modify `Return` handler to distinguish direct vs indirect frames (bit 31 encoding). |
| `blc/src/vm/codegen.rs` | Change `gen_call_direct()` to emit `CallDirect(chunk_idx, n)` instead of `LoadConst + Call`. |
| `blc/src/vm/compiler.rs` | If the legacy compiler also emits direct calls, apply the same change. If not, leave it -- imports compiled via legacy path will still use `Call`. |
| `blc/src/vm/module_compiler.rs` | Ensure `offset_chunk_refs` handles `CallDirect` chunk index offsetting for imports. |

### Acceptance Criteria

**Functional Correctness:**
- AC-4.1: ALL 494 lib tests MUST pass: `cargo test --lib`
- AC-4.2: ALL 145 conformance tests MUST pass: `cargo test conformance`
- AC-4.3: ALL 42 integration tests MUST pass: `cargo test --test integration_tests`
- AC-4.4: ALL 4 property tests MUST pass: `cargo test --test property_tests`
- AC-4.5: ALL 2 differential tests MUST pass: `cargo test differential`
- AC-4.6: The fib(35) program MUST produce the correct result: `9227465`
- AC-4.7: The `CallDirect` opcode MUST NOT be emitted for closure calls or calls through variables. It MUST only be emitted when the callee is a statically-known function name resolved in `self.functions`.
- AC-4.8: `CallFrame` MUST remain 16 bytes or smaller: add `static_assert!(std::mem::size_of::<CallFrame>() <= 16)` as a compile-time check in a test.
- AC-4.9: Recursive functions MUST work correctly with `CallDirect` (fibonacci, factorial). The function's own chunk index MUST be available during codegen of its body.
- AC-4.10: Stack overflow detection MUST still work. The frame depth check (`self.frames.len() >= MAX_CALL_DEPTH`) MUST remain in both `Call` and `CallDirect` handlers.
- AC-4.11: Programs with imports MUST still work. The `offset_chunk_refs` method MUST offset the chunk index in `CallDirect` operands.
- AC-4.12: Closure calls (via `Call`) MUST be unaffected. The `MakeClosure` + `Call` path MUST continue to work.
- AC-4.13: The `TailCall` opcode MUST be unaffected (it already reuses the current frame, does not push a new one).
- AC-4.14: HOF dispatch (`dispatch_hof` / `call_nvalue`) MUST continue to work -- these use `Call` via internal frame manipulation.

**Performance:**
- AC-4.15: fib(35) MUST complete in under 0.82s on a release build (`cargo build --release`). Measure as median of 5 runs.
- AC-4.16: fib(35) SHOULD complete in under 0.76s. If it does not, document the measured time and proceed.

**Code Quality:**
- AC-4.17: `cargo clippy --all-targets` MUST produce zero errors.
- AC-4.18: `cargo fmt --check` MUST pass.
- AC-4.19: No new `unsafe` blocks beyond what is strictly necessary for the `FrameStack` array access. Each `unsafe` block MUST have a `// SAFETY:` comment explaining the invariant.

### Verification Steps

```bash
# 1. Build
cargo build --release --bin blc

# 2. Run all tests
cargo test --manifest-path blc/Cargo.toml

# 3. Benchmark (median of 5 runs)
for i in 1 2 3 4 5; do time ./target/release/blc run tests/programs/tier1/fibonacci.bl; done

# 4. Verify correct output
./target/release/blc run tests/programs/tier1/fibonacci.bl 2>&1 | grep "fib(35)"
# Expected: fib(35) = 9227465

# 5. Code quality
cargo clippy --manifest-path blc/Cargo.toml --all-targets
cargo fmt --manifest-path blc/Cargo.toml --check
```

### Rollback Criteria

If fib(35) performance is slower than 0.95s (the pre-change baseline) after implementing all of Phase 4, revert. Likely cause: the bit-encoding of `base_slot` introduces branch mispredictions in Return. Fallback: add a separate `frame_has_func: Vec<bool>` side-array instead of bit-packing, and re-measure. If still slower, revert `FrameStack` and keep only `CallDirect`.

---

## Phase 5: Bytecode Compaction and Dispatch Tightening

### Goal

Reduce bytecode size for better I-cache utilization and reduce per-dispatch overhead. Current `Op` enum is 6 bytes per instruction (Rust enum discriminant + payload). For fib(35), the hot chunk is ~12 instructions = 72 bytes, which fits in a single cache line pair. The benefit of compaction is marginal for fib but significant for larger programs.

More importantly, this phase eliminates no-op instructions (`Jump(0)`) left behind by superinstruction fusion, and adds new fused opcodes for the remaining hot patterns.

**Target: fib(35) in 0.65-0.72s** (down from Phase 4's 0.72-0.80s, total ~30% improvement from baseline).

### Technique 5A: Eliminate Jump(0) No-ops

The current superinstruction fusion pass (`fuse_superinstructions` in `chunk.rs`) replaces the first two instructions of a fused triple with `Jump(0)` no-ops. This means the VM dispatches two wasted no-op jumps for every superinstruction. For fib(35), this adds ~18.4M wasted dispatches (two per call for the `GetLocalLeInt` and `GetLocalSubInt` fusions).

**Fix:** After fusion, run a compaction pass that removes `Jump(0)` entries from the code array and adjusts all jump offsets accordingly. This requires:

1. Build a mapping from old indices to new indices
2. Walk the code array, removing `Jump(0)` entries
3. Adjust all `Jump(u16)`, `JumpIfFalse(u16)`, `JumpIfTrue(u16)`, `JumpBack(u16)` offsets based on how many no-ops were removed in the jump's target range
4. Update the `source_map` array to match the compacted code

Add this as a new method `compact_nops(&mut self)` on `Chunk`, called after `fuse_superinstructions()` in `Program::optimize()`.

### Technique 5B: `CallDirectLeInt` Superinstruction

After Phase 4, the fib(35) hot loop becomes approximately:
```
GetLocalLeInt(0, 1)      ; n <= 1
JumpIfFalse(offset)
GetLocal(0)              ; return n
Return
; else:
GetLocalSubInt(0, 1)     ; n - 1
CallDirect(fib_idx, 1)   ; fibonacci(n-1)
GetLocalSubInt(0, 2)     ; n - 2
CallDirect(fib_idx, 1)   ; fibonacci(n-2)
AddInt                   ; +
Return
```

The pattern `GetLocalLeInt(slot, k) + JumpIfFalse(offset)` appears in every recursive call's base case check. Fuse this into:

**`GetLocalLeIntJumpIfFalse(u16, i16, u16)`** -- tests `local[slot] <= k` and jumps if false, without pushing/popping a bool value on the stack. This saves 1 push + 1 pop + 1 dispatch per call.

Implementation:
```rust
Op::GetLocalLeIntJumpIfFalse(slot, k, offset) => {
    let idx = base_slot + slot as usize;
    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
    if val > k as i64 {
        ip += offset as usize;
    }
}
```

Note: This is a 3-field opcode. The `Op` enum already has 3-field variants (e.g., `MakeClosure(u16, u8)`). Adding `(u16, i16, u16)` will increase `Op` size from 6 to 8 bytes. **This is acceptable** -- the extra 2 bytes per instruction are offset by fewer total instructions after compaction.

Verify that `sizeof(Op)` after adding this variant is 8 bytes. If Rust pads it further, consider encoding the jump offset in the i16 range (max 32K instruction forward jump, which is sufficient for all practical functions).

**Alternative encoding if 8 bytes is too large:** Encode as `GetLocalLeIntJumpIfFalse(u16, i16)` where the jump offset is embedded as the *next* instruction's immediate. But this is more complex. Prefer the direct 3-field encoding.

Fusion pattern in `fuse_superinstructions`:
```
GetLocalLeInt(slot, k) + JumpIfFalse(offset) => GetLocalLeIntJumpIfFalse(slot, k, offset)
```
The two original instructions become `Jump(0)` + the new superinstruction. After compaction (5A), the no-op is removed.

### Technique 5C: `GetLocalLtIntJumpIfFalse` Superinstruction

Same pattern for strict less-than:
```
GetLocalLtInt(slot, k) + JumpIfFalse(offset) => GetLocalLtIntJumpIfFalse(slot, k, offset)
```
Less commonly hit than LeInt but still a useful pattern.

### Technique 5D: Peephole NOP Elimination for Unfused Paths

In addition to `Jump(0)` from fusion, there are other patterns that generate unnecessary instructions:

1. `LoadConst(idx)` immediately followed by `Pop` (dead code after DCE, or unused expression results). These can be eliminated entirely during peephole.
2. `CloseScope(0)` -- no-op scope close. Should never be emitted but guard against it.

Add a `peephole_cleanup(&mut self)` pass on `Chunk` that removes these patterns. Run it after `compact_nops`.

### Files to Modify

| File | Changes |
|------|---------|
| `blc/src/vm/chunk.rs` | Add `GetLocalLeIntJumpIfFalse(u16, i16, u16)` and `GetLocalLtIntJumpIfFalse(u16, i16, u16)` to `Op`. Add `compact_nops()` method. Extend `fuse_superinstructions()` to detect the new patterns. Add `peephole_cleanup()`. Update `Program::optimize()` to call the new passes in order: `specialize_int_ops` -> `fuse_superinstructions` -> `compact_nops` -> `peephole_cleanup`. |
| `blc/src/vm/vm.rs` | Add dispatch handlers for the two new superinstructions. |
| `blc/src/vm/codegen.rs` | No changes needed -- fusion happens post-codegen. |

### Acceptance Criteria

**Functional Correctness:**
- AC-5.1: ALL tests from AC-4.1 through AC-4.5 MUST pass.
- AC-5.2: fib(35) MUST produce `9227465`.
- AC-5.3: The compaction pass MUST NOT change program semantics. Specifically: all jump targets MUST resolve to the same logical instruction as before compaction.
- AC-5.4: The compacted bytecode for any chunk MUST NOT contain any `Jump(0)` instructions.
- AC-5.5: `source_map` MUST remain correctly indexed after compaction. Error messages MUST still point to correct source locations. Verify by running: `./target/release/blc run tests/conformance/runtime_error.bl` (or any conformance test that triggers a runtime error) and checking that the error line/column is correct.
- AC-5.6: The new superinstructions MUST NOT be emitted for `JumpIfTrue` (only `JumpIfFalse`).
- AC-5.7: Programs with no fusable patterns MUST NOT be affected. The compaction pass MUST be a no-op when there are no `Jump(0)` instructions.

**Performance:**
- AC-5.8: fib(35) MUST complete in under 0.74s (combining Phase 4 + 5).
- AC-5.9: fib(35) SHOULD complete in under 0.68s. If it does not, document the measured time and proceed.
- AC-5.10: The total instruction count for fib(35)'s hot chunk (the fibonacci function body) MUST be fewer instructions than before compaction. Add a test or print the chunk size during compilation when an env var `BLC_DUMP_CHUNKS=1` is set.

**Code Quality:**
- AC-5.11: `cargo clippy --all-targets` MUST produce zero errors.
- AC-5.12: `cargo fmt --check` MUST pass.
- AC-5.13: The `compact_nops` pass MUST have unit tests covering: (a) no-ops in the middle, (b) no-ops at the start, (c) jump targets that span removed no-ops, (d) backward jumps (JumpBack), (e) empty chunk (no-op).
- AC-5.14: `sizeof(Op)` MUST be at most 8 bytes. Add a compile-time assertion. If adding the 3-field superinstruction makes Op larger than 8 bytes, use the alternative 2-field encoding described in 5B.

### Verification Steps

```bash
# 1. Build
cargo build --release --bin blc

# 2. Run all tests
cargo test --manifest-path blc/Cargo.toml

# 3. Benchmark (median of 5 runs, Phases 4+5 combined)
for i in 1 2 3 4 5; do time ./target/release/blc run tests/programs/tier1/fibonacci.bl; done

# 4. Verify no Jump(0) in output
BLC_DUMP_CHUNKS=1 ./target/release/blc run tests/programs/tier1/fibonacci.bl 2>&1 | grep "Jump(0)" | wc -l
# Expected: 0

# 5. Code quality
cargo clippy --manifest-path blc/Cargo.toml --all-targets
cargo fmt --manifest-path blc/Cargo.toml --check
```

### Rollback Criteria

If `sizeof(Op)` exceeds 8 bytes due to the 3-field superinstructions, drop Techniques 5B and 5C (the fused compare-and-jump opcodes) and keep only 5A (NOP compaction) and 5D (peephole cleanup). These alone should give 5-10% improvement. The compare-and-jump fusion can be revisited with a different encoding (e.g., a separate `Op::CompareAndJump` that reads its operands differently).

If total Phase 4+5 performance is slower than Phase 4 alone, revert Phase 5 entirely. The most likely cause is that 8-byte Op (vs 6-byte) hurts I-cache for larger programs. Verify by benchmarking a non-fib program (e.g., sorting, string manipulation) to confirm the regression is not fib-specific.

---

## Phase 6: Cranelift JIT Compilation

### Goal

Compile hot functions to native machine code via Cranelift, achieving 3-5x speedup over the bytecode interpreter for compute-intensive code.

**Target: fib(35) in 0.15-0.25s** (matching LuaJIT interpreter / OCaml bytecode territory).

This is a separate milestone from Phases 4-5. It adds a new dependency (`cranelift-codegen`, `cranelift-frontend`, `cranelift-module`, `cranelift-jit`) and a new module (`blc/src/vm/jit.rs`). The JIT operates as an optional backend: if enabled, it compiles functions to native code before execution. If disabled (or if JIT compilation fails for a function), the bytecode interpreter runs as a fallback.

### Architecture

```
IrModule (from lowerer + optimizer)
    |
    +--> Codegen --> Program (bytecode) --> VM interpreter (fallback)
    |
    +--> JIT compiler --> native code --> direct execution
```

The JIT compiler operates on `IrFunction` from the IR, NOT on bytecode. This is important: the IR is a better compilation input than bytecode because it preserves structure (if/else, match, let bindings) that would need to be reconstructed from flat bytecode. The IR also carries `Option<Type>` on every node, enabling type-directed code generation.

### Technique 6A: Function-at-a-Time JIT

For each `IrFunction` in the module:
1. Create a Cranelift `Function` with the correct signature (all args are `i64` -- the NaN-boxed representation)
2. Lower each `Expr` node to Cranelift IR
3. Compile to native code
4. Store the compiled function pointer in a dispatch table

**Calling convention:** All values are passed as raw `i64` (the NaN-boxed `NValue` bit pattern). This means the JIT does not need to unbox/rebox values at call boundaries when calling other JIT-compiled functions. When calling native functions or falling back to the interpreter, a thin wrapper handles the ABI translation.

**Integer fast path:** When the IR type annotation says `Some(Type::Int)`, the JIT can emit native integer arithmetic directly (no NaN-boxing overhead within the function body). On function entry, it extracts the i64 from the NaN-boxed value; on return, it re-boxes.

### Technique 6B: Selective Compilation

Not all functions benefit from JIT compilation. For v0.1:
- Compile ALL functions in the module that have more than N IR nodes (threshold: 5). Functions smaller than this (trivial getters, constructors) are not worth the compile time.
- If a function contains constructs the JIT does not handle (closures with upvalues, try expressions, HOF calls), fall back to bytecode for that function only.
- The `--jit` CLI flag enables JIT compilation. Without it, the bytecode interpreter runs (preserving backward compatibility).

### Technique 6C: JIT-to-Interpreter Fallback

When JIT-compiled code needs to call a function that was NOT JIT-compiled (e.g., a closure, or a native function), it must re-enter the interpreter. This is done via a trampoline:

```rust
/// Called from JIT code when it needs to invoke a non-JIT function.
extern "C" fn jit_trampoline(vm: *mut Vm, chunk_idx: u32, args: *const i64, nargs: u32) -> i64 {
    // Push args to VM stack, create frame, call run_frames, return result as i64
}
```

For fib(35), ALL calls are to the same function (fibonacci), so if fibonacci is JIT-compiled, there are ZERO fallbacks. This is the ideal case.

### Files to Create/Modify

| File | Changes |
|------|---------|
| `blc/Cargo.toml` | Add optional `cranelift-*` dependencies behind a `jit` feature flag: `cranelift-codegen`, `cranelift-frontend`, `cranelift-module`, `cranelift-jit`. Use `[features] jit = ["dep:cranelift-codegen", ...]`. |
| `blc/src/vm/jit.rs` (NEW) | JIT compiler: takes `&IrModule`, produces a `JitProgram` with function pointers. Core types: `JitCompiler`, `JitProgram`, `CompiledFunction`. |
| `blc/src/vm/jit_lower.rs` (NEW) | IR expression -> Cranelift IR lowering. Handles: Int/Float/Bool/Unit literals, Var (as Cranelift variables), BinOp (native arithmetic with type-directed specialization), If (Cranelift block branching), CallDirect (direct call to other JIT functions), CallNative (via trampoline), MakeEnum/MakeList/etc (via helper function calls). |
| `blc/src/vm/mod.rs` | Add `#[cfg(feature = "jit")] pub mod jit;` and `#[cfg(feature = "jit")] pub mod jit_lower;`. |
| `blc/src/main.rs` | Add `--jit` flag to `Commands::Run`. When enabled, attempt JIT compilation before falling back to interpreter. |

### Scope Limitations for v0.1 JIT

The JIT MUST handle:
- Integer and float arithmetic (Add, Sub, Mul, Div, Mod, Negate)
- Comparisons (Eq, Ne, Lt, Gt, Le, Ge) -- integer fast path when type is known
- Boolean logic (And, Or, Not)
- If/else
- Let bindings
- Direct function calls (CallDirect)
- Tail calls (TailCall)
- Return

The JIT MUST gracefully fall back to interpreter for:
- Closures (Lambda with captures)
- Match expressions
- For loops
- Try expressions
- HOF calls (List.map, List.filter, etc.)
- String operations
- Record/Tuple/List/Enum construction
- Field access
- Native function calls (via trampoline)

Functions containing any fall-back-required expression are compiled entirely via the bytecode path. There is no mixed-mode execution within a single function in v0.1.

### Acceptance Criteria

**Functional Correctness:**
- AC-6.1: ALL tests from AC-4.1 through AC-4.5 MUST pass with `--features jit` (and without -- the feature is additive).
- AC-6.2: `cargo test --features jit` MUST pass all tests.
- AC-6.3: fib(35) run with `--jit` MUST produce `9227465`.
- AC-6.4: Programs that the JIT cannot handle MUST silently fall back to the bytecode interpreter. No error, no warning (unless `BLC_TRACE_JIT=1` env var is set, in which case print which functions were JIT-compiled and which fell back).
- AC-6.5: `--jit` without `--features jit` in the build MUST produce a clear error message: "JIT compilation requires the 'jit' feature. Rebuild with: cargo build --features jit".
- AC-6.6: All existing tests MUST pass WITHOUT `--features jit` (the JIT is purely additive).
- AC-6.7: The JIT MUST handle recursive functions correctly (fibonacci is the primary test case).
- AC-6.8: The JIT MUST respect the same MAX_CALL_DEPTH limit as the interpreter for stack overflow detection.
- AC-6.9: Memory allocated by the JIT (compiled code pages) MUST be freed when the `JitProgram` is dropped. Cranelift's `JITModule` handles this automatically -- verify by running under Valgrind or Miri.

**Performance:**
- AC-6.10: fib(35) with `--jit` MUST complete in under 0.30s.
- AC-6.11: fib(35) with `--jit` SHOULD complete in under 0.20s.
- AC-6.12: JIT compilation time for fib(35)'s module (2 functions: fibonacci + main!) MUST be under 50ms. This is startup overhead -- measure separately from execution time.
- AC-6.13: Programs that fall back to the interpreter MUST NOT be slower than running without `--jit`. The fallback path MUST have zero overhead (just the initial JIT attempt, which is bounded by AC-6.12).

**Code Quality:**
- AC-6.14: `cargo clippy --all-targets --features jit` MUST produce zero errors.
- AC-6.15: `cargo fmt --check` MUST pass.
- AC-6.16: The JIT module MUST have at least 10 unit tests covering: integer arithmetic, comparisons, if/else, function calls, tail calls, and the fallback path.
- AC-6.17: The JIT MUST NOT use `unsafe` beyond what Cranelift requires for calling compiled code (the `transmute` to function pointer). Each unsafe block MUST have a SAFETY comment.
- AC-6.18: The Cranelift dependency MUST be behind a feature flag. The default build (`cargo build`) MUST NOT pull in Cranelift.

### Verification Steps

```bash
# 1. Build with JIT
cargo build --release --bin blc --features jit

# 2. Run all tests (with and without jit feature)
cargo test --manifest-path blc/Cargo.toml
cargo test --manifest-path blc/Cargo.toml --features jit

# 3. Benchmark JIT vs interpreter
echo "=== Interpreter ==="
for i in 1 2 3 4 5; do time ./target/release/blc run tests/programs/tier1/fibonacci.bl; done

echo "=== JIT ==="
for i in 1 2 3 4 5; do time ./target/release/blc run tests/programs/tier1/fibonacci.bl --jit; done

# 4. Verify fallback works (use a program with closures/match)
./target/release/blc run examples/hello.bl --jit  # Should work via fallback

# 5. Verify correct output
./target/release/blc run tests/programs/tier1/fibonacci.bl --jit 2>&1 | grep "fib(35)"
# Expected: fib(35) = 9227465

# 6. Code quality
cargo clippy --manifest-path blc/Cargo.toml --all-targets --features jit
cargo fmt --manifest-path blc/Cargo.toml --check

# 7. Check JIT compile time
BLC_TRACE_JIT=1 time ./target/release/blc run tests/programs/tier1/fibonacci.bl --jit
# Should show: "JIT: compiled fibonacci (Nms), compiled __main (Nms)" or similar
```

### Rollback Criteria

If Cranelift compilation time exceeds 200ms for the fibonacci module, investigate whether function complexity analysis is working (we should not be JIT-compiling trivially small functions). If compilation time remains high, raise the IR node threshold from 5 to 20.

If JIT-compiled fib(35) is slower than 0.30s, the IR-to-Cranelift lowering likely has inefficiencies (unnecessary boxing/unboxing at every operation). Verify that the integer fast path is active by checking that the IR type annotations are flowing through (they should be `Some(Type::Int)` for all arithmetic in fibonacci). If types are missing (`None`), the issue is in the lowerer, not the JIT.

If the Cranelift dependency causes issues (version conflicts, build time, binary size), consider `cranelift-codegen = { version = "0.x", default-features = false }` to minimize features. The JIT only needs the host architecture's backend.

---

## Implementation Order

**Recommended sequence:**
1. Phase 4 first (1-2 days). Delivers the biggest interpreter win.
2. Phase 5 second (1-2 days). Builds on Phase 4's `CallDirect` to create better fusion patterns.
3. Phase 6 last (3-5 days for a working prototype). Independent of Phases 4-5 but benefits from the cleaner IR that those phases validate.

Phases 4 and 5 can be implemented and shipped as a single PR. Phase 6 should be a separate PR due to the new dependency.

## Summary of Expected Performance Progression

| State | fib(35) Time | vs Baseline |
|-------|-------------|-------------|
| Current (pre-Phase 4) | 0.95s | 1.00x |
| After Phase 4 | 0.72-0.80s | 1.19-1.32x |
| After Phase 4+5 | 0.65-0.72s | 1.32-1.46x |
| After Phase 6 (JIT) | 0.15-0.25s | 3.8-6.3x |
| Reference: LuaJIT -joff | 0.24s | 3.96x |
| Reference: OCaml bytecode | 0.25s | 3.80x |
| Reference: Node.js V8 | 0.075s | 12.7x |

## Appendix: Benchmark Program for Validation

Ensure this file exists at `/Users/alastair/work/baseline/tests/programs/bench/fib35.bl`:

```baseline
fibonacci : Int -> Int
fibonacci = |n| if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2)

main : () -> Int
main = fibonacci(35)
```

This version has NO side effects (no Console, no prelude), so it returns the raw integer. Use it for clean benchmarking:

```bash
time ./target/release/blc run tests/programs/bench/fib35.bl
```

The output should be exactly: `9227465`
