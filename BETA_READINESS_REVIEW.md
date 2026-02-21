# Baseline Language — Beta Readiness Performance Review

**Date:** 2026-02-21
**Scope:** Compiler pipeline performance and generated code performance
**Codebase:** `blc` compiler (Rust), `baseline-rt` runtime, `tree-sitter-baseline` grammar

---

## Executive Summary

The Baseline compiler shows strong engineering fundamentals — NaN-boxed values, superinstructions, tail call optimization, Cranelift JIT, and a well-structured IR pipeline. The JIT backend produces **competitive-with-C code** on microbenchmarks (0.7x–1.6x of C). However, several performance issues need attention before a beta release, ranging from an `O(n)` field lookup on every record access to an optimizer equality check that allocates strings via `format!("{:?}")` in a hot loop.

**Verdict:** The architecture is sound for beta, but the issues below should be triaged. Tier 1 items are likely visible to users in real programs.

---

## 1. Benchmark Summary (reference data)

Source: `benchmarks/cpu/results/reference.md` — arm64 Darwin, median of 3 runs.

| Benchmark | JIT vs C | VM vs C | VM vs Python |
|-----------|----------|---------|--------------|
| fib(35) — call overhead | **0.9x** | 12.7x | 1.7x faster |
| tak(30,20,10) — deep recursion | **0.7x** | 33.5x | 1.0x (parity) |
| divsum(10000) — loop arithmetic | **1.6x** | 49.3x | 1.5x **slower** |

**Key takeaway:** The JIT is production-ready for numeric code. The VM interpreter is adequate for scripting but **loses to Python on loop-heavy arithmetic** (divsum), which is a poor signal for beta messaging. The divsum regression is likely caused by the `Arc::strong_count` overhead in `NValue::drop` (see §3.1).

---

## 2. Compiler Pipeline Performance

### 2.1 Parsing (tree-sitter)

**Status: Good.** Tree-sitter parsing is effectively free relative to later phases. The grammar (`tree-sitter-baseline/grammar.js`) declares 11 conflict pairs which is within normal range. External scanners handle string interpolation and call-expression disambiguation (newline-sensitive `(`). No performance issues identified.

`blc/src/parse.rs:38-80` — Pipeline: parse → type check → effect check → refinement check → closure cycle detection, all sequential and single-threaded.

### 2.2 Type Checking

**Status: Needs attention.**

- **No type-check result caching.** Each imported module is re-type-checked every time it's imported (`analysis/types/mod.rs:214-233`). Diamond dependencies (A→B, A→C, B→D, C→D) cause module D to be type-checked twice. For projects with deep import graphs, this is quadratic in the number of shared dependencies.

- **O(n) field lookup during type checking.** Record field access resolves fields by linear scan of `Vec<(String, Type)>` in `analysis/types/check_node.rs`. With struct-heavy code, this adds up.

- **No trait implementation caching.** Trait `impl` lookup is a linear search through the type map's `impls` vector in `vm/lower/mod.rs`. Each call to a trait method re-scans.

- **Excessive `Type` cloning.** The `Type` enum is `Clone`-heavy (contains `Vec`, `String`, `HashMap`). Many type-checking operations clone types unnecessarily rather than borrowing.

### 2.3 IR Lowering (`vm/lower/mod.rs`)

**Status: Adequate.** The lowerer is a single-pass CST-to-IR translation. Two minor issues:

- **O(n²) type-map scanning** for trait implementations: scans `Vec<Dict>` linearly for each method resolution.
- **No function deduplication**: if the same function appears in multiple contexts, it may be lowered twice.

### 2.4 IR Optimizer (`vm/optimize_ir.rs`, 1949 lines)

**Status: Has a critical bug.**

The optimizer runs 5 passes over max 3 rounds: constant propagation/folding, function inlining (threshold=30 nodes), dead let elimination, block simplification, and lambda lifting.

**Critical: `exprs_equal()` uses Debug formatting for structural comparison** (`optimize_ir.rs:1092-1094`):

```rust
fn exprs_equal(a: &Expr, b: &Expr) -> bool {
    format!("{:?}", a) == format!("{:?}", b)
}
```

This allocates two `String`s and formats the entire AST subtree every time it's called. It's used to check whether an inlining round changed the IR (to decide if another round is needed). For a module with 100 functions averaged at 50 nodes each, this formats ~5000 nodes × 2 strings × 3 rounds = ~30,000 allocations per compile. This should be replaced with a derived `PartialEq` on `Expr`.

**Other optimizer notes:**
- Inline threshold (30 nodes) is reasonable.
- Lambda lifting correctly transforms closures into top-level functions with `MakeClosure`/`GetClosureVar` nodes.
- No loop-invariant code motion, no strength reduction — acceptable for v0.1 but limits loop performance in the VM path.

### 2.5 Bytecode Codegen (`vm/codegen.rs`)

**Status: Good.** Clean two-pass compilation (register names, then compile bodies). Notable features:
- Constant pool deduplication via `HashMap<ConstKey, u16>` (`chunk.rs:229`)
- Small integer optimization: `LoadSmallInt(i16)` avoids constant pool for small values
- Type-directed int specialization in binops (`BinOp::Add` with `ty == Some(Type::Int)` → `Op::AddInt`)

### 2.6 Peephole Optimizer (`vm/chunk.rs:353-784`)

**Status: Good.** Post-codegen optimization with:
- Integer op specialization (generic `Add` → `AddInt` when operand is known-int)
- Superinstruction fusion: `GetLocal + LoadSmallInt + SubInt → GetLocalSubInt` (6 patterns + 2 compare-and-jump fusions)
- NOP compaction after fusion
- Dead code elimination (`LoadConst + Pop` → removed, `CloseScope(0)` → removed)
- `Op` enum is ≤8 bytes (tested with `op_size_at_most_8_bytes`)

---

## 3. Generated Code Performance

### 3.1 VM Interpreter (`vm/exec/mod.rs`)

**Status: Well-optimized core loop, but bottlenecked by data structure operations.**

**Strengths:**
- NaN-boxed values: 8 bytes per stack slot (vs 24-byte enum)
- Frame state kept in local variables (ip, chunk_idx, base_slot)
- Unsafe stack access eliminates bounds checks
- Instruction limit checked every 10,000 ops, not per-op
- Fixed-capacity `FrameStack` (1024 × 16 bytes = 16KB) with unchecked push/pop
- 16-byte call frames with bit-packed base+flags (`PackedBase`)
- Tail call optimization via `TailCall` opcode
- Superinstructions reduce dispatch overhead for common patterns

**`NValue::drop` — performance overhead in all builds** (`baseline-rt/src/nvalue.rs:649-668`):

```rust
fn drop(&mut self) {
    if self.is_heap() {
        let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
        let is_last = unsafe {
            let arc = Arc::from_raw(ptr);
            let count = Arc::strong_count(&arc);
            std::mem::forget(arc);
            count == 1
        };
        if is_last {
            ALLOC_STATS.frees.fetch_add(1, Ordering::Relaxed);
        }
        unsafe { Arc::decrement_strong_count(ptr); }
    }
}
```

Every drop of a heap `NValue` reconstructs an `Arc` from raw, reads `strong_count`, forgets it, then calls `decrement_strong_count` — two atomic operations where one would suffice. The `ALLOC_STATS` tracking is useful for debugging but should be gated behind `#[cfg(debug_assertions)]` or a feature flag. In tight loops with temporary heap values, this doubles the atomic operation count.

**Recommendation:** Gate the `strong_count` + `ALLOC_STATS` tracking behind `cfg(debug_assertions)`. In release builds, just call `Arc::decrement_strong_count(ptr)` directly.

### 3.2 Record/Struct Field Access — O(n) per access

**`dispatch_data.rs:172-215` (`GetField`):** Linear scan through `Vec<(RcStr, NValue)>` to find a field by name. Every `obj.field` in user code triggers this scan.

**`dispatch_data.rs:217-267` (`GetFieldIdx`):** Attempts O(1) index access with name verification fallback. This is the right approach, but the fallback still scans linearly. The codegen emits `GetFieldIdx` only when `field_idx` is resolved at compile time (`codegen.rs:303-312`), which requires type information — not always available.

**`dispatch_data.rs:402-465` (`UpdateRecord`):** Clones the entire field vector, then does a linear scan for each update field. For a record with `m` fields and `n` updates, this is `O(m + m*n)`.

**Recommendation:** For beta, this is acceptable for records with <20 fields. For production, consider a field-offset table or sorted fields with binary search.

### 3.3 List Operations

- **`ListConcat`** (`dispatch_data.rs:120-128`): Clones the entire left list, then extends. No structural sharing. `[a] ++ [b, c]` allocates a fresh list of size 3.
- **`ListTailFrom`** (`dispatch_data.rs:129-143`): Copies the tail slice into a new `Vec`. No O(1) slice sharing.

These are inherent to the `Vec`-backed list representation. Acceptable for beta; functional programs with heavy list manipulation will be slow.

### 3.4 Handler/Effect System

**`exec/mod.rs:498-517` (`find_handler`):** Handler lookup iterates the handler stack in reverse, then does a `HashMap::get` for the effect name, then a linear scan of record fields for the method name. The `HashMap` layer is efficient; the field scan is O(fields per handler).

**Continuation capture** (`HeapObject::Continuation`): Captures stack segments, frame segments, upvalue segments, and handler boundaries. This is a significant allocation but only happens on `perform` with non-tail-resumptive handlers — correct design choice.

### 3.5 JIT Compiler (Cranelift)

**Status: Excellent code quality, some compiler-side inefficiencies.**

**Generated code strengths:**
- Base-case speculation: recursive fib compiles the `n <= 1` case as straight-line code, falling back to general path on speculation failure
- Scalar replacement of aggregates (SRA): records/tuples with known layouts are decomposed into individual Cranelift values, avoiding heap allocation
- Tail calls via `return_call_indirect`
- NaN-box encoding/decoding directly in generated machine code

**Compiler-side issues (affect compile time, not runtime):**
- Sequential function compilation (`vm/jit/mod.rs`): functions are compiled one at a time. No parallelism. For large modules, this is a bottleneck.
- `func_names` HashMap rebuilt on every `run_jit` call instead of being cached with the module.
- Repeated HashMap lookups in hot compilation paths (`vm/jit/compile.rs`): variable lookups during codegen scan a HashMap per reference.
- String cloning during JIT compilation: function names and field names are cloned frequently during code generation.

These affect JIT compilation speed (startup latency) but not the quality of generated code. For ahead-of-time compilation, they contribute to overall compile time.

---

## 4. Prioritized Issue List

### Tier 1 — Fix Before Beta (User-Visible Performance)

| # | Issue | Location | Impact |
|---|-------|----------|--------|
| 1 | `NValue::drop` double-atomic in release builds | `nvalue.rs:649-668` | 2x atomic ops per heap value drop; explains VM divsum regression vs Python |
| 2 | `exprs_equal()` uses `format!("{:?}")` for comparison | `optimize_ir.rs:1092-1094` | Thousands of allocations per compile; easy fix (derive PartialEq) |
| 3 | No type-check caching for imported modules | `analysis/types/mod.rs:214-233` | Quadratic re-checking on diamond imports |

### Tier 2 — Fix for Beta Quality (Moderate Impact)

| # | Issue | Location | Impact |
|---|-------|----------|--------|
| 4 | O(n) record field lookup (GetField) | `dispatch_data.rs:172-215` | Every `obj.field` is linear scan; mitigated by GetFieldIdx but not always available |
| 5 | O(m×n) UpdateRecord | `dispatch_data.rs:402-465` | Record update clones + linear scan per update field |
| 6 | No trait impl lookup cache | `vm/lower/mod.rs` | Linear scan per trait method call |
| 7 | Sequential JIT compilation | `vm/jit/mod.rs` | Compile time for large modules |

### Tier 3 — Post-Beta (Design Improvements)

| # | Issue | Location | Impact |
|---|-------|----------|--------|
| 8 | Vec-backed lists with no structural sharing | `dispatch_data.rs:120-143` | List concat/tail is always O(n) copy |
| 9 | Excessive String/Type cloning in type checker | `analysis/types/` | Compile-time allocation pressure |
| 10 | No loop-invariant code motion in optimizer | `optimize_ir.rs` | Missed optimization for loop-heavy code |
| 11 | HashMap for handler stack effect lookup | `exec/mod.rs:96` | Allocates HashMap per handler scope; could use a flat assoc list for small N |
| 12 | `Arc<HeapObject>` for all heap values | `nvalue.rs` | Single-threaded VM pays for atomic refcounts; could use `Rc` (already `!Send`) |

### Tier 4 — Nice to Have

| # | Issue | Location | Impact |
|---|-------|----------|--------|
| 13 | Constant pool linear scan for Float/List/Record | `chunk.rs:280-283` | Rare types fall through HashMap to O(n) scan |
| 14 | No generic type schema memoization | `analysis/types/` | Generic instantiation re-evaluates constraints |
| 15 | Handler field lookup is O(fields) after HashMap | `exec/mod.rs:505-513` | Minor; handler records are typically small |

---

## 5. Architecture Assessment

### What's Working Well

1. **NaN-boxing** — 8-byte values with inline int/float/bool/unit/function-ref is a strong design. The encoding scheme is clean and the tag dispatch is efficient.

2. **Superinstructions** — The 8 fused opcodes (GetLocalSubInt, GetLocalLeIntJumpIfFalse, etc.) target the exact patterns that dominate recursive functions. This is why fib(35) at 12.7x C is respectable for an interpreter.

3. **Tail call optimization** — Both VM (TailCall opcode) and JIT (return_call_indirect) support TCO. The codegen detects self-recursive tail calls in the IR lowerer.

4. **JIT quality** — Cranelift JIT with base-case speculation and SRA produces code competitive with C. The 0.7x on tak (faster than C) is an artifact of Cranelift's register allocation on this particular benchmark but demonstrates the backend is not leaving performance on the table.

5. **Call frame design** — 16-byte packed frames with `PackedBase` bitfield is space-efficient. Fixed-capacity `FrameStack` avoids Vec capacity checks.

6. **Compilation caching** — Module resolver caches parsed trees; module compiler caches compiled chunks for diamond dependencies.

7. **Effect handler design** — Clean separation of tail-resumptive (no continuation capture) vs full handlers. One-shot continuations avoid the complexity of multi-shot.

### What Needs Work

1. **The gap between VM and JIT is too large.** 12.7x vs 0.9x on fib means the VM is ~14x slower than the JIT. Users who can't use JIT (unsupported platform, debugging) get a dramatically worse experience. The NValue::drop fix (Tier 1, #1) would help significantly.

2. **Record operations are the weak point.** O(n) field access, O(m×n) update, no field-offset table. For a language that encourages record-heavy programming, this will show up in real applications.

3. **Compile times will degrade with project size.** No type-check caching + sequential JIT compilation + the `exprs_equal` bug means compilation time grows super-linearly with codebase size.

---

## 6. Recommendations for Beta

### Must-Do (gate beta on these)

1. **Fix `NValue::drop`** — Gate `ALLOC_STATS` tracking behind `cfg(debug_assertions)`. Eliminates `Arc::from_raw` + `strong_count` + `forget` on every heap value drop in release builds. Expected improvement: 20-40% on VM divsum-class workloads.

2. **Fix `exprs_equal`** — Derive `PartialEq` on `Expr` (and sub-types) and use `a == b` instead of `format!("{:?}")`. This is a 5-minute fix that eliminates thousands of allocations per compile.

3. **Cache type-check results for imports** — Store type-check diagnostics keyed by canonical path. Return cached results on re-import. Prevents quadratic blowup on diamond dependencies.

### Should-Do (quality of life for beta users)

4. **Ensure `GetFieldIdx` coverage** — Audit the lowerer to ensure field indices are populated whenever type information is available. This converts O(n) field lookups to O(1) for the common case.

5. **Gate `Arc` usage** — The VM is explicitly `!Send` (uses `Rc` for upvalue_stack). Consider using `Rc<HeapObject>` instead of `Arc<HeapObject>` in the NValue implementation, or at minimum ensure the compiler eliminates the atomic overhead via escape analysis.

### Can Wait (post-beta)

6. Parallel JIT compilation
7. Persistent/structural-sharing lists
8. Loop optimizations in the IR optimizer

---

## 7. Conclusion

The Baseline compiler and runtime are architecturally well-designed for a v0.1 bootstrap. The JIT backend is a standout — producing code that matches or beats C on microbenchmarks is an achievement. The VM interpreter has solid fundamentals (NaN-boxing, superinstructions, TCO) but is held back by a few specific issues, particularly the `NValue::drop` overhead and `O(n)` record field access.

The three must-do fixes (NValue::drop, exprs_equal, type-check caching) are all localized changes with high impact-to-effort ratios. Addressing them would put the implementation in strong shape for a beta release.
