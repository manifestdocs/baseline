# RFC: 100% JIT Correctness

**Status:** Draft
**Date:** 2026-02-22
**Motivation:** Hanabi benchmark submission requires correct output. The JIT currently segfaults on all three non-trivial benchmarks (binarytrees, nbody, fasta). The VM is 20-50x slower than CPython on these workloads, making the JIT the only viable competition backend.

---

## Problem

The JIT passes all 45 unit tests but crashes on every real program:

| Benchmark | Exit Code | Root Cause |
|-----------|-----------|------------|
| nbody | 139 (SIGSEGV) | Float arithmetic dispatched to `jit_int_*` helpers |
| fasta | 139 (SIGSEGV) | Same — `Float.from_int(x) / Float.from_int(y)` |
| binarytrees | 133 (SIGABRT) | RC lifecycle bug in deep enum destructuring |

The unit tests never exercise Float BinOps in JIT context because `is_scalar_only` excludes Float from the unboxed path, and the boxed tests use Int-only arithmetic.

## Root Cause Analysis

### P0: Float arithmetic crashes (nbody, fasta)

**Location:** `blc/src/vm/jit/compile.rs:872-948`

```rust
Expr::BinOp { op, lhs, rhs, ty } => {
    let is_int = matches!(ty, Some(Type::Int));
    // ...
    if is_int {
        // correct: jit_int_add, jit_int_sub, etc.
    } else {
        // BUG: Float falls here, also calls jit_int_add/sub/mul/div
        BinOp::Add => Ok(self.call_helper("jit_int_add", &[lhs_val, rhs_val])),
    }
}
```

The `else` branch handles **all** non-Int types (Float, String, Bool, Unknown) by calling `jit_int_*` helpers. These helpers call `nv_as_any_int()` which:

1. Checks if the NaN-boxed bits have `TAG_INT` tag — Float bits don't
2. Falls through to `NValue::borrow_from_raw(float_bits)` — interprets IEEE 754 double as an Arc heap pointer
3. Segfaults immediately

**Location:** `baseline-rt/src/helpers.rs:791-800`

```rust
fn nv_as_any_int(bits: u64) -> i64 {
    if bits & TAG_MASK == TAG_INT {
        ((bits << 16) as i64) >> 16       // inline int path
    } else {
        let nv = unsafe { NValue::borrow_from_raw(bits) };  // CRASH: float bits aren't a pointer
        nv.as_any_int()
    }
}
```

The `can_jit` analysis (`analysis.rs:58`) lets Float BinOps through without checking the type — it only validates structural compatibility, not type support.

### P1: Binarytrees RC bug

Binarytrees uses `type Tree = | Leaf | Node(Int, Tree, Tree)` with deep recursive matching:

```
match tree
  Leaf -> 0
  Node(_, l, r) -> 1 + check(l) + check(r)
```

The `Node(_, l, r)` pattern extracts fields via `jit_enum_field_get` (which does `Arc::clone` = incref), binds them in an RC scope, then the scope exit decrefs `l` and `r`, and the merge block decrefs `tree`. Under deep recursion (~18 levels = 2^18 nodes), this creates a cascade of decrefs that may trigger use-after-free if the incref/decref sequence isn't perfectly balanced.

Exit code 133 (SIGABRT) suggests a double-free or heap corruption detected by the allocator, consistent with RC imbalance.

### P2: Float comparison produces wrong results

Even for operations that don't crash (comparisons), the `else` branch does:

```rust
BinOp::Lt => {
    let a = self.untag_int(lhs_val);  // ishl 16 / sshr 16 on float bits = garbage
    let b = self.untag_int(rhs_val);
    let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
    Ok(self.tag_bool(cmp))
}
```

`untag_int` sign-extends the low 48 bits — meaningless for IEEE 754 doubles. This means `rf < threshold` in fasta's `select()` function returns garbage, producing wrong characters.

---

## Proposed Fix

### Phase 1: Float arithmetic (unblocks nbody + fasta)

Add a `Type::Float` branch to `BinOp` handling in `compile_expr`:

```rust
Expr::BinOp { op, lhs, rhs, ty } => {
    let is_int = matches!(ty, Some(Type::Int));
    let is_float = matches!(ty, Some(Type::Float));
    let lhs_val = self.compile_expr(lhs)?;
    let rhs_val = self.compile_expr(rhs)?;

    if is_int {
        // existing int path (unchanged)
    } else if is_float {
        // NEW: proper float path
        // NaN-boxed floats store raw IEEE 754 bits directly (below TAG_THRESHOLD)
        let lhs_f64 = self.builder.ins().bitcast(types::F64, MemFlags::new(), lhs_val);
        let rhs_f64 = self.builder.ins().bitcast(types::F64, MemFlags::new(), rhs_val);
        match op {
            BinOp::Add => {
                let result = self.builder.ins().fadd(lhs_f64, rhs_f64);
                Ok(self.builder.ins().bitcast(types::I64, MemFlags::new(), result))
            }
            BinOp::Sub => { /* fsub */ }
            BinOp::Mul => { /* fmul */ }
            BinOp::Div => { /* fdiv */ }
            BinOp::Mod => { /* call jit_float_mod helper (no Cranelift fmod) */ }
            BinOp::Lt => {
                let cmp = self.builder.ins().fcmp(FloatCC::LessThan, lhs_f64, rhs_f64);
                Ok(self.tag_bool(cmp))
            }
            // Gt, Le, Ge, Eq, Ne similarly
        }
    } else {
        // existing fallback for String, Bool, etc.
    }
}
```

This is zero-overhead — `bitcast` between I64 and F64 is a no-op on all architectures (same register file on x86 and aarch64 when using Cranelift's value type system). The NaN-boxing scheme already stores floats as raw IEEE 754 bits, so bitcast is semantically correct.

**Edge cases:**
- `Float % Float` needs a runtime helper (`fmod` from libm) — add `jit_float_mod` to `baseline-rt/src/helpers.rs`
- NaN comparisons: Cranelift's `fcmp` handles NaN correctly per IEEE 754 (unordered = false for all except `!=`)
- Result of float arithmetic is already a valid NaN-boxed float (raw bits below `TAG_THRESHOLD`) unless the result is NaN/Infinity — these are valid IEEE 754 bit patterns that are below `TAG_THRESHOLD` (0xFFFA...), so the NaN-boxing is preserved

### Phase 2: RC correctness for enum destructuring (unblocks binarytrees)

Audit the RC lifecycle in `bind_pattern_vars_rc` for multi-field Constructor patterns:

1. `jit_enum_field_get(subject, i)` does `Arc::clone` (incref) for each extracted field
2. `pop_rc_scope(Some(ret_var))` decrefs all locals except `ret_var`
3. Merge block decrefs the match subject

**Hypothesis:** The decref of the match subject drops the enum's internal payload array, which still holds the original (non-cloned) refs to `l` and `r`. Since `jit_enum_field_get` already cloned them, the subject's payload refs are extra. When the subject is decref'd and its strong_count hits 1, `Arc::drop` frees the payload array. But if JIT CoW raw-pointer mutations have modified the payload, the drop may corrupt memory.

**Fix approach:**
- Add a JIT test for `Node(_, l, r)` destructuring under recursion
- Instrument `jit_enum_field_get` and `jit_decref` with debug assertions checking strong_count > 0
- Bisect whether the crash is in the decref sequence or in the recursive call return path

### Phase 3: `can_jit` gate hardening

Add type-awareness to the analysis pass:

```rust
Expr::BinOp { ty, .. } => {
    match ty {
        Some(Type::Int) | Some(Type::Float) | Some(Type::Bool) | None => {
            // supported — recurse into children
        }
        _ => return false,  // reject unsupported types
    }
}
```

This prevents future crashes by rejecting types the codegen can't handle, rather than silently dispatching to wrong helpers.

---

## Verification Plan

### New JIT tests needed

```
jit_float_add          — Float + Float
jit_float_sub          — Float - Float
jit_float_mul          — Float * Float
jit_float_div          — Float / Float
jit_float_compare      — Float < Float, >=, ==, !=
jit_float_mixed_expr   — (a * b) + (c * d) chained float ops
jit_float_native_call  — Float.format, Math.sqrt on JIT floats
jit_enum_multi_field   — Node(Int, Tree, Tree) deep recursion
jit_nbody_correctness  — full nbody at input 1000 vs reference
jit_binarytrees_correctness — full binarytrees at input 10 vs reference
jit_fasta_correctness  — full fasta at input 1000 vs reference
```

### Benchmark gates

After fixes, re-run the hanabi benchmarks with `--jit` and verify:

1. Byte-identical output against reference files
2. JIT faster than VM on all three benchmarks
3. No segfaults or aborts at any input size

---

## Estimated Impact

| Benchmark | Current VM | Expected JIT | Expected vs C | Expected vs Python |
|-----------|-----------|-------------|---------------|-------------------|
| nbody 5M | 476s | ~0.3-0.5s | 1-2x | 30-50x faster |
| binarytrees 18 | 115s | ~2-4s | 5-10x | 1-2x faster |
| fasta 2.5M | 132s | ~1-2s | n/a | 1-2x faster |

The float fix alone (Phase 1) should make nbody and fasta competitive. Binarytrees requires the RC fix (Phase 2) and will likely remain the slowest benchmark due to allocation pressure.

---

## Priority

**Phase 1 (Float) is the highest-impact fix in the project.** It's a localized change (~50 lines in `compile.rs` + ~20 lines in `helpers.rs`), unblocks 2 of 3 benchmarks, and turns nbody from a segfault into a potential headline result (JIT was already 0.8x C on integer recursion benchmarks like tak).

Phase 2 (RC) is harder to debug and may require fundamental changes to how match arms handle enum payload lifetimes.

Phase 3 (analysis) is a safety net — should be done but doesn't unblock anything.

---

## Long-Term: Full JIT-Only Runtime

Once correctness is achieved, the aspiration remains to eliminate the bytecode VM entirely:

1. **Remaining `can_jit` blockers:** Lambda, WithHandlers/HandleEffect/PerformEffect, Expect, Pattern::List
2. **Evidence passing** already transforms effects into plain calls — wire it before JIT to dissolve effect nodes
3. **Prelude in Baseline** — move List.map/filter/fold from Rust natives to `.bl` implementations, eliminating HOF boundary crossings
4. **Prelude caching** — AOT-compile the standard library to a cached `.so`, `dlopen` at startup for zero JIT latency on stdlib

But that's a separate RFC. This one is about making `--jit` produce correct output for the programs we already have.
