# RFC: Clone-on-Write Optimizations for Heap-Heavy Workloads

**Status:** Implemented
**Date:** 2026-02-21

## Problem

Baseline's heap-heavy benchmarks are 16-33x slower than C/Go. Every "mutating" operation on Maps, Lists, and Records clones the entire data structure because Baseline is functionally immutable. This makes Baseline uncompetitive for real-world programs that use data structures.

### Benchmark Results (2026-02-21, Apple Silicon)

| Benchmark | C | Go | Python | Baseline VM | Baseline JIT | VM vs C |
|-----------|---|-----|--------|-------------|-------------|---------|
| primes | 0.019s | 0.025s | 0.422s | 0.367s | 0.022s | 19x |
| mapbuild | 0.021s | 0.013s | 0.037s | 0.337s | 0.504s | 16x |
| mergesort | — | 0.020s | 0.040s | 0.253s | N/A | 16x |
| treemap | 0.026s | 0.027s | 0.213s | 0.872s | 0.628s | 33x |
| strrev | 0.016s | 0.015s | 0.031s | 0.032s | 0.030s | 2x |

Pure-compute benchmarks (fib, tak, divsum, primes) are competitive. Heap-heavy benchmarks (mapbuild, treemap, mergesort) are not. Baseline is **slower than Python** on mapbuild — the worst result. Additionally, the JIT produces incorrect results for mergesort, and string operations are bottlenecked by native call overhead.

### Root Cause Analysis

#### Memory/Clone Overhead
1. **Map.insert clones the entire HashMap on every call** (`natives/map.rs:15`). The mapbuild benchmark does 20,000 inserts, so total work is O(n^2).
2. **List HOFs (map, filter, fold) clone the input list** (`hof.rs:53,84,114`). The clone is unnecessary — the list is alive on the stack frame and only needs read access.
3. **ListConcat and ListTailFrom always allocate new lists** (`dispatch_data.rs:120-162`). Mergesort's hot path is `List.concat([x], tail)` which allocates a new list per merge step.
4. **List.reverse, List.sort, List.concat natives clone** (`natives/list.rs`). Same unnecessary clone pattern.

#### JIT Correctness & General Overhead
5. **JIT `is_scalar_only` Bug**: The `mergesort` benchmark fails on the JIT because `type_is_scalar` optimistically returns `true` when type information is missing (`None`). Thus, generic list operations may be improperly classified as unboxed scalars, breaking heap pointers.
6. **String Operations Overhead**: `strrev` is slower because string operations map onto expensive native Rust dispatch mechanisms involving large boxing/unboxing costs instead of specialized inline logic.

## Proposal 1: Clone-on-Write (CoW) for Heap Operations

When the VM holds the **only reference** to a heap object (`Arc::strong_count == 1`), it can mutate in place instead of cloning. This preserves immutability semantics because no other code can observe the mutation.

### Step 1: `try_unwrap_heap` primitive

Add to NValue:
```rust
pub fn try_unwrap_heap(mut self) -> Result<HeapObject, Self>
```

Uses `Arc::try_unwrap`. On success (sole owner), returns the inner HeapObject. On failure, re-leaks the Arc and returns `Err(self)`.

**Files:** `blc/src/vm/nvalue.rs`, `baseline-rt/src/nvalue.rs`

### Step 2: Owning call convention for natives

Current native functions receive `&[NValue]` (borrowed). CoW requires ownership. Add a `FLAG_OWNING` dispatch path.

- New flag: `FLAG_OWNING: u8 = 0x08`
- New type: `OwningFn = fn(Vec<NValue>) -> Result<NValue, NativeError>`
- New dispatch: owning natives drain args off the stack before calling

**Files:** `blc/src/vm/natives/mod.rs`, `blc/src/vm/exec/dispatch_call.rs`

### Step 3: CoW Map.insert / Map.remove

Register with `FLAG_OWNING`. In the owning variant, call `try_unwrap_heap` on the map. If sole owner, mutate HashMap in-place (`O(1)`). Otherwise fall back to clone (`O(n)`).

**Expected impact:** mapbuild O(n^2) -> O(n). The recursive `build_map` passes the map as an argument that is consumed, so refcount will be 1 on the hot path.

**File:** `blc/src/vm/natives/map.rs`

### Step 4: Borrow instead of clone in List HOFs

Change `List.map`, `List.filter`, `List.fold`, `List.find` to use index-based iteration over the list instead of cloning it. 

**File:** `blc/src/vm/exec/hof.rs`

### Step 5: CoW for ListConcat and ListTailFrom opcodes

For `ListConcat [x] ++ list`: if right list is uniquely owned, `insert(0, x)` in-place.
For `ListConcat list ++ [x]`: if left list is uniquely owned, `push(x)` in-place.
For `ListTailFrom(n)`: if list is uniquely owned, `drain(..n)` in-place.

**File:** `blc/src/vm/exec/dispatch_data.rs`

### Step 6: CoW for list natives

Register `List.concat`, `List.reverse`, `List.sort` with `FLAG_OWNING`. Use `try_unwrap_heap` for in-place reverse/sort when sole owner.

**File:** `blc/src/vm/natives/list.rs`

## Proposal 2: JIT & String Path Fixes

### Step 7: Fix `is_scalar_only`

Update `type_is_scalar` to default to `false` (pessimistic fallback) if `None` type is present. Ensure polymorphic/list variables properly fail the scalar check to keep pointers boxed. Provide better type inference propagation down to IR.

**File:** `blc/src/vm/jit/analysis.rs`

### Step 8: String Native Intrinsics (Stretch Goal)

Implement fast-paths in the VM or specialized JIT intrinsics for simple `strrev`, `concat`, and `length` operations to avoid full native calling contention overhead.

## Safety

CoW is safe because:
- **Immutability guarantee:** If `Arc::strong_count == 1`, no other code holds a reference. The mutation is unobservable.
- **Aliased values are protected:** `let m2 = m1; Map.insert(m2, ...)` creates refcount 2, so CoW does NOT fire — the clone fallback preserves correctness.
- **Thread safety:** `Arc::try_unwrap` is atomic and thread-safe (relevant for fiber runtime).

## Risks

1. **Double-free in try_unwrap_heap** — the `forget(self)` pattern must be exact. Mitigate with unit tests and Miri.
2. **Vec::drain overhead** — the owning dispatch drains the stack instead of borrowing. Marginally slower for non-CoW paths. Only flag functions where CoW fires frequently.
3. **JIT compatibility** — Map.insert goes through CallNative, so CoW applies to both VM and JIT. ListConcat is an opcode, so only VM benefits (JIT would need separate work).

## Expected Outcomes

| Benchmark | Before (VM) | Target | Mechanism |
|-----------|------------|--------|-----------|
| mapbuild  | 0.337s (16x C) | < 0.100s (5x C) | CoW Map.insert (Step 3) |
| treemap   | 0.872s (33x C) | < 0.400s (15x C) | Reduced clone overhead (Steps 4-6) |
| mergesort | 0.253s (16x Rust) | < 0.100s (6x Rust) | CoW ListConcat + borrow HOFs (Steps 4-5) |
| mergesort | JIT N/A | Output == OK | Fix JIT is_scalar BUG (Step 7) |

