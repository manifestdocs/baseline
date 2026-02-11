# Building a functional language on Cranelift

**Cranelift provides a fast, correct code generation backend that handles low-level optimization well — but building a functional language on it requires the compiler engineer to own most of the high-level transformation work.** This guide covers every major language feature of Baseline, a statically typed, effect-tracked functional language, and maps each to specific Cranelift APIs, CLIF IR patterns, and architectural decisions. The target API is cranelift-codegen 0.128.x (Wasmtime 42.x era), though the patterns apply broadly to the 0.125–0.130 range. Cranelift's `Tail` calling convention, e-graph mid-end, user stack maps, and new exception handling primitives form the core building blocks; everything else — escape analysis, closure conversion, pattern match compilation, fusion, effect compilation — must be built in the compiler frontend before CLIF emission.

---

## 1. Algebraic effects: from handler stacks to native code

Baseline tracks effects in function types (e.g., `fn fetch!(url: Url) -> {Http, Log} Response`) and resolves them via handler stacks at runtime. Cranelift offers three primitives that partially map to effect semantics, plus compilation strategies proven by Koka and OCaml 5.

### Cranelift's exception handling primitives

The `try_call` instruction, added in mid-2025 (PR #10510 by Chris Fallin), is a block terminator that branches to either a normal return path or an exception handler path. Exception payloads arrive in **two fixed registers** per platform: `rax`/`rdx` on x86-64, `x0`/`x1` on AArch64, `a0`/`a1` on RISC-V 64. There is no explicit `throw` opcode in CLIF — throws are mediated through runtime libcalls at call boundaries.

```
function %handled_scope() -> i64 tail {
    fn0 = %effectful_computation(i64) -> i64 tail

block0:
    v0 = iconst.i64 42
    try_call fn0(v0), block_ok(ret0), { tag0: block_handler(exn0, exn1) }

block_ok(v1: i64):
    return v1

block_handler(v2: i64, v3: i64):
    ;; v2 = effect tag, v3 = payload pointer
    ;; dispatch to appropriate handler closure
    return v2
}
```

The `get_exception_handler_address` instruction (PR #11629) returns the handler's PC as a first-class value, enabling custom resume logic — a key building block for resumable effects. The `stack_switch` instruction (added Wasmtime v25) provides symmetric stack switching for fiber-based effect implementations, passing up to four transfer values between stacks.

### The gap: resumption

Cranelift's exception handling is designed for **one-shot, non-resumable exceptions**. Algebraic effects require delimited continuations — the ability to capture the stack up to a handler and resume it later. `try_call` unwinds and discards frames. Bridging this requires one of three compilation strategies.

### Recommended strategy: evidence-passing (Koka-style)

The evidence-passing approach, proven by Koka (Leijen, ICFP 2021), passes an evidence vector as an extra parameter to all functions. The vector maps effect types to handler records with O(1) lookup. A `is_yielding` flag enables lazy resumption construction without stack manipulation.

```rust
// Every function signature gains an evidence-vector parameter
let mut sig = Signature::new(CallConv::Tail);
sig.params.push(AbiParam::new(types::I64)); // regular arg
sig.params.push(AbiParam::new(types::I64)); // evidence vector pointer
sig.returns.push(AbiParam::new(types::I64));
```

After every call to a potentially-effectful function, emit a flag check:

```
block_after_call:
    v_yielding = load.i8 notrap v_ctx+8    ;; is_yielding flag
    brif v_yielding, block_yield, block_continue(v_result)

block_continue(v_r: i64):
    ;; normal path — use v_r
    ...

block_yield:
    ;; store resumption closure, propagate yield
    v_resume = func_addr.i64 %this_resume_point
    call %yield_extend(v_ctx, v_resume)
    v_dummy = iconst.i64 0
    return v_dummy
```

This approach works on **all platforms today**, has near-zero cost when no effects are performed (branch prediction handles the common case), and compiles to simple CLIF with no special runtime machinery. Use `CallConv::Tail` with `return_call` to prevent stack overflow in CPS-compiled effect handlers.

### Alternative: stack switching for high-performance effects

When targeting x86-64 and performance of effect-heavy code matters, use `stack_switch` for continuation capture/resume combined with `try_call` for handler installation:

```
;; Performing an effect: switch back to handler's stack
stack_switch my_context_ptr, handler_context_ptr,
             effect_tag, payload, my_context_ptr, 0
;; When resumed, transfer values contain the handler's response
```

OCaml 5's fiber implementation uses this pattern — each `handle` block runs on a new stack segment, `perform` switches back to the handler's stack, and the detached fibers become the captured continuation. The `stack_switch` instruction implements exactly this primitive, though its platform support is currently narrower than the evidence-passing approach.

### Strategy comparison

The CPS transformation approach (converting effectful functions to continuation-passing style, using `return_call_indirect` for all continuations) works but causes significant code bloat and allocation overhead. Evidence-passing is strictly superior for production use. Reserve stack switching for performance-critical effect-heavy code paths, and avoid relying solely on `try_call` since it cannot express resumable continuations.

---

## 2. Refinement types compile to traps and branches

Baseline's refinement types (`type Port = Int where 1 <= self <= 65535`) lower to comparison-plus-trap sequences in CLIF. Modern Cranelift (post-0.126) uses `icmp` + `trapnz`/`trapz` — the older `trapif` and `ifcmp` instructions have been removed.

### The single-comparison trick

For range checks, exploit unsigned arithmetic: `(x - lower) <u (upper - lower)` replaces two comparisons with one:

```rust
fn emit_refinement_check(builder: &mut FunctionBuilder, x: Value) {
    let x_minus_1 = builder.ins().iadd_imm(x, -1);
    let limit = builder.ins().iconst(types::I64, 65535);
    let oob = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, x_minus_1, limit);
    builder.ins().trapnz(oob, TrapCode::unwrap_user(1));
}
```

The equivalent CLIF:

```
v1 = iadd_imm v0, -1
v2 = iconst.i64 65535
v3 = icmp uge v1, v2
trapnz v3, user1
;; v0 is now guaranteed in [1, 65535]
```

### TrapCode API

`TrapCode` is now an opaque struct with associated constants (`STACK_OVERFLOW`, `HEAP_OUT_OF_BOUNDS`, `INTEGER_OVERFLOW`, `INTEGER_DIVISION_BY_ZERO`, `BAD_CONVERSION_TO_INTEGER`) plus user-defined codes via `TrapCode::unwrap_user(n: u8)`. This gives **~245 distinct user trap codes**, sufficient for differentiating refinement error types. At runtime, trap instructions emit `ud2` on x86-64 or `brk` on AArch64; a signal handler maps the faulting PC to the trap code via `CompiledCode::buffer.traps()`.

### Traps versus branches

Use **traps for invariants the type system should guarantee** — zero overhead on the fast path, expensive only on violation (signal handler). Use **branches producing `Result<T, E>`** for user-facing validation where failure is expected:

```
block0(v0: i64):
    v1 = icmp.i64 slt v0, 1
    brif v1, block_err, block_check_high

block_check_high:
    v2 = icmp.i64 sgt v0, 65535
    brif v2, block_err, block_ok

block_ok:
    v3 = iconst.i8 0          ;; Ok tag
    return v3, v0

block_err:
    v4 = iconst.i8 1          ;; Err tag
    v5 = iconst.i64 0
    return v4, v5
```

Cranelift's new `try_call` exception handling offers a middle ground — structured unwinding without signal handlers — but traps remain simpler for single-predicate checks.

---

## 3. Immutable records demand frontend-driven optimization

Cranelift has no aggregate types. Records must be lowered to raw memory operations, and the **single most important optimization** — scalar replacement of aggregates — must happen before CLIF emission.

### Memory representation and access

Stack-allocate non-escaping records via `StackSlot`, heap-allocate escaping ones via a runtime allocator:

```rust
let ss = builder.create_sized_stack_slot(StackSlotData {
    kind: StackSlotKind::ExplicitSlot,
    size: 24,       // {a: i64, b: i64, c: i64}
    align_shift: 3, // 8-byte alignment
});

// Prefer stack_load/stack_store — Cranelift reasons about these more precisely
let field_a = builder.ins().stack_load(types::I64, ss, 0);
let field_b = builder.ins().stack_load(types::I64, ss, 8);
```

For heap-allocated records, mark all loads with **`readonly`** — this is the single most impactful flag for immutable data:

```rust
let flags = MemFlags::new()
    .with_notrap()
    .with_aligned()
    .with_readonly();  // No memory deps — enables LICM, dedup, reordering
let field = builder.ins().load(types::I64, flags, record_ptr, 8);
```

The `readonly` flag tells Cranelift's e-graph optimizer that this load has no memory dependencies — it can be freely hoisted out of loops, deduplicated with identical loads, and reordered past stores. For truly immutable records, this is always valid and unlocks the optimizer's full power.

### Scalar replacement: the critical optimization Cranelift lacks

**Cranelift has no SRA pass.** For non-escaping records, represent fields as separate `Variable`s instead of memory operations:

```rust
// Instead of allocating {x: 1, y: 2} in memory:
let x_var = Variable::new(0);
let y_var = Variable::new(1);
builder.declare_var(x_var, types::I64);
builder.declare_var(y_var, types::I64);
builder.def_var(x_var, builder.ins().iconst(types::I64, 1));
builder.def_var(y_var, builder.ins().iconst(types::I64, 2));

// "Access field x" — no memory operation at all
let x_val = builder.use_var(x_var);
```

The `FunctionBuilder` handles SSA construction automatically — `use_var` across blocks inserts block parameters (φ-nodes) at merge points. This transforms record operations into pure register operations, enabling the e-graph optimizer to constant-fold, propagate, and eliminate them entirely.

### Functional record update

For `{ ...user, age: 31 }` on small records (≤4 fields), load each field individually and store to a new record, substituting the updated field. This keeps values in registers and enables redundant-load elimination. For large records, use `emit_small_memory_copy` or `call_memcpy` followed by a single field store.

### Row polymorphism via dictionary passing

For records with statically unknown additional fields, pass a field-offset dictionary alongside the record pointer. Two loads instead of one, but both can be marked `readonly`:

```
v_offset = load.i32 notrap aligned readonly v_dict+0   ;; offset of field 'x'
v_addr = iadd v_record, v_offset
v_value = load.i64 notrap aligned readonly v_addr      ;; field value
```

---

## 4. Pattern matching maps naturally to CLIF blocks

Cranelift's block-parameter SSA model is a natural fit for compiled pattern matches — each match arm becomes a basic block, and the merge point receives results as block parameters.

### Tag dispatch with the Switch utility

Use `cranelift_frontend::Switch` rather than raw `br_table` — it automatically chooses between binary search (sparse tags) and jump tables (dense tags):

```rust
use cranelift_frontend::Switch;

let mut switch = Switch::new();
switch.set_entry(0, point_block);    // tag 0 → Point
switch.set_entry(1, circle_block);   // tag 1 → Circle
switch.set_entry(2, rect_block);     // tag 2 → Rect
switch.emit(&mut builder, tag_value, default_block);
```

Raw `br_table` is available for dense, 0-based tag spaces but does **not** support block arguments — you must create intermediate trampoline blocks:

```rust
let jt = builder.create_jump_table(JumpTableData::new(
    default_block, &[trampoline0, trampoline1, trampoline2]
));
builder.ins().br_table(tag, jt);

// Each trampoline jumps to the real target with arguments
builder.switch_to_block(trampoline0);
let result = /* compute arm result */;
builder.ins().jump(merge_block, &[result]);
```

### Decision tree compilation

Implement Maranget's (2008) decision tree algorithm, which maps directly to CLIF:

- Each **internal node** → a basic block with a `Switch`/`brif` on a tag or sub-field
- Each **leaf** → a block that evaluates the arm body and `jump`s to the merge point with the result as a block parameter
- **Shared arms** → "join blocks" receiving bound variables as block parameters, eliminating code duplication

For nested patterns like `Cons(Cons(x, _), _)`:

```
block_dispatch:
    v_tag1 = load.i32 notrap readonly v_scrutinee+0
    ;; Switch on outer tag

cons_block:
    v_inner = load.i64 notrap readonly v_scrutinee+8
    v_tag2 = load.i32 notrap readonly v_inner+0
    ;; Switch on inner tag

cons_cons_block:
    v_x = load.i64 notrap readonly v_inner+8
    jump merge(v_x)

merge(v_result: i64):
    return v_result
```

Decision trees are strongly preferred over backtracking automata for CLIF targeting — they map directly to the block-parameter SSA model without requiring mutable state or block re-entry.

---

## 5. Pipeline fusion must happen before CLIF

Baseline's `|>` operator with `List.map`, `List.filter`, `List.fold` requires two layers of optimization: high-level fusion (before CLIF) and closure elimination (with Cranelift's help).

### Closure representation

Closures are fat pointers — a `(function_pointer, environment_pointer)` pair. The closure entry function takes the environment as its first argument:

```rust
// Creating a closure
let env_ptr = /* allocate environment, store captures */;
let fn_ptr = builder.ins().func_addr(types::I64, closure_fn_ref);

// Calling a closure
let mut args = vec![env_ptr];
args.extend_from_slice(&user_args);
let result = builder.ins().call_indirect(closure_sig, fn_ptr, &args);
```

For tail-position pipeline stages, use `return_call_indirect` to avoid stack growth.

### Stream fusion strategy

Transform `list |> map f |> filter g |> fold h init` into a single loop **in your high-level IR**, before CLIF generation:

```
;; Fused loop — no intermediate allocations
block_loop(v_idx: i64, v_acc: i64):
    v_done = icmp uge v_idx, v_len
    brif v_done, block_result(v_acc), block_body

block_body:
    v_x = load.i64 notrap aligned v_list_ptr, v_idx*8  ;; list[i]
    v_y = /* inlined f(x) */
    v_keep = /* inlined g(y) */
    brif v_keep, block_accum(v_idx, v_acc, v_y), block_skip(v_idx, v_acc)

block_accum(v_i: i64, v_a: i64, v_val: i64):
    v_new_acc = /* inlined h(a, val) */
    v_next = iadd_imm v_i, 1
    jump block_loop(v_next, v_new_acc)
```

Cranelift cannot perform this transformation — it operates on individual loads, stores, and arithmetic, with no knowledge of list semantics. GHC's foldr/build and stream fusion laws, OCaml's Flambda closure unboxing, and the recent Lumberhack type-directed deforestation (ICFP 2024 Distinguished Paper) all operate at the high-level IR and are applicable to your frontend. After fusion, Cranelift's e-graph handles GVN, LICM for hoisting captured constants, and dead code elimination for unused allocations.

### Cranelift's inlining support

Function inlining landed in Wasmtime 36 (November 2025), off by default, callback-based via an `Inline` trait. The embedder controls heuristics — return `Some(callee_body)` to inline, `None` to leave as a call. After inlining, the e-graph optimizer eliminates dead `func_addr` computations, environment loads, and closure allocations when all uses are resolved. For a functional language, performing inlining in your own IR (where you have type information and cross-module visibility) is likely more effective, but Cranelift's inliner provides a valuable second pass.

---

## 6. Tail calls are a first-class Cranelift feature

### The Tail calling convention

**`CallConv::Tail` is the recommended default** for all language-internal functions. It now matches SystemV performance for non-tail calls (after callee-save register fixes in PR #8540) while enabling guaranteed tail call elimination. Key properties: the callee cleans up stack arguments (essential for tail calls between functions with different argument counts), and exception payload registers (`rax`/`rdx` on x86-64) are ABI-stable.

```rust
let mut sig = Signature::new(CallConv::Tail);
sig.params.push(AbiParam::new(types::I64));
sig.returns.push(AbiParam::new(types::I64));
```

### return_call and return_call_indirect

These block-terminator instructions perform guaranteed tail calls — they lower to `jmp` (not `call`) after tearing down the current frame:

```
function %factorial(i64, i64) -> i64 tail {
    fn0 = %factorial(i64, i64) -> i64 tail

block0(v0: i64, v1: i64):
    v2 = icmp_imm eq v0, 0
    brif v2, block_done, block_recurse

block_done:
    return v1

block_recurse:
    v3 = iadd_imm v0, -1
    v4 = imul v0, v1
    return_call fn0(v3, v4)
}
```

Both caller and callee must use the `Tail` convention. You **cannot** tail-call across convention boundaries — FFI calls to C (`SystemV`) must use regular `call`.

### Self-recursion should become a loop

Cranelift does **not** convert self-tail-recursion to loops. Do this in your frontend — use `jump` back to the entry block with new arguments. This avoids prologue/epilogue overhead and enables loop optimizations (LICM, etc.):

```
;; Self-recursive factorial as a CLIF loop — no function call overhead
block0(v0: i64, v1: i64):
    v2 = icmp_imm eq v0, 0
    brif v2, block_done, block_loop

block_loop:
    v3 = iadd_imm v0, -1
    v4 = imul v0, v1
    jump block0(v3, v4)

block_done:
    return v1
```

For **mutual tail recursion**, `return_call` / `return_call_indirect` is the correct approach — each function is compiled separately, and tail calls jump between them without growing the stack.

### Architecture support

| Architecture | Tail CC | return_call | Production-ready |
|---|---|---|---|
| x86-64 | ✅ | ✅ | ✅ Since Wasmtime 22 |
| AArch64 | ✅ | ✅ | ✅ Since Wasmtime 22 |
| RISC-V 64 | ✅ | ✅ | ✅ Since Wasmtime 22 |
| s390x | ✅ | ✅ | Later addition (PR #9052) |
| Pulley | ✅ | ✅ | ✅ Portable interpreter |

---

## 7. Garbage collection via user stack maps

Cranelift's user stack maps system (redesigned in Wasmtime v25, September 2024) shifts GC-root tracking responsibility to the frontend. The old regalloc-based system caused subtle miscompilations; the new design is simpler, supports compressed references, and works with any GC algorithm.

### Marking GC references

Two APIs mark values for stack map inclusion. The frontend's `SafepointSpiller` performs backward liveness analysis, allocates spill slots, and inserts spill/reload pairs around every safepoint (call instruction):

```rust
// Mark a variable as containing a GC reference
let gc_ref = Variable::new(0);
builder.declare_var(gc_ref, types::I64);
builder.declare_var_needs_stack_map(gc_ref);

// Use normally — spills/reloads are inserted automatically
builder.def_var(gc_ref, some_heap_pointer);
let v = builder.use_var(gc_ref);
let field = builder.ins().load(types::I32, mem_flags, v, 8);

// This call is a safepoint — gc_ref is spilled before, reloaded after
builder.ins().call(some_function, &[field]);

// Subsequent uses get the reloaded value (GC may have moved the object)
let v2 = builder.use_var(gc_ref);
```

For individual SSA values not tied to variables, use `declare_value_needs_stack_map(val)`.

### Extracting and consuming stack maps

After compilation, extract stack maps from the compiled code:

```rust
let compiled = ctx.compile(&*isa, &mut Default::default())?;
for (offset, frame_size, stack_map) in compiled.buffer.stack_maps() {
    for (ty, sp_offset) in stack_map.entries() {
        // At instruction `offset`, a live GC ref of type `ty`
        // lives at SP + sp_offset
        gc_metadata.add_root(code_base + offset, sp_offset, ty);
    }
}
```

At GC time, walk the stack (using libunwind or frame pointers — enable with `preserve_frame_pointers = true`), look up the stack map for each return address, and enumerate roots. For **moving GCs**, update the spill slot in-place — the automatic reload after the safepoint picks up the new pointer.

### Compressed references

**32-bit GC references on 64-bit ISAs are fully supported.** Stack map entries carry their type, so `types::I32` refs work naturally. Wasmtime uses this: all GC references are `i32` indices into a GC heap, with the base pointer stored in `VMContext`. This halves reference storage size and improves cache locality.

### GC algorithm compatibility

- **Non-moving (mark-sweep, refcounting)**: Simplest — only need root enumeration from stack maps
- **Semi-space / copying**: Fully supported — the spill/reload pattern enables pointer updates
- **Generational**: Emit write barriers as inline CLIF (card-table marking) or function calls
- **Concurrent/incremental**: Insert explicit safepoint polls at loop back-edges and function entries as regular CLIF branches
- **Immix**: Compatible — bump allocation fast paths can be inlined as CLIF

Write barriers are regular CLIF instructions — no special Cranelift support needed:

```rust
// Inline card-table write barrier
let card_index = builder.ins().ushr_imm(obj_addr, CARD_SHIFT);
let card_addr = builder.ins().iadd(card_table_base, card_index);
builder.ins().store(MemFlags::new(), dirty_byte, card_addr, 0);
```

**Interior pointers**: always keep the base GC reference in a stack-map-tracked variable and recompute derived pointers after safepoints.

---

## 8. JIT infrastructure for tiered execution

### The cranelift-jit workflow

The core API follows a declare → define → finalize → execute cycle:

```rust
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Module, Linkage};

let builder = JITBuilder::with_flags(
    &[("opt_level", "speed")],
    default_libcall_names(),
)?;
let mut module = JITModule::new(builder);

// Declare, build, define
let func_id = module.declare_function("add1", Linkage::Export, &sig)?;
/* ... build function body with FunctionBuilder ... */
module.define_function(func_id, &mut ctx)?;
module.finalize_definitions()?;

// Execute
let ptr = module.get_finalized_function(func_id);
let add1: fn(i64) -> i64 = unsafe { std::mem::transmute(ptr) };
assert_eq!(add1(41), 42);
```

Register host functions for callbacks via `builder.symbol("name", ptr)` or provide a custom resolver via `builder.symbol_lookup_fn(fn)`.

### Function hotswapping for tier promotion

Enable hotswap mode to get indirect-jump trampolines that can be repatched:

```rust
let mut builder = JITBuilder::new(default_libcall_names())?;
builder.hotswap(true);  // All calls go through patchable trampolines
let mut module = JITModule::new(builder);

// Initial definition (e.g., interpreter stub)
let func_id = module.declare_function("compute", Linkage::Export, &sig)?;
module.define_function(func_id, &mut interp_stub_ctx)?;
module.finalize_definitions()?;

// Later — tier up to optimized code:
module.prepare_for_function_redefine(func_id)?;
module.define_function(func_id, &mut optimized_ctx)?;
module.finalize_definitions()?;
// Existing callers automatically pick up the new implementation
```

The function signature **cannot change** between redefinitions. The `rustc_codegen_cranelift` project uses exactly this pattern for lazy JIT compilation — functions start as stubs calling `__clif_jit_fn`, which compiles them on first invocation.

### Incremental compilation cache

The `incremental-cache` feature caches compiled function stencils keyed by SHA-256 hash of ISA settings plus function IR. Implement the `CacheKvStore` trait:

```rust
pub trait CacheKvStore {
    fn get(&self, key: &[u8]) -> Option<Cow<'_, [u8]>>;
    fn insert(&mut self, key: &[u8], val: Vec<u8>);
}

// Use via Context::compile_with_cache
let (compiled, was_cached) = ctx.compile_with_cache(
    &*isa, &mut my_cache, &mut ControlPlane::default()
)?;
```

The cache stores `CompiledCodeStencil` — machine code, relocations template, stack maps — and applies parameter-specific fixups on cache hit. Version markers ensure cache invalidation on Cranelift upgrades.

### Deoptimization without native support

Cranelift is not a speculative optimizer and has no deoptimization primitives. Three practical approaches:

**Guard + branch (recommended for frequent deopts):**

```rust
let type_tag = builder.ins().load(I32, MemFlags::trusted(), obj_ptr, TAG_OFFSET);
let is_int = builder.ins().icmp_imm(IntCC::Equal, type_tag, TYPE_INT as i64);
builder.ins().brif(is_int, fast_path, &[], deopt_block, &[]);

// deopt_block: spill state to interpreter frame, call interpreter_reentry
```

**Side exits via traps (best for rare deopts):** zero fast-path overhead, but signal handler complexity. Map trap PCs to bytecode offsets via a side table; reconstruct interpreter state from the compiled frame using stack map metadata.

**Function-level replacement (simplest):** avoid mid-function OSR entirely — let the current invocation finish in the current tier, then route the next call to the higher tier via hotswap. This avoids the extreme complexity of mid-execution transfer and is recommended as the primary strategy.

### Other Cranelift-based JITs as reference

**revmc** (paradigmxyz) compiles EVM bytecode to native code via Cranelift, achieving up to **6.9× speedup** over interpreted EVM. Its Cranelift backend is currently limited by missing `i256` support. **Lumina** is an eager-by-default functional language using Cranelift via `cranelift-object` for AOT compilation, with a Source → HIR → MIR → LIR → CLIF pipeline. **rustc_codegen_cranelift** provides a production-grade reference for hotswap-based lazy JIT with Cranelift.

---

## 9. WebAssembly requires a separate emission path

Cranelift cannot emit Wasm — it is strictly a native code generator. A Baseline compiler targeting both native and Wasm needs a shared high-level IR that lowers to CLIF (for native) or Wasm bytecode (for web).

### Wasm emitters

**`wasm-encoder`** (Bytecode Alliance, part of `wasm-tools`) is the recommended emitter. It provides a type-safe streaming API for constructing Wasm modules section by section, supports Wasm 3.0, and has ~1.4M monthly downloads. **`walrus`** offers a higher-level in-memory IR for Wasm modules, better suited for transforming existing modules. **Binaryen** (`wasm-opt`) provides 96+ optimization passes and should be used as a post-processing step after emission.

### Wasm 3.0 changes the game for functional languages

Wasm 3.0, released as a W3C standard in September 2025 and shipped in all major browsers, includes three proposals critical for functional language deployment:

- **Wasm GC**: Struct and array heap types with host-managed garbage collection. Eliminates the need to ship a custom GC in the binary. Chrome 119+, Firefox 120+, Safari 18.2+.
- **Wasm Tail Calls**: `return_call` and `return_call_indirect` instructions, essential for recursive functional programs. Universally available.
- **Wasm Exception Handling**: Native `try_table`/`throw`/`throw_ref` with typed exception references. Cross-browser support complete in 2025.

These three proposals mean a functional language can target Wasm with direct representations of ADTs (Wasm-GC structs), proper tail calls (no trampoline overhead), and structured error handling — without compromises that were necessary even two years ago.

### Dual-target architecture

```
Source Language (Baseline)
    ↓
[Frontend: Parsing, Type Checking, Effect Resolution]
    ↓
[High-Level IR]  ← All semantic optimizations here:
    │               inlining, escape analysis, fusion,
    │               closure conversion, effect specialization
    ├─────────────────────┐
    ↓                     ↓
[CLIF Builder]      [Wasm Builder]
(cranelift-frontend) (wasm-encoder)
    ↓                     ↓
[Cranelift Pipeline]  [wasm-opt]
    ↓                     ↓
Native binary        .wasm binary
```

The shared high-level IR is where all language-specific optimizations happen. CLIF and Wasm representations are fundamentally incompatible (SSA CFG vs. stack-based structured control flow), so the lowering paths must be separate.

---

## 10. Calling conventions: eight options, one recommendation

Cranelift's `CallConv` enum has **eight variants** as of 0.130:

| Convention | Use case |
|---|---|
| **`Tail`** | **Default for all language-internal functions.** Supports tail calls, at performance parity with SystemV. |
| `Fast` | Internal optimization, no tail calls. Marginal benefit over Tail. |
| `SystemV` | FFI with C on Linux/macOS x86-64. ABI-stable. |
| `WindowsFastcall` | FFI with C on Windows x64. ABI-stable. |
| `AppleAarch64` | FFI with C on macOS ARM64. Required due to Apple's ABI tweaks. |
| `Winch` | Wasmtime's baseline compiler. No callee-saves. |
| `Probestack` | Internal use for stack probing. |
| `PreserveAll` | Instrumentation — callee saves all registers. No returns. |

**Use `Tail` everywhere** in your language runtime. For FFI to C libraries, create thin wrapper functions with `SystemV`/`WindowsFastcall`/`AppleAarch64` conventions:

```rust
// Language function (Tail convention)
let mut lang_sig = Signature::new(CallConv::Tail);
// ...

// FFI wrapper (SystemV for C interop)
let mut ffi_sig = Signature::new(CallConv::SystemV);
ffi_sig.params.push(AbiParam::new(types::I64));
ffi_sig.returns.push(AbiParam::new(types::I32));
```

For mixed interpreter/JIT execution, use a function pointer table with `return_call_indirect` for JIT→JIT dispatch, and `SystemV`-convention wrapper functions for JIT→interpreter callbacks that re-enter the interpreter loop.

The `Tail` convention's exception payload registers are **ABI-stable** (the only stable part of the convention): `rax`/`rdx` on x86-64, `x0`/`x1` on AArch64, `a0`/`a1` on RISC-V 64. Use `CallConv::exception_payload_types(pointer_ty)` to query these programmatically.

---

## 11. Where Cranelift optimizes and where it does not

Understanding the boundary between Cranelift's responsibilities and yours is the most important architectural decision for a functional language compiler.

### What the e-graph mid-end handles automatically

The e-graph (acyclic equality saturation, introduced 2022, now default) runs GVN, constant folding, algebraic simplifications, LICM, and alias analysis in a unified framework that **eliminates the phase-ordering problem**. ISLE rewrite rules express optimizations declaratively:

```scheme
;; Strength reduction
(rule (simplify (imul x 2))
      (ishl x 1))

;; Identity elimination
(rule (simplify (iadd x 0))
      x)
```

The alias analysis categorizes memory into disjoint regions and tracks "last store" per region. Stores in one region don't invalidate loads in another. Loads marked `readonly` are immune to all stores. After the e-graph reaches fixpoint (or an iteration limit), cost-based extraction selects the best representation. Impact: **~16% runtime speedup** on SpiderMonkey.wasm over the legacy pass suite.

### What must be built in your frontend

| Optimization | Why Cranelift can't do it |
|---|---|
| Inlining decisions | Cranelift provides the mechanism (callback-based) but not the heuristics |
| Closure conversion & lambda lifting | CLIF has no closure concept |
| Escape analysis | Requires interprocedural analysis |
| Stream/pipeline fusion | Requires high-level semantic knowledge |
| Type specialization / monomorphization | CLIF is untyped at the source level |
| Pattern match compilation | Converting `match` to decision trees |
| Effect specialization | Language-specific |
| Unboxing decisions | Requires type-level reasoning |
| Tail call detection | Frontend must emit `return_call` |
| Scalar replacement of aggregates | Must happen before CLIF generation |

### opt_level and register allocation

Three optimization levels control the mid-end:

- **`none`**: Skips e-graph and alias analysis. Fastest compilation. Use for debug builds or JIT cold tier.
- **`speed`** (default): Full e-graph, alias analysis, all ISLE rewrite rules. Use for production.
- **`speed_and_size`**: Like `speed` plus code-size-reducing transformations.

The **regalloc2** register allocator offers two algorithms: `backtracking` (default, better code quality, range splitting and eviction heuristics) and `single_pass` / Fastalloc (faster compilation, more spills). Use `backtracking` for production and `single_pass` for development iteration or JIT cold-tier compilation where compile speed matters more than code quality.

### The optimization pipeline in practice

```
Your High-Level IR
    ↓ [Frontend: inlining, closure conversion, escape analysis,
    ↓  pattern match compilation, fusion, effect specialization,
    ↓  scalar replacement, unboxing, tail call detection]
CLIF IR (via cranelift-frontend FunctionBuilder)
    ↓ [Cranelift: optional inlining pass (Context::inline)]
    ↓ [Cranelift: e-graph mid-end — GVN, constant folding, LICM,
    ↓  alias analysis, algebraic simplifications via ISLE]
    ↓ [Cranelift: ISLE instruction selection → VCode]
    ↓ [regalloc2: register allocation]
    ↓ [Cranelift: binary emission]
Native machine code
```

Cranelift is a **"last mile" code generator**. The quality of your compiled output is predominantly determined by what happens before CLIF emission. Cranelift excels at turning clean SSA into efficient machine code — but it will not discover that your pipeline of closures can be fused into a tight loop, or that your record allocation can be eliminated. Those wins must come from your compiler's own optimization passes.

---

## Conclusion

Building Baseline on Cranelift is architecturally sound but demands that the compiler engineer take ownership of high-level optimizations. The key insight is that **Cranelift's strength lies in low-level code generation quality and compilation speed, not in high-level program transformation**. Effect handlers should use evidence-passing (Koka-style) for portability, with stack-switching as a performance upgrade path. Records require frontend-driven scalar replacement since Cranelift lacks SRA. Pattern matching compiles naturally to CLIF's block-parameter model via decision trees. Pipeline fusion must happen entirely in your IR. The `Tail` calling convention should be used universally for language-internal functions, providing guaranteed tail call elimination at SystemV-equivalent performance. GC integration through user stack maps is well-designed and supports everything from simple mark-sweep to concurrent copying collectors. The JIT infrastructure via `cranelift-jit` with hotswap trampolines enables clean tiered compilation. And for Wasm deployment, the standardization of Wasm GC, tail calls, and exception handling in Wasm 3.0 means functional languages can now target the web without major compromises — but through a separate `wasm-encoder` emission path, not through Cranelift.
