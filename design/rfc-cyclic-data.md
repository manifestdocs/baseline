# RFC: Ergonomic Memory Management for Cyclic Data

**Status:** Option A Phase 1 [IMPLEMENTED] — compiler emits `W_CYCLE_001` for mutual closure captures. Remaining options deferred.

## 1. Summary

Baseline utilizes **Perceus reference counting** as its primary memory management strategy. This provides deterministic latency, predictable performance, and enables Functional-But-In-Place (FBIP) optimizations. However, the fundamental weakness of any pure reference-counting system is its inability to reclaim memory trapped in reference cycles.

This RFC explores the realities of cyclic data in Baseline's strict, immutable paradigm, evaluates the current escape hatches (`Weak<T>` and Regions), and proposes systemic enhancements to handle cycles ergonomically without compromising performance or the developer experience.

## 2. The Nature of Cycles in Baseline

Because Baseline is strictly evaluated and heavily biased toward immutability, creating reference cycles is significantly harder than in object-oriented or dynamically typed languages. You cannot easily instantiate an object `A`, instantiate object `B`, and then mutate `B` to point back to `A`.

In Baseline, cycles typically arise in only three scenarios:
1. **Mutually Recursive Closures:** If function `A` captures function `B` in its environment, and `B` captures `A`, their environment closures form a cycle.
2. **Explicit Shared Mutability:** If/when Baseline introduces mutable reference types (e.g., `Ref<T>` or `Shared<T>`) for graph construction. Neither of these types exists today; this scenario is entirely future-facing.
3. **Lazy Evaluation (Future):** If Baseline introduces lazy thunks (`lazy x = ...`), these can trivially form cycles evaluating to themselves.

When these cycles occur, the reference count of the involved allocations never reaches zero, resulting in a memory leak.

**Practical impact today:** Only scenario #1 is possible in the current language. Scenarios #2 and #3 require language features that do not yet exist. This significantly narrows the problem space and makes targeted solutions tractable.

## 3. Current Mitigation Strategies

### 3.1 Region Inference / Arenas (The Ideal Path)
Baseline's vision includes Region-Based Memory Management: *"Region-scoped data is unaffected — bulk-freed regardless of cycles."*

If a cyclic graph is built entirely within the scope of a web request handler, the cycle is irrelevant. When the request completes, the entire arena is reclaimed in O(1) time.

**Current implementation status:** The JIT backend implements an arena lifecycle (`jit_drain_arena`) that bulk-frees intermediate allocations after JIT execution. However, full compiler-driven region inference — statically proving that allocations do not escape a scope and routing them to arenas automatically — is not yet implemented. Today, the arena acts as a safety net for JIT-compiled code, not as a general region inference system. Expanding this to cover request handler scopes and `scope!` blocks is future work and a key enabler for this RFC's recommendation.

### 3.2 `Weak<T>` References (The Manual Path)
For long-lived, cross-request state (e.g., a persistent cache), developers can manually break cycles using `Weak.downgrade` and `Weak.upgrade`. Both functions are implemented in the runtime today.

**The Problem:** This pushes memory management back onto the developer. It requires them to mentally model the ownership graph of their domain data, violating the principle that pure functional programming should abstract away memory topology.

## 4. Proposed Enhancements

To maintain the DX of a high-level language, we should avoid forcing developers to manually sprinkle `Weak<T>` throughout their business logic. We propose four directions, layered from immediate to longer-term:

### Option A: Compiler Warnings + Automatic Weak Insertion for Closure Cycles [PHASE 1 IMPLEMENTED]

Since mutually recursive closures are the **only** cycle-formation mechanism in today's language, the compiler can target them precisely. The analysis is tractable: detect `let rec f = ... g ... and g = ... f ...` patterns (or their desugared equivalents) where both closures capture each other in their environment.

**Phase 1 — Diagnostic [IMPLEMENTED]:**
Implemented in `blc/src/analysis/types/check_cycles.rs` (AST pass) and `blc/src/vm/optimize_ir.rs` (`detect_module_closure_cycles`). Wired into all three parse entry points in `blc/src/parse.rs`. The compiler emits `W_CYCLE_001` when it detects a mutual closure capture:

```
warning[W_CYCLE]: mutually recursive closures may form a reference cycle
  --> src/agent.bl:12:5
   |
12 |   let handler = |msg| dispatch(msg, processor)
   |       ^^^^^^^ captures `processor`
13 |   let processor = |data| transform(data, handler)
   |       ^^^^^^^^^ captures `handler`
   |
   = note: if these closures escape the current scope, their memory
           will not be reclaimed
   = help: consider using `Weak<T>` or restructuring to avoid mutual capture
```

**Phase 2 — Automatic insertion [DEFERRED — medium-term]:**
When the compiler detects a closure cycle that escapes a region, it automatically inserts `Weak.downgrade` on one edge of the cycle in the generated bytecode. The closure that is "downgraded" receives `Weak.upgrade` at each call site, with a runtime panic if the referent has been freed. The developer's source code remains clean.

```baseline
// Developer writes:
let handler = |msg| dispatch(msg, processor)
let processor = |data| transform(data, handler)

// Compiler emits (conceptually):
let handler = |msg| dispatch(msg, processor)
let processor_weak = Weak.downgrade(processor)
let handler' = |msg|
  match Weak.upgrade(processor_weak)
    Some(p) -> dispatch(msg, p)
    None -> panic!("cyclic closure referent freed")
```

This is **not** a general cycle collector — it is a targeted rewrite for a known, statically-detectable pattern. It is analogous to what OCaml and Koka do for recursive value bindings.

**Scope limitation:** This approach only handles closure cycles. If `Shared<T>` or lazy thunks are added later, they will need their own mitigation (Options B or C below).

### Option B: Opt-In Graph Types (Domain-Specific Arenas) [DEFERRED]

Instead of tracing the whole heap or making developers use `Weak<T>`, we introduce standard library types modeled for cyclic data that encapsulate arena logic.

**How it works:** We provide a `Data.Graph` module where all nodes belong to an internal opaque arena tied to the `Graph` instance. Internally, nodes refer to each other via indices (e.g., `NodeId`), not direct Perceus pointers. This sidesteps reference counting entirely for intra-graph references.

**Sketch API:**

```baseline
// Construction (persistent / FBIP-friendly — each operation returns a new graph)
let g = Graph.empty()
let (g, alice) = Graph.add_node(g, { name: "Alice", role: "admin" })
let (g, bob) = Graph.add_node(g, { name: "Bob", role: "user" })
let g = Graph.add_edge(g, alice, bob, "manages")

// Traversal
Graph.neighbors(g, alice)          // → [bob]
Graph.edges(g, alice)              // → [{ target: bob, label: "manages" }]
Graph.node(g, alice)               // → Some({ name: "Alice", role: "admin" })

// Algorithms
Graph.bfs(g, alice, |node| ...)    // breadth-first traversal
Graph.topo_sort(g)                 // → Result<List<NodeId>, CycleError>

// Bulk operations
Graph.map_nodes(g, |id, data| transform(data))
Graph.filter_nodes(g, |id, data| data.role == "admin")
```

**Design decision:** The API uses persistent (FBIP-friendly) updates. Each `add_node` / `add_edge` returns a new graph value. When the graph is uniquely owned (ref count 1), the compiler's FBIP optimization mutates in-place — giving O(1) amortized updates without the developer needing to think about it.

**Pros:** Safe, entirely avoids Perceus reference cycles, compiles down to highly cache-efficient contiguous memory (arrays of node/edge data with index-based adjacency).
**Cons:** Developer must opt-in to a specific API for building graphs.

### Option C: Backup Cycle Collector (The "Python" Approach) [NOT RECOMMENDED]
We introduce a concurrent, low-priority tracing cycle collector that runs periodically.
*   **How it works:** The primary memory management remains Perceus ARC. 99% of data (which is acyclic) is freed deterministically at `ref_count == 0`. The cycle collector *only* scans for orphaned cycles.
*   **Pros:** Zero cognitive burden on the developer. Cycles just disappear eventually.
*   **Cons:** Introduces non-determinism. While it won't cause "stop-the-world" pauses for the entire heap, it complicates the runtime and violates the "No GC" positioning of the language. **Not recommended as a default runtime component.**

### Option D: Debug-Mode Cycle Detector [DEFERRED]

Independent of the production strategy, a diagnostic cycle detector provides a critical safety net during development. At process exit (or on `--check-leaks`), the runtime runs a Bacon-Rajan trial deletion sweep over surviving ref-counted allocations and reports any leaked cycles with allocation-site stack traces.

```
$ baseline run --check-leaks src/app.bl

Leak report: 2 reference cycles detected

  Cycle 1 (2 objects, 384 bytes):
    Arc<Closure> allocated at src/agent.bl:12:5
    Arc<Closure> allocated at src/agent.bl:13:5
    → These closures capture each other. Consider restructuring
      to break the mutual reference.

  Cycle 2 (3 objects, 1.2 KB):
    Arc<Record{cache, lookup}> allocated at src/cache.bl:45:3
    Arc<Record{entries, evict}> allocated at src/cache.bl:47:3
    Arc<Closure> allocated at src/cache.bl:50:7
    → Use Data.Graph or Weak<T> for intentionally cyclic structures.
```

This is a **development tool only** — never a runtime dependency. It has zero cost in release builds. It catches accidental cycles that slip past the compiler's static analysis.

## 5. Interaction with FBIP and `@fip`

Functions annotated with `@fip` (Functional-But-In-Place) are inherently safe from closure cycles. The `@fip` annotation requires the compiler to prove that the function's captures and allocations satisfy reuse constraints — mutual closure capture would violate these constraints and be rejected at compile time. Tail-recursive `@fip` loops (the FBIP Agent pattern from the vision doc §9.5) are therefore cycle-free by construction.

This means the primary long-lived concurrent pattern in Baseline — a tail-recursive agent processing a channel — cannot produce closure cycles. The cycle risk is limited to ad-hoc closure construction outside of `@fip` contexts.

## 6. Recommendation

We recommend a **layered strategy** that addresses cycles at every level:

| Layer | Mechanism | Status | Catches |
|-------|-----------|--------|---------|
| **Compile-time** | Closure cycle warnings `W_CYCLE_001` (Option A Phase 1) | **[IMPLEMENTED]** | Mutual closure captures |
| **Compile-time** | Automatic Weak insertion (Option A Phase 2) | Deferred — medium-term | Mutual closure captures that escape regions |
| **Library** | `Data.Graph` with index-based nodes (Option B) | Deferred — medium-term | Intentional graph/network structures |
| **Runtime (regions)** | Aggressive region inference for scopes | Ongoing | All cycles within request/scope lifetime |
| **Dev tooling** | `--check-leaks` cycle detector (Option D) | Deferred — near-term | Everything else |

Concretely:

1. **For request-scoped data:** Improve compiler heuristics to aggressively prove that allocations do not escape the current handler, moving them to the arena where cycles are irrelevant. This is the highest-leverage investment.
2. **For closure cycles:** The compiler warns on mutual captures (Phase 1) and eventually auto-inserts `Weak<T>` in generated code (Phase 2), keeping developer source code clean.
3. **For intentional graph structures:** A standard library `Data.Graph` backed by indices completely solves the cyclical GC problem while providing massive L1/L2 cache locality benefits over pointer-chasing. Promote ECS / relational modeling over self-referential pointer graphs.
4. **As a safety net:** A debug-mode cycle detector catches anything the above layers miss during development.

By adopting this layered approach, Baseline can legitimately retain its "No Garbage Collector" guarantee while providing safe, ergonomic ways to model complex, interconnected domains. Developers writing normal application code will never need to think about cycles; those building graph-heavy systems get a purpose-built API that is faster than pointer-based graphs anyway.
