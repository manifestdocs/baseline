# Brainstorming: Efficient Array/List Operations in Baseline

The Fannkuch and Nsieve benchmarks highlight a limitation in Baseline's current memory model: **the lack of efficient array/list mutations**. Currently, operations like `List.set` or `List.concat` require copying the entire list, resulting in $O(N)$ operations where $O(1)$ is expected. This makes algorithms heavily dependent on array mutation perform poorly.

Baseline's core DNA emphasizes:
*   **Explicit Code & Simplicity:** Avoiding hidden complexity or "magic" performance optimizations.
*   **Predictability:** Developers should easily reason about performance.
*   **Value Semantics / Functional Core:** Immutable-by-default structures.

Here are a few approaches Baseline could take to introduce efficient array mutations while respecting these principles.

---

### Approach 1: Re-using the Existing Clone-on-Write (CoW) Mechanism
Baseline already implements CoW for large allocations (Strings, Lists, Maps) under the hood using `Arc`. The runtime (`nvalue.rs`) knows if an object has a reference count of 1.

*   **How it works:** We can expose a `List.set_owning` (or keep the name `List.set` and optimize internally) which checks if `Arc::get_mut` succeeds. If the list is uniquely owned (refcount == 1), it mutates the backing vector in-place ($O(1)$). Otherwise, it clones and mutates ($O(N)$). 
*   **Pros:** Requires **zero language changes**! We just add a fast path to the standard library native functions. It stays pure from the user's perspective.
*   **Cons:** Performance is "hidden". If a user accidentally retains a reference to an old version of the list (e.g., by capturing it in a closure), the list gets copied silently, causing a sudden performance cliff.
*   **Baseline Fit:** **High.** We already do this for `List.reverse_owning` and `Map.insert_owning`. We just need to expand it and maybe add a lint or diagnostic for accidental aliasing.

---

### Approach 2: Linear Types / Uniqueness Typing
Introduce a lightweight type-system feature to track unique ownership of data.

*   **How it works:** A type like `mut List<Int>` or `#unique List<Int>` would guarantee that there is exactly one reference to the list at any given time. If the compiler statically proves uniqueness, it compiles `List.set` to an in-place mutation.
*   **Pros:** Predictable performance. The compiler enforces that the fast-path is always taken.
*   **Cons:** High complexity. It requires adding affine or linear types to the type checker, complicating the language specification and increasing the learning curve.
*   **Baseline Fit:** **Low.** Baseline tries to avoid complex type system features unless absolutely necessary.

---

### Approach 3: Persistent Data Structures (RRB-Trees / Trie Vectors)
Instead of flat arrays, use a specialized data structure like a Radix-Balanced Tree or Hash Array Mapped Trie (HAMT) for `List`.

*   **How it works:** A `List` is represented as a tree of small arrays (e.g., chunks of 32). An update creates a new path from the root to the leaf, taking $O(\log_{32} N)$ time. 
*   **Pros:** Predictable performance regardless of aliasing. Maintains strict immutability semantics.
*   **Cons:** $O(\log_{32} N)$ is slower than $O(1)$ array access. It adds significant complexity to the runtime and garbage collector/refcounter, potentially hurting small-list performance (which is the most common use case).
*   **Baseline Fit:** **Medium.** It's a classic functional programming solution (used by Clojure, Scala), but it might violate the "Simplicity First" rule by making the runtime much heavier.

---

### Approach 4: Encapsulated State (`ST` Effect / Scoped Mutability)
Borrow the concept from Haskell's `ST` monad, but adapt it to Baseline's Effect system.

*   **How it works:** Introduce a `Mutable_Array<T>` type and a set of effects (e.g., `effect read`, `effect write`). You can allocate a mutable array, mutate it in a loop, and then "freeze" it into an immutable `List` when leaving a handler.
*   ```baseline
    fn run_fannkuch(...) -> Int / { Mutate } = {
       let arr = MutableArray.new(10)
       MutableArray.set!(arr, 0, 42)
       // ...
    }
    ```
*   **Pros:** Safe, zero-cost abstractions. Fits well with the existing Algebraic Effects system. 
*   **Cons:** Requires new types (`MutableArray`) and explicitly effectful APIs.
*   **Baseline Fit:** **Medium-High.** If Baseline wants to keep strict purity but allow performant state, Effects are the designated mechanism. This is more explicit than CoW.

---

### Recommendation for Baseline

If we look at `List.sort_owning` and `List.reverse_owning` in `blc/src/vm/natives/list.rs`, Baseline **already embraces the CoW (Approach 1)** strategy.

The most "Baseline" way forward is to:
1.  Implement `List.set` using `try_unwrap_heap` to mutate in-place if uniquely owned.
2.  Add a `List.init(n, val)` to quickly allocate an owned list.
3.  If users hit performance cliffs, introduce a `MutableArray` type utilizing the Effect system for extreme performance scenarios.
