# Designing a new functional language for LLM code generation and web APIs

A new statically-typed functional programming language targeting LLM code generation and web API development must balance sophisticated type-theoretic foundations with pragmatic usability. Research across effect systems, refinement types, memory management, compiler diagnostics, adoption patterns, and LLM capabilities reveals a clear design blueprint: **algebraic effects with row polymorphism, tiered refinement types, Perceus-style reference counting, human-centered error messages, and explicit type annotations that optimize for LLM generation accuracy**.

## Algebraic effects outperform monads for composition and usability

Effect systems determine how a language handles side effects—the core challenge in functional programming. Three approaches dominate: monads (Haskell), algebraic effects (Koka, Eff), and capabilities (Scala 3). For a language targeting web APIs, **algebraic effects with row polymorphism** provide the optimal balance of expressiveness, inference, and performance.

Monads encapsulate effects within sequential computation but suffer from the **monad transformer problem**—ordering matters (StateT over ErrorT ≠ ErrorT over StateT), and composing multiple effects requires explicit lifting. Daan Leijen's research at Microsoft demonstrates that algebraic effects solve this by separating effect signatures from their interpretation via handlers. The landmark paper "Monad Transformers and Modular Algebraic Effects: What Binds Them Together" (Haskell 2019) proves that algebraic effects compose freely since they're restricted to free monads.

**Row polymorphism**, pioneered in Koka, enables Hindley-Milner-style inference for effects. Effect types become rows `⟨l₁, l₂, ... | µ⟩` where µ is an effect variable, allowing functions like `map` to be naturally effect-polymorphic without explicit annotations. Leijen's "Type Directed Compilation of Row-Typed Algebraic Effects" (POPL 2017) demonstrates that this approach achieves full inference with competitive performance.

Frank's "ambient ability propagation" offers an elegant refinement—effect variables propagate inward rather than accumulating outward, eliminating explicit effect variables from source code. Unison adopted this approach, calling their effects "abilities." Runar Bjarnason notes: "Monads are awkward—they come with syntactic overhead as well as cognitive overhead...you spend time figuring out how to lift things."

For **compilation strategy**, Koka's evidence-passing approach (ICFP 2021 Distinguished Paper) achieves competitive performance without a complex runtime. The "Efficient Compilation of Algebraic Effect Handlers" (OOPSLA 2021) by Karachalias et al. shows type-and-effect directed optimization eliminates most handler overhead, achieving performance competitive with Multicore OCaml's dedicated runtime.

**Design recommendation**: Implement row-polymorphic algebraic effects with automatic inference and evidence-passing compilation. Direct-style code (enabled by algebraic effects) is easier for LLMs to generate than monadic code, and web APIs naturally model effects like HTTP, database access, and async operations.

## Refinement types should be tiered from automatic to expert

Refinement types augment standard types with logical predicates—`{n: Int | n > 0}` specifies positive integers. Research on Liquid Haskell, F*, and Dafny reveals that **practical refinement types must balance automation against verification power**, with significant usability implications.

The "Liquid Types" algorithm (Rondon, Kawaguchi, Jhala; PLDI 2008) combines Hindley-Milner inference with predicate abstraction. Key innovation: predicates are restricted to conjunctions of user-provided qualifiers, making inference decidable while remaining expressive. This reduced annotation burden from **31% to under 1%** compared to Dependent ML on the same benchmarks, while verifying over **10,000 lines** of production Haskell libraries.

SMT integration presents critical tradeoffs. All three systems use Z3, but they differ in what they attempt to verify. Liquid Haskell restricts to decidable fragments (quantifier-free uninterpreted functions + linear arithmetic), ensuring predictable verification times. F* supports full dependent types with more manual proof effort. Dafny occupies a middle ground with auto-active verification requiring explicit loop invariants.

A major usability study, "On the Impact of Formal Verification on Software Development" (Mugnier et al., OOPSLA 2025), interviewed **14 experienced Dafny users** and identified critical adoption barriers:

- **Error messages are opaque**: "The thing just tells you I can't prove that 0.1 * 0.1 is greater than 0 without any further explanation"
- **Proof brittleness**: Small code changes can break proofs, causing "existential dread" (P7) and month-long "major refactoring" (P6)
- **Annotation burden**: Often **10× more proof code** than implementation

The study identified two developer personas: "formal-first" developers who appreciate automation, and "engineering-first" developers who want quick fixes. Current tools serve the former poorly.

**Design recommendation**: Implement a **tiered refinement type system**:

- **Tier 1 (Zero annotation)**: Automatic null-safety, bounds checking, division-by-zero prevention through built-in qualifiers
- **Tier 2 (Simple refinements)**: User-defined refined types like `type Port = {n: Int | 0 < n && n <= 65535}` and built-in web types (URL, Email, UUID)
- **Tier 3 (Contracts)**: Pre/postconditions for critical code paths (payments, authentication)

Restrict to decidable SMT fragments, set strict resource limits for reproducibility, and provide escape hatches (`@unchecked`). For LLM-generated code, keep annotations minimal and ensure error messages identify exactly which constraint failed.

## Perceus reference counting enables functional efficiency without GC

For a language without garbage collection, four approaches merit consideration: region-based memory (MLKit), ownership systems (Rust), generational references (Vale), and **Perceus reference counting with reuse analysis** (Koka).

Region-based memory management (Tofte & Talpin, 1997) divides the heap into pools deallocated in bulk. MLKit's retrospective (HOSC 2004) notes that while theoretically elegant, "small changes to programs can have drastic, unintuitive effects on object lifetimes." Regions work well for arena-style patterns but struggle with complex lifetimes.

Rust's ownership system provides zero-runtime-overhead memory safety but imposes significant cognitive burden. More problematic for functional languages: tail-call optimization is often unsound in Rust, making functional programs "second-class citizens."

Vale's generational references offer a pragmatic alternative—every object has a generation counter, and dereferencing asserts generations match. This allows mutable aliasing (impossible in Rust) with only **10.84% overhead**, less than half of naive reference counting. However, this is still runtime checking.

**Perceus** (Reinking, Xie, de Moura, Leijen; PLDI 2021 Distinguished Paper) represents the breakthrough for functional languages. Unlike scoped reference counting (C++ shared_ptr, Swift), Perceus frees objects immediately when references reach zero—halving memory usage for operations like `map`. The key innovations:

1. **Drop specialization**: Inline and specialize `drop` operations, then fuse matching `dup`/`drop` pairs, eliminating most RC operations
2. **Reuse analysis**: Pair pattern matches with constructors of the same size—when an object is unique (refcount=1), its memory is reused directly for new allocations
3. **FBIP (Functional-But-In-Place)**: Purely functional algorithms execute with in-place mutation semantics when data is uniquely owned

The follow-up paper "FP²: Fully in-Place Functional Programming" (Lorenzen, Leijen, Swierstra; ICFP 2023) formalizes this with "reuse credits"—matching a constructor earns a credit, constructing requires spending one. Functions can be marked `fip` (fully in-place) for compile-time verification of zero allocation.

Benchmarks show Koka with Perceus competes with OCaml and Haskell, with purely functional red-black tree insertion within **10% of C++ std::map** (in-place mutating). Lobster's compile-time ownership analysis demonstrates that **95% of RC operations** can be eliminated statically.

**Design recommendation**: Adopt Perceus-style reference counting with FBIP, enabling purely functional code to execute with imperative efficiency. For concurrent workloads, use thread-local RC by default (fast path) with atomic RC only for explicitly shared data. Consider linear types as a gradual addition for static guarantees, following Linear Haskell's approach of linear function arrows.

## Error messages must be human-centered and machine-parseable

Elm and Rust have established the industry standard for compiler diagnostics. Academic research (Becker et al.'s survey covering **300+ papers over 60 years**) confirms that error messages significantly impact learning and productivity, yet remain "consistently described as inadequate, frustrating, and undecipherable."

Evan Czaplicki's philosophy for Elm: "Compilers should be assistants, not adversaries. They should not just detect bugs but help you understand why there is a bug." His 2015 "Compiler Errors for Humans" blog post directly influenced Rust's diagnostic improvements.

Rust's diagnostics follow a precise structure: error code, primary message, source location with spans, secondary labels for context, notes for explanation, and help with actionable suggestions. The Rust style guide mandates: "If your message cannot be understood by a normal programmer who just came out of bed after a night partying, it's too complex."

Rust's suggestion system distinguishes confidence levels: **MachineApplicable** (safe to auto-apply), **HasPlaceholders** (requires user input), **MaybeIncorrect** (might not match intent), and **Unspecified**. This enables `rustfix` to automatically apply certain corrections while flagging uncertain ones.

Academic research (Marceau, Fisler, Krishnamurthi; SIGCSE 2011 Best Paper) developed rubrics measuring whether student code edits reflect understanding, quantifying "the great extent with which poor error messages undermine computer programming education."

For complex type systems, TypeScript demonstrates common pitfalls—generic constraint errors often confuse because they report `Can't assign false to T` without explaining that T could be a more specific subtype. Best practice: show concrete type instantiations, not just type parameters.

**Design recommendation**: Implement structured diagnostics with:

- **Multi-format output**: Human-readable terminal output with colors, JSON for tools (LSP, IDE plugins), plain text for accessibility
- **Progressive disclosure**: Brief by default, expanded pedagogical content on demand via error codes
- **Library author customization**: `#[on_unimplemented]` attributes allowing domain-specific error messages
- **LLM-optimized format**: Exact byte offsets, directly applicable fix suggestions, consistent JSON schema

For effect system errors specifically: explain what effect the code requires versus what's available, show the effect handler stack, and provide "effect diff" visualization.

## Language adoption requires ecosystem over features

Empirical research (Meyerovich & Rabkin, OOPSLA 2013) analyzing **200,000+ SourceForge projects** and surveys of **1,000-13,000 programmers** establishes that **extrinsic factors dominate intrinsic language features**. Open source libraries, existing code, and developer experience strongly influence choice. Performance, reliability, and simple semantics do not.

Go succeeded through deliberate simplicity. Rob Pike: "The stability—Go programs written in 2012 will still compile and run perfectly today—was a huge enabler for growth." The Go 1 compatibility promise gave companies confidence. Tooling from day one (gofmt, gopls) and killer applications (Docker, Kubernetes) cemented adoption.

Rust achieved adoption despite a steep learning curve through a clear value proposition (memory safety without GC), excellent tooling (Cargo has 71% admiration rating—highest in its category), and community building (Rust is consistently "most loved" in Stack Overflow surveys at 72% in 2025). The Rust Foundation structure—AWS, Google, Microsoft, Huawei as founding members—combines corporate backing with community governance.

TypeScript succeeded where CoffeeScript, Flow, and Dart failed because any valid JavaScript code is also valid TypeScript—zero switching cost. Dropbox's migration of **100,000+ lines from CoffeeScript to TypeScript** highlights why: TypeScript's `--strictNullChecks` eliminated an entire class of errors while preserving JavaScript investment.

Haskell remains niche despite 30+ years and academic prestige because, as one analysis notes: "A C, Java, Python, or Ruby programmer can pick up Go easily. They can't pick up Haskell so easily, as even in beginner Haskell, you are immediately confronted with lots of unfamiliar concepts." The hiring bottleneck creates a vicious cycle.

OCaml demonstrates that corporate success (Jane Street uses it for everything) doesn't translate to ecosystem growth. Jane Street's tools remain internal; their open-source build system Jenga saw "weak enough" adoption that they "actually decided to un-open source it."

**Design recommendation**: Prioritize:

1. **Gradual adoption path**: Enable use alongside existing languages (Python, TypeScript); support calling existing ecosystems (npm, PyPI)
2. **Tooling before launch**: Package manager (like Cargo), LSP server, auto-formatter, helpful error messages
3. **Stability commitment from 1.0**: Go's compatibility promise was "critical to success"
4. **JavaScript/WASM compilation target**: For web APIs, npm package compatibility is essential
5. **Familiar syntax**: C-family syntax is more accessible than ML-family; avoid pure functional purity requirements

## LLM code generation favors explicit types and moderate verbosity

Research reveals that **33.6% of failed LLM-generated programs fail due to type errors** (Tambon et al., 2025; Dou et al., 2024). GitHub Copilot evaluation on LeetCode showed **24% of suggestions resulted in compilation errors**, mainly from type mismatches. For code translation, GPT-4 produces **44.3% compilation errors**.

Type-constrained decoding (arXiv 2504.09246) uses "prefix automata" to guide generation toward well-typed code, **reducing compilation errors by more than half** on HumanEval and MBPP. The TyFlow system (arXiv 2510.10216) internalizes type reasoning by maintaining isomorphism between type derivation trees and synthesis derivation trees—key insight: **LLMs treat programs as plain text and struggle to recover underlying type systems**.

The FPEval benchmark (arXiv 2601.02060) evaluated GPT models across Haskell, OCaml, Scala, and Java, finding that **error rates remain significantly higher in purely functional languages** than in hybrid or imperative languages. LLMs frequently generate non-idiomatic functional code that follows imperative patterns. Research confirms: knowledge of imperative languages does not transfer well to functional languages.

For syntax, the MultiPL-E benchmark (translating HumanEval to 18+ languages) found that **low-resource languages (Rust, Kotlin, Swift) show significantly lower performance** than high-resource languages. However, GPT-4 shows comparable performance across Python, C++, Java, and JavaScript with appropriate prompting—variation from prompting technique exceeds variation from language syntax.

Training data bootstrapping strategies include:

- **Self-Instruct** (Code Alpaca): Start with ~20 seed tasks, use LLM to generate 20K+ examples
- **OSS-Instruct** (Magicoder): Collect code snippets from GitHub, generate instruction prompts that would lead to them, creating 75K synthetic pairs
- **TransCoder** (NeurIPS 2020): Unsupervised translation trained on 2.8M repositories achieved **74.8% accuracy on C++-to-Java**, outperforming commercial tools by 33 percentage points

With **100 real training examples**, augmenting with synthetic examples yields **3-26% improvement**. Beyond 1000 real examples, synthetic augmentation provides diminishing returns.

**Design recommendation**: Design for LLM generation success:

- **Explicit, visible type annotations** rather than heavy inference (LLMs must see types to use them)
- **Moderate verbosity**: Python/TypeScript level, not Haskell terseness or Java boilerplate
- **Support imperative patterns** alongside functional idioms (hybrid approach like Scala)
- **Bootstrap strategy**: Design syntax similar to well-supported languages (TypeScript, Rust), create ~100-1000 high-quality examples, use translation from related languages, apply synthetic expansion to 20K-75K examples, filter through execution-based testing

## Synthesized design blueprint

For a new statically-typed functional language targeting LLM code generation and web API development, research across all six domains converges on a coherent design:

**Type system**: Row-polymorphic algebraic effects with automatic inference, tiered refinement types (automatic safety → simple refinements → full contracts), explicit type annotations visible to LLMs, and a type-checker API enabling constrained decoding integration.

**Memory management**: Perceus-style reference counting with FBIP, enabling purely functional code to execute with in-place mutation when data is uniquely owned. Thread-local RC by default, optional linear type annotations for static guarantees.

**Syntax**: C-family syntax with TypeScript-level verbosity, supporting both functional idioms and imperative patterns. Clear block structure with explicit delimiters. Keywords like `effect`, `handle`, `ensure` for domain concepts.

**Tooling**: Package manager, LSP server, and auto-formatter ready at 1.0. JSON error output with exact spans for LLM self-repair. Type-directed constrained decoding support. Translation bridges to Python/JavaScript for data generation.

**Ecosystem strategy**: JavaScript/WASM compilation targets, npm package interoperability, Python FFI for AI/ML ecosystem access. Go-style compatibility promise. Clear migration path from TypeScript for gradual adoption.

The research is unambiguous: **technical excellence is necessary but insufficient** for language success. A language optimized for LLM code generation and web APIs must excel at pragmatic usability—excellent error messages, familiar syntax, rich ecosystem, and explicit types that both humans and machines can understand.
