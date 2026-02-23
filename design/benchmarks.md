# Baseline Benchmark Strategy

**Target:** [hanabi1224/Programming-Language-Benchmarks](https://github.com/hanabi1224/Programming-Language-Benchmarks) — industry-recognized, Docker-based benchmark suite used by ~28 languages. Published results at [programming-language-benchmarks.vercel.app](https://programming-language-benchmarks.vercel.app/).

**Status:** 3 of 18 problems implemented and **output-validated** against reference. JIT produces competitive native code.

---

## Validated Results (2026-02-22)

All three implementations produce **byte-identical output** to hanabi reference files:

| Problem | Input | Expected | Baseline Output | Status |
|---------|-------|----------|-----------------|--------|
| **binary-trees** | 6 | `6_out` | ✅ Match | Correct |
| **binary-trees** | 10 | `10_out` | ✅ Match | Correct |
| **nbody** | 1000 | `1000_out` (`-0.169075164` / `-0.169087605`) | ✅ Match | Correct |
| **fasta** | 1000 | `1000_out` | ✅ Match | Correct |

### JIT Performance (CPU Micro-Benchmarks)

From `benchmarks/cpu/results/reference.json`, measured on arm64 Darwin 26.2:

| Benchmark | C | Baseline JIT | JIT vs C |
|-----------|---|-------------|----------|
| fib(35) | 0.043s | 0.050s | **1.2x** |
| tak | 0.097s | 0.082s | **0.8x** ✦ |
| divsum | 0.052s | 0.059s | **1.1x** |

✦ JIT is *faster* than C on tak — Cranelift tail call optimization is working exceptionally well.

---

## Hanabi Submission Requirements

### How it Works

1. Each language has a `bench/include/<lang>/` directory with build config (Dockerfile, project files)
2. Source files go in `bench/algorithm/<problem>/<N>.<ext>` (e.g., `1.bl`)
3. A .NET CLI tool runs: **build** → **test** (byte-for-byte diff against `*_out` files) → **bench** (timing)
4. All builds run in Docker/Podman containers for reproducibility

### What Baseline Needs

1. **File extension mapping**: `.bl` → `baseline` language name
2. **Docker image**: Rust toolchain to build `blc`, then `blc run --jit` to execute
3. **Include directory**: `bench/include/baseline/` with Dockerfile and build script
4. **Source files**: `bench/algorithm/<problem>/1.bl` for each problem

### The 18 Benchmark Problems

| Problem | Status | Notes |
|---------|--------|-------|
| **binarytrees** | ✅ Ready | Allocation/GC, recursion, sum types |
| **nbody** | ✅ Ready | Floating-point, struct mutation |
| **fasta** | ✅ Ready | I/O throughput, PRNG, string building |
| helloworld | Trivial | Quick win, do first |
| spectral-norm | Not started | Dense linear algebra, just math |
| fannkuch-redux | Not started | Integer permutation, parallelism |
| nsieve | Not started | Sieve, integer computation |
| mandelbrot | Not started | Complex arithmetic, bit output |
| knucleotide | Not started | Hash tables, needs stdin |
| regex-redux | Not started | Needs PCRE2 FFI |
| pidigits | Not started | Needs GMP FFI |
| edigits | Not started | Same as pidigits |
| merkletrees | Not started | Variant of binarytrees |
| json-serde | Not started | Needs JSON parser |
| coro-prime-sieve | Not started | Coroutines / channels |
| http-server | Not started | HTTP stack |
| lru | Not started | LRU cache data structure |
| secp256k1 | Not started | Crypto, needs FFI |

---

## Phase 1: First PR (Immediate)

### Step 1: Fork and Set Up

```bash
# Fork hanabi1224/Programming-Language-Benchmarks
git clone git@github.com:<your-fork>/Programming-Language-Benchmarks.git
cd Programming-Language-Benchmarks
```

### Step 2: Create Include Directory

Create `bench/include/baseline/` with:

**`Dockerfile`** — builds the compiler, runs programs:
```dockerfile
FROM rust:1.85-slim as builder

# Install blc from source
COPY blc-src/ /opt/blc-src/
WORKDIR /opt/blc-src
RUN cargo build --features jit --release
RUN cp target/release/blc /usr/local/bin/blc

FROM debian:bookworm-slim
COPY --from=builder /usr/local/bin/blc /usr/local/bin/blc

# The harness copies source to /tmp/app/ and calls build/run scripts
WORKDIR /tmp/app
```

**Build/run scripts** as needed by the harness — typically a `build.sh` that compiles and a `run.sh` that executes. Since Baseline uses `blc run --jit`, the build step can be a no-op and the run step invokes `blc run --jit app.bl -- $@`.

### Step 3: Add Source Files

Copy and rename existing implementations:

```
bench/algorithm/binarytrees/1.bl  ← benchmarks/hanabi/binary-trees/binary_trees.bl
bench/algorithm/nbody/1.bl        ← benchmarks/hanabi/nbody/nbody.bl
bench/algorithm/fasta/1.bl        ← benchmarks/hanabi/fasta/fasta.bl
bench/algorithm/helloworld/1.bl   ← trivial (2 lines)
```

### Step 4: Test Locally

```bash
cd bench
# Test correctness
dotnet run --project tool -- --task test --langs baseline --problems binarytrees nbody fasta
# Benchmark
dotnet run --project tool -- --task bench --langs baseline --problems binarytrees nbody fasta
```

### Step 5: Submit PR

Open PR with:
- 4 source files (helloworld, binarytrees, nbody, fasta)
- Include directory with Dockerfile
- Brief in the PR description: "Adding Baseline — a functional language with algebraic effects, compiled via Cranelift JIT"

---

## Phase 2: Expand Coverage (Weeks 2–4)

### Priority Problems (No FFI Required)

These should be straightforward to implement:

| Problem | Priority | What It Tests | Expected Competitiveness |
|---------|----------|---------------|--------------------------|
| **spectral-norm** | High | Dense linear algebra | Good — pure numeric, JIT strong here |
| **fannkuch-redux** | High | Integer permutation | Good — similar to tak profile |
| **nsieve** | High | Sieve computation | Good — simple loops |
| **mandelbrot** | Medium | Complex arithmetic, bit output | Moderate — needs byte output |
| **merkletrees** | Medium | Variant of binarytrees | Good — same pattern we already have |

### Problems Requiring New Capabilities

| Problem | Blocker | When Feasible |
|---------|---------|---------------|
| knucleotide | Needs stdin input, hash tables | After stdin support |
| coro-prime-sieve | Needs channels/coroutines | After bounded channels |
| lru | Needs mutable data structures | After mutability support |
| regex-redux | Needs PCRE2 FFI | After FFI |
| pidigits / edigits | Needs GMP FFI | After FFI |
| json-serde | Needs JSON parser | After stdlib |
| secp256k1 | Needs crypto FFI | After FFI |
| http-server | Needs HTTP stack | After async runtime |

---

## Phase 3: Optimize and Improve Rankings (Weeks 4–8)

### Optimization Checklist Per Problem

**Level 1 — Algorithmic** (read the fastest C/Rust implementation first)
- Same algorithm?
- Same parallelism strategy?
- Same I/O strategy (buffered writes)?

**Level 2 — Language-level**
- Eliminate unnecessary allocations
- Use mutable local state where needed
- Inline hot lambdas
- Fixed-size arrays where sizes are known

**Level 3 — Compiler-level**
- Profile with `perf stat` / `perf record`
- Check Cranelift IR for missed optimizations (bounds checks, failed inlining, register pressure)
- File compiler issues for each missed optimization

### Expected Impact

| Optimization | Speedup | Effort |
|---|---|---|
| Parallel execution | 2–4x | Medium |
| Allocation elimination | 1.5–3x | Medium |
| Inlining | 1.2–2x | Low |
| I/O buffering | 1.5–5x | Low |
| SIMD | 1.2–2x | High |
| Effect erasure | 1.1–1.3x | Medium |

---

## Phase 4: Official Benchmarks Game (Months 4–6)

Once hanabi results are published, use them as evidence to request inclusion in the [official Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/) (~25 languages, updated annually).

Requirements:
- Stable compiler on Ubuntu 24.04
- Simple installation (apt or binary)
- Community traction
- Submit programs one at a time as GitLab issues

Submit best-performing problems first — currently tak (0.8x C) would be the headline result.

---

## Existing Infrastructure

### `benchmarks/cpu/` — Internal Micro-Benchmarks

8 benchmarks in 9 languages with regression harness:
- `run_all.sh` — full cross-language comparison, saves JSON + markdown
- `bench.sh` — Baseline-only regression test with ±10% threshold
- Benchmarks: fib, tak, divsum, primes, treemap, mergesort, strrev, mapbuild

### `benchmarks/hanabi/` — Contest Implementations

Three validated implementations ready for submission:
- `binary-trees/binary_trees.bl` (60 lines) — sum types, recursion
- `nbody/nbody.bl` (249 lines) — struct spread updates, tuple state threading
- `fasta/fasta.bl` (121 lines) — PRNG, cumulative probability selection

### `benchmarks/` — HTTP Server Comparison

Baseline vs Axum (Rust) vs Fiber (Go) with wrk load testing.

---

## Key Risks

| Risk | Mitigation |
|---|---|
| Cranelift slower than LLVM on numeric loops | Accept for now; already competitive on integer recursion |
| GC pauses on binary-trees at large inputs | Arena allocation, region-based memory |
| Allocation overhead on collections benchmarks | CoW optimizations (see `rfc-cow-optimizations.md`) |
| hanabi maintainer doesn't merge quickly | Results still useful for blog posts, talks, internal tracking |

---

## Success Metrics

| Metric | Target | Status |
|---|---|---|
| Implementations correct (byte-identical) | 3/3 validated | ✅ **Done** |
| JIT competitive on integer recursion | <1.5x C | ✅ **Done** (0.8–1.2x) |
| PR submitted to hanabi1224 | Month 1 | ◯ |
| PR merged, results published | Month 2 | ◯ |
| 5+ problems submitted | Month 2 | ◯ |
| Official Benchmarks Game issue opened | Month 4 | ◯ |
