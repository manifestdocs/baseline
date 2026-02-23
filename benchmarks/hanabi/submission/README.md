# Baseline — Hanabi Benchmark Submission

Submission materials for adding [Baseline](https://github.com/manifestdocs/baseline) to [hanabi1224/Programming-Language-Benchmarks](https://github.com/hanabi1224/Programming-Language-Benchmarks).

## Benchmarks Included

| Problem | File | Validated Against |
|---------|------|-------------------|
| helloworld | `helloworld/1.bl` | Expected "Hello, World!" |
| binarytrees | `binarytrees/1.bl` | C reference (inputs 6, 10) |
| nbody | `nbody/1.bl` | C reference (inputs 1000, 10000) |
| fasta | `fasta/1.bl` | C reference (input 1000) |

All outputs are byte-identical to the C reference implementations.

## Submission Steps

### 1. Fork the Repo

```bash
gh repo fork hanabi1224/Programming-Language-Benchmarks --clone
cd Programming-Language-Benchmarks
```

### 2. Copy Source Files

```bash
cp submission/helloworld/1.bl    bench/algorithm/helloworld/1.bl
cp submission/binarytrees/1.bl   bench/algorithm/binarytrees/1.bl
cp submission/nbody/1.bl         bench/algorithm/nbody/1.bl
cp submission/fasta/1.bl         bench/algorithm/fasta/1.bl
```

### 3. Copy Config

```bash
cp submission/bench_baseline.yaml bench/bench_baseline.yaml
```

### 4. Build and Publish Docker Image

From the baseline repo root:

```bash
docker build -f benchmarks/hanabi/submission/Dockerfile -t ghcr.io/<your-org>/baseline-bench:latest .
docker push ghcr.io/<your-org>/baseline-bench:latest
```

Update `bench_baseline.yaml` to reference your published image URL.

### 5. Local Validation

With `blc` installed locally:

```bash
blc run helloworld/1.bl
# Expected: Hello, World!

blc run binarytrees/1.bl -- 6
# Expected: stretch tree of depth 7  check: 255 ...

blc run nbody/1.bl -- 1000
# Expected: -0.169075164 / -0.169087605

blc run fasta/1.bl -- 1000
# Diff against bench/algorithm/fasta/1000_out
```

### 6. Submit PR

```bash
gh pr create --title "Add Baseline language" --body "$(cat <<'EOF'
## Summary

Add Baseline as a new language with 4 benchmark implementations:
- helloworld
- binarytrees
- nbody
- fasta

Baseline is a strongly typed, effect-tracked functional language with a bytecode VM runtime.
All outputs validated byte-identical against C reference implementations.

## Runtime

- Compiler: `blc` (Rust, tree-sitter parser)
- Backend: Bytecode VM (stack-based, NaN-boxed values)
- Docker image: `ghcr.io/<org>/baseline-bench:latest`
EOF
)"
```

## Runtime Notes

- **Backend:** Bytecode VM (`blc run`). A Cranelift JIT backend exists (`blc run --jit`) but has correctness issues on some benchmarks. The VM backend produces correct output for all programs.
- **Performance:** The VM achieves ~CPython parity on integer recursion (233M ops/sec). JIT reaches 0.8-1.2x C on integer benchmarks when working correctly.
- **Source format:** Single-file programs, no build step needed. The harness copies source to `app.bl` and runs `blc run app.bl -- <args>`.

## File Inventory

```
submission/
├── README.md              # This file
├── Dockerfile             # Multi-stage build for blc Docker image
├── bench_baseline.yaml    # Harness config (goes in bench/)
├── helloworld/1.bl        # → bench/algorithm/helloworld/
├── binarytrees/1.bl       # → bench/algorithm/binarytrees/
├── nbody/1.bl             # → bench/algorithm/nbody/
└── fasta/1.bl             # → bench/algorithm/fasta/
```
