# CPU Benchmark Reference Results

**Date:** 2026-02-08T21:13:19Z
**System:** arm64 Darwin 26.2
**Runs:** 3 (median)

## fib(35) — recursive Fibonacci, tests function call overhead

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.042 | 6448 | 0.9x |
| c | 0.047 | 1296 | 1.0x |
| go | 0.048 | 3584 | 1.0x |
| rust | 0.056 | 1472 | 1.2x |
| ocaml | 0.058 | 2128 | 1.2x |
| node | 0.154 | 44992 | 3.3x |
| baseline_vm | 0.599 | 4320 | 12.7x |
| ruby | 0.968 | 34304 | 20.6x |
| python | 1.027 | 14480 | 21.9x |
| fsharp | 1.273 | 287888 | 27.1x |

## tak(30,20,10) — Takeuchi function, deep recursion stress test

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.075 | 6640 | 0.7x |
| c | 0.106 | 1296 | 1.0x |
| rust | 0.107 | 1472 | 1.0x |
| go | 0.116 | 3808 | 1.1x |
| ocaml | 0.129 | 2128 | 1.2x |
| node | 0.351 | 45024 | 3.3x |
| fsharp | 1.433 | 254912 | 13.5x |
| ruby | 2.707 | 34384 | 25.5x |
| baseline_vm | 3.551 | 4400 | 33.5x |
| python | 3.567 | 14480 | 33.7x |

## divsum(10000) — sum of divisors 1..N, loop-heavy arithmetic

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| c | 0.049 | 1280 | 1.0x |
| rust | 0.060 | 1472 | 1.2x |
| go | 0.060 | 3936 | 1.2x |
| baseline_jit | 0.076 | 6608 | 1.6x |
| ocaml | 0.080 | 2112 | 1.6x |
| node | 0.140 | 45712 | 2.9x |
| fsharp | 1.230 | 236624 | 25.1x |
| python | 1.588 | 14480 | 32.4x |
| ruby | 1.733 | 33920 | 35.4x |
| baseline_vm | 2.415 | 4448 | 49.3x |

