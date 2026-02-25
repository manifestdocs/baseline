# CPU Benchmark Reference Results

**Date:** 2026-02-23T22:37:56Z
**System:** arm64 Darwin 26.2
**Runs:** 3 (median)

## divsum

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| go | 0.057 | 3504 | — |
| baseline_jit | 0.059 | 12128 | — |
| rust | 0.060 | 1456 | — |
| ocaml | 0.062 | 2112 | — |
| node | 0.142 | 45584 | — |
| fsharp | 1.238 | 285616 | — |
| python | 1.474 | 14480 | — |
| ruby | 1.649 | 33792 | — |
| baseline_vm | 41.750 | 9776 | — |

## fib

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| rust | 0.044 | 1440 | — |
| go | 0.046 | 3408 | — |
| baseline_jit | 0.047 | 11872 | — |
| ocaml | 0.050 | 2112 | — |
| node | 0.146 | 44960 | — |
| ruby | 0.906 | 33760 | — |
| fsharp | 1.007 | 287856 | — |
| python | 1.016 | 14448 | — |
| baseline_vm | 10.039 | 9856 | — |

## mapbuild

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| ocaml | 0.009 | 1856 | — |
| rust | 0.014 | 2176 | — |
| go | 0.016 | 4208 | — |
| baseline_jit | 0.027 | 23168 | — |
| python | 0.049 | 14880 | — |
| baseline_vm | 0.066 | 11712 | — |
| node | 0.080 | 48240 | — |
| ruby | 0.268 | 34368 | — |
| fsharp | 0.723 | 265552 | — |

## mergesort

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| ocaml | 0.009 | 1840 | — |
| rust | 0.011 | 1648 | — |
| go | 0.015 | 4384 | — |
| baseline_jit | 0.021 | 15056 | — |
| python | 0.048 | 14608 | — |
| node | 0.075 | 47904 | — |
| ruby | 0.260 | 33888 | — |
| fsharp | 0.722 | 265648 | — |
| baseline_vm | 1.383 | 11568 | — |

## primes

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| ocaml | 0.009 | 1856 | — |
| go | 0.018 | 3584 | — |
| rust | 0.020 | 1456 | — |
| baseline_jit | 0.023 | 12272 | — |
| node | 0.085 | 45648 | — |
| python | 0.424 | 14400 | — |
| ruby | 0.525 | 33680 | — |
| fsharp | 0.679 | 265456 | — |
| baseline_vm | 6.615 | 10032 | — |

## strrev

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| ocaml | 0.008 | 1872 | — |
| rust | 0.013 | 1552 | — |
| go | 0.014 | 4496 | — |
| python | 0.038 | 14528 | — |
| baseline_jit | 0.042 | 52368 | — |
| baseline_vm | 0.046 | 10928 | — |
| node | 0.062 | 44240 | — |
| ruby | 0.216 | 34080 | — |
| fsharp | 0.705 | 265504 | — |

## tak

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.084 | 12096 | — |
| rust | 0.098 | 1456 | — |
| go | 0.098 | 3408 | — |
| ocaml | 0.125 | 2112 | — |
| node | 0.353 | 44912 | — |
| fsharp | 1.240 | 288192 | — |
| ruby | 2.703 | 33904 | — |
| python | 3.164 | 14384 | — |
| baseline_vm | 57.295 | 9728 | — |

## treemap

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| ocaml | 0.009 | 1856 | — |
| go | 0.028 | 5488 | — |
| node | 0.107 | 54944 | — |
| rust | 0.119 | 5488 | — |
| python | 0.226 | 21280 | — |
| baseline_jit | 0.320 | 56544 | — |
| ruby | 0.438 | 38592 | — |
| fsharp | 0.736 | 265472 | — |
| baseline_vm | 5.155 | 22624 | — |

