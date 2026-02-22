# CPU Benchmark Reference Results

**Date:** 2026-02-22T02:13:10Z
**System:** arm64 Darwin 26.2
**Runs:** 3 (median)

## divsum

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| c | 0.043 | 1280 | 1.0x |
| go | 0.052 | 3760 | 1.2x |
| baseline_jit | 0.059 | 12048 | 1.4x |
| rust | 0.061 | 1472 | 1.4x |
| ocaml | 0.063 | 2128 | 1.5x |
| node | 0.118 | 45536 | 2.7x |
| fsharp | 0.971 | 287072 | 22.6x |
| python | 1.438 | 14304 | 33.4x |
| ruby | 1.580 | 34240 | 36.7x |
| baseline_vm | 2.270 | 10032 | 52.8x |

## fib

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| rust | 0.041 | 1472 | 0.9x |
| c | 0.044 | 1280 | 1.0x |
| ocaml | 0.044 | 2128 | 1.0x |
| baseline_jit | 0.047 | 11904 | 1.1x |
| go | 0.052 | 3696 | 1.2x |
| node | 0.124 | 45136 | 2.8x |
| baseline_vm | 0.663 | 9968 | 15.1x |
| ruby | 0.905 | 33872 | 20.6x |
| fsharp | 0.998 | 287264 | 22.7x |
| python | 1.060 | 14352 | 24.1x |

## tak

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.080 | 11952 | 0.8x |
| c | 0.095 | 1296 | 1.0x |
| rust | 0.096 | 1472 | 1.0x |
| go | 0.099 | 3584 | 1.0x |
| ocaml | 0.138 | 2112 | 1.5x |
| node | 0.334 | 45040 | 3.5x |
| fsharp | 1.161 | 288688 | 12.2x |
| ruby | 2.593 | 34112 | 27.3x |
| python | 3.149 | 14304 | 33.1x |
| baseline_vm | 3.342 | 10000 | 35.2x |

