# CPU Benchmark Reference Results

**Date:** 2026-02-21T21:56:27Z
**System:** arm64 Darwin 26.2
**Runs:** 3 (median)

## divsum

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| c | 0.041 | 1280 | 1.0x |
| baseline_jit | 0.052 | 12048 | 1.3x |
| rust | 0.057 | 1472 | 1.4x |
| ocaml | 0.058 | 2128 | 1.4x |
| go | 0.060 | 3840 | 1.5x |
| node | 0.116 | 45344 | 2.8x |
| fsharp | 0.983 | 287360 | 24.0x |
| python | 1.438 | 14432 | 35.1x |
| ruby | 1.544 | 33872 | 37.7x |
| baseline_vm | 2.203 | 10048 | 53.7x |

## fib

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| rust | 0.037 | 1472 | 0.5x |
| baseline_jit | 0.042 | 11808 | 0.6x |
| go | 0.043 | 3744 | 0.6x |
| ocaml | 0.047 | 2128 | 0.6x |
| c | 0.074 | 1296 | 1.0x |
| node | 0.122 | 45008 | 1.6x |
| baseline_vm | 0.649 | 9904 | 8.8x |
| ruby | 0.834 | 33808 | 11.3x |
| fsharp | 0.946 | 286384 | 12.8x |
| python | 1.025 | 14352 | 13.9x |

## tak

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.080 | 11968 | 0.9x |
| c | 0.093 | 1296 | 1.0x |
| rust | 0.096 | 1472 | 1.0x |
| go | 0.096 | 3696 | 1.0x |
| ocaml | 0.121 | 2128 | 1.3x |
| node | 0.327 | 45264 | 3.5x |
| fsharp | 1.126 | 287280 | 12.1x |
| ruby | 2.540 | 34160 | 27.3x |
| python | 3.079 | 14368 | 33.1x |
| baseline_vm | 3.418 | 10032 | 36.8x |

