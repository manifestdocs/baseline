# CPU Benchmark Reference Results

**Date:** 2026-02-22T16:52:51Z
**System:** arm64 Darwin 26.2
**Runs:** 3 (median)

## divsum

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| c | 0.052 | 1280 | 1.0x |
| go | 0.055 | 3712 | 1.1x |
| baseline_jit | 0.059 | 12096 | 1.1x |
| rust | 0.061 | 1472 | 1.2x |
| ocaml | 0.063 | 2144 | 1.2x |
| node | 0.138 | 45648 | 2.7x |
| fsharp | 1.047 | 285920 | 20.1x |
| python | 1.435 | 14416 | 27.6x |
| ruby | 1.629 | 33984 | 31.3x |
| baseline_vm | 2.287 | 10048 | 44.0x |

## fib

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| rust | 0.040 | 1472 | 0.9x |
| c | 0.043 | 1280 | 1.0x |
| ocaml | 0.050 | 2128 | 1.2x |
| baseline_jit | 0.050 | 11872 | 1.2x |
| go | 0.051 | 3584 | 1.2x |
| node | 0.147 | 45280 | 3.4x |
| baseline_vm | 0.683 | 9984 | 15.9x |
| ruby | 0.977 | 34048 | 22.7x |
| fsharp | 1.008 | 286496 | 23.4x |
| python | 1.063 | 14352 | 24.7x |

## tak

| Language | Time (s) | Memory (KB) | vs C |
|----------|----------|-------------|------|
| baseline_jit | 0.082 | 11984 | 0.8x |
| go | 0.096 | 3664 | 1.0x |
| c | 0.097 | 1280 | 1.0x |
| rust | 0.098 | 1472 | 1.0x |
| ocaml | 0.125 | 2112 | 1.3x |
| node | 0.353 | 45120 | 3.6x |
| fsharp | 1.232 | 287568 | 12.7x |
| ruby | 2.606 | 34208 | 26.9x |
| python | 3.134 | 14288 | 32.3x |
| baseline_vm | 3.417 | 10016 | 35.2x |

