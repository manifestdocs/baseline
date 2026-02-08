# Phase 1 Implementation: Async HTTP Server Foundation

## Summary

This document tracks the Phase 1 implementation of the high-performance HTTP server for Baseline, targeting parity with Rust Axum and Go Fiber.

## Status: ✅ Phase 1 Complete

Phase 1 establishes the async server foundation with:
- Hyper + Tokio async HTTP server
- Thread-safe VM execution bridge
- Native fast-path handlers
- Radix tree routing with parameter extraction
- Comprehensive TDD test suite

## Files Created/Modified

### New Files

| File | Purpose |
|------|---------|
| `blc/src/stdlib/hyper_server.rs` | Core async HTTP server using Hyper + Tokio |
| `blc/src/stdlib/hyper_server_tests.rs` | TDD test suite for async server |
| `blc/src/vm/async_executor.rs` | Thread-safe bridge between async server and VM |
| `blc/examples/async_bench_server.rs` | Native benchmark server example |
| `examples/bench_server.bl` | Baseline benchmark server script |
| `benchmarks/README.md` | Benchmark documentation |
| `benchmarks/bench.sh` | Automated benchmark runner |
| `benchmarks/axum_server/` | Axum comparison implementation |
| `benchmarks/fiber_server/` | Fiber comparison implementation |
| `docs/PHASE1_ASYNC_SERVER.md` | This documentation |

### Modified Files

| File | Changes |
|------|---------|
| `blc/Cargo.toml` | Added hyper, hyper-util, http-body-util, bytes dependencies |
| `blc/src/stdlib/mod.rs` | Added hyper_server module |
| `blc/src/vm/mod.rs` | Added async_executor module |
| `blc/src/vm/vm.rs` | Made `call_nvalue` and `apply_mw_chain` public |
| `blc/src/vm/nvalue.rs` | Added `as_str`, `as_tuple`, `as_heap_ref_checked` methods |

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Async HTTP Server                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │   Hyper     │  │  Tokio      │  │    AsyncRouteTree       │  │
│  │  (HTTP/1.1) │  │  (Runtime)  │  │  (Radix tree routing)   │  │
│  └──────┬──────┘  └──────┬──────┘  └───────────┬─────────────┘  │
│         │                │                      │                │
│         └────────────────┼──────────────────────┘                │
│                          │                                       │
│                ┌─────────▼─────────┐                            │
│                │  AsyncVmExecutor  │                            │
│                │  (spawn_blocking) │                            │
│                │  + Semaphore      │                            │
│                └─────────┬─────────┘                            │
│                          │                                       │
└──────────────────────────┼───────────────────────────────────────┘
                           │
                ┌──────────▼──────────┐
                │         VM          │
                │  (Synchronous exec) │
                │  - call_nvalue      │
                │  - apply_mw_chain   │
                └─────────────────────┘
```

## Key Components

### 1. SendableValue / SendableHandler (async_executor.rs)
Thread-safe value representations that convert NValue (Rc-based, not Send) to owned types:
- `SendableValue` - Int, Float, String, Bool, List, Record, Tuple, Enum, Function
- `SendableHandler` - Function(usize) or Closure { chunk_idx, upvalues }
- Bidirectional conversion: `from_nvalue()` and `to_nvalue()`

### 2. AsyncRouteTree + Builder (hyper_server.rs)
- Radix tree for O(log n) route matching
- Parameter extraction (`:id` syntax)
- Separate handlers per HTTP method
- Fluent builder API: `.get()`, `.post()`, `.route()`

### 3. AsyncRequest / AsyncResponse (hyper_server.rs)
- Zero-copy body handling with `Bytes`
- Lazy query string parsing
- NValue conversion for VM interop
- Hyper response building

### 4. AsyncVmExecutor (async_executor.rs)
- Semaphore-limited concurrency (default: 2x CPU cores)
- `spawn_blocking` for VM execution
- Handler/middleware chain support
- Converts SendableValue ↔ NValue at thread boundaries

### 5. AsyncHandler Enum
```rust
pub enum AsyncHandler {
    /// VM function handler (thread-safe)
    Vm(SendableHandler),
    /// Native fast path (no VM overhead)
    Native(Arc<dyn Fn(&AsyncRequest) -> AsyncResponse + Send + Sync>),
}
```

## Test Coverage

### Unit Tests (in hyper_server.rs)
- Query string parsing
- URL decoding  
- Response builders

### Unit Tests (in async_executor.rs)
- SendableValue roundtrip conversions
- SendableHandler conversions
- Executor configuration
- Semaphore concurrency limits

### Integration Tests (in hyper_server_tests.rs)
- Route matching (exact, params, methods)
- NValue conversions
- Performance baselines

### Performance Tests
- Response creation: <100ms for 10k ops
- Route matching: <50ms for 10k ops
- Mixed workload: <200ms for 70k ops

## Running Tests

```bash
# All unit tests
cargo test -p blc

# Just hyper_server tests
cargo test -p blc hyper_server

# Just async_executor tests
cargo test -p blc async_executor

# Native benchmark server (no VM)
cargo run --example async_bench_server -p blc --release

# Benchmark with wrk
wrk -t4 -c100 -d10s http://localhost:8080/hello
```

## Dependencies Added

```toml
hyper = { version = "1", features = ["http1", "server"] }
hyper-util = { version = "0.1", features = ["tokio"] }
http-body-util = "0.1"
bytes = "1"
```

## Next Steps (Phase 2)

1. **Wire VM execution to Server.listen!** 
   - Add `Server.listen_async!` builtin or flag
   - Convert router NValue to AsyncRouteTree
   - Pass chunks to AsyncVmExecutor

2. **Zero-copy improvements**
   - Reduce allocations in request parsing
   - Use Bytes for response bodies
   
3. **Connection management**
   - HTTP keep-alive configuration
   - Connection limits
   - Graceful shutdown

4. **Benchmarking**
   - Run comparative benchmarks vs Axum/Fiber
   - Profile hot paths
   - Identify bottlenecks

## Performance Expectations

| Metric | Current (tiny_http) | Target (Hyper) | Axum Reference |
|--------|---------------------|----------------|----------------|
| Hello RPS | ~10k | ~100k+ | ~500k |
| JSON RPS | ~8k | ~80k+ | ~400k |
| p99 Latency | ~10ms | ~1ms | ~0.5ms |
