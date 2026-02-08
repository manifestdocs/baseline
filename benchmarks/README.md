# HTTP Server Benchmark Suite

This directory contains benchmark implementations for comparing Baseline's HTTP server
performance against Rust Axum and Go Fiber.

## Running Benchmarks

### Prerequisites

- Install `wrk` or `hey` for load testing:
  ```bash
  # macOS
  brew install wrk

  # Or use hey (Go-based)
  go install github.com/rakyll/hey@latest
  ```

### 1. Baseline Server (Current Implementation)

```bash
# Terminal 1: Start server
blc run examples/bench_server.bl

# Terminal 2: Run benchmark
wrk -t4 -c100 -d10s http://localhost:8080/hello
```

### 2. Axum Comparison Server

```bash
cd benchmarks/axum_server
cargo run --release

# In another terminal
wrk -t4 -c100 -d10s http://localhost:8081/hello
```

### 3. Fiber Comparison Server

```bash
cd benchmarks/fiber_server
go run main.go

# In another terminal
wrk -t4 -c100 -d10s http://localhost:8082/hello
```

## Benchmark Endpoints

All servers implement the same endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/hello` | GET | Plain text "Hello, World!" |
| `/json` | GET | JSON `{"message":"Hello, World!"}` |
| `/echo` | POST | Echo request body |
| `/health` | GET | Health check JSON |
| `/users/:id` | GET | User lookup with path param |

## Expected Results

| Server | Hello RPS (target) | JSON RPS (target) | Notes |
|--------|-------------------|-------------------|-------|
| Axum | ~500k | ~400k | Baseline target |
| Fiber | ~450k | ~350k | Go comparison |
| Baseline (current) | ~10k | ~8k | tiny_http sync |
| Baseline (goal) | ~400k | ~300k | Hyper async |

## Metrics to Track

1. **Requests/second (RPS)** - Primary throughput metric
2. **Latency (p50, p99)** - Response time distribution
3. **Memory usage** - RSS during load
4. **CPU utilization** - Per-core usage
