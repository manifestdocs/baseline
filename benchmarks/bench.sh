#!/usr/bin/env bash
# bench.sh — Run HTTP server benchmarks comparing Baseline, Axum, and Fiber
#
# Usage:
#   ./benchmarks/bench.sh [baseline|axum|fiber|all]
#
# Requirements:
#   - wrk (brew install wrk)
#   - Rust toolchain (for Axum)
#   - Go toolchain (for Fiber)
#   - blc in PATH

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Benchmark parameters
THREADS=4
CONNECTIONS=100
DURATION=10s

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

run_wrk() {
    local url=$1
    local name=$2
    echo -e "${BLUE}Testing $name: $url${NC}"
    wrk -t$THREADS -c$CONNECTIONS -d$DURATION "$url" 2>&1
    echo ""
}

benchmark_server() {
    local port=$1
    local name=$2

    echo -e "${GREEN}=== Benchmarking $name (port $port) ===${NC}"
    echo ""

    # Wait for server to be ready
    for i in {1..30}; do
        if curl -s "http://localhost:$port/health" > /dev/null 2>&1; then
            break
        fi
        sleep 0.1
    done

    run_wrk "http://localhost:$port/hello" "$name /hello (plain text)"
    run_wrk "http://localhost:$port/json" "$name /json (JSON)"
    run_wrk "http://localhost:$port/users/123" "$name /users/:id (params)"
}

start_baseline() {
    echo -e "${BLUE}Starting Baseline server...${NC}"
    cd "$ROOT_DIR"
    cargo run -p blc --release -- run examples/bench_server.bl &
    BASELINE_PID=$!
    sleep 2
}

start_axum() {
    echo -e "${BLUE}Starting Axum server...${NC}"
    cd "$SCRIPT_DIR/axum_server"
    cargo run --release &
    AXUM_PID=$!
    sleep 2
}

start_fiber() {
    echo -e "${BLUE}Starting Fiber server...${NC}"
    cd "$SCRIPT_DIR/fiber_server"
    go run main.go &
    FIBER_PID=$!
    sleep 2
}

cleanup() {
    echo -e "${BLUE}Cleaning up...${NC}"
    [ -n "${BASELINE_PID:-}" ] && kill $BASELINE_PID 2>/dev/null || true
    [ -n "${AXUM_PID:-}" ] && kill $AXUM_PID 2>/dev/null || true
    [ -n "${FIBER_PID:-}" ] && kill $FIBER_PID 2>/dev/null || true
}

trap cleanup EXIT

MODE="${1:-all}"

case "$MODE" in
    baseline)
        start_baseline
        benchmark_server 8080 "Baseline"
        ;;
    axum)
        start_axum
        benchmark_server 8081 "Axum"
        ;;
    fiber)
        start_fiber
        benchmark_server 8082 "Fiber"
        ;;
    all)
        echo -e "${GREEN}========================================${NC}"
        echo -e "${GREEN}  HTTP Server Benchmark Suite${NC}"
        echo -e "${GREEN}========================================${NC}"
        echo ""
        echo "Parameters: threads=$THREADS, connections=$CONNECTIONS, duration=$DURATION"
        echo ""

        start_baseline
        benchmark_server 8080 "Baseline"
        kill $BASELINE_PID 2>/dev/null || true
        sleep 1

        start_axum
        benchmark_server 8081 "Axum"
        kill $AXUM_PID 2>/dev/null || true
        sleep 1

        start_fiber
        benchmark_server 8082 "Fiber"
        ;;
    *)
        echo "Usage: $0 [baseline|axum|fiber|all]"
        exit 1
        ;;
esac

echo -e "${GREEN}Benchmark complete!${NC}"
