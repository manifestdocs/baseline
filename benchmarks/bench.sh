#!/usr/bin/env bash
# bench.sh — Run HTTP server benchmarks comparing Baseline, Axum, and FastAPI
#
# Usage:
#   ./benchmarks/bench.sh [baseline|axum|fastapi|all]
#
# Requirements:
#   - wrk (brew install wrk)
#   - Rust toolchain (for Axum)
#   - Python 3 + FastAPI + uvicorn (pip install fastapi uvicorn)
#   - blc in PATH or built via cargo

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Benchmark parameters
THREADS=4
CONNECTIONS=100
DURATION=10s

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
RED='\033[0;31m'
BOLD='\033[1m'
NC='\033[0m'

RESULTS_DIR="$SCRIPT_DIR/results"
mkdir -p "$RESULTS_DIR"
RESULT_FILE="$RESULTS_DIR/server_bench_$(date +%Y%m%d_%H%M%S).md"
TMP_DIR=$(mktemp -d)

run_wrk() {
    local url=$1
    local label=$2
    local key=$3
    echo -e "  ${BLUE}$label${NC}"
    local output
    output=$(wrk -t$THREADS -c$CONNECTIONS -d$DURATION "$url" 2>&1)
    echo "$output" | grep -E "Req/Sec|Latency|Requests/sec|Transfer/sec" | sed 's/^/    /'
    echo ""

    # Extract and save metrics
    local rps latency
    rps=$(echo "$output" | grep "Requests/sec" | awk '{print $2}')
    latency=$(echo "$output" | grep "Latency" | awk '{print $2}')
    echo "${key}_rps=$rps" >> "$TMP_DIR/metrics.txt"
    echo "${key}_lat=$latency" >> "$TMP_DIR/metrics.txt"
}

benchmark_server() {
    local port=$1
    local name=$2
    local key=$3

    echo -e "${GREEN}=== $name (port $port) ===${NC}"

    # Wait for server to be ready
    for i in $(seq 1 50); do
        if curl -s "http://localhost:$port/health" > /dev/null 2>&1; then
            break
        fi
        sleep 0.1
    done

    if ! curl -s "http://localhost:$port/health" > /dev/null 2>&1; then
        echo -e "${RED}  Server failed to start on port $port${NC}"
        return 1
    fi

    run_wrk "http://localhost:$port/hello" "/hello (plain text)" "${key}_hello"
    run_wrk "http://localhost:$port/json" "/json (JSON)" "${key}_json"
    run_wrk "http://localhost:$port/users/123" "/users/:id (params)" "${key}_users"
    echo ""
}

start_baseline() {
    echo -e "${CYAN}Building and starting Baseline server...${NC}"
    cd "$ROOT_DIR"
    cargo build -p blc --release --features async-server 2>&1 | tail -1
    local ncpu
    ncpu=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 1)
    echo -e "  Workers: $ncpu"
    BL_RATE_LIMIT=0 BL_LOG_REQUESTS=0 ./target/release/blc run --workers "$ncpu" examples/bench_server.bl &
    BASELINE_PID=$!
    sleep 2
}

start_axum() {
    echo -e "${CYAN}Building and starting Axum server...${NC}"
    cd "$SCRIPT_DIR/axum_server"
    cargo build --release 2>&1 | tail -1
    ./target/release/axum_server &
    AXUM_PID=$!
    sleep 1
}

start_fastapi() {
    echo -e "${CYAN}Starting FastAPI server (uvicorn)...${NC}"
    cd "$SCRIPT_DIR/fastapi_server"
    python3 -m uvicorn main:app --host 0.0.0.0 --port 8082 --log-level warning &
    FASTAPI_PID=$!
    sleep 2
}

stop_server() {
    local pid_var=$1
    local pid=${!pid_var:-}
    if [ -n "$pid" ]; then
        kill "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    fi
}

cleanup() {
    stop_server BASELINE_PID
    stop_server AXUM_PID
    stop_server FASTAPI_PID
    rm -rf "$TMP_DIR"
}

trap cleanup EXIT

print_summary() {
    # Load metrics
    local bl_hello_rps bl_json_rps bl_users_rps
    local ax_hello_rps ax_json_rps ax_users_rps
    local fa_hello_rps fa_json_rps fa_users_rps
    local bl_hello_lat bl_json_lat bl_users_lat
    local ax_hello_lat ax_json_lat ax_users_lat
    local fa_hello_lat fa_json_lat fa_users_lat

    while IFS='=' read -r key val; do
        case "$key" in
            baseline_hello_rps) bl_hello_rps=$val ;; baseline_json_rps) bl_json_rps=$val ;; baseline_users_rps) bl_users_rps=$val ;;
            axum_hello_rps) ax_hello_rps=$val ;; axum_json_rps) ax_json_rps=$val ;; axum_users_rps) ax_users_rps=$val ;;
            fastapi_hello_rps) fa_hello_rps=$val ;; fastapi_json_rps) fa_json_rps=$val ;; fastapi_users_rps) fa_users_rps=$val ;;
            baseline_hello_lat) bl_hello_lat=$val ;; baseline_json_lat) bl_json_lat=$val ;; baseline_users_lat) bl_users_lat=$val ;;
            axum_hello_lat) ax_hello_lat=$val ;; axum_json_lat) ax_json_lat=$val ;; axum_users_lat) ax_users_lat=$val ;;
            fastapi_hello_lat) fa_hello_lat=$val ;; fastapi_json_lat) fa_json_lat=$val ;; fastapi_users_lat) fa_users_lat=$val ;;
        esac
    done < "$TMP_DIR/metrics.txt"

    echo -e "${BOLD}${GREEN}========================================${NC}"
    echo -e "${BOLD}${GREEN}  Results (req/sec, higher is better)${NC}"
    echo -e "${BOLD}${GREEN}========================================${NC}"
    echo ""
    printf "  %-12s %12s %12s %12s\n" "Endpoint" "Baseline" "Axum" "FastAPI"
    printf "  %-12s %12s %12s %12s\n" "--------" "--------" "----" "-------"
    printf "  %-12s %12s %12s %12s\n" "/hello" "${bl_hello_rps:-n/a}" "${ax_hello_rps:-n/a}" "${fa_hello_rps:-n/a}"
    printf "  %-12s %12s %12s %12s\n" "/json" "${bl_json_rps:-n/a}" "${ax_json_rps:-n/a}" "${fa_json_rps:-n/a}"
    printf "  %-12s %12s %12s %12s\n" "/users/:id" "${bl_users_rps:-n/a}" "${ax_users_rps:-n/a}" "${fa_users_rps:-n/a}"

    echo ""
    echo "  Avg latency (lower is better):"
    echo ""
    printf "  %-12s %12s %12s %12s\n" "Endpoint" "Baseline" "Axum" "FastAPI"
    printf "  %-12s %12s %12s %12s\n" "--------" "--------" "----" "-------"
    printf "  %-12s %12s %12s %12s\n" "/hello" "${bl_hello_lat:-n/a}" "${ax_hello_lat:-n/a}" "${fa_hello_lat:-n/a}"
    printf "  %-12s %12s %12s %12s\n" "/json" "${bl_json_lat:-n/a}" "${ax_json_lat:-n/a}" "${fa_json_lat:-n/a}"
    printf "  %-12s %12s %12s %12s\n" "/users/:id" "${bl_users_lat:-n/a}" "${ax_users_lat:-n/a}" "${fa_users_lat:-n/a}"

    # Write markdown
    cat > "$RESULT_FILE" <<MDEOF
# Server Benchmark Results

Date: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
Parameters: threads=$THREADS, connections=$CONNECTIONS, duration=$DURATION

## Requests/sec (higher is better)

| Endpoint | Baseline | Axum | FastAPI |
|----------|----------|------|---------|
| /hello | ${bl_hello_rps:-n/a} | ${ax_hello_rps:-n/a} | ${fa_hello_rps:-n/a} |
| /json | ${bl_json_rps:-n/a} | ${ax_json_rps:-n/a} | ${fa_json_rps:-n/a} |
| /users/:id | ${bl_users_rps:-n/a} | ${ax_users_rps:-n/a} | ${fa_users_rps:-n/a} |

## Avg Latency (lower is better)

| Endpoint | Baseline | Axum | FastAPI |
|----------|----------|------|---------|
| /hello | ${bl_hello_lat:-n/a} | ${ax_hello_lat:-n/a} | ${fa_hello_lat:-n/a} |
| /json | ${bl_json_lat:-n/a} | ${ax_json_lat:-n/a} | ${fa_json_lat:-n/a} |
| /users/:id | ${bl_users_lat:-n/a} | ${ax_users_lat:-n/a} | ${fa_users_lat:-n/a} |
MDEOF

    echo ""
    echo -e "${CYAN}Results saved to $RESULT_FILE${NC}"
}

MODE="${1:-all}"

echo -e "${BOLD}${GREEN}========================================${NC}"
echo -e "${BOLD}${GREEN}  HTTP Server Benchmark Suite${NC}"
echo -e "${BOLD}${GREEN}========================================${NC}"
echo ""
echo "Parameters: threads=$THREADS, connections=$CONNECTIONS, duration=$DURATION"
echo ""

case "$MODE" in
    baseline)
        start_baseline
        benchmark_server 8080 "Baseline" "baseline"
        ;;
    axum)
        start_axum
        benchmark_server 8081 "Axum" "axum"
        ;;
    fastapi)
        start_fastapi
        benchmark_server 8082 "FastAPI" "fastapi"
        ;;
    all)
        start_baseline
        benchmark_server 8080 "Baseline" "baseline"
        stop_server BASELINE_PID
        sleep 1

        start_axum
        benchmark_server 8081 "Axum" "axum"
        stop_server AXUM_PID
        sleep 1

        start_fastapi
        benchmark_server 8082 "FastAPI" "fastapi"

        print_summary
        ;;
    *)
        echo "Usage: $0 [baseline|axum|fastapi|all]"
        exit 1
        ;;
esac

echo -e "${GREEN}Benchmark complete!${NC}"
