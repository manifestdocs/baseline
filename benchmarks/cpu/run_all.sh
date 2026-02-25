#!/usr/bin/env bash
# Run all CPU benchmarks across all languages and save reference results.
# Run this ONCE to establish baseline numbers. Then use bench.sh for ongoing
# Baseline-only comparisons.
#
# Prerequisites: gcc/clang, rustc, go, python3, ruby, node, ocaml, dotnet
# Optional: hyperfine (for more accurate timing)
#
# Usage: ./benchmarks/cpu/run_all.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"
BUILD_DIR="$SCRIPT_DIR/.build"
WARMUP=1
RUNS=3

mkdir -p "$RESULTS_DIR" "$BUILD_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Detect available tools
HAS_HYPERFINE=false
if command -v hyperfine &>/dev/null; then
    HAS_HYPERFINE=true
fi

echo -e "${BOLD}CPU Benchmark Suite${NC}"
echo "==================="
echo "Runs: $RUNS | Warmup: $WARMUP"
echo "Hyperfine: $HAS_HYPERFINE"
echo ""

# ── Helpers ──────────────────────────────────────────────────────────

measure() {
    # Usage: measure "label" "command"
    # Outputs: time_secs memory_kb
    local label="$1"
    local cmd="$2"

    if $HAS_HYPERFINE; then
        # Use hyperfine for timing
        local json
        json=$(hyperfine --warmup "$WARMUP" --runs "$RUNS" --export-json /dev/stdout "$cmd" 2>/dev/null)
        local time_s
        time_s=$(echo "$json" | python3 -c "import sys,json; print(f'{json.load(sys.stdin)[\"results\"][0][\"median\"]:.3f}')")

        # Use /usr/bin/time for memory (single run)
        local mem_output
        mem_output=$(/usr/bin/time -l sh -c "$cmd" 2>&1 >/dev/null || true)
        local mem_kb
        mem_kb=$(echo "$mem_output" | grep "maximum resident set size" | awk '{print int($1/1024)}')
        [ -z "$mem_kb" ] && mem_kb=0

        echo "$time_s $mem_kb"
    else
        # Fallback: /usr/bin/time for both (best of N runs)
        local best_time=999999
        local mem_kb=0
        for _ in $(seq 1 $((WARMUP + RUNS))); do
            local output
            output=$(/usr/bin/time -l sh -c "$cmd" 2>&1 >/dev/null || true)
            local t
            t=$(echo "$output" | grep "real" | awk '{print $1}' | sed 's/s//')
            # macOS /usr/bin/time format: "N.NN real ..."
            local elapsed
            elapsed=$(echo "$output" | awk '/real/{print $1}')
            [ -z "$elapsed" ] && elapsed=$(echo "$output" | awk '/elapsed/{gsub(/elapsed/,""); print $1}')

            local m
            m=$(echo "$output" | grep "maximum resident set size" | awk '{print int($1/1024)}')
            [ -n "$m" ] && mem_kb=$m
        done

        # Use python for accurate timing via subprocess
        local time_s
        time_s=$(python3 -c "
import subprocess, time, statistics
times = []
for _ in range($RUNS):
    start = time.perf_counter()
    subprocess.run('$cmd', shell=True, capture_output=True)
    times.append(time.perf_counter() - start)
print(f'{statistics.median(times):.3f}')
")
        echo "$time_s $mem_kb"
    fi
}

# JSON output helpers
json_entry() {
    local lang="$1" bench="$2" time_s="$3" mem_kb="$4"
    echo "    {\"lang\": \"$lang\", \"bench\": \"$bench\", \"time_s\": $time_s, \"mem_kb\": $mem_kb}"
}

# ── Compile all compiled languages ───────────────────────────────────

echo -e "${CYAN}Compiling...${NC}"

compile_if_available() {
    local lang="$1" cmd="$2" output="$3"
    if eval "$cmd" 2>/dev/null; then
        echo -e "  ${GREEN}✓${NC} $lang"
        return 0
    else
        echo -e "  ${YELLOW}⊘${NC} $lang (not available)"
        return 1
    fi
}

LANGS_AVAILABLE=()

# C
if compile_if_available "C (clang -O2)" \
    "clang -O2 -o $BUILD_DIR/fib_c $SCRIPT_DIR/fib/fib.c && \
     clang -O2 -o $BUILD_DIR/tak_c $SCRIPT_DIR/tak/tak.c && \
     clang -O2 -o $BUILD_DIR/divsum_c $SCRIPT_DIR/divsum/divsum.c && \
     clang -O2 -o $BUILD_DIR/primes_c $SCRIPT_DIR/primes/primes.c && \
     clang -O2 -o $BUILD_DIR/treemap_c $SCRIPT_DIR/treemap/treemap.c && \
     clang -O2 -o $BUILD_DIR/mergesort_c $SCRIPT_DIR/mergesort/mergesort.c && \
     clang -O2 -o $BUILD_DIR/strrev_c $SCRIPT_DIR/strrev/strrev.c && \
     clang -O2 -o $BUILD_DIR/mapbuild_c $SCRIPT_DIR/mapbuild/mapbuild.c" \
    ""; then
    LANGS_AVAILABLE+=(c)
fi

# Rust
if compile_if_available "Rust (rustc -O)" \
    "rustc -O -o $BUILD_DIR/fib_rust $SCRIPT_DIR/fib/fib.rs && \
     rustc -O -o $BUILD_DIR/tak_rust $SCRIPT_DIR/tak/tak.rs && \
     rustc -O -o $BUILD_DIR/divsum_rust $SCRIPT_DIR/divsum/divsum.rs && \
     rustc -O -o $BUILD_DIR/primes_rust $SCRIPT_DIR/primes/primes.rs && \
     rustc -O -o $BUILD_DIR/treemap_rust $SCRIPT_DIR/treemap/treemap.rs && \
     rustc -O -o $BUILD_DIR/mergesort_rust $SCRIPT_DIR/mergesort/mergesort.rs && \
     rustc -O -o $BUILD_DIR/strrev_rust $SCRIPT_DIR/strrev/strrev.rs && \
     rustc -O -o $BUILD_DIR/mapbuild_rust $SCRIPT_DIR/mapbuild/mapbuild.rs" \
    ""; then
    LANGS_AVAILABLE+=(rust)
fi

# Go
if compile_if_available "Go" \
    "cd $SCRIPT_DIR/fib && go build -o $BUILD_DIR/fib_go fib.go && \
     cd $SCRIPT_DIR/tak && go build -o $BUILD_DIR/tak_go tak.go && \
     cd $SCRIPT_DIR/divsum && go build -o $BUILD_DIR/divsum_go divsum.go && \
     cd $SCRIPT_DIR/primes && go build -o $BUILD_DIR/primes_go primes.go && \
     cd $SCRIPT_DIR/treemap && go build -o $BUILD_DIR/treemap_go treemap.go && \
     cd $SCRIPT_DIR/mergesort && go build -o $BUILD_DIR/mergesort_go mergesort.go && \
     cd $SCRIPT_DIR/strrev && go build -o $BUILD_DIR/strrev_go strrev.go && \
     cd $SCRIPT_DIR/mapbuild && go build -o $BUILD_DIR/mapbuild_go mapbuild.go" \
    ""; then
    LANGS_AVAILABLE+=(go)
fi

# OCaml
if compile_if_available "OCaml (ocamlfind/ocamlopt)" \
    "ocamlfind ocamlopt -package stdlib -linkpkg -O2 -o $BUILD_DIR/fib_ml $SCRIPT_DIR/fib/fib.ml 2>/dev/null || \
     ocamlopt -O2 -o $BUILD_DIR/fib_ml $SCRIPT_DIR/fib/fib.ml && \
     (ocamlfind ocamlopt -package stdlib -linkpkg -O2 -o $BUILD_DIR/tak_ml $SCRIPT_DIR/tak/tak.ml 2>/dev/null || \
      ocamlopt -O2 -o $BUILD_DIR/tak_ml $SCRIPT_DIR/tak/tak.ml) && \
     (ocamlfind ocamlopt -package stdlib -linkpkg -O2 -o $BUILD_DIR/divsum_ml $SCRIPT_DIR/divsum/divsum.ml 2>/dev/null || \
      ocamlopt -O2 -o $BUILD_DIR/divsum_ml $SCRIPT_DIR/divsum/divsum.ml)" \
    ""; then
    LANGS_AVAILABLE+=(ocaml)
fi

# Interpreted languages (always available if interpreter exists)
for lang_cmd in "python3:python" "ruby:ruby" "node:node"; do
    lang="${lang_cmd%%:*}"
    name="${lang_cmd##*:}"
    if command -v "$lang" &>/dev/null; then
        echo -e "  ${GREEN}✓${NC} $name"
        LANGS_AVAILABLE+=("$name")
    else
        echo -e "  ${YELLOW}⊘${NC} $name (not available)"
    fi
done

# F# (dotnet fsi)
if command -v dotnet &>/dev/null; then
    echo -e "  ${GREEN}✓${NC} F# (dotnet fsi)"
    LANGS_AVAILABLE+=(fsharp)
else
    echo -e "  ${YELLOW}⊘${NC} F# (dotnet not available)"
fi

# Baseline JIT (always present — we're in its repo)
BLC="$(cd "$SCRIPT_DIR/../.." && pwd)/target/release/blc"
if [ ! -f "$BLC" ]; then
    echo -e "  ${YELLOW}Building blc...${NC}"
    (cd "$SCRIPT_DIR/../.." && cargo build --features jit --release --bin blc 2>/dev/null)
fi

# Verify JIT produces correct output
JIT_TEST_OUTPUT=$("$BLC" run "$SCRIPT_DIR/fib/fib.bl" 2>/dev/null || true)
if [ "$JIT_TEST_OUTPUT" = "9227465" ]; then
    echo -e "  ${GREEN}✓${NC} Baseline (JIT)"
    LANGS_AVAILABLE+=(baseline_jit)
else
    echo -e "  ${YELLOW}⊘${NC} Baseline (JIT) — build with: cargo build --features jit --release"
fi

# Check for LLVM JIT
LLVM_TEST_OUTPUT=$("$BLC" run --llvm "$SCRIPT_DIR/fib/fib.bl" 2>/dev/null || true)
if [ "$LLVM_TEST_OUTPUT" = "9227465" ]; then
    echo -e "  ${GREEN}✓${NC} Baseline (LLVM)"
    LANGS_AVAILABLE+=(baseline_llvm)
else
    echo -e "  ${YELLOW}⊘${NC} Baseline (LLVM) — build with: cargo build --features llvm --release"
fi

echo ""
echo -e "${CYAN}Running benchmarks...${NC}"
echo ""

# ── Run all benchmarks ───────────────────────────────────────────────

ENTRIES=()

run_bench() {
    local lang="$1" bench="$2" cmd="$3"
    printf "  %-16s %-10s " "$lang" "$bench"
    local result
    result=$(measure "$lang-$bench" "$cmd")
    local time_s="${result%% *}"
    local mem_kb="${result##* }"
    printf "${GREEN}%8ss${NC}  %6s KB\n" "$time_s" "$mem_kb"
    ENTRIES+=("$(json_entry "$lang" "$bench" "$time_s" "$mem_kb")")
}

BENCHES=(fib tak divsum primes treemap mergesort strrev mapbuild)

for bench in "${BENCHES[@]}"; do
    echo -e "${BOLD}$bench${NC}"

    # C
    [[ " ${LANGS_AVAILABLE[*]} " == *" c "* ]] && \
        run_bench "c" "$bench" "$BUILD_DIR/${bench}_c"

    # Rust
    [[ " ${LANGS_AVAILABLE[*]} " == *" rust "* ]] && \
        run_bench "rust" "$bench" "$BUILD_DIR/${bench}_rust"

    # Go
    [[ " ${LANGS_AVAILABLE[*]} " == *" go "* ]] && \
        run_bench "go" "$bench" "$BUILD_DIR/${bench}_go"

    # OCaml
    [[ " ${LANGS_AVAILABLE[*]} " == *" ocaml "* ]] && \
        run_bench "ocaml" "$bench" "$BUILD_DIR/${bench}_ml"

    # F#
    [[ " ${LANGS_AVAILABLE[*]} " == *" fsharp "* ]] && \
        run_bench "fsharp" "$bench" "dotnet fsi $SCRIPT_DIR/$bench/$bench.fsx"

    # Node
    [[ " ${LANGS_AVAILABLE[*]} " == *" node "* ]] && \
        run_bench "node" "$bench" "node $SCRIPT_DIR/$bench/$bench.js"

    # Python
    [[ " ${LANGS_AVAILABLE[*]} " == *" python "* ]] && \
        run_bench "python" "$bench" "python3 $SCRIPT_DIR/$bench/$bench.py"

    # Ruby
    [[ " ${LANGS_AVAILABLE[*]} " == *" ruby "* ]] && \
        run_bench "ruby" "$bench" "ruby $SCRIPT_DIR/$bench/$bench.rb"

    # Baseline JIT (Cranelift)
    [[ " ${LANGS_AVAILABLE[*]} " == *" baseline_jit "* ]] && \
        run_bench "baseline_jit" "$bench" "$BLC run $SCRIPT_DIR/$bench/$bench.bl"

    # Baseline LLVM JIT
    [[ " ${LANGS_AVAILABLE[*]} " == *" baseline_llvm "* ]] && \
        run_bench "baseline_llvm" "$bench" "$BLC run --llvm $SCRIPT_DIR/$bench/$bench.bl"

    echo ""
done

# ── Save results ─────────────────────────────────────────────────────

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
RESULTS_FILE="$RESULTS_DIR/reference.json"

{
    echo "{"
    echo "  \"timestamp\": \"$TIMESTAMP\","
    echo "  \"system\": \"$(uname -m) $(uname -s) $(sw_vers -productVersion 2>/dev/null || uname -r)\","
    echo "  \"runs\": $RUNS,"
    echo "  \"results\": ["
    for i in "${!ENTRIES[@]}"; do
        if [ "$i" -lt $((${#ENTRIES[@]} - 1)) ]; then
            echo "${ENTRIES[$i]},"
        else
            echo "${ENTRIES[$i]}"
        fi
    done
    echo "  ]"
    echo "}"
} > "$RESULTS_FILE"

echo -e "${GREEN}Results saved to:${NC} $RESULTS_FILE"
echo ""

# ── Generate markdown table ─────────────────────────────────────────

MD_FILE="$RESULTS_DIR/reference.md"

python3 - "$RESULTS_FILE" "$MD_FILE" << 'PYEOF'
import json, sys

with open(sys.argv[1]) as f:
    data = json.load(f)

lines = []
lines.append(f"# CPU Benchmark Reference Results")
lines.append(f"")
lines.append(f"**Date:** {data['timestamp']}")
lines.append(f"**System:** {data['system']}")
lines.append(f"**Runs:** {data['runs']} (median)")
lines.append(f"")

benches = sorted(set(r["bench"] for r in data["results"]))
for bench in benches:
    lines.append(f"## {bench}")
    lines.append(f"")
    lines.append(f"| Language | Time (s) | Memory (KB) | vs C |")
    lines.append(f"|----------|----------|-------------|------|")

    results = [r for r in data["results"] if r["bench"] == bench]
    results.sort(key=lambda r: r["time_s"])

    c_time = next((r["time_s"] for r in results if r["lang"] == "c"), None)

    for r in results:
        ratio = f"{r['time_s']/c_time:.1f}x" if c_time and c_time > 0 else "—"
        lines.append(f"| {r['lang']} | {r['time_s']:.3f} | {r['mem_kb']} | {ratio} |")

    lines.append(f"")

with open(sys.argv[2], 'w') as f:
    f.write('\n'.join(lines) + '\n')

print(f"Markdown saved to: {sys.argv[2]}")
PYEOF
