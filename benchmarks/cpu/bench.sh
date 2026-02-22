#!/usr/bin/env bash
# Run Baseline CPU benchmarks and compare against saved reference results.
# Use this after making changes to the compiler/VM/JIT to check for regressions.
#
# Usage:
#   ./benchmarks/cpu/bench.sh          # Run all benchmarks
#   ./benchmarks/cpu/bench.sh fib      # Run only fib
#   ./benchmarks/cpu/bench.sh --save   # Save current results as new reference
#
# Exit codes: 0 = ok, 1 = regression (>10% slower)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"
REFERENCE="$RESULTS_DIR/reference.json"
RUNS=3
REGRESSION_THRESHOLD=10  # percent slower = regression

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Parse args
SAVE_MODE=false
FILTER=""
for arg in "$@"; do
    case "$arg" in
        --save) SAVE_MODE=true ;;
        *) FILTER="$arg" ;;
    esac
done

# Build blc
echo -e "${CYAN}Building blc (release)...${NC}"
(cd "$REPO_DIR" && cargo build --release --bin blc 2>&1 | tail -1)
BLC="$REPO_DIR/target/release/blc"

# Detect JIT availability (must produce correct output, not just exit 0)
HAS_JIT=false
JIT_OUT=$($BLC run --jit "$SCRIPT_DIR/fib/fib.bl" 2>/dev/null || true)
if [ "$JIT_OUT" = "9227465" ]; then
    HAS_JIT=true
fi

HAS_LLVM=false
LLVM_OUT=$($BLC run --llvm "$SCRIPT_DIR/fib/fib.bl" 2>/dev/null || true)
if [ "$LLVM_OUT" = "9227465" ]; then
    HAS_LLVM=true
fi

echo -e "${BOLD}Baseline CPU Benchmarks${NC}"
echo "======================="
echo "Runs: $RUNS | JIT: $HAS_JIT | LLVM: $HAS_LLVM"
echo ""

# ── Measurement ──────────────────────────────────────────────────────

measure() {
    local cmd="$1"
    local time_s mem_kb

    # Time via python (accurate cross-platform)
    time_s=$(python3 -c "
import subprocess, time, statistics
times = []
for _ in range($RUNS):
    start = time.perf_counter()
    subprocess.run('$cmd', shell=True, capture_output=True)
    times.append(time.perf_counter() - start)
print(f'{statistics.median(times):.3f}')
")

    # Memory via /usr/bin/time
    local mem_output
    mem_output=$(/usr/bin/time -l sh -c "$cmd" 2>&1 >/dev/null || true)
    mem_kb=$(echo "$mem_output" | grep "maximum resident set size" | awk '{print int($1/1024)}')
    [ -z "$mem_kb" ] && mem_kb=0

    echo "$time_s $mem_kb"
}

# ── Run benchmarks ───────────────────────────────────────────────────

BENCHES=(fib tak divsum primes treemap mergesort strrev mapbuild)
ENTRIES=()
HAS_REGRESSION=false

for bench in "${BENCHES[@]}"; do
    # Apply filter if set
    if [ -n "$FILTER" ] && [ "$FILTER" != "$bench" ]; then
        continue
    fi

    echo -e "${BOLD}$bench${NC}"

    for mode in vm jit llvm; do
        case "$mode" in
            vm)   flag=""; label="baseline_vm" ;;
            jit)  $HAS_JIT || continue; flag="--jit"; label="baseline_jit" ;;
            llvm) $HAS_LLVM || continue; flag="--llvm"; label="baseline_llvm" ;;
        esac

        printf "  %-18s " "$label"
        result=$(measure "$BLC run $flag $SCRIPT_DIR/$bench/$bench.bl")
        time_s="${result%% *}"
        mem_kb="${result##* }"

        # Compare against reference if available
        delta=""
        if [ -f "$REFERENCE" ] && ! $SAVE_MODE; then
            ref_time=$(python3 -c "
import json
with open('$REFERENCE') as f:
    data = json.load(f)
matches = [r for r in data['results'] if r['lang'] == '$label' and r['bench'] == '$bench']
print(f'{matches[0][\"time_s\"]:.3f}' if matches else '')
" 2>/dev/null || true)

            if [ -n "$ref_time" ] && [ "$ref_time" != "0" ]; then
                pct=$(python3 -c "
ref=$ref_time; cur=$time_s
diff = ((cur - ref) / ref) * 100
print(f'{diff:+.1f}')
")
                pct_num=$(python3 -c "print(abs($pct))" | sed 's/+//')

                if python3 -c "exit(0 if $time_s > $ref_time * 1.10 else 1)"; then
                    delta="${RED}${pct}%${NC} (was ${ref_time}s)"
                    HAS_REGRESSION=true
                elif python3 -c "exit(0 if $time_s < $ref_time * 0.95 else 1)"; then
                    delta="${GREEN}${pct}%${NC} (was ${ref_time}s)"
                else
                    delta="~same (was ${ref_time}s)"
                fi
            fi
        fi

        printf "${GREEN}%8ss${NC}  %6s KB" "$time_s" "$mem_kb"
        [ -n "$delta" ] && echo -ne "  $delta"
        printf "\n"

        ENTRIES+=("{\"lang\": \"$label\", \"bench\": \"$bench\", \"time_s\": $time_s, \"mem_kb\": $mem_kb}")
    done

    echo ""
done

# ── Save if requested ────────────────────────────────────────────────

if $SAVE_MODE; then
    TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    # Merge with existing reference (keep non-baseline entries)
    if [ -f "$REFERENCE" ]; then
        python3 - "$REFERENCE" "$TIMESTAMP" "${ENTRIES[@]}" << 'PYEOF'
import json, sys

ref_file = sys.argv[1]
timestamp = sys.argv[2]
new_entries = [json.loads(e) for e in sys.argv[3:]]

with open(ref_file) as f:
    data = json.load(f)

# Keep non-baseline entries, replace baseline ones
old = [r for r in data["results"] if not r["lang"].startswith("baseline")]
data["results"] = old + new_entries
data["baseline_updated"] = timestamp

with open(ref_file, 'w') as f:
    json.dump(data, f, indent=2)
print(f"Updated baseline entries in {ref_file}")
PYEOF
    else
        # No reference yet, save just baseline
        {
            echo "{"
            echo "  \"timestamp\": \"$TIMESTAMP\","
            echo "  \"system\": \"$(uname -m) $(uname -s) $(sw_vers -productVersion 2>/dev/null || uname -r)\","
            echo "  \"runs\": $RUNS,"
            echo "  \"results\": ["
            for i in "${!ENTRIES[@]}"; do
                [ "$i" -gt 0 ] && echo ","
                printf "    %s" "${ENTRIES[$i]}"
            done
            echo ""
            echo "  ]"
            echo "}"
        } > "$REFERENCE"
        echo -e "${GREEN}Saved baseline results to:${NC} $REFERENCE"
    fi
fi

# ── Summary ──────────────────────────────────────────────────────────

if $HAS_REGRESSION; then
    echo -e "${RED}REGRESSION DETECTED${NC} (>${REGRESSION_THRESHOLD}% slower than reference)"
    exit 1
else
    echo -e "${GREEN}All benchmarks within tolerance${NC}"
    exit 0
fi
