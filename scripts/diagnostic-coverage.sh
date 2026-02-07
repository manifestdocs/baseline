#!/usr/bin/env bash
# diagnostic-coverage.sh — Find which error codes are tested and which are not.
#
# Usage: ./scripts/diagnostic-coverage.sh
#
# Extracts error codes from analysis source files and checks which are
# exercised in the test suite.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="$REPO_ROOT/blc/src"
TEST_DIR="$REPO_ROOT/blc/tests"
EXAMPLES_DIR="$REPO_ROOT/examples"
CONFORMANCE_DIR="$REPO_ROOT/tests/conformance"

# Extract all error codes from source (TYP_xxx, CAP_xxx, REF_xxx, etc.)
echo "=== Diagnostic Coverage Report ==="
echo

CODES=$(grep -ohE '"[A-Z]{2,4}_[0-9]{3}"' "$SRC_DIR/analysis/types.rs" \
                                             "$SRC_DIR/analysis/effects.rs" \
                                             "$SRC_DIR/analysis/refinements.rs" \
                                             "$SRC_DIR/resolver.rs" 2>/dev/null \
        | tr -d '"' | sort -u)

TOTAL=0
TESTED=0
UNTESTED_CODES=()

for code in $CODES; do
    TOTAL=$((TOTAL + 1))
    # Search in test files + examples + conformance tests
    if grep -rq "$code" "$TEST_DIR" "$EXAMPLES_DIR" "$CONFORMANCE_DIR" 2>/dev/null; then
        TESTED=$((TESTED + 1))
    else
        UNTESTED_CODES+=("$code")
    fi
done

echo "Total error codes in source: $TOTAL"
echo "Tested error codes:          $TESTED"
echo "Untested error codes:        $((TOTAL - TESTED))"
echo

if [ ${#UNTESTED_CODES[@]} -gt 0 ]; then
    echo "--- Untested codes ---"
    for code in "${UNTESTED_CODES[@]}"; do
        # Show where the code is defined
        location=$(grep -n "\"$code\"" "$SRC_DIR/analysis/types.rs" \
                                        "$SRC_DIR/analysis/effects.rs" \
                                        "$SRC_DIR/analysis/refinements.rs" \
                                        "$SRC_DIR/resolver.rs" 2>/dev/null | head -1)
        echo "  $code  ($location)"
    done
else
    echo "All error codes are covered!"
fi

echo
echo "Coverage: $TESTED / $TOTAL ($((TESTED * 100 / TOTAL))%)"
