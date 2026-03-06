#!/usr/bin/env bash
# test.sh — Run Baseline compiler tests
#
# Usage:
#   ./scripts/test.sh fast     # Unit + lib tests only (< 30s)
#   ./scripts/test.sh gates    # Conformance regression gate (< 15s)
#   ./scripts/test.sh full     # All tests including conformance + differential (< 5 min)
#   ./scripts/test.sh          # Default: full

set -euo pipefail

MODE="${1:-full}"

case "$MODE" in
  fast)
    echo "=== Fast: unit + lib tests ==="
    cargo test --lib
    ;;

  gates)
    echo "=== Gates: conformance regression check ==="
    cargo test -p blc --test conformance_tests -- --nocapture
    echo "=== PASS ==="
    ;;

  full)
    echo "=== Full: all tests ==="
    cargo test -p blc
    echo
    echo "=== Diagnostic Coverage ==="
    ./scripts/diagnostic-coverage.sh
    ;;

  *)
    echo "Usage: $0 {fast|gates|full}"
    exit 1
    ;;
esac
