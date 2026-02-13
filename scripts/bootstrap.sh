#!/usr/bin/env bash
# Bootstrap Verification — validates the self-hosted compiler pipeline
#
# Stage 1: Self-hosted compiler (running on blc VM) compiles test programs to C
# Stage 2: Generated C compiles with cc and produces correct results
# Stage 3: Determinism — same input produces identical output
#
# Usage: ./scripts/bootstrap.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COMPILER="$ROOT/selfhost/compiler.bl"
TEST_DIR="$ROOT/selfhost/tests"
BUILD_DIR="$ROOT/selfhost/.build"
ORIGINAL_INPUT="$ROOT/selfhost/test_input.bl"

PASS=0
FAIL=0
TOTAL=0

cleanup() {
  if [ -f "$BUILD_DIR/test_input.bl.bak" ]; then
    cp "$BUILD_DIR/test_input.bl.bak" "$ORIGINAL_INPUT"
  fi
}
trap cleanup EXIT

mkdir -p "$BUILD_DIR"
cp "$ORIGINAL_INPUT" "$BUILD_DIR/test_input.bl.bak"

echo "=== Bootstrap Verification ==="
echo ""

if [ ! -f "$ROOT/target/debug/blc" ] || [ "$COMPILER" -nt "$ROOT/target/debug/blc" ]; then
  echo "[build] Compiling blc..."
  cargo build --bin blc --manifest-path "$ROOT/Cargo.toml" 2>/dev/null
fi

BLC="$ROOT/target/debug/blc"

run_test() {
  local test_file="$1"
  local expected_exit="$2"
  local test_name="${test_file%.bl}"
  local c_file="$BUILD_DIR/${test_name}.c"
  local bin_file="$BUILD_DIR/${test_name}"

  TOTAL=$((TOTAL + 1))

  # Stage 1: Copy test file as input, run self-hosted compiler
  cp "$TEST_DIR/$test_file" "$ORIGINAL_INPUT"

  if ! "$BLC" run "$COMPILER" >/dev/null 2>&1; then
    echo "  FAIL  $test_name — blc run failed"
    FAIL=$((FAIL + 1))
    return
  fi

  cp "$ROOT/selfhost/output.c" "$c_file"

  # Stage 2: Compile C and check exit code
  if ! cc -o "$bin_file" "$c_file" 2>/dev/null; then
    echo "  FAIL  $test_name — cc compilation failed"
    FAIL=$((FAIL + 1))
    return
  fi

  set +e
  "$bin_file"
  local actual_exit=$?
  set -e

  if [ "$actual_exit" -ne "$expected_exit" ]; then
    echo "  FAIL  $test_name — expected exit $expected_exit, got $actual_exit"
    FAIL=$((FAIL + 1))
    return
  fi

  # Stage 3: Determinism — run again and diff
  cp "$TEST_DIR/$test_file" "$ORIGINAL_INPUT"
  "$BLC" run "$COMPILER" >/dev/null 2>&1

  if ! diff -q "$ROOT/selfhost/output.c" "$c_file" >/dev/null 2>&1; then
    echo "  FAIL  $test_name — non-deterministic output"
    FAIL=$((FAIL + 1))
    return
  fi

  echo "  PASS  $test_name (exit=$actual_exit, deterministic)"
  PASS=$((PASS + 1))
}

echo "[stage 1] Compiling test programs via self-hosted compiler..."
echo "[stage 2] Verifying generated C produces correct results..."
echo "[stage 3] Checking determinism (identical output on re-run)..."
echo ""

run_test "arithmetic.bl" 42
run_test "factorial.bl" 120
run_test "fibonacci.bl" 55
run_test "types.bl" 7
run_test "nested_calls.bl" 10

echo ""
echo "=== Results: $PASS/$TOTAL passed, $FAIL failed ==="

rm -rf "$BUILD_DIR"

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi

echo ""
echo "Bootstrap Stage 1 verified — self-hosted compiler produces correct C."
echo ""
echo "Note: Full self-compilation (Stage 2 bootstrap) requires codegen for:"
echo "  match, lambda, block, list, record, for-loop"
