#!/usr/bin/env bash
set -euo pipefail

VERSION="${1:?Usage: release.sh <version>  (e.g. 0.1.0)}"
TARGET="aarch64-apple-darwin"
BINARY="blc"
ARCHIVE="${BINARY}-v${VERSION}-${TARGET}.tar.gz"
REPO="manifestdocs/baseline"

echo "==> Building ${BINARY} v${VERSION} (${TARGET}, release)..."
cargo build --release --target "${TARGET}"

BINARY_PATH="target/${TARGET}/release/${BINARY}"
if [[ ! -f "${BINARY_PATH}" ]]; then
  echo "ERROR: Binary not found at ${BINARY_PATH}"
  exit 1
fi

echo "==> Packaging ${ARCHIVE}..."
tar -czf "${ARCHIVE}" -C "target/${TARGET}/release" "${BINARY}"

SHA256=$(shasum -a 256 "${ARCHIVE}" | awk '{print $1}')
echo "==> SHA256: ${SHA256}"

echo "==> Creating GitHub release v${VERSION}..."
gh release create "v${VERSION}" "${ARCHIVE}" \
  --repo "${REPO}" \
  --title "v${VERSION}" \
  --notes "Baseline Language v${VERSION}" \
  --latest

echo ""
echo "Done! Release: https://github.com/${REPO}/releases/tag/v${VERSION}"
echo ""
echo "SHA256 for Homebrew formula:"
echo "  ${SHA256}"
