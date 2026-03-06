#!/usr/bin/env bash
set -euo pipefail

VERSION="${1:?Usage: release.sh <version|tag> [target] [repo]  (e.g. 0.1.0)}"
if [[ "$VERSION" == v* ]]; then
  TAG="$VERSION"
else
  TAG="v${VERSION}"
fi
TARGET="${2:-aarch64-apple-darwin}"
BINARY="blc"
ARCHIVE="${BINARY}-${TAG}-${TARGET}.tar.gz"
REPO="${3:-baseline-lang/baseline}"

echo "==> Building ${BINARY} ${TAG} (${TARGET}, release)..."
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

echo "==> Creating GitHub release ${TAG}..."
gh release create "${TAG}" "${ARCHIVE}" \
  --repo "${REPO}" \
  --title "${TAG}" \
  --notes "Baseline Language ${TAG}" \
  --latest

echo ""
echo "Done! Release: https://github.com/${REPO}/releases/tag/${TAG}"
echo ""
echo "SHA256 for Homebrew formula:"
echo "  ${SHA256}"
