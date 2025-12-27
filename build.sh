#!/bin/bash
set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang
export LIBRARY_PATH=/opt/homebrew/lib:${LIBRARY_PATH:-}

# Build afferent first if needed
if [ -d "../afferent" ]; then
    (cd ../afferent && ./build.sh Afferent 2>/dev/null || true)
fi

TARGET="${1:-vane}"
lake build "$TARGET"
