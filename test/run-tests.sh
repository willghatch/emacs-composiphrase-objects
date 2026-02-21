#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPS_DIR="$SCRIPT_DIR/dependencies"

mkdir -p "$DEPS_DIR"

if [ ! -d "$DEPS_DIR/carettest" ]; then
    git clone https://github.com/willghatch/emacs-carettest "$DEPS_DIR/carettest"
fi

if [ ! -d "$DEPS_DIR/smartparens" ]; then
    git clone --depth 1 https://github.com/Fuco1/smartparens "$DEPS_DIR/smartparens"
fi

if [ ! -d "$DEPS_DIR/dash" ]; then
    git clone --depth 1 https://github.com/magnars/dash.el "$DEPS_DIR/dash"
fi

# Build load args for generated test files; gracefully handle missing/empty directory
generated_load_args=()
if [ -d "$SCRIPT_DIR/generated-tests" ]; then
    shopt -s nullglob
    for f in "$SCRIPT_DIR/generated-tests/"*.el; do
        generated_load_args+=(-l "$f")
    done
    shopt -u nullglob
fi

emacs -batch -q \
    -L "$SCRIPT_DIR/.." \
    -L "$DEPS_DIR/carettest" \
    -L "$DEPS_DIR/dash" \
    -L "$DEPS_DIR/smartparens" \
    -l "$SCRIPT_DIR/../cpo-tree-walk.el" \
    -l "$SCRIPT_DIR/../cpo-indent-tree.el" \
    -l "$SCRIPT_DIR/test-cpo-indent-tree.el" \
    -l "$SCRIPT_DIR/../cpo-smartparens.el" \
    -l "$SCRIPT_DIR/test-cpo-smartparens.el" \
    -l "$SCRIPT_DIR/../cpo-text-object-stuff.el" \
    -l "$SCRIPT_DIR/test-cpo-text-object-stuff.el" \
    "${generated_load_args[@]}" \
    -f ert-run-tests-batch-and-exit
