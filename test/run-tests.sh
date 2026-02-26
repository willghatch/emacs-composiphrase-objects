#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPS_DIR="$SCRIPT_DIR/dependencies"

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Run the composiphrase-objects test suite.

Options:
  --core             Run the core unit tests.
  --generated        Run the generated tests.
  --irta             Run the IRTA found tests.
  --cvg              Run the characteristic-via-generated tests.
  --cvg-treesitter   Run the treesitter characteristic tests (installs grammars first).
  --all              Run all tests (--core + --generated + --irta + --cvg + --cvg-treesitter).
  --help             Show this message and exit.
EOF
}

if [ $# -eq 0 ]; then
    usage
    exit 0
fi

RUN_CORE=false
RUN_GENERATED=false
RUN_IRTA=false
RUN_CVG=false
RUN_CVG_TREESITTER=false

for arg in "$@"; do
    case "$arg" in
        --help)
            usage
            exit 0
            ;;
        --core)
            RUN_CORE=true
            ;;
        --generated)
            RUN_GENERATED=true
            ;;
        --irta)
            RUN_IRTA=true
            ;;
        --cvg)
            RUN_CVG=true
            ;;
        --cvg-treesitter)
            RUN_CVG_TREESITTER=true
            ;;
        --all)
            RUN_CORE=true
            RUN_GENERATED=true
            RUN_IRTA=true
            RUN_CVG=true
            RUN_CVG_TREESITTER=true
            ;;
        *)
            echo "Unknown option: $arg" >&2
            usage >&2
            exit 1
            ;;
    esac
done

mkdir -p "$DEPS_DIR"

CARETTEST_HASH=747487aeca80b4ce70de113853ddcfbccde21c1b
SMARTPARENS_HASH=82d2cf084a19b0c2c3812e0550721f8a61996056
DASH_HASH=fb443e7a6e660ba849cafcd01021d9aac3ac6764

pin_repo() {
    local dir="$1" hash="$2"
    if git -C "$dir" checkout "$hash" --quiet 2>/dev/null; then
        return 0
    else
        echo "error: could not check out pinned commit $hash in $dir" >&2
        echo "  The clone may be shallow and not contain that commit." >&2
        exit 1
    fi
}

if [ ! -d "$DEPS_DIR/carettest" ]; then
    git clone https://github.com/willghatch/emacs-carettest "$DEPS_DIR/carettest"
    pin_repo "$DEPS_DIR/carettest" "$CARETTEST_HASH"
fi

if [ ! -d "$DEPS_DIR/smartparens" ]; then
    git clone https://github.com/Fuco1/smartparens "$DEPS_DIR/smartparens"
    pin_repo "$DEPS_DIR/smartparens" "$SMARTPARENS_HASH"
fi

if [ ! -d "$DEPS_DIR/dash" ]; then
    git clone https://github.com/magnars/dash.el "$DEPS_DIR/dash"
    pin_repo "$DEPS_DIR/dash" "$DASH_HASH"
fi

# Base emacs arguments: load paths and every library .el from the parent directory
TEST_ARGS=(
    -q
    -L "$SCRIPT_DIR/.."
    -L "$DEPS_DIR/carettest"
    -L "$DEPS_DIR/dash"
    -L "$DEPS_DIR/smartparens"
)

shopt -s nullglob
for f in "$SCRIPT_DIR"/../*.el; do
    TEST_ARGS+=(-l "$f")
done
shopt -u nullglob

if $RUN_CORE; then
    TEST_ARGS+=(
        -l "$SCRIPT_DIR/test-cpo-indent-tree.el"
        -l "$SCRIPT_DIR/test-cpo-smartparens.el"
        -l "$SCRIPT_DIR/test-cpo-text-object-stuff.el"
        -l "$SCRIPT_DIR/test-cpo-misc.el"
    )
fi

if $RUN_GENERATED; then
    if [ -d "$SCRIPT_DIR/generated-tests" ]; then
        shopt -s nullglob
        for f in "$SCRIPT_DIR/generated-tests/"*.el; do
            TEST_ARGS+=(-l "$f")
        done
        shopt -u nullglob
    fi
fi

if $RUN_CVG; then
    if [ -d "$SCRIPT_DIR/characteristic-via-generated" ]; then
        shopt -s nullglob
        for f in "$SCRIPT_DIR/characteristic-via-generated/"*.el; do
            TEST_ARGS+=(-l "$f")
        done
        shopt -u nullglob
    fi
fi

if $RUN_IRTA; then
    if [ -d "$SCRIPT_DIR/irta-found-tests" ]; then
        shopt -s nullglob
        for f in "$SCRIPT_DIR/irta-found-tests/"*.el; do
            TEST_ARGS+=(-l "$f")
        done
        shopt -u nullglob
    fi
fi

if $RUN_CVG_TREESITTER; then
    "$SCRIPT_DIR/install-treesitter-grammars.sh"
    if [ -d "$SCRIPT_DIR/characteristic-via-generated_treesitter" ]; then
        shopt -s nullglob
        for f in "$SCRIPT_DIR/characteristic-via-generated_treesitter/"*.el; do
            TEST_ARGS+=(-l "$f")
        done
        shopt -u nullglob
    fi
fi

emacs -batch --init-directory "$DEPS_DIR" "${TEST_ARGS[@]}" -f ert-run-tests-batch-and-exit
