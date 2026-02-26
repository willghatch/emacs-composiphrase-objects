#!/usr/bin/env bash
set -euo pipefail

# Install treesitter language grammars needed for cpo-treesitter-qd tests.
# Grammars are installed into ./dependencies/tree-sitter/ (not ~/.emacs.d).
# Requires: gcc, git, emacs (29+)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPS_DIR="$SCRIPT_DIR/dependencies"
GRAMMAR_DIR="$DEPS_DIR/tree-sitter"

for cmd in gcc git emacs; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "Error: $cmd is required but not found in PATH." >&2
        exit 1
    fi
done

# Check Emacs version supports treesit
if ! emacs --batch --eval '(unless (fboundp (quote treesit-available-p)) (kill-emacs 1))' 2>/dev/null; then
    echo "Error: Emacs does not have treesitter support (need Emacs 29+)." >&2
    exit 1
fi

mkdir -p "$GRAMMAR_DIR"
echo "Installing treesitter grammars into $GRAMMAR_DIR ..."

# --init-directory sets user-emacs-directory to $DEPS_DIR, so treesitter
# installs to and looks in $DEPS_DIR/tree-sitter/ without touching ~/.emacs.d.
emacs --batch --init-directory "$DEPS_DIR" --eval '
(progn
  (setq treesit-language-source-alist
    (quote ((python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.4")
            (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.3.0")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5"))))
  (let ((all-ok t))
    (dolist (lang (mapcar (function car) treesit-language-source-alist))
      (if (treesit-language-available-p lang)
          (message "Grammar for %s already installed." lang)
        (message "Installing grammar for %s..." lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error
           (message "Error installing %s: %s" lang err)
           (setq all-ok nil)))))
    (dolist (lang (mapcar (function car) treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (message "FAILED: %s grammar not available after installation." lang)
        (setq all-ok nil)))
    (unless all-ok
      (kill-emacs 1))))
'

echo "Treesitter grammars installed successfully into $GRAMMAR_DIR"
