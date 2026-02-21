#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-text-modifications.el --- Generator for text modifications function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmut-generator)
(require 'cpo-text-object-stuff)

;; Generate tests for text transposition functions
;; Text includes hyphenated and underscored tokens so word and symbol boundaries differ.
(carettest-tesmut-generate-tests
 "The quick_brown fox jumps over the lazy-dog.
This is a second sentence with different-words and some_symbols.
Final sentence for testing transposition_operations here."
 15  ; number of test positions
 '(cpo-transpose-word-forward
   cpo-transpose-word-backward
   cpo-transpose-symbol-forward
   cpo-transpose-symbol-backward
   cpo-transpose-sentence-forward
   cpo-transpose-sentence-backward
   ("transpose-word-forward-2" (lambda () (cpo-transpose-word-forward 2)))
   ("transpose-symbol-backward-2" (lambda () (cpo-transpose-symbol-backward 2))))
 "test-text-modifications"
 :output-file "_generated-tests-text-modifications.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.6
 :transient-mark-mode-prob 0.8)

;; Generate tests for line modification functions
(carettest-tesmut-generate-tests
 "First line of text
Second line with content
Third line here
Fourth line for testing"
 10  ; number of test positions
 '(cpo-transpose-line-forward
   cpo-transpose-line-backward
   cpo-open-line-below
   cpo-open-line-above
   ("transpose-line-forward-2" (lambda () (cpo-transpose-line-forward 2)))
   ;; Sequence tests
   (list cpo-open-line-below cpo-transpose-line-forward)
   (list cpo-open-line-above cpo-transpose-line-backward))
 "test-line-modifications"
 :output-file "_generated-tests-line-modifications.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.9)
