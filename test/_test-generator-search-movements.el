#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-search-movements.el --- Generator for search movements function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'cpo-search-movements)

;; Generate tests for find-char movements
(carettest-tesmo-generate-tests
 "The quick brown fox jumps over the lazy dog.

This line.has repeated.letters like aaa and bbb.
Final line with punctuation: .,;!? for testing. "
 15  ; number of test positions
 '(("find-char-a-forward" (lambda () (let ((char ?a)) (cpo--find-char-in-line/impl char 1 'beginning))))
   ("find-char-e-backward" (lambda () (let ((char ?e)) (cpo--find-char-in-line/impl char -1 'beginning))))
   ("find-char-o-end-forward" (lambda () (let ((char ?o)) (cpo--find-char-in-line/impl char 1 'end))))
   ("find-char-t-end-backward" (lambda () (let ((char ?t)) (cpo--find-char-in-line/impl char -1 'end))))
   ("find-char-space-forward" (lambda () (let ((char ? )) (cpo--find-char-in-line/impl char 1 'beginning))))
   ("find-char-space-backward" (lambda () (let ((char ? )) (cpo--find-char-in-line/impl char -1 'beginning))))
   ("find-char-period-forward" (lambda () (let ((char ?.)) (cpo--find-char-in-line/impl char 1 'beginning))))
   ("find-char-period-backward" (lambda () (let ((char ?.)) (cpo--find-char-in-line/impl char -1 'beginning)))))
 "test-search-movements"
 :output-file "_generated-tests-search-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.2
 :transient-mark-mode-prob 0.8)
