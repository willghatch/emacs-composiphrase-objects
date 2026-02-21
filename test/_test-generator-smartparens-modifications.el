#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" -L "$(dirname "$0")/dependencies/smartparens" -L "$(dirname "$0")/dependencies/dash" --load "$0" "$@" # ;;; _test-generator-smartparens-modifications.el --- Generator for smartparens modifications function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmut-generator)
(require 'cpo-smartparens)

;; Generate tests for smartparens modification functions
;; Text includes both () parens and [] brackets so join/barf/slurp can be tested
;; on multiple delimiter types.
(carettest-tesmut-generate-tests
 "(defun test-function (arg1 arg2)
  \"Docstring here\"
  (let ((var1 (+ arg1 arg2))
        (var2 [arg1 arg2])
        (bzaz {fnarfl}))
    (setq foo 'bar)

    [with squares {braces (paren [brack {brace (par)}])}]

    (if (> var1 var2)
        (list var1 var2 (concat \"result: \" (number-to-string var1)))
      (vector var2 var1 (format \"%s and %s\" arg1 arg2)))))
(defun quux (a b)
  (+ 1 (* 2 3)))"
 20  ; number of test positions
 '(cpo-smartparens-forward-slurp
   cpo-smartparens-backward-slurp
   cpo-smartparens-forward-barf
   cpo-smartparens-backward-barf
   cpo-smartparens-splice
   cpo-smartparens-kill-sexp
   cpo-smartparens-join-sexp-forward
   cpo-smartparens-join-sexp-backward
   cpo-smartparens-open-sibling-forward
   cpo-smartparens-open-sibling-backward
   cpo-smartparens-transpose-sibling-forward
   cpo-smartparens-transpose-sibling-backward
   ("wrap-with-parens" (lambda () (cpo-smartparens-wrap-with-delimiter "(")))
   ("wrap-with-brackets" (lambda () (cpo-smartparens-wrap-with-delimiter "["))))
 "test-smartparens-modifications"
 :output-file "_generated-tests-smartparens-modifications.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.5
 :transient-mark-mode-prob 0.8
 :setup '(progn (smartparens-mode 1) (sp-local-pair 'emacs-lisp-mode "\"" "\"")))
