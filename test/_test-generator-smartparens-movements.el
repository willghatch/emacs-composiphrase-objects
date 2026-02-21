#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" -L "$(dirname "$0")/dependencies/dash" -L "$(dirname "$0")/dependencies/smartparens" --load "$0" "$@" # ;;; _test-generator-smartparens-movements.el --- Generator for smartparens movements function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'carettest-tesmut-generator)
(require 'cpo-smartparens)

;; Generate tests for smartparens movement functions
;; Text includes both () parens and [] brackets for multi-delimiter testing.
(carettest-tesmo-generate-tests
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
 25  ; number of test positions
 '(cpo-smartparens-forward-sibling-beginning
   cpo-smartparens-backward-sibling-beginning
   cpo-smartparens-forward-sibling-end
   cpo-smartparens-backward-sibling-end
   cpo-smartparens-up-parent-beginning
   cpo-smartparens-up-parent-end
   cpo-smartparens-down-first-child-beginning
   cpo-smartparens-down-last-child-beginning
   cpo-smartparens-down-last-child-end
   cpo-smartparens-move-to-other-end-of-sexp
   cpo-smartparens-up-to-root
   cpo-smartparens-down-to-last-descendant
   cpo-smartparens-forward-inorder-traversal
   cpo-smartparens-backward-inorder-traversal
   ("forward-sibling-beginning-2" (lambda () (cpo-smartparens-forward-sibling-beginning 2)))
   ("up-parent-beginning-3" (lambda () (cpo-smartparens-up-parent-beginning 3)))
   cpo-smartparens-move-to-current-sexp-beginning
   cpo-smartparens-expand-region-to-any-delimiter
   ("expand-region-to-parens" (lambda () (cpo-smartparens-expand-region-to-delimiter "(")))
   ("expand-region-to-brackets" (lambda () (cpo-smartparens-expand-region-to-delimiter "[")))
   ("expand-region-to-parens-children" (lambda () (cpo-smartparens-expand-region-to-delimiter/children-region "(")))
   ("expand-region-to-any-delimiter-2" (lambda () (cpo-smartparens-expand-region-to-any-delimiter 2)))
   ("expand-region-to-brackets-children-2" (lambda () (cpo-smartparens-expand-region-to-delimiter/children-region "[" 2))))
 "test-smartparens-movements"
 :output-file "_generated-tests-smartparens-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8
 :setup '(progn (smartparens-mode 1) (sp-local-pair 'emacs-lisp-mode "\"" "\"")))


