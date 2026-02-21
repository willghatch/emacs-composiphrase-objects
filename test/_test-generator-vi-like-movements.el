#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-vi-like-movements.el --- Generator for vi-like movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'carettest-tesmut-generator)
(require 'cpo-text-object-stuff)

;; Generate tests for vi-like movement functions
;; Text includes hyphenated, underscored, camelCase, and numeric tokens.
(carettest-tesmo-generate-tests
 "hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + 3.14;
    foo = bar

    count42 = result_value + 100;
}

camelCaseVariable = snake_case_var + hyphenated-name;"
 14  ; number of test positions
 '(cpo-forward-cpo-vi-like-word-beginning
   cpo-backward-cpo-vi-like-word-beginning
   cpo-forward-cpo-vi-like-word-end
   cpo-backward-cpo-vi-like-word-end
   ("forward-cpo-vi-like-word-beginning-2" (lambda () (cpo-forward-cpo-vi-like-word-beginning 2)))
   ("forward-cpo-vi-like-word-end-2" (lambda () (cpo-forward-cpo-vi-like-word-end 2)))
   ("backward-cpo-vi-like-word-beginning-2" (lambda () (cpo-backward-cpo-vi-like-word-beginning 2)))
   ("backward-cpo-vi-like-word-end-2" (lambda () (cpo-backward-cpo-vi-like-word-end 2))))
 "test-vi-like-movements"
 :output-file "_generated-tests-vi-like-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)

(carettest-tesmut-generate-tests
 "hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + 3.14;
    foo = bar

    count42 = result_value + 100;
}

camelCaseVariable = snake_case_var + hyphenated-name;"
 14  ; number of test positions
 '(cpo-transpose-cpo-vi-like-word-forward
   cpo-transpose-cpo-vi-like-word-backward)
 "test-vi-like-movements"
 :output-file "_generated-tests-vi-like-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)
