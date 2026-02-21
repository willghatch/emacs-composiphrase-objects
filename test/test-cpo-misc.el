;;; -*- lexical-binding: t; -*-

;; To run these tests from the command line:
;; emacs -batch -l ert -l dependencies/carettest/carettest-tesmo.el -l ../cpo-tree-walk.el -l ../cpo-text-object-stuff.el -l test-cpo-misc.el -f ert-run-tests-batch-and-exit

(require 'carettest-tesmo)

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word--fundamental-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third<p0>_<p1>var
    return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (fundamental-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word--emacs-lisp-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third<p0>_<p1>var
    return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (emacs-lisp-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

;; Regression test: the function used to land 2 chars into "var" (at "r")
;; when running interactively in emacs-lisp-mode or any mode where `\n' does
;; not have space syntax.  The bug was that `[[:space:]]' in Emacs regexp is
;; syntax-table-dependent (it matches chars with SPACE SYNTAX, not POSIX
;; whitespace), so in emacs-lisp-mode `\n' has comment-end syntax and was not
;; excluded from the punctuation-word match pattern, causing it to be treated
;; as a separate word token.  Fixed by explicitly excluding "\n" from the
;; `(not (any word space))' match.
(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word_2__regression-newline-syntax--fundamental-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + <p0>third <p1>var
    return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (fundamental-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word_2__regression-newline-syntax--emacs-lisp-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + <p0>third <p1>var
    return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (emacs-lisp-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-line--fundamental-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third var<p0>
    <p1>return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (fundamental-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-line--emacs-lisp-mode
                      "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third var<p0>
    <p1>return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (emacs-lisp-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-no-indentation--fundamental-mode
                      "
Notice the lack of indentation.
some_variable = another-variable + third var<p0>
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (fundamental-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-no-indentation--emacs-lisp-mode
                      "
Notice the lack of indentation.
some_variable = another-variable + third var<p0>
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
                      'cpo-forward-cpo-vi-like-word-beginning
                      :setup (emacs-lisp-mode)
                      :transient-mark-mode t :points ("<p0>" "<p1>"))
