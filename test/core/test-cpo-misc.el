;;; -*- lexical-binding: t; -*-

;; To run these tests from the command line:
;; emacs -batch -l ert -l dependencies/carettest/carettest-tesmo.el -l ../cpo-tree-walk.el -l ../cpo-text-object-stuff.el -l test-cpo-misc.el -f ert-run-tests-batch-and-exit

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-text-object-stuff-symbol-variants)

;; The native symbol object changes with the mode's syntax table.  These tests
;; pin down the elisp-like and C-like variants so they keep their intended
;; boundaries across different modes.
(carettest-tesmo-test
 test-misc-elisp-like-symbol-forward-end--c-mode
 "alpha fo<p0>o-bar_baz<p1> omega"
 'cpo-forward-elisp-like-symbol-end
 :setup (c-mode)
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-misc-elisp-like-symbol-forward-end--emacs-lisp-mode
 "alpha fo<p0>o-bar_baz<p1> omega"
 'cpo-forward-elisp-like-symbol-end
 :setup (emacs-lisp-mode)
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;; C-like symbols split at hyphens, but keep underscores together.
(carettest-tesmo-test
 test-misc-c-like-symbol-forward-end--c-mode
 "alpha fo<p0>o<p1>-bar_baz omega"
 'cpo-forward-c-like-symbol-end
 :setup (c-mode)
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-misc-c-like-symbol-forward-end--emacs-lisp-mode
 "alpha fo<p0>o<p1>-bar_baz omega"
 'cpo-forward-c-like-symbol-end
 :setup (emacs-lisp-mode)
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word--fundamental-mode
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

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word--emacs-lisp-mode
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
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word_2__regression-newline-syntax--fundamental-mode
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

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word_2__regression-newline-syntax--emacs-lisp-mode
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

;; Regression test: when point is IN the last word of a line (not after it),
;; forward-vi-like-word-beginning used to signal "Search failed" because the
;; forward-op's re-search-forward was bounded to the end of the current line
;; and could not find another word.  The movement should cross the newline and
;; land on the first word of the next line.
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_last-word-of-line-goes-to-next-line--fundamental-mode
 "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third <p0>var
    <p1>return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (fundamental-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_last-word-of-line-goes-to-next-line--emacs-lisp-mode
 "
hello-world_test function_name(arg1, arg2) {
    some_variable = another-variable + third <p0>var
    <p1>return result_value;
}

camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (emacs-lisp-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_last-word-of-line-goes-to-next-no-indentation--fundamental-mode
 "
Notice the lack of indentation.
some_variable = another-variable + third <p0>var
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (fundamental-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_last-word-of-line-goes-to-next-no-indentation--emacs-lisp-mode
 "
Notice the lack of indentation.
some_variable = another-variable + third <p0>var
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (emacs-lisp-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-line--fundamental-mode
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

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-line--emacs-lisp-mode
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

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-no-indentation--fundamental-mode
 "
Notice the lack of indentation.
some_variable = another-variable + third var<p0>
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (fundamental-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning_end-of-word-goes-to-next-no-indentation--emacs-lisp-mode
 "
Notice the lack of indentation.
some_variable = another-variable + third var<p0>
<p1>return result_value;


camelCaseVariable = snake_case_var + hyphenated-name
"
 'cpo-forward-cpo-vi-like-word-beginning
 :setup (emacs-lisp-mode)
 :transient-mark-mode t :points ("<p0>" "<p1>"))


;; Transposition tests
(carettest-tesmut-test test-vi-like-transpose-word-forward_1
                       "foo <p>bar baz quux "
                       "foo baz <p>bar quux "
                       'cpo-transpose-cpo-vi-like-word-forward
                       :setup (emacs-lisp-mode))
(carettest-tesmut-test test-vi-like-transpose-word-forward_2
                       "foo b<p>ar baz quux "
                       "foo baz b<p>ar quux "
                       'cpo-transpose-cpo-vi-like-word-forward
                       :setup (emacs-lisp-mode))
(carettest-tesmut-test test-vi-like-transpose-word-forward_3
                       "foo bar<p> baz quux "
                       "foo baz bar<p> quux "
                       'cpo-transpose-cpo-vi-like-word-forward
                       :setup (emacs-lisp-mode))
(carettest-tesmut-test test-vi-like-transpose-word-backward_1
                       "foo bar <p>baz quux "
                       "foo <p>baz bar quux "
                       'cpo-transpose-cpo-vi-like-word-backward
                       :setup (emacs-lisp-mode))
(carettest-tesmut-test test-vi-like-transpose-word-backward_2
                       "foo bar b<p>az quux "
                       "foo b<p>az bar quux "
                       'cpo-transpose-cpo-vi-like-word-backward
                       :setup (emacs-lisp-mode))
(carettest-tesmut-test test-vi-like-transpose-word-backward_3
                       "foo bar baz<p> quux "
                       "foo baz<p> bar quux "
                       'cpo-transpose-cpo-vi-like-word-backward
                       :setup (emacs-lisp-mode))
