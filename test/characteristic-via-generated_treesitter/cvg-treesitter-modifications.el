;;; cvg-treesitter-modifications.el --- Tests for treesitter modification operations -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'cpo-treesitter-qd)

;; These tests verify treesitter modification functions from cpo-treesitter-qd.
;; Each test uses :setup to activate the appropriate treesitter parser.

;;;; ================================================================
;;;; Raise tests (elisp)
;;;; ================================================================

;; Raise a symbol out of a list: point on "b" in (a (b c) d) -> (a b d)
(carettest-tesmut-test
 test-ts-elisp-raise_symbol
 :before "(a (<p>b c) d)"
 :after "(a <p>b d)"
 :function 'cpo-treesitter-qd-raise
 :setup (treesit-parser-create 'elisp))

;; Raise a list out of a parent list: point on open paren of (c d) in (a (b (c d)) e)
(carettest-tesmut-test
 test-ts-elisp-raise_list
 :before "(a (b <p>(c d)) e)"
 :after "(a <p>(c d) e)"
 :function 'cpo-treesitter-qd-raise
 :setup (treesit-parser-create 'elisp))

;; Raise a defun: from nested defun to top-level
(carettest-tesmut-test
 test-ts-elisp-raise_defun
 :before "(progn\n  <p>(defun foo () 42)\n  (defun bar () 99))"
 :after "<p>(defun foo () 42)"
 :function 'cpo-treesitter-qd-raise
 :setup (treesit-parser-create 'elisp))

;;; cvg-treesitter-modifications.el ends here
