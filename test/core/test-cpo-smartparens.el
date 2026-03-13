;;; -*- lexical-binding: t; -*-

(require 'cpo-smartparens)
(require 'carettest-tesmut)
(require 'carettest-tesmo)
(require 'ert)
(require 'test-cpo-helpers)

;; TODO - actually write a bunch of tests
;; TODO - maybe use smartparens testing infrastructure, it has a lot of convenience definitions.  They are probably mostly internal, but they are probably sufficiently stable.

(ert-deftest test-expand-region-to-any-delimiter-after-last-child_smartparens ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (goto-char 1)
    (search-forward "))")
    (backward-char 5)
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")
    )

  )

(ert-deftest misc-cpo-smartparens-tests ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (goto-char 1)
    (transient-mark-mode 1)
    (emacs-lisp-mode)

    (search-forward "foo")
    (cpo-smartparens-expand-region)
    (should/looking-at "foo")
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")

    (set-mark nil)
    (goto-char 1)
    (search-forward "foo")
    (backward-char 1)
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")

    (set-mark nil)
    (goto-char 1)
    (search-forward "foo")
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")
    )

  )

(carettest-tesmut-test
 test-smartparens-forward-slurp-1
 :before "(<p>(a b) c d e)"
 :after "(<p>(a b c) d e)"
 :function 'cpo-smartparens-forward-slurp
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-forward-barf-1
 :before "(<p>(a b c) d e)"
 :after "(<p>(a b) c d e)"
 :function 'cpo-smartparens-forward-barf
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-forward-slurp-all
 :before "(<p>(a b) c d e)"
 :after "(<p>(a b c d e))"
 :function 'cpo-smartparens-forward-slurp-all
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-forward-slurp-all-nested
 :before "(o2 (outer <p>(inner a) b c) d)"
 :after "(o2 (outer <p>(inner a b c)) d)"
 :function 'cpo-smartparens-forward-slurp-all
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-forward-slurp-all-no-siblings
 :before "(outer <p>(inner a b))"
 :after "(outer <p>(inner a b))"
 :function 'cpo-smartparens-forward-slurp-all
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-backward-slurp-all-no-siblings
 :before "(<p>(inner a b) c)"
 :after "(<p>(inner a b) c)"
 :function 'cpo-smartparens-backward-slurp-all
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-smartparens-backward-slurp-all
 :before "(o2 (outer a b c <p>(inner d)) e)"
 :after "(o2 (<p>(outer a b c inner d)) e)"
 :function 'cpo-smartparens-backward-slurp-all
 :setup (progn (emacs-lisp-mode)
               (smartparens-mode 1)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-cpo-smartparens-raise_symbol
 :before "(a (b <p>c) d)"
 :after "(a <p>c d)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_symbol-2
 :before "(a (<p>b c) d)"
 :after "(a <p>b d)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_delimited
 :before "(a (b <p>(c d)) e)"
 :after "(a <p>(c d) e)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_delimited_at-end
 :before "(a (b (c d)<p>) e)"
 :after "(a (c d)<p> e)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_delimited_at-end-2
 :before "(a (b (c d)<p> more after the target) e)"
 :after "(a (c d)<p> e)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

;; When sandwiches between two s-expressions (IE with no space), prefer choosing the one after point, like we do for all cpo-smartparens operations.
(carettest-tesmut-test
 test-cpo-smartparens-raise_sandwich-1
 :before "(a (b (c d)<p>(e f)) g)"
 :after "(a <p>(e f) g)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_sandwich-2
 :before "(a (b (c d)<p>after) g)"
 :after "(a <p>after g)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise_sandwich-3
 :before "(a (b before<p>(e f)) g)"
 :after "(a <p>(e f) g)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

(carettest-tesmut-test
 test-cpo-smartparens-raise-to-top
 :before "(a <p>(b c) d) (e)"
 :after "<p>(b c) (e)"
 :function 'cpo-smartparens-raise
 :setup (progn (setq-local sp-pair-list '(("(" . ")")))
               (emacs-lisp-mode)))

;; Test that up-parent-beginning from near a close delimiter position goes
;; to the actual parent, not to the beginning of a child sexp.
;; IE this was an issue with selecting the current symex to target, not adhering to the selection rules (prefer at over in).
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_at-close-delimiter
 :before "(a (b c)<p> d)"
 :after "<p>(a (b c) d)"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;;; Tests for up-parent-beginning from middle of symbol (the "back-up" motion).
;;; When in the middle of a non-prefixed symbol, up-parent-beginning should go
;;; directly to the parent delimiter, not to the start of the current symbol.

;; From the middle of a non-prefixed symbol, go to the parent delimiter.
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_middle-of-symbol-to-parent
 :before "(foo b<p>ar baz)"
 :after "<p>(foo bar baz)"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; From the start of a non-prefixed symbol (not middle), also go to parent.
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_start-of-symbol-to-parent
 :before "(foo <p>bar baz)"
 :after "<p>(foo bar baz)"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; From the middle of a symbol in a nested context, go to the innermost
;; parent delimiter, not to the start of the symbol.
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_middle-of-nested-symbol
 :before "(outer (inner f<p>oo bar))"
 :after "(outer <p>(inner foo bar))"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; Two consecutive up-parent-beginning calls from the middle of a symbol
;; should reach the grandparent delimiter (skipping the symbol start).
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_two-steps-from-middle
 :before "(outer (inner f<p>oo bar))"
 :after "<p>(outer (inner foo bar))"
 :function (lambda () (cpo-smartparens-up-parent-beginning 2))
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_two-steps-from-middle_zyxwv
 ;; the quote is the first place it goes up to
 :before "(outer (inner 'f<p>oo bar))"
 :after "(outer <p>(inner 'foo bar))"
 :function (lambda () (cpo-smartparens-up-parent-beginning 2))
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; From middle of symbol, sp-backward-up-sexp may fail when a quote
;; character confuses smartparens.  The scan-lists fallback handles this.
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_with-quote-in-sexp
 :before "(setq f<p>oo 'bar)"
 :after "<p>(setq foo 'bar)"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (smartparens-mode 1)
               (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_with-quote-in-sexp-to-quote
 :before "(setq 'f<p>oo 'bar)"
 :after "(setq <p>'foo 'bar)"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (smartparens-mode 1)
               (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; From an open delimiter, should go to parent.
(carettest-tesmut-test
 test-cpo-smartparens-up-parent-beginning_from-open-delimiter
 :before "(outer <p>(inner foo bar))"
 :after "<p>(outer (inner foo bar))"
 :function 'cpo-smartparens-up-parent-beginning
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;;; Position argument tests for tree-walk expand-region

;; expand-region: default position puts point at beginning
(carettest-tesmo-test
 test-smartparens-expand-region__default-position
 "(outer (inner <p1>foo<p0><m1> bar))"
 'cpo-smartparens-expand-region
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>")
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; expand-region: position 'end puts point at end
(carettest-tesmo-test
 test-smartparens-expand-region__position-end
 "(outer (inner <m1>foo<p0><p1> bar))"
 (lambda () (cpo-smartparens-expand-region 1 :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>")
 :setup (progn (emacs-lisp-mode)
               (setq-local sp-pair-list '(("(" . ")")))))

;; expand-region-to-any-delimiter: position 'end puts point at end
;; Note: sp-pair-list is NOT set explicitly here -- emacs-lisp-mode
;; lets smartparens lazily populate sp-pair-list, which is needed for
;; multi-level expansion (the predicate uses at-open-delimiter-p,
;; which requires sp-pair-list to have been populated by sp-get-thing).
(carettest-tesmo-test
 test-smartparens-expand-region-to-any-delimiter__position-end
 "(outer <m1>(inner foo<p0> bar)<p1>)"
 (lambda () (cpo-smartparens-expand-region-to-any-delimiter 1 :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>")
 :setup (emacs-lisp-mode))
