;;; -*- lexical-binding: t; -*-

(require 'cpo-smartparens)
(require 'carettest-tesmut)
(require 'ert)
(require 'carettest-tesmut)

;; TODO - actually write a bunch of tests
;; TODO - maybe use smartparens testing infrastructure, it has a lot of convenience definitions.  They are probably mostly internal, but they are probably sufficiently stable.

(defmacro should/region-equal (region-cons)
  `(progn
     (should (region-active-p))
     (should (equal ,region-cons
                    (cons (region-beginning) (region-end))))))
(defmacro should/looking-at (at-string)
  `(progn
     (let* ((str ,at-string)
            (result (looking-at-p str)))
       (when (not result)
         (message "Test failure, should look at: %s" str)
         (message "actually looking at: %s" (buffer-substring (point) (min (point-max) (+ (point) 10)))))
       (should result))))
(defmacro should/mark-looking-at (at-string)
  `(progn
     (save-mark-and-excursion
       (exchange-point-and-mark)
       (should/looking-at ,at-string))))

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
