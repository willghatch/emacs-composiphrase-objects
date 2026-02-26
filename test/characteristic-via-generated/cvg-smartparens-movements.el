;;; cvg-smartparens-movements.el --- Characteristic tests for smartparens movements -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'dash)
(require 'smartparens)
(require 'cpo-smartparens)

;;; cpo-smartparens-forward-sibling-beginning

;; From atom bzaz, moves to next sibling {xyz}.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-forward-sibling-beginning__mid-binding
 "((aa (+ x y))
  (bb [x y])
  (<p0>bzaz <p1>{xyz}))"
 'cpo-smartparens-forward-sibling-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside symbol, moves to beginning of next sibling atom.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-forward-sibling-beginning__in-vector
 "(vector arg1<p0> <p1>arg2)"
 'cpo-smartparens-forward-sibling-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside let, from let keyword moves to next sibling (the binding list).
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-forward-sibling-beginning__let-keyword
 "(l<p0>et <p1>((x 1) (y 2))
  (+ x y))"
 'cpo-smartparens-forward-sibling-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-backward-sibling-beginning

;; Inside braces, single atom, moves back to beginning of that atom.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-backward-sibling-beginning__in-braces
 "({<p1>fnarf<p0>l})"
 'cpo-smartparens-backward-sibling-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside parens, from arg2 moves back to arg1.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-backward-sibling-beginning__in-parens
 "(+ <p1>arg1 <p0>arg2)"
 'cpo-smartparens-backward-sibling-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-forward-sibling-end

;; Mid-symbol concat, moves to end of concat (the symbol boundary).
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-forward-sibling-end__mid-list
 "(list var1 var2 (co<p0>ncat<p1> \"result\" (+ 1 2)))"
 'cpo-smartparens-forward-sibling-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-var2 in list, moves to end of var2.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-forward-sibling-end__var-in-list
 "(list var1 v<p0>ar2<p1> (concat \"x\" y))"
 'cpo-smartparens-forward-sibling-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-backward-sibling-end

;; From test-function atom, goes back to end of defun keyword.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-backward-sibling-end__defun-name
 "(defun<p1> test-func<p0>tion (a b)
  (+ a b))"
 'cpo-smartparens-backward-sibling-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside braces, backward sibling end: from mid-atom to end of previous sibling.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-backward-sibling-end__in-braces
 "[with squares {b<p1><p0>races (paren [brack {brace (par)}])}]"
 'cpo-smartparens-backward-sibling-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-up-parent-beginning

;; Already at outer level, stays at start of file-level sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-parent-beginning__outermost
 "<p1><p0>(defun foo (a b)
  (+ a b))"
 'cpo-smartparens-up-parent-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside concat sexp, moves up to beginning of containing list.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-parent-beginning__in-concat
 "(list x y (<p1>con<p0>cat \"r\" (+ 1 2)))"
 'cpo-smartparens-up-parent-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside setq, moves up to beginning of containing sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-parent-beginning__in-setq
 "(let ((x 1))
  (<p1>set<p0>q foo 'bar)
  (+ x 1))"
 'cpo-smartparens-up-parent-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-up-parent-end

;; Inside a sub-list, moves to end of the containing list sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-parent-end__in-sub-list
 "(if (> x y)
    (list x y<p0> (concat \"r\" (+ 1 2)))<p1>
  (vector y x))"
 'cpo-smartparens-up-parent-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Deeply nested, moves to end of containing sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-parent-end__deeply-nested
 "(concat \"r\" (number-to-<p0>string x)<p1>)"
 'cpo-smartparens-up-parent-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-down-first-child-beginning

;; Mid-symbol concat, no nested sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-first-child-beginning__on-call
 "(list x y (co<p1><p0>ncat \"r\" (+ 1 2)))"
 'cpo-smartparens-down-first-child-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside if condition variable, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-first-child-beginning__if-var
 "(if (> var1 var<p1><p0>2)
    (list x y)
  (vector y x))"
 'cpo-smartparens-down-first-child-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-down-last-child-beginning

;; Mid-symbol in brackets, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-last-child-beginning__in-brackets
 "[with squares {braces (paren [brack<p1><p0> {brace (par)}])}]"
 'cpo-smartparens-down-last-child-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside setq at quoted symbol, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-last-child-beginning__in-setq
 "(setq foo 'b<p1><p0>ar)"
 'cpo-smartparens-down-last-child-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-down-last-child-end

;; Inside vector args, mid-symbol, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-last-child-end__in-vector-args
 "(vector x y (format \"%s %s\" a<p1><p0>rg1 arg2))"
 'cpo-smartparens-down-last-child-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; At binding start, mid-symbol, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-last-child-end__at-binding-start
 "((var1 (+ a b))
  (<p1><p0>bzaz {xyz}))"
 'cpo-smartparens-down-last-child-end
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-move-to-other-end-of-sexp

;; Inside atom (not at delimiter): point stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-other-end-of-sexp__in-braces-noop
 "({fn<p1><p0>arfl})"
 'cpo-smartparens-move-to-other-end-of-sexp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-symbol var1 inside condition: no delimiter, stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-other-end-of-sexp__in-if-cond
 "(if (> var<p1><p0>1 var2)
    (list x)
  (vector y))"
 'cpo-smartparens-move-to-other-end-of-sexp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-symbol concat: no delimiter, stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-other-end-of-sexp__on-concat
 "(list x y (con<p1><p0>cat \"r\" (+ 1 2)))"
 'cpo-smartparens-move-to-other-end-of-sexp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-up-to-root

;; Deep in docstring, moves to beginning of file-level sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-to-root__from-docstring
 "<p1>(defun foo (a b)
  \"Do<p0>cstring here\"
  (+ a b))"
 'cpo-smartparens-up-to-root
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside second defun, moves to root beginning of that defun.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-to-root__second-defun
 "(defun foo (a b)
  (+ a b))
<p1>(defun<p0> quux (x y)
  (+ 1 (* 2 3)))"
 'cpo-smartparens-up-to-root
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Deep inside nested list, moves to root of defun.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-up-to-root__deep-nested
 "<p1>(defun foo (a b)
  (if (> a b)
   <p0>   (list a b (concat \"r\" (+ 1 2)))
    (vector b a)))"
 'cpo-smartparens-up-to-root
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-down-to-last-descendant

;; Mid-symbol, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-to-last-descendant__in-vector
 "((var2<p1><p0> [arg1 arg2])
  (bzaz {xyz}))"
 'cpo-smartparens-down-to-last-descendant
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-symbol concat, no sexp to descend into: stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-down-to-last-descendant__on-concat
 "(list x y (con<p1><p0>cat \"r\" (+ 1 2)))"
 'cpo-smartparens-down-to-last-descendant
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-sibling-beginning-2 -- moves 2 siblings forward

;; Inside (setq ...), moves forward 2 siblings from foo.
(carettest-tesmo-test
 test-smartparens-movements-forward-sibling-beginning-2__in-setq
 "(setq f<p1><p0>oo 'bar)"
 (lambda nil
   (cpo-smartparens-forward-sibling-beginning 2))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; In (a b) arg list: from a, jump 2 to reach b (skips 1 implicit, arrives at 2nd).
(carettest-tesmo-test
 test-smartparens-movements-forward-sibling-beginning-2__in-arg-list
 "(defun quux (<p0>a <p1>b)
  (+ 1 (* 2 3)))"
 (lambda nil
   (cpo-smartparens-forward-sibling-beginning 2))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; up-parent-beginning-3 -- moves 3 levels up

;; From bzaz in let binding, 3 levels up reaches the bindings list.
(carettest-tesmo-test
 test-smartparens-movements-up-parent-beginning-3__from-binding-atom
 "(defun foo (a b)
  (let <p1>((var1 (+ a b))
        (bz<p0>az {xyz}))
    (+ a b)))"
 (lambda nil
   (cpo-smartparens-up-parent-beginning 3))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From deeply nested arg, 3 up reaches (concat ...).
(carettest-tesmo-test
 test-smartparens-movements-up-parent-beginning-3__from-string-arg
 "(if (> x y)
    (list x y <p1>(concat \"r\" (number-to-string<p0> var1)))
  (vector y x))"
 (lambda nil
   (cpo-smartparens-up-parent-beginning 3))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From braces inside brackets, 3 up goes to the square bracket sexp.
(carettest-tesmo-test
 test-smartparens-movements-up-parent-beginning-3__from-braces-in-brackets
 "(let ((x 1))
  <p1>[with squares {b<p0>races (paren [brack {brace (par)}])}]
  (+ x 1))"
 (lambda nil
   (cpo-smartparens-up-parent-beginning 3))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-move-to-current-sexp-beginning

;; Already at beginning of (setq ...), stays put.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-current-sexp-beginning__at-beginning
 "(let ((x 1))
  <p1><p0>(setq foo 'bar)
  (+ x 1))"
 'cpo-smartparens-move-to-current-sexp-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside [arg1 arg2]: moves to beginning of atom at point.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-current-sexp-beginning__in-vector
 "((var2 [<p1><p0>arg1 arg2]))"
 'cpo-smartparens-move-to-current-sexp-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside (vector ...) sexp, moves to beginning of symbol at point.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-move-to-current-sexp-beginning__in-vector-call
 "(if (> x y)
    (list x y)
  (<p1>ve<p0>ctor y x (format \"%s\" a)))"
 'cpo-smartparens-move-to-current-sexp-beginning
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-smartparens-expand-region-to-any-delimiter

;; Inside (+ arg1 arg2): expands region to encompass entire sexp.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-expand-region-to-any-delimiter__in-add
 "((var1 <p1>(+ a<p0>rg1 arg2)<m1>)
  (var2 [x y]))"
 'cpo-smartparens-expand-region-to-any-delimiter
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside string literal: expands to encompass the quoted string.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-expand-region-to-any-delimiter__in-string
 "(vector x y (format <p1>\"%s and %s\"<m1><p0> arg1 arg2))"
 'cpo-smartparens-expand-region-to-any-delimiter
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside setq, mid-symbol: expand is a no-op here.
(carettest-tesmo-test
 test-smartparens-movements-cpo-smartparens-expand-region-to-any-delimiter__in-setq
 "(setq f<p1><p0>oo 'bar)"
 'cpo-smartparens-expand-region-to-any-delimiter
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; expand-region-to-parens -- expands to nearest paren delimiter

;; Deep inside format with mark set: large active region means expand is a no-op.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-parens__deep-in-format
 "(let ()
  (setq f<m1><m0>oo 'bar)
  (vector x y (format \"%s %s\" arg1 a<p1><p0>rg2)))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter "("))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Region spanning multiple args: expands to larger (if ...) sexp.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-parens__spanning-args
 "<p1>(if (> va<m0>r1 var2)
    (list x y (concat \"r\" (+ 1 2)))
  (vector y x (format \"%s %s\" arg1 a<p0>rg2)))<m1>"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter "("))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; expand-region-to-brackets -- expands to nearest bracket delimiter

;; Inside (+ arg1 arg2) with no brackets above, stays at paren level.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-brackets__no-brackets-above
 "((var1 <p1><p0>(+ arg1 arg2))
  (bza<m1><m0>z {xyz}))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter "["))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside (+ arg1 arg2), expands to bracket containing it.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-brackets__in-add
 "((var1 (+ arg1<p1><p0> arg2))
  (var2 [x y]))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter "["))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; expand-region-to-parens-children -- expands to children region of paren sexp

;; Inside (* 2 3): expands to the children region of (* 2 3).
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-parens-children__in-multiply
 "(defun quux (a b)
  (+ 1 (<p1>* 2 3<m1><p0>)))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter/children-region "("))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside (vector ...) body: expands to children region of that sexp.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-parens-children__in-vector-call
 "(if (> x y)
    (list x y)
  (<p1>vecto<p0>r y x (format \"%s %s\" a b)<m1>))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter/children-region "("))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; expand-region-to-any-delimiter-2 -- expands 2 levels out

;; Deep in concat: expands 2 levels out with mark set.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-any-delimiter-2__in-concat
 "<p1>(defun foo (a b)
  (let ((x 1)
    <m0>    (y 2))
    (if (> a b)
        (list a b (conc<p0>at \"r\" (+ 1 2)))
      (vector b a))))<m1>"
 (lambda nil
   (cpo-smartparens-expand-region-to-any-delimiter 2))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside (concat ...): expands 2 levels to (list ...) containing it.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-any-delimiter-2__in-string-literal
 "(if (> x y)
    (list x y <p1>(concat \"result: <p0>\" (+ 1 2))<m1>)
  (vector y x))"
 (lambda nil
   (cpo-smartparens-expand-region-to-any-delimiter 2))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; expand-region-to-brackets-children-2 -- bracket children region, 2 levels

;; Inside (* 2 3): 2-level brackets-children expand (no brackets, so no-op).
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-brackets-children-2__in-multiply
 "(defun foo (a b)
  (+ a b))
(defun quux (a b)
  (+ 1 (* 2<p1><p0> 3)))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter/children-region "[" 2))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside second defun name: 2-level brackets-children expand.
(carettest-tesmo-test
 test-smartparens-movements-expand-region-to-brackets-children-2__in-defun-name
 "(defun foo (a b)
  (+ a b))
(de<p1><p0>fun quux (x y)
  (+ 1 (* 2 3)))"
 (lambda nil
   (cpo-smartparens-expand-region-to-delimiter/children-region "[" 2))
 :transient-mark-mode
 t
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\""))
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(provide 'cvg-smartparens-movements)
;;; cvg-smartparens-movements.el ends here
