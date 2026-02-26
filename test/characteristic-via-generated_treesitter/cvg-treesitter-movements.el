;;; cvg-treesitter-movements.el --- Characteristic tests for treesitter movements -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-treesitter-qd)

;; These tests verify treesitter navigation functions from cpo-treesitter-qd.
;; Each test uses :setup to activate the appropriate treesitter parser.
;; Elisp uses (treesit-parser-create 'elisp) since there is no elisp-ts-mode.
;; Python uses (python-ts-mode) to get the full ts-mode setup.


;;;; ================================================================
;;;; Elisp tests
;;;; ================================================================
;; Buffer: (defun foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n
;; Tree (interesting nodes with anchors):
;;   function_definition[1-28] anchor=1
;;     "(" "defun"[2] symbol[8] list[12] list[20] ")"
;;   function_definition[30-66] anchor=30
;;     "(" "defun"[31] symbol[37] list[41] list[47] ")"

;;; cpo-treesitter-qd-up-to-parent-anchor-point

;; From symbol "foo" to parent function_definition anchor (the open paren).
(carettest-tesmo-test
 test-ts-elisp-up-to-parent__symbol-to-defun
 "<p1>(defun <p0>foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-up-to-parent-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From body list (+ x y) to parent function_definition.
(carettest-tesmo-test
 test-ts-elisp-up-to-parent__body-to-defun
 "<p1>(defun foo (x y)\n  <p0>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-up-to-parent-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-up-to-root

;; From symbol "bar" in second defun, up-to-root lands at the function_definition.
(carettest-tesmo-test
 test-ts-elisp-up-to-root__deep-symbol
 "(defun foo (x y)\n  (+ x y))\n\n<p1>(defun <p0>bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-up-to-root
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From list (+ x y) up to root (first function_definition).
(carettest-tesmo-test
 test-ts-elisp-up-to-root__body-list
 "<p1>(defun foo (x y)\n  <p0>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-up-to-root
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-forward-sibling-anchor-point

;; Between top-level defuns: first to second.
(carettest-tesmo-test
 test-ts-elisp-forward-sibling__defun-to-defun
 "<p0>(defun foo (x y)\n  (+ x y))\n\n<p1>(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Within defun: from "defun" symbol to "foo" symbol.
(carettest-tesmo-test
 test-ts-elisp-forward-sibling__defun-sym-to-name
 "(<p0>defun <p1>foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Within defun: from "foo" symbol to parameter list (x y).
(carettest-tesmo-test
 test-ts-elisp-forward-sibling__name-to-params
 "(defun <p0>foo <p1>(x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Within defun: from param list (x y) to body list (+ x y).
(carettest-tesmo-test
 test-ts-elisp-forward-sibling__params-to-body
 "(defun foo <p0>(x y)\n  <p1>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-backward-sibling-anchor-point

;; Between top-level defuns: second back to first.
(carettest-tesmo-test
 test-ts-elisp-backward-sibling__defun-to-defun
 "<p1>(defun foo (x y)\n  (+ x y))\n\n<p0>(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-backward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Within defun: from "foo" back to "defun".
(carettest-tesmo-test
 test-ts-elisp-backward-sibling__name-to-defun
 "(<p1>defun <p0>foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-backward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Within defun: from body list (+ x y) back to param list (x y).
(carettest-tesmo-test
 test-ts-elisp-backward-sibling__body-to-params
 "(defun foo <p1>(x y)\n  <p0>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-backward-sibling-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-down-to-first-child-anchor-point

;; From function_definition to first interesting child ("defun" symbol).
(carettest-tesmo-test
 test-ts-elisp-down-first-child__defun-to-symbol
 "(<p0><p1>defun foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-down-to-first-child-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From second function_definition to its first child.
(carettest-tesmo-test
 test-ts-elisp-down-first-child__second-defun
 "(defun foo (x y)\n  (+ x y))\n\n(<p0><p1>defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-down-to-first-child-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-down-to-last-child-anchor-point

;; From function_definition to last interesting child (body list).
;; Point at the open paren places us on the function_definition node.
(carettest-tesmo-test
 test-ts-elisp-down-last-child__defun-to-body
 "<p0>(defun foo (x y)\n  <p1>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 'cpo-treesitter-qd-down-to-last-child-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From second function_definition to its last interesting child.
(carettest-tesmo-test
 test-ts-elisp-down-last-child__second-defun
 "(defun foo (x y)\n  (+ x y))\n\n<p0>(defun bar (a)\n  <p1>(list a (foo a 1)))\n"
 'cpo-treesitter-qd-down-to-last-child-anchor-point
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-forward-inorder-traversal

;; From function_definition to first child.
(carettest-tesmo-test
 test-ts-elisp-forward-inorder__begin
 "<p0>(<p1>defun foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From "defun" to "foo" symbol.
(carettest-tesmo-test
 test-ts-elisp-forward-inorder__defun-to-name
 "(<p0>defun <p1>foo (x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From param list to first symbol inside it ("x").
(carettest-tesmo-test
 test-ts-elisp-forward-inorder__params-to-child
 "(defun foo <p0>(<p1>x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-backward-inorder-traversal

;; From second function_definition back to last node in first defun.
(carettest-tesmo-test
 test-ts-elisp-backward-inorder__cross-defun
 "(defun foo (x y)\n  (+ x <p1>y))\n\n<p0>(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-backward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From body list back to last symbol in param list ("y").
(carettest-tesmo-test
 test-ts-elisp-backward-inorder__body-to-params
 "(defun foo (x <p1>y)\n  <p0>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-backward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From param list back to "foo" symbol.
(carettest-tesmo-test
 test-ts-elisp-backward-inorder__params-to-name
 "(defun <p1>foo <p0>(x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-backward-inorder-traversal 1))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; up-to-parent-2 (skip 2 parent levels)

;; From symbol "x" inside (x y) list up 2 levels to function_definition.
(carettest-tesmo-test
 test-ts-elisp-up-to-parent-2__param-symbol
 "<p1>(defun foo (<p0>x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; forward-sibling-2 (skip 2 siblings)

;; From "defun" skip 2 siblings: defun->foo->list(x y), lands on (x y).
(carettest-tesmo-test
 test-ts-elisp-forward-sibling-2__defun
 "(<p0>defun foo <p1>(x y)\n  (+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; backward-sibling-2 (skip 2 siblings backward)

;; From body list (+ x y) skip 2 back: body->params->foo, lands on "foo".
(carettest-tesmo-test
 test-ts-elisp-backward-sibling-2__body
 "(defun <p1>foo (x y)\n  <p0>(+ x y))\n\n(defun bar (a)\n  (list a (foo a 1)))\n"
 (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))
 :setup (treesit-parser-create 'elisp)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;;; ================================================================
;;;; Python tests
;;;; ================================================================
;; Buffer: def foo(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n
;; Tree (interesting nodes with anchors):
;;   function_definition[1-32] anchor=1 (from "def" keyword)
;;     identifier[5-8] anchor=5, parameters[8-14] anchor=8
;;       identifier[9-10] identifier[12-13]
;;     block[20-32] anchor=nil
;;   function_definition[34-66] anchor=34
;;     identifier[38-41] anchor=38, parameters[41-44] anchor=41
;;       identifier[42-43]
;;     block[50-66] anchor=nil

;;; cpo-treesitter-qd-up-to-parent-anchor-point

;; From identifier "foo" to parent function_definition.
(carettest-tesmo-test
 test-ts-python-up-to-parent__ident-to-funcdef
 "<p1>def <p0>foo(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-up-to-parent-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From parameter "x" to parent parameters node.
(carettest-tesmo-test
 test-ts-python-up-to-parent__param-to-params
 "def foo<p1>(<p0>x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-up-to-parent-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-up-to-root

;; From identifier "foo" up to top-level function_definition.
(carettest-tesmo-test
 test-ts-python-up-to-root__ident
 "<p1>def <p0>foo(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-up-to-root
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From parameter identifier up to top-level function_definition.
(carettest-tesmo-test
 test-ts-python-up-to-root__param
 "<p1>def foo(<p0>x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-up-to-root
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-forward-sibling-anchor-point

;; Between top-level function definitions.
(carettest-tesmo-test
 test-ts-python-forward-sibling__funcdef
 "<p0>def foo(x, y):\n    return x + y\n\n<p1>def bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From identifier to parameters within function_definition.
(carettest-tesmo-test
 test-ts-python-forward-sibling__ident-to-params
 "def <p0>foo<p1>(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Between parameter identifiers: x to y.
(carettest-tesmo-test
 test-ts-python-forward-sibling__param-x-to-y
 "def foo(<p0>x, <p1>y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-forward-sibling-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-backward-sibling-anchor-point

;; Between top-level function definitions (backward).
(carettest-tesmo-test
 test-ts-python-backward-sibling__funcdef
 "<p1>def foo(x, y):\n    return x + y\n\n<p0>def bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-backward-sibling-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Between parameter identifiers: y back to x.
(carettest-tesmo-test
 test-ts-python-backward-sibling__param-y-to-x
 "def foo(<p1>x, <p0>y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-backward-sibling-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-down-to-first-child-anchor-point

;; From function_definition to first interesting child (identifier "foo").
(carettest-tesmo-test
 test-ts-python-down-first-child__funcdef
 "<p0>def <p1>foo(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-down-to-first-child-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From parameters node to first parameter "x".
(carettest-tesmo-test
 test-ts-python-down-first-child__params
 "def foo<p0>(<p1>x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-down-to-first-child-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-down-to-last-child-anchor-point

;; From parameters node to last parameter "y".
(carettest-tesmo-test
 test-ts-python-down-last-child__params
 "def foo<p0>(x, <p1>y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 'cpo-treesitter-qd-down-to-last-child-anchor-point
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-forward-inorder-traversal

;; From function_definition to identifier "foo".
(carettest-tesmo-test
 test-ts-python-forward-inorder__funcdef-to-ident
 "<p0>def <p1>foo(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From identifier "foo" to parameters.
(carettest-tesmo-test
 test-ts-python-forward-inorder__ident-to-params
 "def <p0>foo<p1>(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From param "y" to second function_definition.
(carettest-tesmo-test
 test-ts-python-forward-inorder__param-to-funcdef
 "def foo(x, <p0>y):\n    return x + y\n\n<p1>def bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-forward-inorder-traversal 1))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-treesitter-qd-backward-inorder-traversal

;; From second function_definition back to first.
(carettest-tesmo-test
 test-ts-python-backward-inorder__funcdef
 "<p1>def foo(x, y):\n    return x + y\n\n<p0>def bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-backward-inorder-traversal 1))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; From parameters back to identifier "foo".
(carettest-tesmo-test
 test-ts-python-backward-inorder__params-to-ident
 "def <p1>foo<p0>(x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-backward-inorder-traversal 1))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; up-to-parent-2

;; From param identifier "x" up 2 levels to function_definition.
(carettest-tesmo-test
 test-ts-python-up-to-parent-2__param
 "<p1>def foo(<p0>x, y):\n    return x + y\n\ndef bar(a):\n    return foo(a, 1)\n"
 (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2))
 :setup (python-ts-mode)
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))



(provide 'cvg-treesitter-movements)
;;; cvg-treesitter-movements.el ends here
