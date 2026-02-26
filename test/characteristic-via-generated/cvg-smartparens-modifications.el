;;; cvg-smartparens-modifications.el --- Characteristic tests for smartparens modification functions -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'dash)
(require 'smartparens)
(require 'cpo-smartparens)

;;; cpo-smartparens-forward-slurp

;; Point inside [x y], forward slurp pulls in the next sibling (bzaz (foo)).
;; The indentation changes because sp-forward-slurp-sexp reindents.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-forward-slurp__mid-list
 :before "(let ((var1 (+ a b))
      (var2 [x<p> y])
      (bzaz (foo)))
  body)"
 :after "(let ((var1 (+ a b))
(var2 [x<p> y]
(bzaz (foo))))
  body)"
 :function
 'cpo-smartparens-forward-slurp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-backward-slurp

;; Point inside a bracket sexp, backward slurp consumes prev sibling.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-backward-slurp__bracket-mid
 :before "(setq foo 'bar)
[with squa<p>res]"
 :after "[(setq foo 'bar)
with squa<p>res]"
 :function
 'cpo-smartparens-backward-slurp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-forward-barf

;; Point inside paren sexp, forward barf ejects last element.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-forward-barf__paren-barf
 :before "(if (> var1 var<p>2)
    (list a b)
  (vector c d))"
 :after "(if (> var1<p>) var2
    (list a b)
  (vector c d))"
 :function
 'cpo-smartparens-forward-barf
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point at beginning of inner element, barf ejects last element of containing sexp.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-forward-barf__bracket-inner-barf
 :before "[with {braces (<p>paren [brack])}]"
 :after "[with {braces (<p>paren) [brack]}]"
 :function
 'cpo-smartparens-forward-barf
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-backward-barf

;; Point inside paren sexp, backward barf ejects first element.
;; Actual indentation after barf depends on sp reindentation.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-backward-barf__paren-barf
 :before "(let ((var1 (+ a b))
      (bz<p>az (foo)))
  body)"
 :after "(let ((var1 (+ a b))
bzaz (<p>(foo)))
  body)"
 :function
 'cpo-smartparens-backward-barf
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point inside a string sexp, barf ejects leading content.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-backward-barf__string-barf
 :before "(defun foo (arg1)
  \"Doc<p>string here\"
  body)"
 :after "(defun foo (arg1)
Docstring \"<p>here\"
  body)"
 :function
 'cpo-smartparens-backward-barf
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-splice

;; Point in inner paren, splice removes parens and leaves content.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-splice__inner-paren
 :before "(concat \"result: \" (number-to-str<p>ing var1))"
 :after "(concat \"result: \" number-to-str<p>ing var1)"
 :function
 'cpo-smartparens-splice
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in top-level defun, splice removes outer parens.
;; sp-splice reindents so inner content loses indentation.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-splice__top-level
 :before "(defun<p> foo (a b)
  (+ a b))"
 :after "defun<p> foo (a b)
(+ a b)"
 :function
 'cpo-smartparens-splice
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-kill-sexp

;; Point in middle of symbol, kill-sexp kills that symbol.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-kill-sexp__kill-symbol-mid
 :before "(concat \"result: \" (numb<p>er-to-string var1))"
 :after "(concat \"result: \" (<p> var1))"
 :function
 'cpo-smartparens-kill-sexp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point at start of symbol, kills that symbol.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-kill-sexp__kill-symbol-start
 :before "(+ <p>arg1 arg2)"
 :after "(+ <p> arg2)"
 :function
 'cpo-smartparens-kill-sexp
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-join-sexp-forward

;; Point inside a symbol sexp, join-forward moves point to end of the containing delimited sexp.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-join-sexp-forward__paren-end
 :before "(let ((var1 (+ a b)))
  <p>(setq foo 'bar)
  body)"
 :after "(let ((var1 (+ a b)))
  (setq foo 'bar)<p>
  body)"
 :function
 'cpo-smartparens-join-sexp-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point inside a bracket sexp with no forward sibling -- join-forward
;; should be a no-op (no sibling to join with).
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-join-sexp-forward__bracket-end
 :before "[with <p>squares {braces}]"
 :after "[with <p>squares {braces}]"
 :function
 'cpo-smartparens-join-sexp-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point inside a string, join-forward moves to end of string.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-join-sexp-forward__string
 :before "(defun foo (a b)
  <p>\"Docstring here\"
  body)"
 :after "(defun foo (a b)
  \"Docstring here\"<p>
  body)"
 :function
 'cpo-smartparens-join-sexp-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-join-sexp-backward

;; Point in inner paren, join-backward moves to beginning of containing sexp.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-join-sexp-backward__paren-mid
 :before "(let ((var1 (+ arg1 a<p>rg2))
      (var2 [x y]))
  body)"
 :after "(let ((var1 <p>(+ arg1 arg2))
      (var2 [x y]))
  body)"
 :function
 'cpo-smartparens-join-sexp-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in string, join-backward moves to beginning.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-join-sexp-backward__string
 :before "(defun foo (a b)
  (concat <p>\"result: \" (number-to-string var1)))"
 :after "(defun foo (a b)
  (concat <p>\"result: \" (number-to-string var1)))"
 :function
 'cpo-smartparens-join-sexp-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-open-sibling-forward

;; Point in string, open-sibling-forward inserts space and new position.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-open-sibling-forward__string-mid
 :before "(concat \"result: <p>\" (number-to-string var1))"
 :after "(concat \"result: \" <p> (number-to-string var1))"
 :function
 'cpo-smartparens-open-sibling-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in symbol inside paren, open-sibling inserts space after.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-open-sibling-forward__symbol
 :before "(format \"%s and %s\" <p>arg1 arg2)"
 :after "(format \"%s and %s\" arg1 <p> arg2)"
 :function
 'cpo-smartparens-open-sibling-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-open-sibling-backward

;; Point in symbol, open-sibling-backward inserts space before.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-open-sibling-backward__symbol-mid
 :before "(concat \"result: \" (numb<p>er-to-string var1))"
 :after "(concat \"result: \" (<p> number-to-string var1))"
 :function
 'cpo-smartparens-open-sibling-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in symbol, open-sibling-backward inserts newline before it.
;; When point is in a symbol sexp, inserting before it adds newline+indent.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-open-sibling-backward__newline-before-symbol
 :before "(defun foo (a b)
  let<p> ((x 1))
  body)"
 :after "(defun foo (a b)
  <p>
  let ((x 1))
  body)"
 :function
 'cpo-smartparens-open-sibling-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-transpose-sibling-forward

;; Point in first sibling, transposes with next sibling.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-transpose-sibling-forward__basic
 :before "(+ a<p> b c)"
 :after "(+ b <p>a c)"
 :function
 'cpo-smartparens-transpose-sibling-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in symbol sibling, transposes with next sibling.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-transpose-sibling-forward__symbols
 :before "(concat \"result: \" (number-to-str<p>ing var1))"
 :after "(concat \"result: \" (var1 <p>number-to-string))"
 :function
 'cpo-smartparens-transpose-sibling-forward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; cpo-smartparens-transpose-sibling-backward

;; Point in second sibling, transposes backward.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-transpose-sibling-backward__basic
 :before "(+ 1 (* 2<p> 3))"
 :after "(+ 1 (<p>2 * 3))"
 :function
 'cpo-smartparens-transpose-sibling-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point in inner symbol, transpose moves it before its sibling.
(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-transpose-sibling-backward__list-items
 :before "(let ((var1 (+ a b))
       (var2 [x y])
      <p> (bzaz (foo)))
  body)"
 :after "(let ((var1 (+ a b))
       <p>(bzaz (foo))
       (var2 [x y]))
  body)"
 :function
 'cpo-smartparens-transpose-sibling-backward
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; wrap-with-parens (cpo-smartparens-wrap-with-delimiter "(")

;; Point in symbol, wrap-with-parens wraps it in parens.
(carettest-tesmut-test
 test-smartparens-modifications-wrap-with-parens__symbol-mid
 :before "(if (> v<p>ar1 var2) a b)"
 :after "(if (> (v<p>ar1) var2) a b)"
 :function
 (lambda nil
   (cpo-smartparens-wrap-with-delimiter "("))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point at end of paren sexp, wraps the entire sexp.
(carettest-tesmut-test
 test-smartparens-modifications-wrap-with-parens__after-sexp
 :before "(if (> var1 var2)
    (list a b c)<p>
  (vector d e f))"
 :after "(if (> var1 var2)
    ((list a b c))<p>
  (vector d e f))"
 :function
 (lambda nil
   (cpo-smartparens-wrap-with-delimiter "("))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;;; wrap-with-brackets (cpo-smartparens-wrap-with-delimiter "[")

;; Point in symbol, wrap-with-brackets wraps it in brackets.
(carettest-tesmut-test
 test-smartparens-modifications-wrap-with-brackets__symbol-mid
 :before "(if (> var1 va<p>r2) a b)"
 :after "(if (> var1 [va<p>r2]) a b)"
 :function
 (lambda nil
   (cpo-smartparens-wrap-with-delimiter "["))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

;; Point at beginning of string, wraps the string in brackets.
(carettest-tesmut-test
 test-smartparens-modifications-wrap-with-brackets__before-string
 :before "(defun foo (a b)
<p>  \"Docstring here\"
  body)"
 :after "(defun foo (a b)
<p>  [\"Docstring here\"]
  body)"
 :function
 (lambda nil
   (cpo-smartparens-wrap-with-delimiter "["))
 :transient-mark-mode
 nil
 :setup
 (progn
   (smartparens-mode 1)
   (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(provide 'cvg-smartparens-modifications)
;;; cvg-smartparens-modifications.el ends here
