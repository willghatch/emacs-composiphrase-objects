;;; -*- lexical-binding: t; -*-
;; These tests appear to show bugs or unexpected behavior.

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-text-object-stuff)

(carettest-tesmo-test
 test-date-movements-cpo-forward-date-end__from-middle
 "text<m1><m0> 2024-03-<p0>10<p1> end."
 'cpo-forward-date-end :transient-mark-mode nil :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-datetime-movements-cpo-forward-datetime-end__from-middle-of-date
 "text 2024-<p0>03-10T18:22:45<p1> end."
 'cpo-forward-datetime-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-datetime-movements-cpo-forward-datetime-end__from-middle-of-time
 "text 2024-03-10T18:<p0>22:45<p1> end."
 'cpo-forward-datetime-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-datetime-movements-cpo-forward-datetime-end__nptdez
 "text 2024-03-10<p0>T18:22:45<p1> end."
 'cpo-forward-datetime-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))


(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-splice__etewmz
 :before "(if <p>(> a b) c) (x<m> y)"
 :after "(if <p>> a b c) (x<m> y)"
 :function 'cpo-smartparens-splice
 :transient-mark-mode t
 :setup (progn
          (smartparens-mode 1)
          (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(carettest-tesmut-test
 ;; The point here is that at point there is no symex forward without going up a level, so join-forward should be a no-op.
 test-smartparens-modifications-cpo-smartparens-join-sexp-forward__lpiyzf
 :before "(foo (bar baz<p>)) (aoeu)"
 :after "(foo (bar baz<p>)) (aoeu)"
 :function 'cpo-smartparens-join-sexp-forward
 :transient-mark-mode t
 :setup (progn
          (smartparens-mode 1)
          (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-forward-slurp__djrudz-no-mark
 :before "(defun test-function (arg1 arg2)
  (let ((var1 (+ arg1 arg2))
        (var2 [arg1 arg2])<p>
        (bzaz {fnarfl}))
    (setq foo 'bar)
    ))
"
 :after "(defun test-function (arg1 arg2)
  (let ((var1 (+ arg1 arg2))
        (var2 [arg1 arg2]
        (bzaz {fnarfl}))<p>)
    (setq foo 'bar)
    ))
"
  :function 'cpo-smartparens-forward-slurp
  :transient-mark-mode t
  :setup (progn
           (smartparens-mode 1)
           (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(carettest-tesmo-test
 test-text-object-movements-backward-paragraph-end-2__dwrhss
 ;; shouldn't go backward to the end of a paragraph since there isn't a paragraph to go back to.
 "Aa adi<p1><p0>piscing bb.

Cc cupidata<m1><m0>t dd.

Ee ff."
 (lambda nil
   (cpo-backward-paragraph-end 2))
 :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-backward-symbol-end-2__ovncxf
 ;; shouldn't go forward to the end, there is no symbol to go backward to, so it should be a no-op.
 "<p0><p1>quick_brown fox."
 (lambda nil
   (cpo-backward-symbol-end 2))
 :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-backward-symbol-end-2__qxvwem
 ;; Same as above, but should be a no-op whether at beginning, middle, or end.
 "L<p0><p1>orem ipsum dolor."
 (lambda nil
   (cpo-backward-symbol-end 2))
 :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-line-end__aoyhkn
 ;; backward-line-end should not move forward to a line end
 "Lorem <p0><p1>ipsum dolor.
Second line here."
 'cpo-backward-line-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-paragraph-end__cvznwt
 ;; backward-paragraph-end should not go forward.
 "First conse<p0><p1>quat.

Second paragraph.

Third potenti <m1><m0>nisi.
"
 'cpo-backward-paragraph-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-sentence-end__aemtpd
 ;; backward-sentence-end should not move forward.
 "The quick_brown o<p0><p1>ver lazy-dogs.
Next sentence here."
 'cpo-backward-sentence-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-symbol-end__slfvdm
 ;; backward movement should not move forward
 "<p0><p1>Lorem ipsum dolor."
 'cpo-backward-symbol-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-word-end__ozrueb
 ;; backward movement should not move forward
 "Lore<p0><p1>m ipsum dolor."
 'cpo-backward-word-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-word-end__unrwar
 "<p0><p1>Lorem ipsum dolor."
 'cpo-backward-word-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-backward-word-end__unrwar2
 "Lo<p0><p1>rem ipsum dolor."
 'cpo-backward-word-end :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-forward-line-beginning__icatsa
 ;; forward movement should not move backward
 "First line.
Last line <p0><p1>here."
 'cpo-forward-line-beginning :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-forward-paragraph-beginning__besglj
 ;; forward movement should not move backward
 "Last line (t<p0><p1>est123) end."
 'cpo-forward-paragraph-beginning :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-text-object-movements-cpo-forward-sentence-beginning__safncu
 ;; forward movement should not move backward
 "First sentence.  Last co<p0><p1>nvallis.
"
 'cpo-forward-sentence-beginning :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))
