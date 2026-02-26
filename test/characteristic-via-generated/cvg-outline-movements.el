;;; cvg-outline-movements.el --- Characteristic tests for outline movement functions -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-outline)

;;; cpo-outline-down-to-first-child

;; From body text under first heading, goes to first child heading
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-first-child__at-body-text
 "* First heading
<p0>Some body text.
<p1>** Child heading 1
** Child heading 2
* Second heading"
 'cpo-outline-down-to-first-child
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a heading that has a child, moves to first child (point goes to beginning of child heading line)
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-first-child__at-parent-heading
 "* Root
** <p0>Parent
<p1>*** Child A
*** Child B
** Other"
 'cpo-outline-down-to-first-child
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a leaf heading (no children), stays in place
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-first-child__at-leaf
 "* Root
** Child
*** <p1><p0>Leaf heading"
 'cpo-outline-down-to-first-child
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-outline-down-to-last-child

;; From a heading with multiple children, goes to last child
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-last-child__multiple-children
 "* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
hello
#+end_src
*** Grandchild 1
******** Great-grandchild half sibling A
text here
******* Great-grandchild half sibling B
   indented text here
**** Great-gandchild
***<p1><p0> Grandchild 2
** Child heading 2


**** Deep child
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here
* Third heading"
 'cpo-outline-down-to-last-child
 :transient-mark-mode
 t
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a heading where the last child is a half-sibling (lower depth)
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-last-child__half-sibling-last
 "* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
hello
#+end_src
*** Grandchild 1
******** Great-grandchild half sibling A
text here
******* Great-grandchild half sibling B
   indented text here
**** Great-gandchild
*** Grandchild 2
** Child heading 2


**** Deep child
* Second heading
<p1><p0>*** half-sibling situation
** Another child
*** Grandchild here
* Third heading"
 'cpo-outline-down-to-last-child
 :transient-mark-mode
 t
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a heading with a single deep child
(carettest-tesmo-test
 test-outline-movements-cpo-outline-down-to-last-child__single-deep-child
 "* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
hello
#+end_src
*** Grandchild 1
******** Great-grandchild half sibling A
text here
******* Great-grandchild half sibling B
   indented text here
**** Great-gandchild
*** Grandchild 2
** Child heading 2


<p1><p0>**** Deep child
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here
* Third heading"
 'cpo-outline-down-to-last-child
 :transient-mark-mode
 t
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-outline-up-to-root

;; From a root heading, stays at root
(carettest-tesmo-test
 test-outline-movements-cpo-outline-up-to-root__at-root
 "<p1>* First<p0> heading
Some body text under first heading.
** Child heading 1"
 'cpo-outline-up-to-root
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From body text under a child heading, goes to root
(carettest-tesmo-test
 test-outline-movements-cpo-outline-up-to-root__from-body-text
 "<p1>* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
he<p0>llo
#+end_src
*** Grandchild 1
* Second heading"
 'cpo-outline-up-to-root
 :transient-mark-mode
 t
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a deeply nested grandchild, goes to root
(carettest-tesmo-test
 test-outline-movements-cpo-outline-up-to-root__from-grandchild
 "<p1>* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
hello
#+end_src
*** Grandchild 1
******** Great-grandchild half sibling A
text here
******* Great-grandchild half sibling B
   indented text here
**** G<m1><m0>reat-gandchild
*** Grandchil<p0>d 2
** Child heading 2


**** Deep child
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here
* Third heading"
 'cpo-outline-up-to-root
 :transient-mark-mode
 t
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-outline-forward-half-or-full-sibling
;; Note: requires org-mode (uses org-current-level internally)

;; From a heading, move to full sibling (same level) skipping over children
(carettest-tesmo-test
 test-outline-movements-cpo-outline-forward-half-or-full-sibling__to-full-sibling
 "* Root
** <p0>Alpha
*** Child of Alpha
<p1>** Beta
** Gamma"
 'cpo-outline-forward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a deeply nested heading, move to half-sibling (lower level, child of same parent)
(carettest-tesmo-test
 test-outline-movements-cpo-outline-forward-half-or-full-sibling__to-half-sibling
 "* Root
** Parent
**** <p0>DeepChild
<p1>*** HalfSibling
* Other"
 'cpo-outline-forward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; At middle of siblings, move forward skipping children
(carettest-tesmo-test
 test-outline-movements-cpo-outline-forward-half-or-full-sibling__mid-siblings
 "* Root
** Alpha
** <p0>Beta
*** Child of Beta
<p1>** Gamma"
 'cpo-outline-forward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-outline-backward-half-or-full-sibling
;; Note: requires org-mode (uses org-current-level internally)

;; From a heading, move backward to full sibling
(carettest-tesmo-test
 test-outline-movements-cpo-outline-backward-half-or-full-sibling__to-full-sibling
 "* Root
** Alpha
<p1>** Beta
** <p0>Gamma"
 'cpo-outline-backward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From a half-sibling, move backward to the deeper heading
(carettest-tesmo-test
 test-outline-movements-cpo-outline-backward-half-or-full-sibling__to-half-sibling
 "* Root
** Parent
<p1>**** DeepChild
*** <p0>HalfSibling
* Other"
 'cpo-outline-backward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; At first sibling, stays in place
(carettest-tesmo-test
 test-outline-movements-cpo-outline-backward-half-or-full-sibling__at-first-sibling
 "* Root
<p1><p0>** Alpha
** Beta"
 'cpo-outline-backward-half-or-full-sibling
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-sibling-2 (lambda: move forward by 2 siblings)
;; Note: requires org-mode

;; Move forward 2 full siblings
(carettest-tesmo-test
 test-outline-movements-forward-sibling-2__two-siblings
 "* Root
** <p0>Alpha
** Beta
<p1>** Gamma
** Delta"
 (lambda () (cpo-outline-forward-half-or-full-sibling 2))
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; When two siblings ahead, moves to the second one
(carettest-tesmo-test
 test-outline-movements-forward-sibling-2__skip-intermediate
 "* Root
** <p0>Alpha
*** Child of Alpha
** Beta
<p1>** Gamma"
 (lambda () (cpo-outline-forward-half-or-full-sibling 2))
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-sibling-2 (lambda: move backward by 2 siblings)
;; Note: requires org-mode

;; Move backward 2 full siblings
(carettest-tesmo-test
 test-outline-movements-backward-sibling-2__two-siblings
 "* Root
** Alpha
<p1>** Beta
** Gamma
** <p0>Delta"
 (lambda () (cpo-outline-backward-half-or-full-sibling 2))
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; When only one sibling behind, moves to that one sibling
(carettest-tesmo-test
 test-outline-movements-backward-sibling-2__only-one-behind
 "* Root
<p1>** Alpha
** <p0>Beta"
 (lambda () (cpo-outline-backward-half-or-full-sibling 2))
 :transient-mark-mode
 nil
 :setup
 (org-mode)
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(provide 'cvg-outline-movements)
;;; cvg-outline-movements.el ends here
