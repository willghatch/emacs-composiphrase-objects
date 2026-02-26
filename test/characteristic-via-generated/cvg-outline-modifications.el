;;; cvg-outline-modifications.el --- Characteristic tests for outline modification functions -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'cpo-outline)

;;; cpo-outline-add-heading-above

;; From the first root heading, inserts a new root heading above
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-above__at-root
 :before
 "* First heading<p>
Some body text under first heading.
** Child heading 1
** Child heading 2<m>"
 :after
 "* <p>
* First heading
Some body text under first heading.
** Child heading 1
** Child heading 2<m>"
 :function
 'cpo-outline-add-heading-above
 :transient-mark-mode
 t)

;; From a deeply nested heading, inserts a same-level heading above it
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-above__at-deep-heading
 :before
 "* First heading
** Child heading 1
*** Grandchild 1
******** <p>Great-grandchild half sibling A
******* Great-grandchild half sibling B"
 :after
 "* First heading
** Child heading 1
*** Grandchild 1
******** <p>
******** Great-grandchild half sibling A
******* Great-grandchild half sibling B"
 :function
 'cpo-outline-add-heading-above
 :transient-mark-mode
 t)

;; From a child heading, inserts a sibling heading above
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-above__at-sibling
 :before
 "* First heading
** Child heading 1
*** G<p>randchild 2
** Child heading 2<m>"
 :after
 "* First heading
** Child heading 1
*** <p>
*** Grandchild 2
** Child heading 2<m>"
 :function
 'cpo-outline-add-heading-above
 :transient-mark-mode
 t)

;;; cpo-outline-add-heading-below

;; From a heading with siblings below, inserts below (as next sibling)
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-below__mid-siblings
 :before
 "* First heading
** Child heading 1
**<p>* half-sibling situation
** Another child
*** Grandchild here"
 :after
 "* First heading
** Child heading 1
*** half-sibling situation
*** <p>
** Another child
*** Grandchild here"
 :function
 'cpo-outline-add-heading-below
 :transient-mark-mode
 t)

;; From a heading at end of its siblings, inserts below
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-below__at-last-sibling
 :before
 "* First heading
*** Grandchild 2<p>
** Child heading 2"
 :after
 "* First heading
*** Grandchild 2
*** <p>
** Child heading 2"
 :function
 'cpo-outline-add-heading-below
 :transient-mark-mode
 t)

;; From a root heading, inserts a new root heading below
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-heading-below__at-root
 :before
 "**** Dee<p>p child
* Second heading"
 :after
 "**** Deep child
**** <p>
* Second heading"
 :function
 'cpo-outline-add-heading-below
 :transient-mark-mode
 t)

;;; cpo-outline-add-child-heading

;; From a heading with existing children, adds a new last child
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-child-heading__has-children
 :before
 "* First heading
** Child heading 1
*** Grandchild 1
**** Gr<p>eat-gandchild
*** Grandchild 2"
 :after
 "* First heading
** Child heading 1
*** Grandchild 1
**** Great-gandchild
***** <p>
*** Grandchild 2"
 :function
 'cpo-outline-add-child-heading
 :transient-mark-mode
 t)

;; From a heading with no children (leaf), adds a child
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-child-heading__at-leaf
 :before
 "* First heading
*** Grandchild here
* Third <p>heading"
 :after
 "* First heading
*** Grandchild here
* Third heading
** <p>
"
 :function
 'cpo-outline-add-child-heading
 :transient-mark-mode
 t)

;; From a deeply-nested heading, adds a child one level deeper
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-add-child-heading__deep-heading
 :before
 "* First heading
** Child heading 1
*** Grandchild 1
******** Great-grandchild ha<p>lf sibling A
******* Great-grandchild half sibling B
*** Grandchild 2<m>"
 :after
 "* First heading
** Child heading 1
*** Grandchild 1
******** Great-grandchild half sibling A
********* <p>
******* Great-grandchild half sibling B
*** Grandchild 2<m>"
 :function
 'cpo-outline-add-child-heading
 :transient-mark-mode
 t)

;;; add-child-at-index-1 (lambda: insert child at index 1)

;; From a heading, inserts child at index 1 (after first existing child)
(carettest-tesmut-test
 test-outline-modifications-add-child-at-index-1__mid-children
 :before
 "* First heading
*** half-sib<p>ling situation
**** Child A
**** Child B
** Another"
 :after
 "* First heading
*** half-sibling situation
**** Child A
**** <p>
**** Child B
** Another"
 :function
 (lambda nil
   (cpo-outline-add-child-heading 1))
 :transient-mark-mode
 t)

;; From a heading with no children, inserts child (index 1 with no children is like add-child)
(carettest-tesmut-test
 test-outline-modifications-add-child-at-index-1__no-children
 :before
 "* First heading
*** Gra<p>ndchild 2
** Child heading 2"
 :after
 "* First heading
*** Grandchild 2
**** <p>
** Child heading 2"
 :function
 (lambda nil
   (cpo-outline-add-child-heading 1))
 :transient-mark-mode
 t)

;; From a root heading with multiple children, adds a child at index 1 (after first child)
(carettest-tesmut-test
 test-outline-modifications-add-child-at-index-1__at-root
 :before
 "* First<p> heading
** Child heading 1
*** Grandchild 1
** Child heading 2"
 :after
 "* First heading
** Child heading 1
*** Grandchild 1
** <p>
** Child heading 2"
 :function
 (lambda nil
   (cpo-outline-add-child-heading 1))
 :transient-mark-mode
 t)

;;; cpo-outline-forward-slurp-heading
;; Note: requires org-mode (uses org-demote-subtree internally)

;; Slurp the next same-level sibling into current heading as a child
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-slurp-heading__basic
 :before
 "* Root
** <p>Alpha
** Beta
** Gamma"
 :after
 "* Root
** <p>Alpha
*** Beta
** Gamma"
 :function
 'cpo-outline-forward-slurp-heading
 :transient-mark-mode
 nil
 :setup
 (org-mode))

;; Slurp from middle sibling makes the next one a child
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-slurp-heading__mid-sibling
 :before
 "* Root
** Alpha
** <p>Beta
** Gamma
** Delta"
 :after
 "* Root
** Alpha
** <p>Beta
*** Gamma
** Delta"
 :function
 'cpo-outline-forward-slurp-heading
 :transient-mark-mode
 nil
 :setup
 (org-mode))

;;; cpo-outline-forward-barf-heading
;; Note: requires org-mode (uses org-promote-subtree internally)

;; Barf last child out as next sibling
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-barf-heading__basic
 :before
 "* Root
** <p>Alpha
*** Child A
*** Child B
** Beta"
 :after
 "* Root
** <p>Alpha
*** Child A
** Child B
** Beta"
 :function
 'cpo-outline-forward-barf-heading
 :transient-mark-mode
 nil
 :setup
 (org-mode))

;; Barf when no children does nothing
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-barf-heading__no-children
 :before
 "* Root
** <p>Alpha
** Beta"
 :after
 "* Root
** <p>Alpha
** Beta"
 :function
 'cpo-outline-forward-barf-heading
 :transient-mark-mode
 nil
 :setup
 (org-mode))

;;; add-ancestor-sibling (lambda: insert next sibling of parent)

;; From a grandchild, inserts sibling of its grandparent
(carettest-tesmut-test
 test-outline-modifications-add-ancestor-sibling__from-deep
 :before
 "* First heading
** Child heading 1
*** Grandchild 1
******* Great-grandchild half sib<p>ling B
*** Grandchild 2<m>"
 :after
 "* First heading
** Child heading 1
*** Grandchild 1
******* Great-grandchild half sibling B
*** <p>
*** Grandchild 2<m>"
 :function
 (lambda nil
   (cpo-outline-add-ancestor-next-sibling-heading 1))
 :transient-mark-mode
 t)

;; From a level-3 child with no level-2 parent, inserts a root-level sibling
(carettest-tesmut-test
 test-outline-modifications-add-ancestor-sibling__to-root-level
 :before
 "* First heading
*** G<p>randchild 2
** Child heading 2
* Second heading"
 :after
 "* First heading
*** Grandchild 2
** Child heading 2
* <p>
* Second heading"
 :function
 (lambda nil
   (cpo-outline-add-ancestor-next-sibling-heading 1))
 :transient-mark-mode
 t)

;; From a heading deep in structure, inserts parent's next sibling
(carettest-tesmut-test
 test-outline-modifications-add-ancestor-sibling__from-leaf
 :before
 "* First heading
** Child heading 2


**** <p>Deep child
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here"
 :after
 "* First heading
** Child heading 2


**** Deep child
** <p>
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here"
 :function
 (lambda nil
   (cpo-outline-add-ancestor-next-sibling-heading 1))
 :transient-mark-mode
 t)

(provide 'cvg-outline-modifications)
;;; cvg-outline-modifications.el ends here
