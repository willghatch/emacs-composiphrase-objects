;;; test-cpo-outline-beginning-end.el --- Tests for outline tree forward/backward beginning/end -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-outline)

;;; ---- backward-beginning tests ----

;;; From middle of heading line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-middle-of-heading
 "* Previous Sibling
Previous body.
<p1>* Heading <p0>One
Some body text.
** Child heading
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From end of heading line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-end-of-heading
 "* Previous Sibling
Previous body.
<p1>* Heading One<p0>
Some body text.
** Child heading
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From body text under heading -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-body-text
 "* Previous Sibling
Previous body.
<p1>* Heading One
Some <p0>body text.
** Child heading
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From body text on second line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-body-second-line
 "* Previous Sibling
Previous body.
<p1>* Heading One
Some body text.
More <p0>body text.
** Child heading
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From beginning of heading -> beginning of previous sibling heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-heading-beginning-to-previous
 "<p1>* Heading One
Some body text.
** child heading
<p0>* sibling heading
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From child heading with no previous sibling -> stays (respects tree)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_respect-tree_no-previous-sibling
 ;; This movement should respect tree shape and only move to siblings.
 ;; Since this child heading has no previous sibling at level 2, it stays put.
 "* Heading One
Some body text.
<p1><p0>** Child heading
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From beginning of first heading -> stays (no move)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-first-heading-stays
 "<p0><p1>* Heading One
Some body text.
** Child heading
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From body of child heading -> beginning of child heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-body
 "* Heading One
Some body text.
<p1>** Child heading
Child <p0>body.
*** Grandchild
** Next Child Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; From beginning of second child sibling -> beginning of first child sibling
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-sibling-to-previous-child-sibling
 "* Heading One
Some body text.
<p1>** First Child
First child body.
<p0>** Second Child
Second child body.
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))


(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-sibling-to-previous-child-sibling_with-child
 "* Heading One
Some body text.
<p1>** First Child
First child body.
*** first child child
<p0>** Second Child
Second child body.
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; Backward from child heading should NOT go to parent (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_does-not-go-to-parent
 ;; With only one child heading and no previous sibling, backward stays put.
 "* Heading One
Some body text.
<p1><p0>** Only Child
Child body.
* Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; Backward from heading should NOT go to uncle (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_does-not-go-to-uncle
 ;; Point is at a child heading whose only previous heading at a higher
 ;; level is an uncle.  Backward should stay put, not jump to uncle.
 "* Uncle Heading
Uncle body.
* Parent Heading
Parent body.
<p1><p0>** Only Child
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (org-mode))

;;; ---- forward-beginning tests ----

;;; From beginning of heading -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-heading-to-next-sibling
 "<p0>* Heading One
Some body text.
** Child heading
Child body.
<p1>* Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; From middle of heading -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-middle-of-heading
 "* Heading <p0>One
Some body text.
** Child heading
Child body.
<p1>* Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; From body text -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-body
 "* Heading One
Some <p0>body text.
** Child heading
Child body.
<p1>* Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; From heading with no next sibling -> stays (no move)
(carettest-tesmo-test
 test-outline-tree-forward-beginning_no-next-sibling-stays
 "* Heading One
Some body text.
<p0><p1>** Only Child
Child body.
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; Forward from child heading to next child sibling
(carettest-tesmo-test
 test-outline-tree-forward-beginning_child-to-next-child-sibling
 "* Heading One
Some body text.
<p0>** First Child
Child body.
<p1>** Second Child
Second child body.
* Next Sibling
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; Forward does NOT go to parent or uncle (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-forward-beginning_does-not-go-to-uncle
 ;; Last child heading under a parent.  Next heading is an uncle.
 ;; Forward should stay put, not jump to uncle.
 "* Parent Heading
Parent body.
<p0><p1>** Last Child
Child body.
* Uncle Heading
Uncle body.
"
 'cpo-outline-tree-forward-beginning
 :setup (org-mode))

;;; ---- forward-end tests ----

;;; From beginning of heading -> end of that heading's full tree node
;;; (tree bounds include all descendants)
(carettest-tesmo-test
 test-outline-tree-forward-end_from-heading-beginning
 "* Previous Sibling
Previous body.
<p0>* Heading One
Some body text.
** Child heading
Child body.
<p1>* Next Sibling
"
 'cpo-outline-tree-forward-end
 :setup (org-mode))

;;; From middle of body -> end of tree node
(carettest-tesmo-test
 test-outline-tree-forward-end_from-body-middle
 "* Heading One
Some body text.
** Child heading
Child <p0>body.
<p1>** Next Child Sibling
More body.
"
 'cpo-outline-tree-forward-end
 :setup (org-mode))

;;; From heading beginning to end when it's the last node (end of buffer)
(carettest-tesmo-test
 test-outline-tree-forward-end_last-node-to-eob
 "* Heading One
Some body text.
<p0>** Child heading
Child body.
<p1>"
 'cpo-outline-tree-forward-end
 :setup (org-mode))

;;; ---- backward-end tests ----

;;; From beginning of second sibling -> end of first sibling's subtree
(carettest-tesmo-test
 test-outline-tree-backward-end_from-sibling-to-prev-end
 "* Heading One
Some body text.
** Child One
Child one body.
<p1><p0>** Child Two
Child two body.
"
 'cpo-outline-tree-backward-end
 :setup (org-mode))

;;; Backward end from heading with no previous sibling -> stays
(carettest-tesmo-test
 test-outline-tree-backward-end_no-previous-sibling-stays
 "* Heading One
Some body text.
<p0><p1>** Only Child
Child body.
"
 'cpo-outline-tree-backward-end
 :setup (org-mode))

;;; ---- backward-beginning with count ----

;;; Backward beginning twice from child body: first to child heading,
;;; then to previous child sibling
(carettest-tesmo-test
 test-outline-tree-backward-beginning_count-2-from-body
 "* Heading One
Some body text.
<p1>** First Child
First child body.
** Second Child
Second <p0>child body.
"
 (lambda () (cpo-outline-tree-backward-beginning 2))
 :setup (org-mode))

;;; ---- forward-beginning with count ----

;;; Forward beginning twice through siblings
(carettest-tesmo-test
 test-outline-tree-forward-beginning_count-2
 "<p0>* Heading One
Some body text.
** Child heading
Child body.
* Heading Two
More body text.
<p1>* Heading Three
Even more text.
"
 (lambda () (cpo-outline-tree-forward-beginning 2))
 :setup (org-mode))

;;; ---- Roundtrip: forward then backward stays put ----

(carettest-tesmo-test
 test-outline-tree-beginning-roundtrip_forward-then-backward
 "* Heading One
Some body text.
<p0><p1>** First Child
Child body.
** Second Child
More body.
"
 (lambda ()
   (cpo-outline-tree-forward-beginning)
   (cpo-outline-tree-backward-beginning))
 :setup (org-mode))

;;; test-cpo-outline-beginning-end.el ends here
