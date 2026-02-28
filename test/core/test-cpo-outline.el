;;; -*- lexical-binding: t; -*-

(require 'cpo-outline)
(require 'org)
(require 'ert)
(require 'carettest-tesmo)
(require 'carettest-tesmut)


;;; Test data with half siblings for outline trees.
;;; In org-mode, half siblings arise when a heading has children at varying
;;; depths, eg. a * heading having a *** child and a ** child -- the *** and
;;; ** are half siblings since they share a parent but have different levels.


;;; Forward full sibling: deep1 -> deep2 (success, returns 0)
(carettest-tesmo-test
 test-outline-forward-full-sibling_deep1-to-deep2
 "* root
<p0>*** deep1
<p1>*** deep2
** mid1
** mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))

(carettest-tesmo-test
 test-outline-forward-full-sibling_deep1-to-deep2_with-intermediate
 "* root
<p0>*** deep1
**** child-of-deep1
<p1>*** deep2
** mid1
** mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))

;;; Forward full sibling from deep2 should fail (mid1 is half sibling, returns 1)
(carettest-tesmo-test
 test-outline-forward-full-sibling_deep2-stalls
 "* root
*** deep1
<p0><p1>*** deep2
** mid1
** mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))


(carettest-tesmo-test
 test-outline-forward-full-sibling_deep2-stalls_with-intermediate
 "* root
*** deep1
<p0><p1>*** deep2
**** deep2-child
** mid1
** mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))

;;; Forward half-or-full sibling: deep1 -> deep2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep1-to-deep2
 "* root
<p0>*** deep1
<p1>*** deep2
** mid1
** mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))


(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep1-to-deep2_with-intermediate
 "* root
<p0>*** deep1
**** deep1-child
<p1>*** deep2
** mid1
** mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling: deep2 -> mid1 (crosses half-sibling boundary)
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep2-to-mid1
 "* root
*** deep1
<p0>*** deep2
<p1>** mid1
** mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))


(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep2-to-mid1_with-intermediate
 "* root
*** deep1
<p0>*** deep2
**** deep2-child
<p1>** mid1
** mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling: mid1 -> mid2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_mid1-to-mid2
 "* root
*** deep1
*** deep2
<p0>** mid1
<p1>** mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Backward full sibling: mid2 -> mid1 (success, returns 0)
(carettest-tesmo-test
 test-outline-backward-full-sibling_mid2-to-mid1
 "* root
*** deep1
*** deep2
<p1>** mid1
<p0>** mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling -1))))
 :setup (org-mode))

;;; Backward full sibling from mid1 should fail (deep2 is half sibling, returns 1)
(carettest-tesmo-test
 test-outline-backward-full-sibling_mid1-stalls
 "* root
*** deep1
*** deep2
<p0><p1>** mid1
** mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling -1))))
 :setup (org-mode))

;;; Backward half-or-full sibling: mid2 -> mid1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling_mid2-to-mid1
 "* root
*** deep1
*** deep2
<p1>** mid1
<p0>** mid2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))

;;; Backward half-or-full sibling: mid1 -> deep2 (crosses half-sibling boundary)
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling_mid1-to-deep2
 "* root
*** deep1
<p1>*** deep2
<p0>** mid1
** mid2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))


;;; Multi-level half siblings (t-outline-half-sibling-2)

;;; Forward full sibling: deep1 -> deep2 in multi-level (success, returns 0)
(carettest-tesmo-test
 test-outline-forward-full-sibling-multi-level_deep1-to-deep2
 "* root
<p0>**** deep1
<p1>**** deep2
*** mid1
*** mid2
** shallow1
** shallow2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))

;;; Forward full sibling from deep2 in multi-level should fail (returns 1)
(carettest-tesmo-test
 test-outline-forward-full-sibling-multi-level_deep2-stalls
 "* root
**** deep1
<p0><p1>**** deep2
*** mid1
*** mid2
** shallow1
** shallow2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (org-mode))

;;; Forward half-or-full sibling multi-level: deep1 -> deep2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_deep1-to-deep2
 "* root
<p0>**** deep1
<p1>**** deep2
*** mid1
*** mid2
** shallow1
** shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling multi-level: deep2 -> mid1
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_deep2-to-mid1
 "* root
**** deep1
<p0>**** deep2
<p1>*** mid1
*** mid2
** shallow1
** shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling multi-level: mid1 -> mid2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_mid1-to-mid2
 "* root
**** deep1
**** deep2
<p0>*** mid1
<p1>*** mid2
** shallow1
** shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling multi-level: mid2 -> shallow1
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_mid2-to-shallow1
 "* root
**** deep1
**** deep2
*** mid1
<p0>*** mid2
<p1>** shallow1
** shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Forward half-or-full sibling multi-level: shallow1 -> shallow2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_shallow1-to-shallow2
 "* root
**** deep1
**** deep2
*** mid1
*** mid2
<p0>** shallow1
<p1>** shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (org-mode))

;;; Backward half-or-full sibling multi-level: shallow2 -> shallow1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_shallow2-to-shallow1
 "* root
**** deep1
**** deep2
*** mid1
*** mid2
<p1>** shallow1
<p0>** shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))

;;; Backward half-or-full sibling multi-level: shallow1 -> mid2
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_shallow1-to-mid2
 "* root
**** deep1
**** deep2
*** mid1
<p1>*** mid2
<p0>** shallow1
** shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))

;;; Backward half-or-full sibling multi-level: mid2 -> mid1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_mid2-to-mid1
 "* root
**** deep1
**** deep2
<p1>*** mid1
<p0>*** mid2
** shallow1
** shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))

;;; Backward half-or-full sibling multi-level: mid1 -> deep2
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_mid1-to-deep2
 "* root
**** deep1
<p1>**** deep2
<p0>*** mid1
*** mid2
** shallow1
** shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (org-mode))

;;; Forward full sibling with count 3 from deep1: only reaches deep2 (returns 2)
(carettest-tesmo-test
 test-outline-forward-full-sibling_count-returns-remaining
 "* root
<p0>*** deep1
<p1>*** deep2
** mid1
** mid2
"
 (lambda () (should (= 2 (cpo-outline-forward-full-sibling 3))))
 :setup (org-mode))


;;; Raise tests

(carettest-tesmut-test
 test-outline-raise-basic
 :before
 "* root
** child1
<p>** child2
*** grandchild1
*** grandchild2
** child3
"
 :after
 "<p>* child2
** grandchild1
** grandchild2
"
 :function 'cpo-outline-raise
 :setup (org-mode))

(carettest-tesmut-test
 test-outline-raise-leaf
 :before
 "* root
** child1
<p>** child2
** child3
"
 :after
 "<p>* child2
"
 :function 'cpo-outline-raise
 :setup (org-mode))

(carettest-tesmut-test
 test-outline-raise-deeply-nested
 :before
 "* root
** parent
*** child1
<p>*** child2
**** deep1
*** child3
** sibling
"
 :after
 "* root
<p>** child2
*** deep1
** sibling
"
 :function 'cpo-outline-raise
 :setup (org-mode))

(carettest-tesmut-test
 test-outline-raise-with-body
 :before
 "* root
Some root body text.
** child1
Child1 body.
<p>** child2
Child2 body.
*** grandchild
Grandchild body.
** child3
"
 :after
 "<p>* child2
Child2 body.
** grandchild
Grandchild body.
"
 :function 'cpo-outline-raise
 :setup (org-mode))

(carettest-tesmut-test
 test-outline-raise-preserves-subtree-structure
 :before
 "* root
<p>** child
*** gc1
**** ggc1
*** gc2
"
 :after
 "<p>* child
** gc1
*** ggc1
** gc2
"
 :function 'cpo-outline-raise
 :setup (org-mode))
