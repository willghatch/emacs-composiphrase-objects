;;; -*- lexical-binding: t; -*-

(require 'cpo-indent-tree)
(require 'carettest-tesmut)
(require 'ert)
(require 'carettest-tesmo)

;; TODO - actually write a bunch of tests


;; TODO - these definitions are copy/pasted from the smartparens test file.  If I keep these in the same package, I should make a shared testing infrastructure file...
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


(setq t1
      "
a
  aa
  ab
    aba
    abb
  ac
    aca
    acb
  ad
    ada
    adb
b
  ba
    baa
  bb
")


(ert-deftest test-expand-region-to-any-delimiter-after-last-child_indent-tree ()
  (with-temp-buffer
    (insert t1)
    (transient-mark-mode 1)
    (goto-char 1)
    (search-forward "aba\n")
    (backward-char 4)
    (cpo-indent-tree-expand-region/children-region)
    (should/looking-at " *aba\n")
    (should/mark-looking-at " *ac\n"))
  )

(setq reparent-test-1-pre
      "
if foo()
  i1
  i2
  while bar()
    w1
    w2
    w3
    pivot
      p1
      p2
    w4
    w5
  i3
")
(setq reparent-test-1-post
      "
while bar()
  w1
  w2
  w3
  if foo()
    i1
    i2
    pivot
      p1
      p2
    i3
  w4
  w5
")
(setq reparent-test-2-post_start-with-region
      "
while bar()
  w1
  if foo()
    i1
    i2
    w2
    w3
    pivot
      p1
      p2
    w4
    i3
  w5
")

(carettest-tesmut-test
 test-indent-tree-forward-slurp
 "
<p>parent
  child1
sibling
s2
"
 "
<p>parent
  child1
  sibling
s2
"
 'cpo-indent-tree-forward-slurp)

(carettest-tesmut-test
 test-indent-tree-backward-slurp
 "
sibling
<p>parent
  child1
s2
"
 "
<p>parent
  sibling
  child1
s2
"
 'cpo-indent-tree-backward-slurp)

(carettest-tesmut-test
 test-indent-tree-slurp-all-forward
 "
<p>parent
  child1
sib1
sib2
sib3
"
 "
<p>parent
  child1
  sib1
  sib2
  sib3
"
 'cpo-indent-tree-slurp-all-forward)

(carettest-tesmut-test
 test-indent-tree-slurp-all-backward
 "
sib1
sib2
sib3
<p>parent
  child1
"
 "
<p>parent
  sib1
  sib2
  sib3
  child1
"
 'cpo-indent-tree-slurp-all-backward)

(carettest-tesmut-test
 test-indent-tree-slurp-all-forward-stops-at-parent
 "
grandparent
  <p>parent
    child1
  sib1
  sib2
gsib
"
 "
grandparent
  <p>parent
    child1
    sib1
    sib2
gsib
"
 'cpo-indent-tree-slurp-all-forward)

(carettest-tesmut-test
 test-indent-tree-slurp-all-forward-no-siblings
 "
grandparent
  <p>parent
    child1
gsib
"
 "
grandparent
  <p>parent
    child1
gsib
"
 'cpo-indent-tree-slurp-all-forward)

(carettest-tesmut-test
 test-indent-tree-slurp-all-forward-with-subtrees
 "
<p>parent
  child1
sib1
  sib1child
sib2
"
 "
<p>parent
  child1
  sib1
    sib1child
  sib2
"
 'cpo-indent-tree-slurp-all-forward)

(ert-deftest test-ancestor-reorder-indent-tree ()
  (with-temp-buffer
    (insert reparent-test-1-pre)
    (transient-mark-mode 1)
    (goto-char 1)
    (search-forward "pivot")
    (goto-char (match-beginning 0))
    (should/looking-at "pivot")
    (cpo-indent-tree-ancestor-reorder 1)
    (should (string-equal (buffer-string) reparent-test-1-post))
    (should/looking-at "pivot")
    ))

(ert-deftest test-ancestor-reorder-indent-tree-with-region ()
  (with-temp-buffer
    (insert reparent-test-1-pre)
    (transient-mark-mode 1)
    (goto-char 1)
    ;; Use region from w2 to w4
    (search-forward "w2")
    (beginning-of-line)
    (set-mark (point))
    (forward-line 6)
    (cpo-indent-tree-ancestor-reorder 1)
    (should (string-equal (buffer-string) reparent-test-2-post_start-with-region))
    ))


;;; Half-sibling tests for indent-tree
;;; Test data has a parent with children at varying indentation levels,
;;; where the first child is indented more than later children.

;;; Forward full sibling: deep1 -> deep2 (success, returns 0)
(carettest-tesmo-test test-indent-tree-forward-full-sibling_deep1-to-deep2
                      "
root
      <p0>deep1
      <p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-sibling 1)))))

(carettest-tesmo-test test-indent-tree-forward-full-sibling_deep1-to-deep2_midstuff
                      "
root
      <p0>deep1
        child-of-deep1
      <p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-sibling 1)))))

;;; Forward full sibling from deep2 should fail (mid1 is half sibling, returns 1)
(carettest-tesmo-test test-indent-tree-forward-full-sibling_deep2-stalls
                      "
root
      deep1
      <p0><p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 1 (cpo-indent-tree-forward-full-sibling 1)))))

;;; Forward half-or-full sibling: deep1 -> deep2
(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_deep1-to-deep2
                      "
root
      <p0>deep1
      <p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))


(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_deep1-to-deep2_with-intermediate
                      "
root
      <p0>deep1
        child-of-deep1
      <p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))

;;; Forward half-or-full sibling: deep2 -> mid1 (crosses boundary)
(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_deep2-to-mid1
                      "
root
      deep1
      <p0>deep2
    <p1>mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))


(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_deep2-to-mid1_with-intermediate
                      "
root
      deep1
      <p0>deep2
        deeper
    <p1>mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))

;;; Forward half-or-full sibling: mid1 -> mid2
(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_mid1-to-mid2
                      "
root
      deep1
      deep2
    <p0>mid1
    <p1>mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))

;;; Forward half-or-full sibling: mid2 -> shallow1 (crosses boundary)
(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_mid2-to-shallow1
                      "
root
      deep1
      deep2
    mid1
    <p0>mid2
  <p1>shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 1)))))

;;; Backward full sibling: mid2 -> mid1 (success, returns 0)
(carettest-tesmo-test test-indent-tree-backward-full-sibling_mid2-to-mid1
                      "
root
      deep1
      deep2
    <p1>mid1
    <p0>mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-sibling -1)))))

;;; Backward full sibling from mid1 should fail (deep2 is half sibling, returns 1)
(carettest-tesmo-test test-indent-tree-backward-full-sibling_mid1-stalls
                      "
root
      deep1
      deep2
    <p0><p1>mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 1 (cpo-indent-tree-forward-full-sibling -1)))))

;;; Backward half-or-full sibling: shallow2 -> shallow1
(carettest-tesmo-test test-indent-tree-backward-full-or-half-sibling_shallow2-to-shallow1
                      "
root
      deep1
      deep2
    mid1
    mid2
  <p1>shallow1
  <p0>shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling -1)))))

;;; Backward half-or-full sibling: shallow1 -> mid2 (crosses boundary)
(carettest-tesmo-test test-indent-tree-backward-full-or-half-sibling_shallow1-to-mid2
                      "
root
      deep1
      deep2
    mid1
    <p1>mid2
  <p0>shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling -1)))))

;;; Backward half-or-full sibling: mid2 -> mid1
(carettest-tesmo-test test-indent-tree-backward-full-or-half-sibling_mid2-to-mid1
                      "
root
      deep1
      deep2
    <p1>mid1
    <p0>mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling -1)))))

;;; Backward half-or-full sibling: mid1 -> deep2 (crosses boundary)
(carettest-tesmo-test test-indent-tree-backward-full-or-half-sibling_mid1-to-deep2
                      "
root
      deep1
      <p1>deep2
    <p0>mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling -1)))))


(carettest-tesmo-test test-indent-tree-backward-full-or-half-sibling_mid1-to-deep2_with-intermediate
                      "
root
      deep1
      <p1>deep2
        child-of-deep2
    <p0>mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling -1)))))

;;; Forward full sibling with count 3 from deep1: only reaches deep2 (returns 2)
(carettest-tesmo-test test-indent-tree-forward-full-sibling_count-returns-remaining
                      "
root
      <p0>deep1
      <p1>deep2
    mid1
    mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 2 (cpo-indent-tree-forward-full-sibling 3)))))

;;; Forward half-or-full sibling with count 3: deep1 -> deep2 -> mid1 -> mid2
(carettest-tesmo-test test-indent-tree-forward-full-or-half-sibling_count-crosses-boundaries
                      "
root
      <p0>deep1
      deep2
    mid1
    <p1>mid2
  shallow1
  shallow2
"
                      (lambda () (should (= 0 (cpo-indent-tree-forward-full-or-half-sibling 3)))))
