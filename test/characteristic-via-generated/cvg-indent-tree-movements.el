;;; cvg-indent-tree-movements.el --- Characteristic tests for indent-tree movement functions -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-indent-tree)

;;; cpo-indent-tree-forward-to-last-full-sibling
;; Half-sibling style: moves from first child to last child at same indent level.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-forward-to-last-full-sibling__first-to-last
 "root
  sec<p0>tion-a
    item-one
    item-two
  <p1>section-b
    item-three
grandparent
  branch-x
    leaf-alpha
  branch-y
    leaf-beta"
 'cpo-indent-tree-forward-to-last-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: point on a node that IS the last full sibling -- stays put.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-forward-to-last-full-sibling__already-last
 "root
  section-a
    item-one
    item-two
  section-b
    item-three
gr<p1><p0>andparent
  branch-x
    leaf-alpha"
 'cpo-indent-tree-forward-to-last-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: moves from mid-list sibling to the last sibling.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-forward-to-last-full-sibling__mid-to-last
 "root
  section-a
    item-<p0>one
    item-two
    <p1>item-three
grandparent"
 'cpo-indent-tree-forward-to-last-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: moves from one top-level def to the last top-level def.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-forward-to-last-full-sibling__python-top-level
 "def foo():
    pass

def b<p0>ar():
    pass

<p1>def baz():
    pass"
 'cpo-indent-tree-forward-to-last-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-indent-tree-backward-to-first-full-sibling
;; Half-sibling style: moves from last child to first child at same indent level.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-backward-to-first-full-sibling__last-to-first
 "root
  <p1>section-a
    item-one
    item-two
  section-<p0>b
    item-three
grandparent"
 'cpo-indent-tree-backward-to-first-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: point on a node that IS the first full sibling -- stays put.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-backward-to-first-full-sibling__already-first
 "<p1><p0>root
  section-a
    item-one
grandparent"
 'cpo-indent-tree-backward-to-first-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: moves from last sibling to first among a list of siblings.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-backward-to-first-full-sibling__mid-list
 "root
  section-a
    <p1>item-one
    item-two
    item-t<p0>hree
grandparent"
 'cpo-indent-tree-backward-to-first-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: moves from last method back to first method in a class.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-backward-to-first-full-sibling__python-class-methods
 "class Foo:
    <p1>def __init__(self):
        pass

    def connect(self):
        pass

    def dis<p0>connect(self):
        pass"
 'cpo-indent-tree-backward-to-first-full-sibling
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-indent-tree-up-to-root
;; Half-sibling style: moves from deep child up to the root node.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-up-to-root__deep-to-root
 "<p1>root
  section-a
    item-o<p0>ne
    item-two
grandparent"
 'cpo-indent-tree-up-to-root
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: already at root level stays put.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-up-to-root__already-root
 "ro<p1><p0>ot
  section-a
    item-one
grandparent"
 'cpo-indent-tree-up-to-root
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: from second root, stays at same position (already root-level).
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-up-to-root__second-root
 "root
  section-a
    item-one
gran<p1><p0>dparent
  branch-x
    leaf-alpha"
 'cpo-indent-tree-up-to-root
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: from indented statement goes up to top-level definition.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-up-to-root__python-nested
 "<p1>def foo():
    if True:
        x = <p0>1
    return x"
 'cpo-indent-tree-up-to-root
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-indent-tree-down-to-last-descendant
;; Half-sibling style: from a parent with children, moves to beginning of last leaf descendant line.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-down-to-last-descendant__parent-to-leaf
 "root
  sec<p0>tion-a
    item-one
    <p1>item-two
grandparent"
 'cpo-indent-tree-down-to-last-descendant
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: already at a leaf node stays put.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-down-to-last-descendant__leaf-stays
 "root
  section-a
    item-one
    leaf-<p1><p0>two
grandparent"
 'cpo-indent-tree-down-to-last-descendant
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: from root with deeply nested structure -- moves to start of last descendant line.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-down-to-last-descendant__deep-nested
 "ro<p0>ot
  section-a
    item-one
    item-two
  section-b
    <p1>item-three"
 'cpo-indent-tree-down-to-last-descendant
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: from class to start of last nested statement line.
(carettest-tesmo-test
 test-indent-tree-movements-cpo-indent-tree-down-to-last-descendant__python-class
 "class <p0>Foo:
    def bar(self):
        <p1>return 1"
 'cpo-indent-tree-down-to-last-descendant
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-full-sibling-2 (cpo-indent-tree-forward-full-sibling with count 2)
;; Half-sibling style: jumps forward two full siblings.
(carettest-tesmo-test
 test-indent-tree-movements-forward-full-sibling-2__skip-two
 "root
  section-<p0>a
    item-one
  section-b
    item-two
  <p1>section-c
    item-three"
 (lambda nil
   (cpo-indent-tree-forward-full-sibling 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: only one sibling ahead, moves to it (cannot advance 2 steps).
(carettest-tesmo-test
 test-indent-tree-movements-forward-full-sibling-2__one-ahead
 "root
  section-a
    item-<p0>one
    <p1>item-two
grandparent"
 (lambda nil
   (cpo-indent-tree-forward-full-sibling 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: skip two sibling methods.
(carettest-tesmo-test
 test-indent-tree-movements-forward-full-sibling-2__python-skip-two-methods
 "class Foo:
    def ini<p0>t(self):
        pass

    def connect(self):
        pass

    <p1>def disconnect(self):
        pass"
 (lambda nil
   (cpo-indent-tree-forward-full-sibling 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; up-to-parent-2 (cpo-indent-tree-up-to-parent with count 2)
;; Half-sibling style: from grandchild moves up two levels.
(carettest-tesmo-test
 test-indent-tree-movements-up-to-parent-2__grandchild-to-root
 "<p1>root
  section-a
    item-o<p0>ne
    item-two
grandparent"
 (lambda nil
   (cpo-indent-tree-up-to-parent 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: from child (only one level deep), moving up 2 stops at root.
(carettest-tesmo-test
 test-indent-tree-movements-up-to-parent-2__child-to-root
 "<p1>root
  secti<p0>on-a
    item-one
grandparent"
 (lambda nil
   (cpo-indent-tree-up-to-parent 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: from statement nested 2 deep, goes up to top-level def.
(carettest-tesmo-test
 test-indent-tree-movements-up-to-parent-2__python-two-levels
 "<p1>def outer():
    def inner():
        x = <p0>1"
 (lambda nil
   (cpo-indent-tree-up-to-parent 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; down-to-first-child-2 (cpo-indent-tree-down-to-first-child with count 2)
;; Half-sibling style: from grandparent moves down to grandchild.
(carettest-tesmo-test
 test-indent-tree-movements-down-to-first-child-2__grandparent-to-grandchild
 "gran<p0>dparent
  branch-x
    <p1>leaf-alpha
  branch-y
    leaf-beta"
 (lambda nil
   (cpo-indent-tree-down-to-first-child 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Half-sibling style: from parent with one level of children (no grandchildren), stops at child.
(carettest-tesmo-test
 test-indent-tree-movements-down-to-first-child-2__stops-at-leaf
 "root
  sec<p0>tion-a
    <p1>item-one"
 (lambda nil
   (cpo-indent-tree-down-to-first-child 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Python style: from class down two levels.
(carettest-tesmo-test
 test-indent-tree-movements-down-to-first-child-2__python-class-to-statement
 "class <p0>Foo:
    def bar(self):
        <p1>x = 1
        return x"
 (lambda nil
   (cpo-indent-tree-down-to-first-child 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(provide 'cvg-indent-tree-movements)
;;; cvg-indent-tree-movements.el ends here
