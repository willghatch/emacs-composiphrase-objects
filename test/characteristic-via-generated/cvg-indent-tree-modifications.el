;;; cvg-indent-tree-modifications.el --- Characteristic tests for indent-tree modification functions -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'cpo-indent-tree)

;;; cpo-indent-tree-demote
;; Demotes the current node (and its subtree) one level deeper.

;; Basic demote: a top-level sibling becomes a child of the preceding sibling.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-demote__basic
 :before
 "root
  section-a
    item-one
  bra<p>nch-y
    leaf-beta
    leaf-gamma"
 :after
 "root
  section-a
    item-one
    bra<p>nch-y
      leaf-beta
      leaf-gamma"
 :function
 'cpo-indent-tree-demote
 :transient-mark-mode
 nil)

;; Demote from inner node with children: indents node and its subtree.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-demote__with-subtree
 :before
 "parent
  section-a
    item-one
    item-two
  bra<p>nch-y
    leaf-beta
    leaf-gamma"
 :after
 "parent
  section-a
    item-one
    item-two
    bra<p>nch-y
      leaf-beta
      leaf-gamma"
 :function
 'cpo-indent-tree-demote
 :transient-mark-mode
 nil)

;; Demote leaf node that already has a preceding sibling at same level.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-demote__leaf
 :before
 "root
  branch-x
    leaf-alpha
  bran<p>ch-y
    leaf-beta"
 :after
 "root
  branch-x
    leaf-alpha
    bran<p>ch-y
      leaf-beta"
 :function
 'cpo-indent-tree-demote
 :transient-mark-mode
 nil)

;;; cpo-indent-tree-promote
;; Promotes the current node (and its subtree) one level shallower.

;; Basic promote: a grandchild moves up one level (to be a child of parent, sibling of section-a).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-promote__basic
 :before
 "parent
  section-a
    item-<p>one
    item-two
grandparent"
 :after
 "parent
  section-a
  item-<p>one
    item-two
grandparent"
 :function
 'cpo-indent-tree-promote
 :transient-mark-mode
 nil)

;; Promote a node with its subtree: moves up one indent level.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-promote__with-subtree
 :before
 "root
  section-a
    item-one
    item-two
  section-b
      section b half<p> sibling here
    item-three"
 :after
 "root
  section-a
    item-one
    item-two
  section-b
    section b half<p> sibling here
    item-three"
 :function
 'cpo-indent-tree-promote
 :transient-mark-mode
 nil)

;; Promote a child node: moves up one indent level; sibling stays at original indent.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-promote__inner-child
 :before
 "parent
  section-a
  <p>    section b half sibling here
    item-three"
 :after
 "parent
  section-a
    <p>section b half sibling here
    item-three"
 :function
 'cpo-indent-tree-promote
 :transient-mark-mode
 nil)

;;; cpo-indent-tree-open-sibling-forward
;; Opens a blank sibling line after the current node's subtree.

;; Basic: inserts blank line after current node (after its subtree).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-forward__basic
 :before
 "root
  b<p>ranch-x
    leaf-alpha
  branch-y
    leaf-beta"
 :after
 "root
  branch-x
    leaf-alpha
  <p>
  branch-y
    leaf-beta"
 :function
 'cpo-indent-tree-open-sibling-forward
 :transient-mark-mode
 nil)

;; Open sibling forward from leaf node.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-forward__from-leaf
 :before
 "root
  section-a
    item-<p>one
    item-two"
 :after
 "root
  section-a
    item-one
    <p>
    item-two"
 :function
 'cpo-indent-tree-open-sibling-forward
 :transient-mark-mode
 nil)

;; Open sibling forward from half-sibling node: blank line inserted at the node's indent level.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-forward__mid-list
 :before
 "root
  section-a
    item-one
    item-two
  section-b
    item-three
  <p>    detail
    detail-two"
 :after
 "root
  section-a
    item-one
    item-two
  section-b
    item-three
      detail
      <p>
    detail-two"
 :function
 'cpo-indent-tree-open-sibling-forward
 :transient-mark-mode
 nil)

;;; cpo-indent-tree-open-sibling-backward
;; Opens a blank sibling line before the current node.

;; Basic: inserts blank line before current node.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-backward__basic
 :before
 "root
  section-a
    item-one
    item-three
        half sibling her<p>e
    detail"
 :after
 "root
  section-a
    item-one
    item-three
        <p>
        half sibling here
    detail"
 :function
 'cpo-indent-tree-open-sibling-backward
 :transient-mark-mode
 nil)

;; Open sibling backward on half-sibling node.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-backward__half-sibling
 :before
 "root
  section-a
      section b h<p>alf sibling here
    item-three"
 :after
 "root
  section-a
      <p>
      section b half sibling here
    item-three"
 :function
 'cpo-indent-tree-open-sibling-backward
 :transient-mark-mode
 nil)

;; Open sibling backward from mid-list position.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-open-sibling-backward__mid-list
 :before
 "root
  section-a
    item-one
  section-<p>b
    item-two"
 :after
 "root
  section-a
    item-one
  <p>
  section-b
    item-two"
 :function
 'cpo-indent-tree-open-sibling-backward
 :transient-mark-mode
 nil)

;;; cpo-indent-tree-transpose-sibling-forward
;; Transposes the current node with its next full sibling.

;; Basic transpose: swaps two leaf siblings (trailing content ensures newline separation).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-forward__leaf-siblings
 :before
 "root
  section-a
    item-<p>one
    item-two
grandparent"
 :after
 "root
  section-a
    item-two
<p>    item-one
grandparent"
 :function
 'cpo-indent-tree-transpose-sibling-forward
 :transient-mark-mode
 nil)

;; Transpose a node with subtree with next sibling (trailing content needed for correct newlines).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-forward__with-subtree
 :before
 "root
  section-a
    item-one
    item-two
  sec<p>tion-b
    item-three
  section-c
grandparent"
 :after
 "root
  section-a
    item-one
    item-two
  section-c
<p>  section-b
    item-three
grandparent"
 :function
 'cpo-indent-tree-transpose-sibling-forward
 :transient-mark-mode
 nil)

;; Transpose a half-sibling node with the next full sibling.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-forward__half-sibling-node
 :before
 "root
  item-three
        half siblin<p>g here
      detail
        foo bar baz
      detail-2"
 :after
 "root
  item-three
      detail
        foo bar baz
<p>        half sibling here
      detail-2"
 :function
 'cpo-indent-tree-transpose-sibling-forward
 :transient-mark-mode
 nil)

;;; cpo-indent-tree-transpose-sibling-backward
;; Transposes the current node with its previous full sibling.

;; Basic transpose backward: swaps current sibling with previous one.
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-backward__basic
 :before
 "parent
  section-a
    item-one
    item-two
<p>  section-b
    item-three
grandparent"
 :after
 "parent
<p>  section-b
    item-three
  section-a
    item-one
    item-two
grandparent"
 :function
 'cpo-indent-tree-transpose-sibling-backward
 :transient-mark-mode
 nil)

;; Transpose backward: swaps detail-2 with detail (trailing content needed for newlines).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-backward__inner-nodes
 :before
 "root
  item-three
      detail
        foo bar baz
      d<p>etail-2
grandparent"
 :after
 "root
  item-three
<p>      detail-2
      detail
        foo bar baz
grandparent"
 :function
 'cpo-indent-tree-transpose-sibling-backward
 :transient-mark-mode
 nil)

;; Transpose backward: swaps two sibling sections (trailing content needed for newlines).
(carettest-tesmut-test
 test-indent-tree-modifications-cpo-indent-tree-transpose-sibling-backward__sections
 :before
 "root
  section-a
    item-one
    item-two
<p>  section-b
    item-three
grandparent"
 :after
 "root
<p>  section-b
    item-three
  section-a
    item-one
    item-two
grandparent"
 :function
 'cpo-indent-tree-transpose-sibling-backward
 :transient-mark-mode
 nil)

(provide 'cvg-indent-tree-modifications)
;;; cvg-indent-tree-modifications.el ends here
