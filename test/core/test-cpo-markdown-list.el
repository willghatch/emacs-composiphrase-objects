;;; test-cpo-markdown-list.el --- Tests for cpo-markdown-list -*- lexical-binding: t; -*-

;; Tests-first spec for the markdown-list tree object.
;; The cases below focus on the parts that are easy to get wrong in Markdown:
;; multiline items, nested sublists, and numbered lists that need repair.

(require 'cpo-markdown-list)
(require 'carettest-tesmo)
(require 'carettest-tesmut)

(defun cpo-test--setup-markdown-list ()
  "Use a plain text buffer for markdown-list tests.
The implementation should not depend on markdown-mode being installed."
  (text-mode)
  (setq-local indent-tabs-mode nil))

;;; Tree motion edge cases

(carettest-tesmo-test
 test-markdown-list-forward-beginning_from-body-skips-intermixed-paragraphs-and-sublists
 "1. Parent one
   Fir<p0>st paragraph.
   - Child one
   - Child two

   Final paragraph.
<p1>2. Parent two
3. Parent three
"
 'cpo-markdown-list-forward-beginning
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmo-test
 test-markdown-list-forward-beginning_from-child-body_moves-to-next-child-sibling
 "1. Parent one
   - Child one
     Child body.<p0>
<p1>   - Child two
     Child two body.
2. Parent two
"
 'cpo-markdown-list-forward-beginning
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmo-test
 test-markdown-list-backward-beginning_from-child-body_moves-to-child-bullet
 "1. Parent one
<p1>   - Child one
     Child body.<p0>
   - Child two
2. Parent two
"
 'cpo-markdown-list-backward-beginning
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmo-test
 test-markdown-list-forward-end_from-body-includes-paragraphs-and-sublists
 "1. Parent one
   First paragraph.
   - Child one
   - Child two

   Final paragraph.<p0>
<p1>2. Parent two
"
 'cpo-markdown-list-forward-end
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmo-test
 test-markdown-list-backward-end_from-first-item-stays-put
 "<p0><p1>1. Parent one
   First paragraph.
   - Child one
   - Child two
2. Parent two
"
 'cpo-markdown-list-backward-end
 :setup (cpo-test--setup-markdown-list))

;;; Open behavior

(carettest-tesmut-test
 test-markdown-list-open-backward-default-1
 "1. Parent one
   - Child <p>one
2. Parent two
"
 "1. Parent one
   - Child one
   - <p>
2. Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward)))

(carettest-tesmut-test
 test-markdown-list-open-default-1
 "1. Parent one
   - Child <p>one
2. Parent two
"
 "1. Parent one
   - <p>
   - Child one
2. Parent two
"
 (lambda ()
   (cpo-markdown-list-open-backward)))


(carettest-tesmut-test
 test-markdown-list-open-numbered-preserves-numbering-when-possible
 "1. Parent one
   7. Chi<p>ld one
2. Parent two
"
 "1. Parent one
   7. Child one
   8. <p>
2. Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward)))

(carettest-tesmut-test
 test-markdown-list-open-numbered-falls-back-to-1-outside-numbered-list
 "
- Parent <p>one
- Parent two
"
 "
- Parent one
   1. <p>
- Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward-down :type 'number)))

(carettest-tesmut-test
 test-markdown-list-open-down-0
 "
- Parent <p>one
  - a
  - b
- Parent two
"
 "
- Parent one
  - <p>
  - a
  - b
- Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward-down :count 0)))

(carettest-tesmut-test
 test-markdown-list-open-down-1
 "
- Parent <p>one
  - a
  - b
- Parent two
"
 "
- Parent one
  - a
  - <p>
  - b
- Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward-down :count 1)))

;;; Numbering repair

(carettest-tesmut-test
 test-markdown-list-fix-numbering_nested-numbered-sublist-only
 "
7. aoeu
5. Parent one
   4. Child one
   <p>6. Child two

   Tail paragraph.
2. Parent two
"
 "
7. aoeu
5. Parent one
   1. Child one
   <p>2. Child two

   Tail paragraph.
2. Parent two
"
 'cpo-markdown-list-fix-numbering)

;;; Tree hierarchy movement

(carettest-tesmo-test
 test-markdown-list-up-to-parent_from-child-body_moves-to-parent-bullet
 "<p1>1. Parent one
   Intro paragraph.
   - Child one
     Child bo<p0>dy.
   - Child two
2. Parent two
"
 'cpo-markdown-list-up-to-parent
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmo-test
 test-markdown-list-down-to-first-child_from-parent-body_skips-to-first-child
 "1. Parent one
   Intro par<p0>agraph.

<p1>   - Child one
     Child body.
   - Child two
2. Parent two
"
 'cpo-markdown-list-down-to-first-child
 :setup (cpo-test--setup-markdown-list))

;;; Tree region expansion

(carettest-tesmut-test
 test-markdown-list-expand-region_basic
 :before
 "1. Parent one
   Intro par<p>agraph.
   - Child one
   - Child two
2. Parent two
"
 :after
 "<p>1. Parent one
   Intro paragraph.
   - Child one
   - Child two
<m>2. Parent two
"
 :function 'cpo-markdown-list-expand-region
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmut-test
 test-markdown-list-expand-region_child-subtree
 :before
 "1. Parent one
   - Child one
     Child <p>body.
   - Child two
2. Parent two
"
 :after
 "1. Parent one
<p>   - Child one
     Child body.
<m>   - Child two
2. Parent two
"
 :function 'cpo-markdown-list-expand-region
 :setup (cpo-test--setup-markdown-list))

;;; Tree transposition

(carettest-tesmut-test
 test-markdown-list-transpose-sibling-forward_preserves-nested-structure
 :before
 "1. Root
<p>   - Child one
     - Grandchild one
   - Child two
     - Grandchild two
"
 :after
 "1. Root
   - Child two
     - Grandchild two
<p>   - Child one
     - Grandchild one
"
 :function 'cpo-markdown-list-transpose-sibling-forward
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmut-test
 test-markdown-list-transpose-sibling-backward_preserves-nested-structure
 :before
 "1. Root
   - Child one
     - Grandchild one
<p>   - Child two
     - Grandchild two
"
 :after
 "1. Root
<p>   - Child two
     - Grandchild two
   - Child one
     - Grandchild one
"
 :function 'cpo-markdown-list-transpose-sibling-backward
 :setup (cpo-test--setup-markdown-list))

;;; Tree shape edits

(carettest-tesmut-test
 test-markdown-list-forward-slurp_basic
 :before
 "1. Root
   - <p>Alpha
   - Beta
   - Gamma
"
 :after
 "1. Root
   - <p>Alpha
     - Beta
   - Gamma
"
 :function 'cpo-markdown-list-forward-slurp
 :setup (cpo-test--setup-markdown-list))

(carettest-tesmut-test
 test-markdown-list-forward-barf_basic
 :before
 "1. Root
   - <p>Alpha
     - Child A
     - Child B
   - Beta
"
 :after
 "1. Root
   - <p>Alpha
     - Child A
   - Child B
   - Beta
"
 :function 'cpo-markdown-list-forward-barf
 :setup (cpo-test--setup-markdown-list))

;;; Open-up behavior across tree levels

(carettest-tesmut-test
 test-markdown-list-open-forward-up-1
 "
- Parent one
  - Child one
    - <p>grandchild
- Parent two
"
 "
- Parent one
  - Child one
    - grandchild
  - <p>
- Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward-up :count 1)))

(carettest-tesmut-test
 test-markdown-list-open-forward-up-2
 "
- Parent one
  - Child one
    - <p>grandchild
- Parent two
"
 "
- Parent one
  - Child one
    - grandchild
- <p>
- Parent two
"
 (lambda ()
   (cpo-markdown-list-open-forward-up :count 2)))

;;; test-cpo-markdown-list.el ends here
