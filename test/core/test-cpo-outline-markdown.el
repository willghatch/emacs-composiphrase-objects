;;; -*- lexical-binding: t; -*-

;;; Tests for cpo-outline and cpo-outline-beginning-end in markdown mode.
;;; These are markdown-mode versions of the org-mode tests in
;;; test-cpo-outline.el and test-cpo-outline-beginning-end.el.
;;; They use `#' style headings instead of org-mode `*' headings.
;;; The setup enables outline-minor-mode with markdown-style heading detection.

(require 'cpo-outline)
(require 'cpo-outline-heading)
(require 'ert)
(require 'carettest-tesmo)
(require 'carettest-tesmut)

;;; Setup helper for markdown-style outline-minor-mode.
;;; This sets up outline-minor-mode with `#' headings, similar to markdown-mode,
;;; but without requiring markdown-mode as a dependency.

(defun cpo-test--setup-markdown-outline ()
  "Set up outline-minor-mode with markdown-style `#' headings."
  (outline-minor-mode 1)
  (setq-local outline-regexp "^#+ ")
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward "#")
                  (current-column)))))

(defun cpo-test--setup-markdown-heading-object ()
  "Set up markdown-style outline and heading-object support."
  (cpo-test--setup-markdown-outline)
  (setq-local cpo-outline-heading--regexp "^#+ "))

(carettest-tesmut-test
 test-outline-open-markdown
 :before
 "# root
## child1
<p>## child2
### grandchild1
### grandchild2
## child3
"
 :after
 "# root
## child1
## child2
### grandchild1
### grandchild2
## <p>
## child3
"
 :function 'cpo-outline-add-heading-below
 :setup (cpo-test--setup-markdown-outline))

;;; -----------------------------------------------------------------------
;;; Tests from test-cpo-outline.el, duplicated for markdown mode
;;; -----------------------------------------------------------------------

;;; Test data with half siblings for outline trees.
;;; In markdown mode, half siblings arise when a heading has children at varying
;;; depths, eg. a `#' heading having a `###' child and a `##' child -- the `###' and
;;; `##' are half siblings since they share a parent but have different levels.


;;; Forward full sibling: deep1 -> deep2 (success, returns 0)
(carettest-tesmo-test
 test-outline-forward-full-sibling_deep1-to-deep2_markdown
 "# root
<p0>### deep1
<p1>### deep2
## mid1
## mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmo-test
 test-outline-forward-full-sibling_deep1-to-deep2_with-intermediate_markdown
 "# root
<p0>### deep1
#### child-of-deep1
<p1>### deep2
## mid1
## mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Forward full sibling from deep2 should fail (mid1 is half sibling, returns 1)
(carettest-tesmo-test
 test-outline-forward-full-sibling_deep2-stalls_markdown
 "# root
### deep1
<p0><p1>### deep2
## mid1
## mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))


(carettest-tesmo-test
 test-outline-forward-full-sibling_deep2-stalls_with-intermediate_markdown
 "# root
### deep1
<p0><p1>### deep2
#### deep2-child
## mid1
## mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling: deep1 -> deep2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep1-to-deep2_markdown
 "# root
<p0>### deep1
<p1>### deep2
## mid1
## mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))


(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep1-to-deep2_with-intermediate_markdown
 "# root
<p0>### deep1
#### deep1-child
<p1>### deep2
## mid1
## mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling: deep2 -> mid1 (crosses half-sibling boundary)
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep2-to-mid1_markdown
 "# root
### deep1
<p0>### deep2
<p1>## mid1
## mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))


(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_deep2-to-mid1_with-intermediate_markdown
 "# root
### deep1
<p0>### deep2
#### deep2-child
<p1>## mid1
## mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling: mid1 -> mid2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_mid1-to-mid2_markdown
 "# root
### deep1
### deep2
<p0>## mid1
<p1>## mid2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward full sibling: mid2 -> mid1 (success, returns 0)
(carettest-tesmo-test
 test-outline-backward-full-sibling_mid2-to-mid1_markdown
 "# root
### deep1
### deep2
<p1>## mid1
<p0>## mid2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling -1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Backward full sibling from mid1 should fail (deep2 is half sibling, returns 1)
(carettest-tesmo-test
 test-outline-backward-full-sibling_mid1-stalls_markdown
 "# root
### deep1
### deep2
<p0><p1>## mid1
## mid2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling -1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling: mid2 -> mid1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling_mid2-to-mid1_markdown
 "# root
### deep1
### deep2
<p1>## mid1
<p0>## mid2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling: mid1 -> deep2 (crosses half-sibling boundary)
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling_mid1-to-deep2_markdown
 "# root
### deep1
<p1>### deep2
<p0>## mid1
## mid2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))


;;; Multi-level half siblings

;;; Forward full sibling: deep1 -> deep2 in multi-level (success, returns 0)
(carettest-tesmo-test
 test-outline-forward-full-sibling-multi-level_deep1-to-deep2_markdown
 "# root
<p0>#### deep1
<p1>#### deep2
### mid1
### mid2
## shallow1
## shallow2
"
 (lambda () (should (= 0 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Forward full sibling from deep2 in multi-level should fail (returns 1)
(carettest-tesmo-test
 test-outline-forward-full-sibling-multi-level_deep2-stalls_markdown
 "# root
#### deep1
<p0><p1>#### deep2
### mid1
### mid2
## shallow1
## shallow2
"
 (lambda () (should (= 1 (cpo-outline-forward-full-sibling 1))))
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling multi-level: deep1 -> deep2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_deep1-to-deep2_markdown
 "# root
<p0>#### deep1
<p1>#### deep2
### mid1
### mid2
## shallow1
## shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling multi-level: deep2 -> mid1
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_deep2-to-mid1_markdown
 "# root
#### deep1
<p0>#### deep2
<p1>### mid1
### mid2
## shallow1
## shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling multi-level: mid1 -> mid2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_mid1-to-mid2_markdown
 "# root
#### deep1
#### deep2
<p0>### mid1
<p1>### mid2
## shallow1
## shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling multi-level: mid2 -> shallow1
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_mid2-to-shallow1_markdown
 "# root
#### deep1
#### deep2
### mid1
<p0>### mid2
<p1>## shallow1
## shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling multi-level: shallow1 -> shallow2
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling-multi-level_shallow1-to-shallow2_markdown
 "# root
#### deep1
#### deep2
### mid1
### mid2
<p0>## shallow1
<p1>## shallow2
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling multi-level: shallow2 -> shallow1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_shallow2-to-shallow1_markdown
 "# root
#### deep1
#### deep2
### mid1
### mid2
<p1>## shallow1
<p0>## shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling multi-level: shallow1 -> mid2
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_shallow1-to-mid2_markdown
 "# root
#### deep1
#### deep2
### mid1
<p1>### mid2
<p0>## shallow1
## shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling multi-level: mid2 -> mid1
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_mid2-to-mid1_markdown
 "# root
#### deep1
#### deep2
<p1>### mid1
<p0>### mid2
## shallow1
## shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling multi-level: mid1 -> deep2
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling-multi-level_mid1-to-deep2_markdown
 "# root
#### deep1
<p1>#### deep2
<p0>### mid1
### mid2
## shallow1
## shallow2
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Forward full sibling with count 3 from deep1: only reaches deep2 (returns 2)
(carettest-tesmo-test
 test-outline-forward-full-sibling_count-returns-remaining_markdown
 "# root
<p0>### deep1
<p1>### deep2
## mid1
## mid2
"
 (lambda () (should (= 2 (cpo-outline-forward-full-sibling 3))))
 :setup (cpo-test--setup-markdown-outline))


;;; Raise tests

(carettest-tesmut-test
 test-outline-raise-basic_markdown
 :before
 "# root
## child1
<p>## child2
### grandchild1
### grandchild2
## child3
"
 :after
 "<p># child2
## grandchild1
## grandchild2
"
 :function 'cpo-outline-raise
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-raise-leaf_markdown
 :before
 "# root
## child1
<p>## child2
## child3
"
 :after
 "<p># child2
"
 :function 'cpo-outline-raise
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-raise-deeply-nested_markdown
 :before
 "# root
## parent
### child1
<p>### child2
#### deep1
### child3
## sibling
"
 :after
 "# root
<p>## child2
### deep1
## sibling
"
 :function 'cpo-outline-raise
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-raise-with-body_markdown
 :before
 "# root
Some root body text.
## child1
Child1 body.
<p>## child2
Child2 body.
### grandchild
Grandchild body.
## child3
"
 :after
 "<p># child2
Child2 body.
## grandchild
Grandchild body.
"
 :function 'cpo-outline-raise
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-raise-preserves-subtree-structure_markdown
 :before
 "# root
<p>## child
### gc1
#### ggc1
### gc2
"
 :after
 "<p># child
## gc1
### ggc1
## gc2
"
 :function 'cpo-outline-raise
 :setup (cpo-test--setup-markdown-outline))

;;; cpo-outline-forward-slurp-heading

;; Slurp the next same-level sibling into the current markdown heading as a child.
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-slurp-heading__basic_markdown
 :before
 "# root
## <p>Alpha
## Beta
## Gamma"
 :after
 "# root
## <p>Alpha
### Beta
## Gamma"
 :function 'cpo-outline-forward-slurp-heading
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-slurp-heading_more-children__basic_markdown
 :before
 "# root
## <p>Alpha
### α
## Beta
### β
## Gamma
### γ
"
 :after
 "# root
## <p>Alpha
### α
### Beta
#### β
## Gamma
### γ
"
 :function 'cpo-outline-forward-slurp-heading
 :setup (cpo-test--setup-markdown-outline))

;; Slurp from the middle sibling makes the next one a child.
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-slurp-heading__mid-sibling_markdown
 :before
 "# root
## Alpha
## <p>Beta
## Gamma
## Delta"
 :after
 "# root
## Alpha
## <p>Beta
### Gamma
## Delta"
 :function 'cpo-outline-forward-slurp-heading
 :setup (cpo-test--setup-markdown-outline))

;;; cpo-outline-forward-barf-heading

;; Barf the last child out as the next sibling.
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-barf-heading__basic_markdown
 :before
 "# root
## <p>Alpha
### Child A
### Child B
## Beta"
 :after
 "# root
## <p>Alpha
### Child A
## Child B
## Beta"
 :function 'cpo-outline-forward-barf-heading
 :setup (cpo-test--setup-markdown-outline))

;; Barf when there are no children does nothing.
(carettest-tesmut-test
 test-outline-modifications-cpo-outline-forward-barf-heading__no-children_markdown
 :before
 "# root
## <p>Alpha
## Beta"
 :after
 "# root
## <p>Alpha
## Beta"
 :function 'cpo-outline-forward-barf-heading
 :setup (cpo-test--setup-markdown-outline))



;;; Forward half-or-full sibling at root level (regression test for the bug
;;; where parent-level was nil for root headings, causing forward movement to
;;; fail silently)

;;; cpo-outline-forward-half-or-full-sibling at root level
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_root-level-heading1-to-heading2_markdown
 "<p0># heading1
<p1># heading2
# heading3
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_root-level-heading2-to-heading3_markdown
 "# heading1
<p0># heading2
<p1># heading3
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Backward half-or-full sibling at root level
(carettest-tesmo-test
 test-outline-backward-half-or-full-sibling_root-level-heading3-to-heading2_markdown
 "# heading1
<p1># heading2
<p0># heading3
"
 'cpo-outline-backward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))

;;; Transpose tests
;;; Transpose forward at root level: the core bug fix
(carettest-tesmut-test
 test-outline-transpose-sibling-forward_root-level-basic_markdown
 :before
 "<p># heading1
## child1
# heading2
## child2
# heading3
"
 :after
 "# heading2
## child2
<p># heading1
## child1
# heading3
"
 :function 'cpo-outline-transpose-sibling-forward
 :setup (cpo-test--setup-markdown-outline))

;;; Transpose forward at root level: last heading doesn't move
(carettest-tesmut-test
 test-outline-transpose-sibling-forward_root-level-last-stays_markdown
 :before
 "# heading1
# <p>heading2
"
 :after
 "# heading1
# <p>heading2
"
 :function 'cpo-outline-transpose-sibling-forward
 :setup (cpo-test--setup-markdown-outline))

;;; Transpose backward at root level
(carettest-tesmut-test
 test-outline-transpose-sibling-backward_root-level-basic_markdown
 :before
 "# heading1
## child1
<p># heading2
## child2
# heading3
"
 :after
 "<p># heading2
## child2
# heading1
## child1
# heading3
"
 :function 'cpo-outline-transpose-sibling-backward
 :setup (cpo-test--setup-markdown-outline))

;;; Transpose forward: non-root headings continue to work
(carettest-tesmut-test
 test-outline-transpose-sibling-forward_non-root_markdown
 :before
 "# root
<p>## child1
### grandchild1
## child2
"
 :after
 "# root
## child2
<p>## child1
### grandchild1
"
 :function 'cpo-outline-transpose-sibling-forward
 :setup (cpo-test--setup-markdown-outline))

;;; Transpose backward: non-root headings continue to work
(carettest-tesmut-test
 test-outline-transpose-sibling-backward_non-root_markdown
 :before
 "# root
## child1
<p>## child2
### grandchild2
"
 :after
 "# root
<p>## child2
### grandchild2
## child1
"
 :function 'cpo-outline-transpose-sibling-backward
 :setup (cpo-test--setup-markdown-outline))

;;; Forward half-or-full sibling at root level with subtrees (ensures bounds
;;; computation is correct - point should land at the heading, not inside body)
(carettest-tesmo-test
 test-outline-forward-half-or-full-sibling_root-level-with-body_markdown
 "<p0># heading1
Some body text.
## child under heading1
<p1># heading2
Some body text under heading2.
"
 'cpo-outline-forward-half-or-full-sibling
 :setup (cpo-test--setup-markdown-outline))


;;; Expand-region tests (these are the main failing cases for markdown mode)

;;; Expanding region from inside a heading to the full heading subtree.
;;; The mark lands at the start of the next sibling heading (the end bound
;;; of the current subtree is the start of the next heading at the same level).
(carettest-tesmut-test
 test-outline-expand-region_basic_markdown
 :before
 "# heading1
<p>Some body text.
## child1
## child2
# heading2
"
 :after
 "<p># heading1
Some body text.
## child1
## child2
<m># heading2
"
 :function 'cpo-outline-expand-region
 :setup (cpo-test--setup-markdown-outline))

;;; Expanding region from a child heading: first expansion selects the child's subtree.
;;; Point starts at `## child1' heading; one call expands to the child's subtree.
(carettest-tesmut-test
 test-outline-expand-region_child-subtree_markdown
 :before
 "# parent
<p>## child1
Some child body.
## child2
# sibling
"
 :after
 "# parent
<p>## child1
Some child body.
<m>## child2
# sibling
"
 :function 'cpo-outline-expand-region
 :setup (cpo-test--setup-markdown-outline))

;;; Expanding region from inside child body to the parent subtree.
;;; Starting inside body text under `## child1' triggers expansion to
;;; the parent subtree.
(carettest-tesmut-test
 test-outline-expand-region_child-to-parent_markdown
 :before
 "# parent
## child1
Some <p>child body.
## child2
# sibling
"
 :after
 "# parent
<p>## child1
Some child body.
<m>## child2
# sibling
"
 :function 'cpo-outline-expand-region
 :setup (cpo-test--setup-markdown-outline))

;;; Transposition works when expand-region works
(carettest-tesmut-test
 test-outline-transpose-sibling-forward_with-body_markdown
 :before
 "# root
<p>## child1
Child1 body.
## child2
Child2 body.
"
 :after
 "# root
## child2
Child2 body.
<p>## child1
Child1 body.
"
 :function 'cpo-outline-transpose-sibling-forward
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmut-test
 test-outline-transpose-sibling-backward_with-body_markdown
 :before
 "# root
## child1
Child1 body.
<p>## child2
Child2 body.
"
 :after
 "# root
<p>## child2
Child2 body.
## child1
Child1 body.
"
 :function 'cpo-outline-transpose-sibling-backward
 :setup (cpo-test--setup-markdown-outline))


;;; -----------------------------------------------------------------------
;;; Tests from test-cpo-outline-beginning-end.el, duplicated for markdown mode
;;; -----------------------------------------------------------------------

;;; ---- backward-beginning tests ----

;;; From middle of heading line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-middle-of-heading_markdown
 "# Previous Sibling
Previous body.
<p1># Heading <p0>One
Some body text.
## Child heading
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From end of heading line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-end-of-heading_markdown
 "# Previous Sibling
Previous body.
<p1># Heading One<p0>
Some body text.
## Child heading
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From body text under heading -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-body-text_markdown
 "# Previous Sibling
Previous body.
<p1># Heading One
Some <p0>body text.
## Child heading
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From body text on second line -> beginning of current heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-body-second-line_markdown
 "# Previous Sibling
Previous body.
<p1># Heading One
Some body text.
More <p0>body text.
## Child heading
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From beginning of heading -> beginning of previous sibling heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-heading-beginning-to-previous_markdown
 "<p1># Heading One
Some body text.
## child heading
<p0># sibling heading
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From child heading with no previous sibling -> stays (respects tree)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_respect-tree_no-previous-sibling_markdown
 "# Heading One
Some body text.
<p1><p0>## Child heading
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From beginning of first heading -> stays (no move)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-first-heading-stays_markdown
 "<p0><p1># Heading One
Some body text.
## Child heading
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From body of child heading -> beginning of child heading
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-body_markdown
 "# Heading One
Some body text.
<p1>## Child heading
Child <p0>body.
### Grandchild
## Next Child Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From beginning of second child sibling -> beginning of first child sibling
(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-sibling-to-previous-child-sibling_markdown
 "# Heading One
Some body text.
<p1>## First Child
First child body.
<p0>## Second Child
Second child body.
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))


(carettest-tesmo-test
 test-outline-tree-backward-beginning_from-child-sibling-to-previous-child-sibling_with-child_markdown
 "# Heading One
Some body text.
<p1>## First Child
First child body.
### first child child
<p0>## Second Child
Second child body.
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; Backward from child heading should NOT go to parent (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_does-not-go-to-parent_markdown
 "# Heading One
Some body text.
<p1><p0>## Only Child
Child body.
# Next Sibling
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; Backward from heading should NOT go to uncle (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-backward-beginning_does-not-go-to-uncle_markdown
 "# Uncle Heading
Uncle body.
# Parent Heading
Parent body.
<p1><p0>## Only Child
Child body.
"
 'cpo-outline-tree-backward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; ---- forward-beginning tests ----

;;; From beginning of heading -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-heading-to-next-sibling_markdown
 "<p0># Heading One
Some body text.
## Child heading
Child body.
<p1># Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From middle of heading -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-middle-of-heading_markdown
 "# Heading <p0>One
Some body text.
## Child heading
Child body.
<p1># Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From body text -> beginning of next sibling heading
(carettest-tesmo-test
 test-outline-tree-forward-beginning_from-body_markdown
 "# Heading One
Some <p0>body text.
## Child heading
Child body.
<p1># Heading Two
More body text.
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; From heading with no next sibling -> stays (no move)
(carettest-tesmo-test
 test-outline-tree-forward-beginning_no-next-sibling-stays_markdown
 "# Heading One
Some body text.
<p0><p1>## Only Child
Child body.
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; Forward from child heading to next child sibling
(carettest-tesmo-test
 test-outline-tree-forward-beginning_child-to-next-child-sibling_markdown
 "# Heading One
Some body text.
<p0>## First Child
Child body.
<p1>## Second Child
Second child body.
# Next Sibling
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; Forward does NOT go to parent or uncle (tree-respecting)
(carettest-tesmo-test
 test-outline-tree-forward-beginning_does-not-go-to-uncle_markdown
 "# Parent Heading
Parent body.
<p0><p1>## Last Child
Child body.
# Uncle Heading
Uncle body.
"
 'cpo-outline-tree-forward-beginning
 :setup (cpo-test--setup-markdown-outline))

;;; ---- forward-end tests ----

;;; From beginning of heading -> end of that heading's full tree node
;;; (tree bounds include all descendants)
(carettest-tesmo-test
 test-outline-tree-forward-end_from-heading-beginning_markdown
 "# Previous Sibling
Previous body.
<p0># Heading One
Some body text.
## Child heading
Child body.
<p1># Next Sibling
"
 'cpo-outline-tree-forward-end
 :setup (cpo-test--setup-markdown-outline))

;;; From middle of body -> end of tree node
(carettest-tesmo-test
 test-outline-tree-forward-end_from-body-middle_markdown
 "# Heading One
Some body text.
## Child heading
Child <p0>body.
<p1>## Next Child Sibling
More body.
"
 'cpo-outline-tree-forward-end
 :setup (cpo-test--setup-markdown-outline))

;;; From heading beginning to end when it's the last node (end of buffer)
(carettest-tesmo-test
 test-outline-tree-forward-end_last-node-to-eob_markdown
 "# Heading One
Some body text.
<p0>## Child heading
Child body.
<p1>"
 'cpo-outline-tree-forward-end
 :setup (cpo-test--setup-markdown-outline))

;;; ---- backward-end tests ----

(carettest-tesmo-test
 test-outline-tree-backward-end_from-sibling-to-prev-end_no-more_markdown
 "# Heading One
Some body text.
## Child One
Child one body.
<p1><p0>## Child Two
Child two body.
"
 'cpo-outline-tree-backward-end
 :setup (cpo-test--setup-markdown-outline))

(carettest-tesmo-test
 test-outline-tree-backward-end_from-sibling-to-prev-end_yes-more_markdown
 "# Heading One
Some body text.
## Child Zero
<p1>## Child One
Child one body.
<p0>## Child Two
Child two body.
"
 'cpo-outline-tree-backward-end
 :setup (cpo-test--setup-markdown-outline))

;;; Backward end from heading with no previous sibling -> stays
(carettest-tesmo-test
 test-outline-tree-backward-end_no-previous-sibling-stays_markdown
 "# Heading One
Some body text.
<p0><p1>## Only Child
Child body.
"
 'cpo-outline-tree-backward-end
 :setup (cpo-test--setup-markdown-outline))

;;; ---- backward-beginning with count ----

;;; Backward beginning twice from child body: first to child heading,
;;; then to previous child sibling
(carettest-tesmo-test
 test-outline-tree-backward-beginning_count-2-from-body_markdown
 "# Heading One
Some body text.
<p1>## First Child
First child body.
## Second Child
Second <p0>child body.
"
 (lambda () (cpo-outline-tree-backward-beginning 2))
 :setup (cpo-test--setup-markdown-outline))

;;; ---- forward-beginning with count ----

;;; Forward beginning twice through siblings
(carettest-tesmo-test
 test-outline-tree-forward-beginning_count-2_markdown
 "<p0># Heading One
Some body text.
## Child heading
Child body.
# Heading Two
More body text.
<p1># Heading Three
Even more text.
"
 (lambda () (cpo-outline-tree-forward-beginning 2))
 :setup (cpo-test--setup-markdown-outline))

;;; ---- Roundtrip: forward then backward stays put ----

(carettest-tesmo-test
 test-outline-tree-beginning-roundtrip_forward-then-backward_markdown
 "# Heading One
Some body text.
<p0><p1>## First Child
Child body.
## Second Child
More body.
"
 (lambda ()
   (cpo-outline-tree-forward-beginning)
   (cpo-outline-tree-backward-beginning))
 :setup (cpo-test--setup-markdown-outline))

;;; -----------------------------------------------------------------------
;;; Heading-object tests from test-cpo-outline-heading.el, duplicated for
;;; markdown mode
;;; -----------------------------------------------------------------------

;;; Motion tests -- forward-beginning

(carettest-tesmo-test
 test-heading-forward-beginning-basic_one-to-two_markdown
 "# Heading One
Some body text here.
More body text.
<p0># Heading Two
<p1># Heading Three
## Sub Heading
Sub body text.
# Heading Four
"
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-beginning-basic_two-to-sub_markdown
 "# Heading One
Some body text here.
More body text.
<p0># Heading Two
Body of heading two.
<p1>## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-beginning-basic_sub-to-three_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p0>## Sub Heading
Sub body text.
<p1># Heading Three
"
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-beginning-with-count_markdown
 "<p0># Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>## Sub Heading
Sub body text.
# Heading Three
"
 (lambda () (cpo-outline-heading-forward-beginning 2))
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-beginning-at-last-heading_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
<p0><p1># Heading Three
"
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-beginning-from-body_markdown
 "# Heading One
Some body<p0> text here.
More body text.
<p1># Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

;;; Motion tests -- backward-beginning

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-end-to-three_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
<p1># Heading Three
<p0>"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-middle_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>## Sub <p0>Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-middle-2_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>## Sub Heading<p0>
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-basic_three-to-sub_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>## Sub Heading
Sub body text.
<p0># Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-basic_sub-to-two_markdown
 "# Heading One
Some body text here.
More body text.
<p1># Heading Two
Body of heading two.
<p0>## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-basic_two-to-one_markdown
 "<p1># Heading One
Some body text here.
More body text.
<p0># Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-with-count_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>## Sub Heading
Sub body text.
# Heading Three
<p0>"
 (lambda () (cpo-outline-heading-backward-beginning 2))
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-at-first-heading_markdown
 "<p0><p1># Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-beginning-from-body_markdown
 "<p1># Heading One
Some body<p0> text here.
More body text.
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

;;; Motion tests -- forward-end

(carettest-tesmo-test
 test-heading-forward-end-basic_one-to-two_markdown
 "<p0># Heading One<p1>
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-basic_two-to-sub_markdown
 "# Heading One
Some body text here.
More body text.
<p0># Heading Two<p1>
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-basic_middle_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub<p0> Heading<p1>
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-basic_sub-to-three_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading<p0>
Sub body text.
# Heading Three<p1>
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-with-count_markdown
 "<p0># Heading One
Some body text here.
More body text.
# Heading Two<p1>
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 (lambda () (cpo-outline-heading-forward-end 2))
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-at-last-heading_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
<p0><p1># Heading Three
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-forward-end-from-body_markdown
 "# Heading One
Some body<p0> text here.
More body text.
# Heading Two<p1>
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

;;; Motion tests -- backward-end

(carettest-tesmo-test
 test-heading-backward-end-basic_from-end-to-three_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three<p1>
<p0>"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-basic_three-to-sub_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading<p1>
Sub body text.
<p0># Heading Three
"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-basic_sub-to-two_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two<p1>
Body of heading two.
<p0>## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-basic_two-to-one_markdown
 "# Heading One<p1>
Some body text here.
More body text.
<p0># Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-with-count_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading<p1>
Sub body text.
# Heading Three
<p0>"
 (lambda () (cpo-outline-heading-backward-end 2))
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-at-first-heading_markdown
 "<p0><p1># Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-backward-end-from-body_markdown
 "# Heading One<p1>
Some body<p0> text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

;;; Motion tests -- no headings

(carettest-tesmo-test
 test-heading-no-headings-forward-beginning_markdown
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-forward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-no-headings-backward-beginning_markdown
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-backward-beginning
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-no-headings-forward-end_markdown
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-forward-end
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-no-headings-backward-end_markdown
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-backward-end
 :setup (cpo-test--setup-markdown-heading-object))

;;; Selection tests
;; select puts point at line beginning, mark at end of heading text

(carettest-tesmo-test
 test-heading-select_basic_markdown
 "<p0><p1># Heading One<m1>
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select_nested_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>##<p0> Sub Heading<m1>
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select_from-body_markdown
 "<p1># Heading One<m1>
Some body<p0> text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select
 :setup (cpo-test--setup-markdown-heading-object))

;;; Inner selection tests
;; select-inner puts point after the hash+space, mark at end of heading text

(carettest-tesmo-test
 test-heading-select-inner_basic_markdown
 "<p0># <p1>Heading One<m1>
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-inner
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-inner_nested_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
##<p0> <p1>Sub Heading<m1>
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-inner
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-inner_from-body_markdown
 "# <p1>Heading One<m1>
Some body<p0> text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-inner
 :setup (cpo-test--setup-markdown-heading-object))

;;; Prefix selection tests
;; select-prefix puts point at line beginning, mark after hash+space

(carettest-tesmo-test
 test-heading-select-prefix_basic_markdown
 "<p0><p1># <m1>Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-prefix
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-prefix_nested_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p1>##<p0> <m1>Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-prefix
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-prefix_deeply-nested_markdown
 "# Top Level
## Second Level
<p1>### <m1>Third <p0>Level
Some deep body.
## Another Second
# Another Top
"
 'cpo-outline-heading-select-prefix
 :setup (cpo-test--setup-markdown-heading-object))

;;; Body selection tests
;; select-body puts point at start of body (after heading newline),
;; mark at start of next heading (or end of buffer)

(carettest-tesmo-test
 test-heading-select-body_basic_markdown
 "# He<p0>ading One
<p1>Some body text here.
More body text.
<m1># Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-body
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-body_nested_markdown
 "# Heading One
Some body text here.
More body text.
<p0># Heading Two
<p1>Body of heading two.
<m1>## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-body
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-body_sub-heading_markdown
 "# Heading One
Some body text here.
More body text.
# Heading Two
Body of heading two.
<p0>## Sub Heading
<p1>Sub body text.
<m1># Heading Three
"
 'cpo-outline-heading-select-body
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-body_from-body_markdown
 "# Heading One
<p1>Some body<p0> text here.
More body text.
<m1># Heading Two
Body of heading two.
## Sub Heading
Sub body text.
# Heading Three
"
 'cpo-outline-heading-select-body
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmo-test
 test-heading-select-body_last-heading_markdown
 "<p0># Only Heading
<p1>Some final text.<m1>"
 'cpo-outline-heading-select-body
 :setup (cpo-test--setup-markdown-heading-object))

;;; Edge case tests that verify return values (kept as ert-deftests)

(ert-deftest test-heading-no-headings-bounds-markdown ()
  "All bounds functions return nil when there are no headings."
  (with-temp-buffer
    (cpo-test--setup-markdown-heading-object)
    (insert "Just some plain text.\nNo headings here.")
    (goto-char (point-min))
    (should-not (cpo-outline-heading--bounds))
    (should-not (cpo-outline-heading--inner-bounds))
    (should-not (cpo-outline-heading--prefix-bounds))
    (should-not (cpo-outline-heading--body-bounds))))

(ert-deftest test-heading-body-bounds-no-body-markdown ()
  "Body bounds returns nil when heading has no body text."
  (with-temp-buffer
    (cpo-test--setup-markdown-heading-object)
    (insert "# First\n# Second\n# Third\n")
    (goto-char (point-min))
    (should-not (cpo-outline-heading--body-bounds))))

(ert-deftest test-heading-on-heading-p-markdown ()
  "Test heading detection predicate."
  (with-temp-buffer
    (cpo-test--setup-markdown-heading-object)
    (insert "# Heading One\nSome body text here.\nMore body text.\n# Heading Two\n")
    (goto-char (point-min))
    (should (cpo-outline-heading--on-heading-p))
    (forward-line 1)
    (should-not (cpo-outline-heading--on-heading-p))))

;;; Promote/demote tests

(carettest-tesmut-test
 test-heading-demote_on-heading_markdown
 "<p># Heading One
Some body text here.
"
 "<p>## Heading One
Some body text here.
"
 'cpo-outline-heading-demote
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmut-test
 test-heading-demote_from-body_markdown
 "# Heading One
Some body <p>text here.
More body text.
# Heading Two
"
 "## Heading One
Some body <p>text here.
More body text.
# Heading Two
"
 'cpo-outline-heading-demote
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmut-test
 test-heading-demote_nested_markdown
 "# Heading One
<p>## Sub Heading
Sub body text.
# Heading Two
"
 "# Heading One
<p>### Sub Heading
Sub body text.
# Heading Two
"
 'cpo-outline-heading-demote
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmut-test
 test-heading-promote_on-heading_markdown
 "<p>## Heading One
Some body text here.
"
 "<p># Heading One
Some body text here.
"
 'cpo-outline-heading-promote
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmut-test
 test-heading-promote_from-body_markdown
 "## Heading One
Some body <p>text here.
More body text.
# Heading Two
"
 "# Heading One
Some body <p>text here.
More body text.
# Heading Two
"
 'cpo-outline-heading-promote
 :setup (cpo-test--setup-markdown-heading-object))

(carettest-tesmut-test
 test-heading-promote_nested_markdown
 "# Heading One
<p>### Sub Heading
Sub body text.
# Heading Two
"
 "# Heading One
<p>## Sub Heading
Sub body text.
# Heading Two
"
 'cpo-outline-heading-promote
 :setup (cpo-test--setup-markdown-heading-object))

;;; Point preservation tests for promote/demote

(ert-deftest test-heading-promote-preserves-point-in-body-markdown ()
  "Promote should keep point at the same text when point is in body text."
  (with-temp-buffer
    (cpo-test--setup-markdown-heading-object)
    (insert "## Heading\nSome body text here.\n# Next\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (let ((original-column (current-column))
          (original-line (line-number-at-pos)))
      (cpo-outline-heading-promote)
      (should (= (current-column) original-column))
      (should (= (line-number-at-pos) original-line)))))

(ert-deftest test-heading-demote-preserves-point-in-body-markdown ()
  "Demote should keep point at the same text when point is in body text."
  (with-temp-buffer
    (cpo-test--setup-markdown-heading-object)
    (insert "# Heading\nSome body text here.\n# Next\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (let ((original-column (current-column))
          (original-line (line-number-at-pos)))
      (cpo-outline-heading-demote)
      (should (= (current-column) original-column))
      (should (= (line-number-at-pos) original-line)))))

;;; test-cpo-outline-markdown.el ends here
