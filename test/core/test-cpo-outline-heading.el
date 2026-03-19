;;; test-cpo-outline-heading.el --- Tests for cpo-outline-heading -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-outline-heading)

;;; Motion tests -- forward-beginning

(carettest-tesmo-test
 test-heading-forward-beginning-basic_one-to-two
 "<p0>* Heading One
Some body text here.
More body text.
<p1>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-basic_two-to-sub
 "* Heading One
Some body text here.
More body text.
<p0>* Heading Two
Body of heading two.
<p1>** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-basic_sub-to-three
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p0>** Sub Heading
Sub body text.
<p1>* Heading Three
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-with-count
 "<p0>* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub Heading
Sub body text.
* Heading Three
"
 (lambda () (cpo-outline-heading-forward-beginning 2))
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-at-last-heading
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
<p0><p1>* Heading Three
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-from-body
 "* Heading One
Some body<p0> text here.
More body text.
<p1>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

;;; Motion tests -- backward-beginning

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-end-to-three
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
<p1>* Heading Three
<p0>"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-middle
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub <p0>Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-basic_from-middle-2
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub Heading<p0>
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-basic_three-to-sub
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub Heading
Sub body text.
<p0>* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-basic_sub-to-two
 "* Heading One
Some body text here.
More body text.
<p1>* Heading Two
Body of heading two.
<p0>** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-basic_two-to-one
 "<p1>* Heading One
Some body text here.
More body text.
<p0>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-with-count
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub Heading
Sub body text.
* Heading Three
<p0>"
 (lambda () (cpo-outline-heading-backward-beginning 2))
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-at-first-heading
 "<p0><p1>* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-beginning-from-body
 "<p1>* Heading One
Some body<p0> text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

;;; Motion tests -- nested headings

(carettest-tesmo-test
 test-heading-forward-beginning-nested_top-to-second
 "<p0>* Top Level
<p1>** Second Level
*** Third Level
Some deep body.
** Another Second
* Another Top
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-nested_second-to-third
 "* Top Level
<p0>** Second Level
<p1>*** Third Level
Some deep body.
** Another Second
* Another Top
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-nested_third-to-another-second
 "* Top Level
** Second Level
<p0>*** Third Level
Some deep body.
<p1>** Another Second
* Another Top
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-beginning-nested_another-second-to-another-top
 "* Top Level
** Second Level
*** Third Level
Some deep body.
<p0>** Another Second
<p1>* Another Top
"
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

;; Test forward-backward round-trip through deeply nested headings.
;; Start at top, go forward twice, backward twice -- should return to start.
(carettest-tesmo-test
 test-heading-deeply-nested-motion_round-trip
 "<p0><p1>* Top Level
** Second Level
*** Third Level
Some deep body.
** Another Second
* Another Top
"
 (lambda ()
   (cpo-outline-heading-forward-beginning)
   (cpo-outline-heading-forward-beginning)
   (cpo-outline-heading-backward-beginning)
   (cpo-outline-heading-backward-beginning))
 :transient-mark-mode t)

;;; Motion tests -- negative count

(carettest-tesmo-test
 test-heading-negative-count
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>** Sub Heading
Sub body text.
* Heading Three
<p0>"
 (lambda () (cpo-outline-heading-forward-beginning -2))
 :transient-mark-mode t)

;;; Motion tests -- no headings

(carettest-tesmo-test
 test-heading-no-headings-forward-beginning
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-forward-beginning
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-no-headings-backward-beginning
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-backward-beginning
 :transient-mark-mode t)

;;; Motion tests -- forward-end

(carettest-tesmo-test
 test-heading-forward-end-basic_one-to-two
 "<p0>* Heading One<p1>
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-basic_two-to-sub
 "* Heading One
Some body text here.
More body text.
<p0>* Heading Two<p1>
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-basic_middle
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub<p0> Heading<p1>
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-basic_sub-to-three
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading<p0>
Sub body text.
* Heading Three<p1>
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-with-count
 "<p0>* Heading One
Some body text here.
More body text.
* Heading Two<p1>
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 (lambda () (cpo-outline-heading-forward-end 2))
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-at-last-heading
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
<p0><p1>* Heading Three
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-forward-end-from-body
 "* Heading One
Some body<p0> text here.
More body text.
* Heading Two<p1>
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

;;; Motion tests -- backward-end

(carettest-tesmo-test
 test-heading-backward-end-basic_from-end-to-three
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three<p1>
<p0>"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-basic_three-to-sub
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading<p1>
Sub body text.
<p0>* Heading Three
"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-basic_sub-to-two
 "* Heading One
Some body text here.
More body text.
* Heading Two<p1>
Body of heading two.
<p0>** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-basic_two-to-one
 "* Heading One<p1>
Some body text here.
More body text.
<p0>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-with-count
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading<p1>
Sub body text.
* Heading Three
<p0>"
 (lambda () (cpo-outline-heading-backward-end 2))
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-at-first-heading
 "<p0><p1>* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-backward-end-from-body
 "* Heading One<p1>
Some body<p0> text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

;;; Motion tests -- no headings (end)

(carettest-tesmo-test
 test-heading-no-headings-forward-end
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-forward-end
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-no-headings-backward-end
 "<p0><p1>Just some plain text.
No headings here."
 'cpo-outline-heading-backward-end
 :transient-mark-mode t)

;;; Selection tests
;; select puts point at line beginning, mark at end of heading text

(carettest-tesmo-test
 test-heading-select_basic
 "<p0><p1>* Heading One<m1>
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select_nested
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>**<p0> Sub Heading<m1>
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select_from-body
 "<p1>* Heading One<m1>
Some body<p0> text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select
 :transient-mark-mode t)

;;; Inner selection tests
;; select-inner puts point after the asterisks+space, mark at end of heading text

(carettest-tesmo-test
 test-heading-select-inner_basic
 "<p0>* <p1>Heading One<m1>
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-inner
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-inner_nested
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
**<p0> <p1>Sub Heading<m1>
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-inner
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-inner_from-body
 "* <p1>Heading One<m1>
Some body<p0> text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-inner
 :transient-mark-mode t)

;;; Prefix selection tests
;; select-prefix puts point at line beginning, mark after asterisks+space

(carettest-tesmo-test
 test-heading-select-prefix_basic
 "<p0><p1>* <m1>Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-prefix
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-prefix_nested
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p1>**<p0> <m1>Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-prefix
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-prefix_deeply-nested
 "* Top Level
** Second Level
<p1>*** <m1>Third <p0>Level
Some deep body.
** Another Second
* Another Top
"
 'cpo-outline-heading-select-prefix
 :transient-mark-mode t)

;;; Body selection tests
;; select-body puts point at start of body (after heading newline),
;; mark at start of next heading (or end of buffer)

(carettest-tesmo-test
 test-heading-select-body_basic
 "* He<p0>ading One
<p1>Some body text here.
More body text.
<m1>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-body
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-body_nested
 "* Heading One
Some body text here.
More body text.
<p0>* Heading Two
<p1>Body of heading two.
<m1>** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-body
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-body_sub-heading
 "* Heading One
Some body text here.
More body text.
* Heading Two
Body of heading two.
<p0>** Sub Heading
<p1>Sub body text.
<m1>* Heading Three
"
 'cpo-outline-heading-select-body
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-body_from-body
 "* Heading One
<p1>Some body<p0> text here.
More body text.
<m1>* Heading Two
Body of heading two.
** Sub Heading
Sub body text.
* Heading Three
"
 'cpo-outline-heading-select-body
 :transient-mark-mode t)

(carettest-tesmo-test
 test-heading-select-body_last-heading
 "<p0>* Only Heading
<p1>Some final text.<m1>"
 'cpo-outline-heading-select-body
 :transient-mark-mode t)

;;; Edge case tests that verify return values (kept as ert-deftests)

(ert-deftest test-heading-no-headings-bounds ()
  "All bounds functions return nil when there are no headings."
  (with-temp-buffer
    (insert "Just some plain text.\nNo headings here.")
    (goto-char (point-min))
    (should-not (cpo-outline-heading--bounds))
    (should-not (cpo-outline-heading--inner-bounds))
    (should-not (cpo-outline-heading--prefix-bounds))
    (should-not (cpo-outline-heading--body-bounds))))

(ert-deftest test-heading-body-bounds-no-body ()
  "Body bounds returns nil when heading has no body text."
  (with-temp-buffer
    (insert "* First\n* Second\n* Third\n")
    (goto-char (point-min))
    (should-not (cpo-outline-heading--body-bounds))))

(ert-deftest test-heading-on-heading-p ()
  "Test heading detection predicate."
  (with-temp-buffer
    (insert "* Heading One\nSome body text here.\nMore body text.\n* Heading Two\n")
    (goto-char (point-min))
    (should (cpo-outline-heading--on-heading-p))
    (forward-line 1)
    (should-not (cpo-outline-heading--on-heading-p))))

;;; Promote/demote tests

(carettest-tesmut-test
 test-heading-demote_on-heading
 "<p>* Heading One
Some body text here.
"
 "<p>** Heading One
Some body text here.
"
 'cpo-outline-heading-demote
 :setup (org-mode))

(carettest-tesmut-test
 test-heading-demote_from-body
 "* Heading One
Some body <p>text here.
More body text.
* Heading Two
"
 "** Heading One
Some body <p>text here.
More body text.
* Heading Two
"
 'cpo-outline-heading-demote
 :setup (org-mode))

(carettest-tesmut-test
 test-heading-demote_nested
 "* Heading One
<p>** Sub Heading
Sub body text.
* Heading Two
"
 "* Heading One
<p>*** Sub Heading
Sub body text.
* Heading Two
"
 'cpo-outline-heading-demote
 :setup (org-mode))

(carettest-tesmut-test
 test-heading-promote_on-heading
 "<p>** Heading One
Some body text here.
"
 "<p>* Heading One
Some body text here.
"
 'cpo-outline-heading-promote
 :setup (org-mode))

(carettest-tesmut-test
 test-heading-promote_from-body
 "** Heading One
Some body <p>text here.
More body text.
* Heading Two
"
 "* Heading One
Some body <p>text here.
More body text.
* Heading Two
"
 'cpo-outline-heading-promote
 :setup (org-mode))

(carettest-tesmut-test
 test-heading-promote_nested
 "* Heading One
<p>*** Sub Heading
Sub body text.
* Heading Two
"
 "* Heading One
<p>** Sub Heading
Sub body text.
* Heading Two
"
 'cpo-outline-heading-promote
 :setup (org-mode))

;;; Point preservation tests for promote/demote

(ert-deftest test-heading-promote-preserves-point-in-body ()
  "Promote should keep point at the same text when point is in body text."
  (with-temp-buffer
    (org-mode)
    (insert "** Heading\nSome body text here.\n* Next\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (let ((original-column (current-column))
          (original-line (line-number-at-pos)))
      (cpo-outline-heading-promote)
      ;; Point should be on the same line and column (the body text does not
      ;; move relative to the start of its own line)
      (should (= (current-column) original-column))
      (should (= (line-number-at-pos) original-line)))))

(ert-deftest test-heading-demote-preserves-point-in-body ()
  "Demote should keep point at the same text when point is in body text."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nSome body text here.\n* Next\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (let ((original-column (current-column))
          (original-line (line-number-at-pos)))
      (cpo-outline-heading-demote)
      ;; Point should be on the same line and column
      (should (= (current-column) original-column))
      (should (= (line-number-at-pos) original-line)))))

;;; test-cpo-outline-heading.el ends here
