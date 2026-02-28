;;; test-cpo-comma-list.el --- Tests for cpo-comma-list -*- lexical-binding: t; -*-

(require 'ert)
(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-comma-list)

;;; Test: finding list bounds
;; These test internal APIs that return cons cells, so they remain as plain
;; ert-deftest with a simple with-temp-buffer setup.

(ert-deftest cpo-comma-list-test-find-list-bounds-parens ()
  "Finding list bounds within parentheses."
  (with-temp-buffer
    (insert "(a, b, c)")
    (goto-char (point-min))
    (search-forward "b")
    (goto-char (match-beginning 0))
    (let ((bounds (cpo-comma-list--find-list-bounds)))
      (should bounds)
      (should (equal "a, b, c"
                     (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

(ert-deftest cpo-comma-list-test-find-list-bounds-brackets ()
  "Finding list bounds within brackets."
  (with-temp-buffer
    (insert "[x, y, z]")
    (goto-char (point-min))
    (search-forward "y")
    (goto-char (match-beginning 0))
    (let ((bounds (cpo-comma-list--find-list-bounds)))
      (should bounds)
      (should (equal "x, y, z"
                     (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

(ert-deftest cpo-comma-list-test-find-list-bounds-braces ()
  "Finding list bounds within braces."
  (with-temp-buffer
    (insert "{1, 2, 3}")
    (goto-char (point-min))
    (search-forward "2")
    (goto-char (match-beginning 0))
    (let ((bounds (cpo-comma-list--find-list-bounds)))
      (should bounds)
      (should (equal "1, 2, 3"
                     (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

;;; Test: parsing elements
;; These test internal parsing APIs that return element lists.

(ert-deftest cpo-comma-list-test-parse-simple ()
  "Parse a simple comma-separated list."
  (with-temp-buffer
    (insert "(a, b, c)")
    (goto-char (point-min))
    (search-forward "a")
    (goto-char (match-beginning 0))
    (let* ((bounds (cpo-comma-list--find-list-bounds))
           (elements (cpo-comma-list--parse-elements (car bounds) (cdr bounds))))
      (should (= 3 (length elements)))
      (should (equal "a" (buffer-substring-no-properties
                          (car (cpo-comma-list--trim-bounds (caar elements) (cdar elements)))
                          (cdr (cpo-comma-list--trim-bounds (caar elements) (cdar elements)))))))))

(ert-deftest cpo-comma-list-test-parse-nested-parens ()
  "Parse a list with nested parentheses."
  (with-temp-buffer
    (insert "(a, (b, c), d)")
    (goto-char (point-min))
    (search-forward "a")
    (goto-char (match-beginning 0))
    (let* ((bounds (cpo-comma-list--find-list-bounds))
           (elements (cpo-comma-list--parse-elements (car bounds) (cdr bounds))))
      (should (= 3 (length elements)))
      (let ((second-trimmed (cpo-comma-list--trim-bounds (car (nth 1 elements))
                                                         (cdr (nth 1 elements)))))
        (should (equal "(b, c)" (buffer-substring-no-properties
                                 (car second-trimmed) (cdr second-trimmed))))))))

(ert-deftest cpo-comma-list-test-parse-nested-mixed-delimiters ()
  "Parse a list with mixed nested delimiters."
  (with-temp-buffer
    (insert "(a, [b, c], {d, e})")
    (goto-char (point-min))
    (search-forward "a")
    (goto-char (match-beginning 0))
    (let* ((bounds (cpo-comma-list--find-list-bounds))
           (elements (cpo-comma-list--parse-elements (car bounds) (cdr bounds))))
      (should (= 3 (length elements)))
      (let ((second-trimmed (cpo-comma-list--trim-bounds (car (nth 1 elements))
                                                         (cdr (nth 1 elements)))))
        (should (equal "[b, c]" (buffer-substring-no-properties
                                 (car second-trimmed) (cdr second-trimmed)))))
      (let ((third-trimmed (cpo-comma-list--trim-bounds (car (nth 2 elements))
                                                        (cdr (nth 2 elements)))))
        (should (equal "{d, e}" (buffer-substring-no-properties
                                 (car third-trimmed) (cdr third-trimmed))))))))

;;; Test: inner bounds (selection)
;; select-inner sets point to start and mark to end of trimmed element.

(carettest-tesmo-test cpo-comma-list-test-inner-bounds-first
                      "(<p0><p1>alpha<m1>, beta, gamma)"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-inner-bounds-middle
                      "(alpha, <p0><p1>beta<m1>, gamma)"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-inner-bounds-last
                      "(alpha, beta, <p0><p1>gamma<m1>)"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-inner-bounds-with-whitespace
                      "(  alpha  ,  <p0><p1>beta<m1>  ,  gamma  )"
                      'cpo-comma-list-select)

;;; Test: outer bounds (selection)
;; select-outer sets point to start and mark to end of outer bounds.
;; First/middle: trailing comma+space included; last: leading comma+space.

(carettest-tesmo-test cpo-comma-list-test-outer-bounds-first
                      "(<p0><p1>alpha, <m1>beta, gamma)"
                      'cpo-comma-list-select-outer)

(carettest-tesmo-test cpo-comma-list-test-outer-bounds-middle
                      "(alpha, <p0><p1>beta, <m1>gamma)"
                      'cpo-comma-list-select-outer)

(carettest-tesmo-test cpo-comma-list-test-outer-bounds-last
                      "(alpha, beta<p1>, <p0>gamma<m1>)"
                      'cpo-comma-list-select-outer)

(carettest-tesmo-test cpo-comma-list-test-outer-bounds-single-element
                      "(<p0><p1>alone<m1>)"
                      'cpo-comma-list-select-outer)

;;; Test: movement

(carettest-tesmo-test cpo-comma-list-test-forward-from-first
                      "(<p0>alpha, <p1>beta, gamma)"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-forward-from-middle
                      "(alpha, <p0>beta, <p1>gamma)"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-backward-from-last
                      "(alpha, <p1>beta, <p0>gamma)"
                      'cpo-comma-list-backward-beginning)

(carettest-tesmo-test cpo-comma-list-test-backward-from-middle
                      "(<p1>alpha, <p0>beta, gamma)"
                      'cpo-comma-list-backward-beginning)

(carettest-tesmo-test cpo-comma-list-test-forward-nested
                      "(<p0>a, <p1>(b, c), d)"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-forward-nested-to-last
                      "(a, <p0>(b, c), <p1>d)"
                      'cpo-comma-list-forward-beginning)

;;; Test: transpose

(carettest-tesmut-test cpo-comma-list-test-transpose-forward
                       "(<p>alpha, beta, gamma)"
                       "(beta, <p>alpha, gamma)"
                       'cpo-comma-list-transpose-forward)
(carettest-tesmut-test cpo-comma-list-test-transpose-forward_to-end
                       "(alpha, <p>beta, gamma)"
                       "(alpha, gamma, <p>beta)"
                       'cpo-comma-list-transpose-forward)

(carettest-tesmut-test cpo-comma-list-test-transpose-backward
                       "(alpha, beta, <p>gamma)"
                       "(alpha, <p>gamma, beta)"
                       'cpo-comma-list-transpose-backward)
(carettest-tesmut-test cpo-comma-list-test-transpose-backward_to-beg
                       "(alpha, <p>beta, gamma)"
                       "(<p>beta, alpha, gamma)"
                       'cpo-comma-list-transpose-backward)

(carettest-tesmut-test cpo-comma-list-test-transpose-forward-nested
                       "(<p>a, (b, c), d)"
                       "((b, c), <p>a, d)"
                       'cpo-comma-list-transpose-forward)

(carettest-tesmut-test cpo-comma-list-test-transpose-repeated
                       :before "(<p>a, b, c, d)"
                       :after "(b, c, <p>a, d)"
                       :function (lambda () (cpo-comma-list-transpose-forward 2)))

;;; Test: prose (no explicit delimiters -- line boundaries)

(carettest-tesmo-test cpo-comma-list-test-prose-inner
                      "apples, <p0><p1>oranges<m1>, and bananas"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-prose-outer-first
                      "<p0><p1>apples, <m1>oranges, and bananas"
                      'cpo-comma-list-select-outer)

(carettest-tesmo-test cpo-comma-list-test-prose-outer-last
                      "apples, oranges<p1>, <p0>and bananas<m1>"
                      'cpo-comma-list-select-outer)

(carettest-tesmo-test cpo-comma-list-test-prose-forward
                      "<p0>apples, <p1>oranges, and bananas"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-prose-forward-to-last
                      "apples, <p0>oranges, <p1>and bananas"
                      'cpo-comma-list-forward-beginning)

;;; Test: edge cases

(ert-deftest cpo-comma-list-test-empty-list ()
  "Empty list has one empty element."
  (with-temp-buffer
    (insert "()")
    (goto-char (1+ (point-min)))
    (let ((info (cpo-comma-list--element-at-point)))
      (when info
        (let ((elements (plist-get info :elements)))
          (should (= 1 (length elements))))))))

(ert-deftest cpo-comma-list-test-single-element ()
  "Single element list -- no movement possible."
  (with-temp-buffer
    (insert "(solo)")
    (goto-char (point-min))
    (search-forward "solo")
    (goto-char (match-beginning 0))
    (let ((inner (cpo-comma-list--inner-bounds)))
      (should inner)
      (should (equal "solo" (buffer-substring-no-properties (car inner) (cdr inner)))))
    (should-not (cpo-comma-list--forward-beginning))
    (should-not (cpo-comma-list--backward-beginning))))

(ert-deftest cpo-comma-list-test-trailing-comma ()
  "List with trailing comma."
  (with-temp-buffer
    (insert "(a, b, c,)")
    (goto-char (point-min))
    (search-forward "a")
    (goto-char (match-beginning 0))
    (let* ((bounds (cpo-comma-list--find-list-bounds))
           (elements (cpo-comma-list--parse-elements (car bounds) (cdr bounds))))
      (should (= 4 (length elements))))))

(carettest-tesmo-test cpo-comma-list-test-brackets-inner
                      "[<p0><p1>a<m1>, b]"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-brackets-forward
                      "[<p0>a, <p1>b]"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-deeply-nested-forward-first
                      "(<p0>a, <p1>((b, c), d), e)"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-deeply-nested-forward-second
                      "(a, <p0>((b, c), d), <p1>e)"
                      'cpo-comma-list-forward-beginning)

(carettest-tesmo-test cpo-comma-list-test-nested-inner-list-inner
                      "(a, (<p0><p1>x<m1>, y, z), c)"
                      'cpo-comma-list-select)

(carettest-tesmo-test cpo-comma-list-test-nested-inner-list-forward
                      "(a, (<p0>x, <p1>y, z), c)"
                      'cpo-comma-list-forward-beginning)

;;; Test: forward-end / backward-end movement

(carettest-tesmo-test cpo-comma-list-test-forward-end-from-first
                      "(<p0>alpha<p1>, beta, gamma)"
                      'cpo-comma-list-forward-end)

(carettest-tesmo-test cpo-comma-list-test-forward-end-from-middle
                      "(alpha, <p0>beta<p1>, gamma)"
                      'cpo-comma-list-forward-end)

(carettest-tesmo-test cpo-comma-list-test-forward-end-already-at-end
                      "(alpha<p0>, beta<p1>, gamma)"
                      'cpo-comma-list-forward-end)

(carettest-tesmo-test cpo-comma-list-test-backward-end-from-last
                      "(alpha, beta<p1>, <p0>gamma)"
                      'cpo-comma-list-backward-end)

(carettest-tesmo-test cpo-comma-list-test-backward-end-from-middle
                      "(alpha<p1>, <p0>beta, gamma)"
                      'cpo-comma-list-backward-end)

(carettest-tesmo-test cpo-comma-list-test-forward-end-nested
                      "(<p0>a<p1>, (b, c), d)"
                      'cpo-comma-list-forward-end)

(carettest-tesmo-test cpo-comma-list-test-forward-end-nested-to-last
                      "(a, <p0>(b, c)<p1>, d)"
                      'cpo-comma-list-forward-end)

;;; Test: list open forward / backward

(carettest-tesmut-test cpo-comma-list-test-open-forward
                       "(alpha, <p>beta, gamma)"
                       "(alpha, beta, <p>, gamma)"
                       'cpo-comma-list-open-forward)

(carettest-tesmut-test cpo-comma-list-test-open-forward-last
                       "(alpha, beta, <p>gamma)"
                       "(alpha, beta, gamma, <p>)"
                       'cpo-comma-list-open-forward)

(carettest-tesmut-test cpo-comma-list-test-open-backward
                       "(alpha, <p>beta, gamma)"
                       "(alpha, <p>, beta, gamma)"
                       'cpo-comma-list-open-backward)

(carettest-tesmut-test cpo-comma-list-test-open-backward-first
                       "(<p>alpha, beta, gamma)"
                       "(<p>, alpha, beta, gamma)"
                       'cpo-comma-list-open-backward)

;;; Test: transpose with active region

;; Transpose a region of two elements forward -- the region covers "alpha, beta"
;; and gets swapped with "gamma".
(carettest-tesmut-test cpo-comma-list-test-transpose-region-forward
                       :before "(<p>alpha, beta<m>, gamma)"
                       :after "(gamma, <p>alpha, beta<m>)"
                       :function 'cpo-comma-list-transpose-forward)

;; Transpose a region of two elements backward -- the region covers "beta, gamma"
;; and gets swapped with "alpha".
(carettest-tesmut-test cpo-comma-list-test-transpose-region-backward
                       :before "(alpha, <p>beta, gamma<m>)"
                       :after "(<p>beta, gamma<m>, alpha)"
                       :function 'cpo-comma-list-transpose-backward)

;; Transpose region forward with unequal-length elements.
(carettest-tesmut-test cpo-comma-list-test-transpose-region-forward-unequal
                       :before "(<p>aa, bb<m>, cccc)"
                       :after "(cccc, <p>aa, bb<m>)"
                       :function 'cpo-comma-list-transpose-forward)

;; Transpose region backward with unequal-length elements.
(carettest-tesmut-test cpo-comma-list-test-transpose-region-backward-unequal
                       :before "(aaaa, <p>bb, cc<m>)"
                       :after "(<p>bb, cc<m>, aaaa)"
                       :function 'cpo-comma-list-transpose-backward)

;; Transpose a single-element region forward (should behave like normal transpose).
(carettest-tesmut-test cpo-comma-list-test-transpose-region-forward-single
                       :before "(<p>alpha<m>, beta, gamma)"
                       :after "(beta, <p>alpha<m>, gamma)"
                       :function 'cpo-comma-list-transpose-forward)

;; Transpose region forward in a four-element list, moving middle two.
(carettest-tesmut-test cpo-comma-list-test-transpose-region-forward-four-elems
                       :before "(a, <p>b, c<m>, d)"
                       :after "(a, d, <p>b, c<m>)"
                       :function 'cpo-comma-list-transpose-forward)

;; Transpose region backward in a four-element list, moving middle two.
(carettest-tesmut-test cpo-comma-list-test-transpose-region-backward-four-elems
                       :before "(a, <p>b, c<m>, d)"
                       :after "(<p>b, c<m>, a, d)"
                       :function 'cpo-comma-list-transpose-backward)

;; Repeated region transpose forward -- drag a region across multiple positions.
(carettest-tesmut-test cpo-comma-list-test-transpose-region-forward-repeated
                       :before "(<p>a, b<m>, c, d)"
                       :after "(c, d, <p>a, b<m>)"
                       :function (lambda () (cpo-comma-list-transpose-forward 2)))

;; TODO - open in an empty list () is degenerate and broken, I'll fix it later, maybe, if I get back to it.
;; TODO - maybe I should add up/down tree motions to comma lists?  IE ignore other syntactical elements to be language-generic, but if a list is inside delimiters, go out a level.
;; TODO - this is broken for straight-quote strings that have commas.  But parsing straight-quote strings is language-specific and more difficult than I want to try in this.  At that point hopefully treesitter or something is helpful.  The point of this comma list is to have something generic that works OK much of the time, not to be bulletproof.

;;; test-cpo-comma-list.el ends here
