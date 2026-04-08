;;; test-cpo-table.el --- Tests for cpo-table -*- lexical-binding: t; -*-

(require 'ert)
(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'carettest-tesprop)
(require 'cpo-table)

;;; ---------------------------------------------------------------------------
;;; Cell expand-region tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-inner-first
 "
| <p0><p1>a<m1> | b |
"
 'cpo-table-cell-expand-region-inner)

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-inner-second
 "
| a | <p0><p1>b<m1> |
"
 'cpo-table-cell-expand-region-inner)

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-inner-multichar
 "
| <p0><p1>hello<m1> | world |
"
 'cpo-table-cell-expand-region-inner)

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-inner-position-end
 "
| <m1>hello<p0><p1> | world |
"
 (lambda () (cpo-table-cell-expand-region-inner :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-inner-default-prefers-grown-end
 "
|<m0><m1> h<p0>ello<p1> | world |
"
 'cpo-table-cell-expand-region-inner
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-first
 "
|<p0><p1> a |<m1> b |
"
 'cpo-table-cell-expand-region)

(carettest-tesmo-test
 cpo-table-test-cell-expand-region-second
 "
| a |<p0><p1> b |<m1>
"
 'cpo-table-cell-expand-region)

(carettest-tesmo-test
 cpo-table-test-expand-region-to-cpo-table-cell-inner
 "
| <p0><p1>a<m1> | b |
"
 'cpo-table-cell-expand-region-inner)

(carettest-tesmo-test
 cpo-table-test-expand-region-to-cpo-table-cell
 "
|<p0><p1> a |<m1> b |
"
 'cpo-table-cell-expand-region)

;;; ---------------------------------------------------------------------------
;;; Row expand-region tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-row-expand-region-inner
 "
| <p0><p1>a | b | c<m1> |
"
 'cpo-table-row-expand-region-inner)

(carettest-tesmo-test
 cpo-table-test-row-expand-region
 "
<p0><p1>| a | b | c |
<m1>| --- | --- | --- |
"
 'cpo-table-row-expand-region)

;;; ---------------------------------------------------------------------------
;;; Table expand-region tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-expand-region-to-table
 "
<p0><p1>| a | b |
| --- | --- |
| 1 | 2 |
<m1>
"
 'cpo-table-expand-region)

(carettest-tesmut-test
 cpo-table-test-expand-region-through-hierarchy
 :buffer-states
 (list
  "
| <p>hello | world |
| --- | --- |
| 1 | 2 |
"
  "
| <p>hello<m> | world |
| --- | --- |
| 1 | 2 |
"
  "
|<p> hello |<m> world |
| --- | --- |
| 1 | 2 |
"
  "
<p>| hello | world |
<m>| --- | --- |
| 1 | 2 |
"
  "
<p>| hello | world |
| --- | --- |
| 1 | 2 |
<m>")
 :functions
 (list 'cpo-table-expand-region-through-hierarchy
       'cpo-table-expand-region-through-hierarchy
       'cpo-table-expand-region-through-hierarchy
       'cpo-table-expand-region-through-hierarchy))

;;; ---------------------------------------------------------------------------
;;; Table movement tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-forward-table-beginning
 "
before<p0>
<p1>| a | b |
| --- | --- |
| 1 | 2 |
"
 'cpo-table-forward-beginning)

(carettest-tesmo-test
 cpo-table-test-forward-table-end
 "
| <p0>a | b |
| --- | --- |
| 1 | 2 |
<p1>
"
 'cpo-table-forward-end)

(carettest-tesmo-test
 cpo-table-test-backward-table-beginning
 "
<p1>| a | b |
| --- | --- |
| <p0>1 | 2 |
"
 'cpo-table-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-backward-table-end
 "
| a | b |
| --- | --- |
| 1 | 2 |
<p1>
between
| <p0>c | d |
| --- | --- |
| 3 | 4 |
"
 'cpo-table-backward-end)

;;; ---------------------------------------------------------------------------
;;; Cell movement tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-cell-forward-beginning-within-row
 "
| <p0>a |<p1> b | c |
"
 'cpo-table-cell-forward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-forward-to-third
 "
| a | <p0>b |<p1> c |
"
 'cpo-table-cell-forward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-backward-beginning-within-row
 "
| a |<p1> <p0>b | c |
"
 'cpo-table-cell-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-backward-beginning-within-row-2
 "
|<p0> a |<p0> b | c |
"
 'cpo-table-cell-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-backward-to-first
 "
| a |<p1> b |<p0> c |
"
 'cpo-table-cell-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-forward-wrap-to-next-row
 "
| a | b | <p0>c |
| --- | --- | --- |
|<p1> 1 | 2 | 3 |
"
 'cpo-table-cell-forward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-backward-wrap-to-prev-row
 "
| a | b |<p1> c |
| --- | --- | --- |
|<p0> 1 | 2 | 3 |
"
 'cpo-table-cell-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-cell-forward-end
 "
| <p0>a | b<p1> | c |
"
 'cpo-table-cell-forward-end)

(carettest-tesmo-test
 cpo-table-test-cell-backward-end
 "
| a | b<p1> |<p0> c |
"
 'cpo-table-cell-backward-end)

(carettest-tesmo-test
 cpo-table-test-cell-beginning-of-content
 "
| <p1>he<p0>llo | world |
"
 'cpo-table-cell-beginning-of-content)

(carettest-tesmo-test
 cpo-table-test-cell-end-of-content
 "
| he<p0>llo<p1> | world |
"
 'cpo-table-cell-end-of-content)

;;; ---------------------------------------------------------------------------
;;; Cell up/down navigation
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-cell-down
 "
|<p0> a | b |
| --- | --- |
|<p1> 1 | 2 |
"
 'cpo-table-cell-down)

(carettest-tesmo-test
 cpo-table-test-cell-move-down-beginning
 "
| a | <p0>b |
| --- | --- |
| 1 |<p1> 2 |
"
 (lambda () (cpo-table-cell-move :direction 'down :position 'beginning)))

(carettest-tesmo-test
 cpo-table-test-cell-up
 "
|<p1> a | b |
| --- | --- |
|<p0> 1 | 2 |
"
 'cpo-table-cell-up)

(carettest-tesmo-test
 cpo-table-test-cell-down-second-col
 "
| a |<p0> b |
| --- | --- |
| 1 |<p1> 2 |
"
 'cpo-table-cell-down)

(carettest-tesmo-test
 cpo-table-test-cell-up-second-col
 "
| a |<p1> b |
| --- | --- |
| 1 |<p0> 2 |
"
 'cpo-table-cell-up)

;;; ---------------------------------------------------------------------------
;;; Row navigation tests
;;; ---------------------------------------------------------------------------

(carettest-tesmo-test
 cpo-table-test-row-forward-beginning
 "
| <p0>a | b |
| --- | --- |
<p1>| 1 | 2 |
"
 'cpo-table-row-forward-beginning)

(carettest-tesmo-test
 cpo-table-test-row-backward-beginning
 "
| a | b |
| --- | --- |
<p1>| <p0>1 | 2 |
"
 'cpo-table-row-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-row-backward-beginning-2
 "
<p1>| a | b |
| --- | --- |
<p0>| 1 | 2 |
"
 'cpo-table-row-backward-beginning)

(carettest-tesmo-test
 cpo-table-test-row-forward-end
 "
| <p0>1 | 2 |
<p1>| 3 | 4 |
tail
"
 'cpo-table-row-forward-end)

(carettest-tesmo-test
 cpo-table-test-row-backward-end
 "
| a | b |
| --- | --- |
| 1 | 2 |
<p1>| <p0>3 | 4 |
"
 'cpo-table-row-backward-end)

(carettest-tesmo-test
 cpo-table-test-row-beginning-idempotent-1
 "
| a | b | c |
<p1>| a | b | <p0>c |
"
 (lambda () (cpo-table-row-backward-beginning :idempotent t)))

(carettest-tesmo-test
 cpo-table-test-row-beginning-idempotent-2
 "
| a | b | c |
<p0><p1>| a | b | c |
"
 (lambda () (cpo-table-row-backward-beginning :idempotent t)))

(carettest-tesmo-test
 cpo-table-test-row-end-idempotent-1
 "
| <p0>a | b | c |
<p1>| a | b | c |

"
 (lambda () (cpo-table-row-forward-end :idempotent t)))

(carettest-tesmo-test
 cpo-table-test-row-end-idempotent-2
 "
| a | b | c |
<p0><p1>| a | b | c |

"
 (lambda () (cpo-table-row-forward-end :idempotent t)))

;;; ---------------------------------------------------------------------------
;;; Mutation tests: table and row insertion
;;; ---------------------------------------------------------------------------

(carettest-tesmut-test
 cpo-table-test-open-new-table
 "<p>"
 "| <p> |
|---|
"
 'cpo-table-open-new)

(carettest-tesmut-test
 cpo-table-test-open-row-forward
 "
| a | b |
| --- | --- |
| <p>1 | 2 |
"
 "
| a | b |
| --- | --- |
| 1 | 2 |
| <p> |  |
"
 'cpo-table-row-open-forward)

(carettest-tesmut-test
 cpo-table-test-open-row-backward
 "
| a | b |
| --- | --- |
| <p>1 | 2 |
"
 "
| a | b |
| --- | --- |
| <p> |  |
| 1 | 2 |
"
 'cpo-table-row-open-backward)

(carettest-tesmut-test
 cpo-table-test-open-row-header-only
 "
| <p>a | b |
| --- | --- |
"
 "
| a | b |
| --- | --- |
| <p> |  |
"
 'cpo-table-row-open-forward)

(carettest-tesmut-test
 cpo-table-test-cell-open-down
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 "
| a | b |
| --- | --- |
| 1 | 2 |
|  | <p> |
"
 'cpo-table-cell-open-down)

(carettest-tesmut-test
 cpo-table-test-cell-open-up
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 "
| a | b |
| --- | --- |
|  | <p> |
| 1 | 2 |
"
 'cpo-table-cell-open-up)

;;; ---------------------------------------------------------------------------
;;; Mutation tests: column insertion
;;; ---------------------------------------------------------------------------

(carettest-tesmut-test
 cpo-table-test-cell-open-forward
 "
| <p>a | b |
| --- | --- |
| 1 | 2 |
"
 "
| a | <p> | b |
| --- | --- | --- |
| 1 |  | 2 |
"
 'cpo-table-cell-open-forward)

(carettest-tesmut-test
 cpo-table-test-cell-open-backward
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 "
| a |  | b |
| --- | --- | --- |
| 1 | <p> | 2 |
"
 'cpo-table-cell-open-backward)

(carettest-tesmut-test
 cpo-table-test-column-open-forward
 "
| a | <p>b |
| --- | --- |
| 1 | 2 |
"
 "
| a | b | <p> |
| --- | --- | --- |
| 1 | 2 |  |
"
 'cpo-table-column-open-forward)

(carettest-tesmut-test
 cpo-table-test-column-open-backward
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 "
| a | <p> | b |
| --- | --- | --- |
| 1 |  | 2 |
"
 'cpo-table-column-open-backward)

;;; ---------------------------------------------------------------------------
;;; Mutation tests: row transpose
;;; ---------------------------------------------------------------------------

(carettest-tesmut-test
 cpo-table-test-transpose-row-forward
 "
| a | b |
| --- | --- |
| <p>1 | 2 |
| 3 | 4 |
"
 "
| a | b |
| --- | --- |
| 3 | 4 |
| <p>1 | 2 |
"
 'cpo-table-row-transpose-forward)

(carettest-tesmut-test
 cpo-table-test-transpose-row-backward
 "
| a | b |
| --- | --- |
| 1 | 2 |
| <p>3 | 4 |
"
 "
| a | b |
| --- | --- |
| <p>3 | 4 |
| 1 | 2 |
"
 'cpo-table-row-transpose-backward)

;;; ---------------------------------------------------------------------------
;;; Mutation tests: cell transpose
;;; ---------------------------------------------------------------------------

(carettest-tesmut-test
 cpo-table-test-transpose-cell-right
 "
| <p>a | b | c |
"
 "
| b | <p>a | c |
"
 (lambda () (cpo-table-cell-transpose :direction 'right)))

(carettest-tesmut-test
 cpo-table-test-transpose-cell-left
 "
| a | b | <p>c |
"
 "
| a | <p>c | b |
"
 (lambda () (cpo-table-cell-transpose :direction 'left)))

(carettest-tesmut-test
 cpo-table-test-transpose-cell-down
 "
| a | <p>b |
| --- | --- |
| 1 | 2 |
"
 "
| a | 2 |
| --- | --- |
| 1 | <p>b |
"
 (lambda () (cpo-table-cell-transpose :direction 'down)))

(carettest-tesmut-test
 cpo-table-test-transpose-cell-up
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 "
| a | <p>2 |
| --- | --- |
| 1 | b |
"
 (lambda () (cpo-table-cell-transpose :direction 'up)))

(carettest-tesmut-test
 cpo-table-test-transpose-cell-forward
 "
| <p>a | b | c |
"
 "
| b | <p>a | c |
"
 'cpo-table-cell-transpose-forward)

(carettest-tesmut-test
 cpo-table-test-transpose-cell-backward
 "
| a | b | <p>c |
"
 "
| a | <p>c | b |
"
 'cpo-table-cell-transpose-backward)

(carettest-tesmut-test
 cpo-table-test-transpose-table-forward-contract
 :before
 "before
| <p>a | b |
| --- | --- |
| 1 | 2 |

between

| x | y |
| --- | --- |
| 3 | 4 |
after"
 :after
 "before
| x | y |
| --- | --- |
| 3 | 4 |

between

| <p>a | b |
| --- | --- |
| 1 | 2 |
after"
 :function
 'cpo-table-transpose-forward)

(carettest-tesmut-test
 cpo-table-test-transpose-table-backward-contract
 :before
 "before
| a | b |
| --- | --- |
| 1 | 2 |

between

| x | y |
| --- | --- |
| <p>3 | 4 |
after"
 :after
 "before
| x | y |
| --- | --- |
| <p>3 | 4 |

between

| a | b |
| --- | --- |
| 1 | 2 |
after"
 :function
 'cpo-table-transpose-backward)

(carettest-tesmut-test
 cpo-table-test-column-split-contract
 :before
 "| al<p>pha | beta |
| --- | --- |
| 1 | 2 |
"
 :after
 "| al | <p>pha | beta |
| --- | --- | --- |
| 1 |  | 2 |
"
 :function
 (lambda () (cpo-table-column-split :direction 'forward)))

(carettest-tesmut-test
 cpo-table-test-column-join-contract
 :buffer-states
 (list
  "
| al<p>pha | beta |
| --- | --- |
| 1 | 2 |
"
  "
| al | <p>pha | beta |
| --- | --- | --- |
| 1 |  | 2 |
"
  "
| al<p>pha | beta |
| --- | --- |
| 1 | 2 |
")
 :functions
 (list (lambda () (cpo-table-column-split :direction 'forward))
       (lambda () (cpo-table-column-join :direction 'backward))))

(carettest-tesmut-test
 cpo-table-test-column-join-2
 :buffer-states
 (list
  "
| al<p>pha | beta |
| --- | --- |
| 1 | 2 |
"
  "
| al<p> | pha | beta |
| --- | --- | --- |
| 1 |  | 2 |
"
  "
| al<p>pha | beta |
| --- | --- |
| 1 | 2 |
")
 :functions
 (list (lambda () (cpo-table-column-split :direction 'backward))
       (lambda () (cpo-table-column-join :direction 'forward))))


(carettest-tesmut-test
 cpo-table-test-transpose-column-forward
 "
| <p>a | b |
| --- | --- |
| 1 | 2 |
"
 "
| b | <p>a |
| --- | --- |
| 2 | 1 |
"
 'cpo-table-column-transpose-forward)

(carettest-tesmut-test
 cpo-table-test-transpose-column-backward
 "
| a | <p>b |
| --- | --- |
| 1 | 2 |
"
 "
| <p>b | a |
| --- | --- |
| 2 | 1 |
"
 'cpo-table-column-transpose-backward)

(carettest-tesmut-test
 cpo-table-test-column-delete
 "
| a | <p>b | c |
| --- | --- | --- |
| 1 | 2 | 3 |
"
 "
| a |<p> c |
| --- | --- |
| 1 | 3 |
"
 'cpo-table-column-delete)

(carettest-tesmut-test
 cpo-table-test-simple-align-1
 "
<p>| a | hello |
| --- | --- |
| longer text | b |
"
 "
<p>| a           | hello |
|-------------|-------|
| longer text | b     |
"
 'cpo-table-align)

(carettest-tesmut-test
 cpo-table-test-simple-align-2
 "
| a | hello |
| --- | --- |
| longer <p>text | b |
"
 "
| a           | hello |
|-------------|-------|
| longer <p>text | b     |
"
 'cpo-table-align)

;;; ---------------------------------------------------------------------------
;;; Internal API tests
;;; ---------------------------------------------------------------------------
;;; Beyond this point the tests are useful to help catch regressions, but if
;;; they are ever contrary to tests of the external API above, the external API
;;; tests should take precedence.
;;; ---------------------------------------------------------------------------


;;; Backend separator detection tests

(ert-deftest cpo-table-test-backends-define-required-operations ()
  "The public backend dispatch table contains the expected operations."
  (dolist (backend '(markdown org))
    (let ((ops (alist-get backend cpo-table-backends)))
      (should ops)
      (dolist (operation '(align
                           separator-row-p
                           at-table-p
                           table-begin
                           table-end
                           next-field
                           previous-field
                           insert-column
                           insert-row
                           beginning-of-field
                           end-of-field))
        (should (alist-get operation ops))))))

(carettest-tesprop
 cpo-table-test-at-table-p
 "
<p>| a | b |
| --- | --- |
| 1 | 2 |
"
 (cpo-table--markdown-at-table-p))

(carettest-tesprop
 cpo-table-test-not-at-table-p
 "<p>just some text"
 (not (cpo-table--markdown-at-table-p)))

(carettest-tesprop
 cpo-table-test-separator-row-p
 "<p>| --- | --- | --- |"
 (cpo-table--markdown-separator-row-p))

(carettest-tesprop
 cpo-table-test-separator-row-with-alignment
 "<p>| :--- | :---: | ---: |"
 (cpo-table--markdown-separator-row-p))

(carettest-tesprop
 cpo-table-test-separator-row-not-data
 "<p>| text | text | text |"
 (not (cpo-table--markdown-separator-row-p)))

(carettest-tesprop
 cpo-table-test-org-separator-row-p
 "<p>|---+---+---|"
 (cpo-table--org-separator-row-p))

(carettest-tesprop
 cpo-table-test-org-not-separator
 "<p>| a | b | c |"
 (not (cpo-table--org-separator-row-p)))

(ert-deftest cpo-table-test-markdown-separator-variants ()
  "Various separator row styles are detected."
  (dolist (sep-text '("| --- | --- |"
                      "| :--- | ---: |"
                      "| :---: | :---: |"
                      "|----|-----|"
                      "| - | - |"))
    (carettest-with-tesprop-buffer sep-text
      (should (cpo-table--markdown-separator-row-p)))))

;;; Parsing tests

(ert-deftest cpo-table-test-parse-simple-table ()
  "Parse a simple 2x2 markdown table."
  (with-temp-buffer
    (insert
     "
| a | b |
| --- | --- |
| 1 | 2 |
")
    (goto-char (point-min))
    (search-forward "| a | b |")
    (goto-char (match-beginning 0))
    (let ((table (cpo-table--do-parse-table-at-point)))
      (should table)
      (should (= 3 (length (plist-get table :rows))))
      (should (= 2 (plist-get table :num-columns)))
      (let ((sep-row (nth 1 (plist-get table :rows))))
        (should (plist-get sep-row :separator-p)))
      (should-not (plist-get (nth 0 (plist-get table :rows)) :separator-p))
      (should-not (plist-get (nth 2 (plist-get table :rows)) :separator-p)))))

(ert-deftest cpo-table-test-parse-cell-content ()
  "Check that cell inner bounds point to actual content."
  (with-temp-buffer
    (insert "| hello | world |")
    (goto-char (point-min))
    (let ((table (cpo-table--do-parse-table-at-point)))
      (should table)
      (let* ((row (car (plist-get table :rows)))
             (cells (plist-get row :cells))
             (cell1 (nth 0 cells))
             (cell2 (nth 1 cells)))
        (should (equal "hello"
                       (buffer-substring-no-properties
                        (plist-get cell1 :inner-begin)
                        (plist-get cell1 :inner-end))))
        (should (equal "world"
                       (buffer-substring-no-properties
                        (plist-get cell2 :inner-begin)
                        (plist-get cell2 :inner-end))))))))

(ert-deftest cpo-table-test-parse-cell-column-index ()
  "Check that cells have correct column indices."
  (with-temp-buffer
    (insert "| a | b | c |")
    (goto-char (point-min))
    (let* ((table (cpo-table--do-parse-table-at-point))
           (row (car (plist-get table :rows)))
           (cells (plist-get row :cells)))
      (should (= 0 (plist-get (nth 0 cells) :column-index)))
      (should (= 1 (plist-get (nth 1 cells) :column-index)))
      (should (= 2 (plist-get (nth 2 cells) :column-index))))))

(ert-deftest cpo-table-test-parse-table-bounds ()
  "Table bounds detection with surrounding text."
  (with-temp-buffer
    (insert
     "
text before
| a | b |
| --- | --- |
| 1 | 2 |
text after
")
    (goto-char (point-max))
    (search-backward "a |")
    (let* ((table (cpo-table--do-parse-table-at-point))
           (tbegin (plist-get table :begin)))
      (should table)
      (goto-char tbegin)
      (should (looking-at-p "| a | b |")))))

(ert-deftest cpo-table-test-cell-at-point ()
  "Cell at point returns the correct cell."
  (with-temp-buffer
    (insert "| hello | world |")
    (goto-char (point-min))
    (search-forward "hello")
    (goto-char (match-beginning 0))
    (let ((cell (cpo-table--cell-at-point)))
      (should cell)
      (should (= 0 (plist-get cell :column-index)))
      (should (equal "hello"
                     (buffer-substring-no-properties
                      (plist-get cell :inner-begin)
                      (plist-get cell :inner-end)))))))

(ert-deftest cpo-table-test-row-at-point ()
  "Row at point in a multi-row table."
  (with-temp-buffer
    (insert
     "
| a | b |
| --- | --- |
| 1 | 2 |
")
    (goto-char (point-max))
    (search-backward "1")
    (let ((row (cpo-table--row-at-point)))
      (should row)
      (should-not (plist-get row :separator-p))
      (let ((cells (plist-get row :cells)))
        (should (= 2 (length cells)))))))

;;; Edge cases around parser and movement boundaries

(carettest-tesprop
 cpo-table-test-empty-cell-bounds
 "
| <p> | b |
"
 (let ((cell (cpo-table--cell-at-point)))
   (and cell
        (= (plist-get cell :inner-begin)
           (plist-get cell :inner-end)))))

(carettest-tesprop
 cpo-table-test-single-row-table
 "
| <p>a | b |
| --- | --- |
"
 (let ((table (cpo-table--parse-table-at-point))
       (start (point)))
   (and table
        (= 2 (length (plist-get table :rows)))
        (progn
          (cpo-table-cell-down)
          (= start (point))))))

(carettest-tesprop
 cpo-table-test-navigation-at-first-cell
 "
| <p>a | b |
| --- | --- |
| 1 | 2 |
"
 (let ((start (point)))
   (cpo-table-cell-backward-beginning)
   (= start (point))))

(carettest-tesprop
 cpo-table-test-navigation-at-last-cell
 "
| a | b |
| --- | --- |
| 1 | <p>2 |
"
 (let ((start (point)))
   (cpo-table-cell-forward-beginning)
   (= start (point))))

(carettest-tesprop
 cpo-table-test-table-at-beginning-of-buffer
 "<p>| a | b |
| --- | --- |
| 1 | 2 |
"
 (let ((bounds (cpo-table--table-bounds-at-point)))
   (and bounds
        (= (car bounds) (point-min)))))

(carettest-tesprop
 cpo-table-test-multiple-tables
 "
| a | b |
| --- | --- |
| 1 | 2 |

| <p>c | d |
| --- | --- |
| 3 | 4 |
"
 (let* ((table (cpo-table--do-parse-table-at-point))
        (rows (plist-get table :rows))
        (first-row (car rows))
        (cells (plist-get first-row :cells))
        (c1 (nth 0 cells))
        (c2 (nth 1 cells)))
   (and table
        (= 3 (length rows))
        (equal "c" (buffer-substring-no-properties
                    (plist-get c1 :inner-begin)
                    (plist-get c1 :inner-end)))
        (equal "d" (buffer-substring-no-properties
                    (plist-get c2 :inner-begin)
                    (plist-get c2 :inner-end))))))

(carettest-tesprop
 cpo-table-test-table-with-whitespace-cells
 "
|  <p> | b |
"
 (let ((cell (cpo-table--cell-at-point)))
   (and cell
        (= (plist-get cell :inner-begin)
           (plist-get cell :inner-end)))))

;;; test-cpo-table.el ends here
