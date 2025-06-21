;;; -*- lexical-binding: t; -*-

(require 'cpo-indent-tree)
(require 'ert)

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
