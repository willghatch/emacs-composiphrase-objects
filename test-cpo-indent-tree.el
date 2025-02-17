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


(ert-deftest test-expand-region-to-any-delimiter-after-last-child ()
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
