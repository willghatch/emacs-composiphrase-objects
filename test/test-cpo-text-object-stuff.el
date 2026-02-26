;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cpo-text-object-stuff)
(require 'carettest-tesmo)

(setq some-words "The quick brown fox jumps over the lazy dog.")
(ert-deftest misc-tests ()
  (with-temp-buffer
    (insert some-words)
    (goto-char 6)
    (should (equal (cons 5 10)
                   (cpo-text-object-stuff--expanded-region-to-bounds-of-thing-at-point t nil 'word (cons 7 9))))))

(carettest-tesmo-test
 test-date-movements-cpo-forward-date-beginning__match-datetime-too
 ;; IE the important thing that this needs to catch is that a datetime is still a date, so if the date part ends in a T or something, it should still be matched.
 "2023-01-15T10:30:00 starts the log; error at (2023-02-20 14:45) noted.
<p0>Log entry: <p1>2022-12-01T09:15:30 - System started successfully.
Final timestamp: 2024-03-10T18:22:45 in the logs."
 'cpo-forward-date-beginning :transient-mark-mode t :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))
