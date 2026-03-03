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

;;; Tests for expand-region position argument

;; expand-region-to-word: default position puts point at beginning of region
(carettest-tesmo-test
 test-expand-region-to-word__default-position-is-beginning
 "The <p1>qui<p0>ck<m1> brown fox"
 'cpo-expand-region-to-word
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-word: position 'end puts point at end of region
(carettest-tesmo-test
 test-expand-region-to-word__position-end
 "The <m1>qui<p0>ck<p1> brown fox"
 (lambda () (cpo-expand-region-to-word :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-word: position 'beginning explicitly puts point at beginning
(carettest-tesmo-test
 test-expand-region-to-word__position-beginning-explicit
 "The <p1>qui<p0>ck<m1> brown fox"
 (lambda () (cpo-expand-region-to-word :position 'beginning))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-symbol: default position puts point at beginning
(carettest-tesmo-test
 test-expand-region-to-symbol__default-position
 "let <p1>foo-b<p0>ar<m1> = 42"
 'cpo-expand-region-to-symbol
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-symbol: position 'end puts point at end
(carettest-tesmo-test
 test-expand-region-to-symbol__position-end
 "let <m1>foo-b<p0>ar<p1> = 42"
 (lambda () (cpo-expand-region-to-symbol :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; set-region: default position (nil) puts point at beginning
(carettest-tesmo-test
 test-set-region__default-position
 "<p1>hell<p0>o<m1> world"
 (lambda () (cpo-text-object-stuff--set-region (cons 1 6)))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; set-region: position 'end puts point at end
(carettest-tesmo-test
 test-set-region__position-end
 "<m1>hell<p0>o<p1> world"
 (lambda () (cpo-text-object-stuff--set-region (cons 1 6) :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; set-region: position 'beginning explicitly puts point at beginning
(carettest-tesmo-test
 test-set-region__position-beginning
 "<p1>hell<p0>o<m1> world"
 (lambda () (cpo-text-object-stuff--set-region (cons 1 6) :position 'beginning))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-fill-lines: default position is equivalent to 'beginning
;; Even when mark is before point, the default puts point at beginning.
(carettest-tesmo-test
 test-expand-region-to-fill-lines__default-is-beginning-even-when-mark-before-point
 "line one\n<p1>li<m0>ne two\nSed<p0> three<m1>\nline four"
 (lambda () (cpo-expand-region-to-fill-lines nil))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-expand-region-to-fill-lines__default-is-equivalent-to-beginning
 "line one\n<p1>li<p0>ne two\nSed<m0> three<m1>\nline four"
 (lambda () (cpo-expand-region-to-fill-lines nil))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-fill-lines: position 'beginning puts point at beginning
(carettest-tesmo-test
 test-expand-region-to-fill-lines__position-beginning
 "line one\n<p1>li<m0>ne two\nSed<p0> three<m1>\nline four"
 (lambda () (cpo-expand-region-to-fill-lines nil :position 'beginning))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; expand-region-to-fill-lines: position 'end puts point at end
(carettest-tesmo-test
 test-expand-region-to-fill-lines__position-end
 "line one\n<m1>li<m0>ne two\nSed<p0> three<p1>\nline four"
 (lambda () (cpo-expand-region-to-fill-lines nil :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))
