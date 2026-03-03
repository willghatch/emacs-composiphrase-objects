;;; test-cpo-date-chronological.el --- Tests for cpo-date-move-chronologically -*- lexical-binding: t; -*-

(require 'ert)
(require 'carettest-tesmo)
(require 'cpo-date-object)

;;; Helper: buffer text with dates in non-chronological order.
;; Buffer order:  2023-06-15  2021-01-10  2024-12-01  2022-03-20
;; Chrono order:  2021-01-10(idx0) 2022-03-20(idx1) 2023-06-15(idx2) 2024-12-01(idx3)

(defconst cpo-date-chrono-test-text
  "aaa 2023-06-15 bbb 2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee")

;;; Test: collecting dates and sorting chronologically

(ert-deftest cpo-date-chrono-test-collect-sorts-chronologically ()
  "Collected dates are in chronological order."
  (with-temp-buffer
    (insert cpo-date-chrono-test-text)
    (let ((dates (cpo-date--collect-dates-in-buffer 'date)))
      (should (= 4 (length dates)))
      (should (equal "2021-01-10" (car (nth 0 dates))))
      (should (equal "2022-03-20" (car (nth 1 dates))))
      (should (equal "2023-06-15" (car (nth 2 dates))))
      (should (equal "2024-12-01" (car (nth 3 dates)))))))

;;; Test: relative forward from before all dates

(carettest-tesmo-test cpo-date-chrono-test-relative-forward-from-start
  "<p0>aaa 2023-06-15 bbb <p1>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 1)))

;;; Test: relative forward by 1 from inside chronologically first date

(carettest-tesmo-test cpo-date-chrono-test-relative-forward-from-first-chrono
  "aaa 2023-06-15 bbb <p0>2021-01-10 ccc 2024-12-01 ddd <p1>2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 1)))

;;; Test: relative forward by 2 from chronologically first date

(carettest-tesmo-test cpo-date-chrono-test-relative-forward-by-2
  "aaa <p1>2023-06-15 bbb <p0>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 2)))

;;; Test: relative backward from chronologically last date

(carettest-tesmo-test cpo-date-chrono-test-relative-backward-from-last-chrono
  "aaa <p1>2023-06-15 bbb 2021-01-10 ccc <p0>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'backward :relative 1)))

;;; Test: relative backward by 2 from chronologically last

(carettest-tesmo-test cpo-date-chrono-test-relative-backward-by-2
  "aaa 2023-06-15 bbb 2021-01-10 ccc <p0>2024-12-01 ddd <p1>2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'backward :relative 2)))

;;; Test: absolute positioning -- go to chronologically first (index 0)

(carettest-tesmo-test cpo-date-chrono-test-absolute-first
  "aaa 2023-06-15 bbb <p1>2021-01-10 ccc <p0>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :absolute 0)))

;;; Test: absolute positioning -- go to chronologically last (index 3)

(carettest-tesmo-test cpo-date-chrono-test-absolute-last
  "<p0>aaa 2023-06-15 bbb 2021-01-10 ccc <p1>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :absolute 3)))

;;; Test: absolute with index beyond range clamps to last

(carettest-tesmo-test cpo-date-chrono-test-absolute-clamp-high
  "<p0>aaa 2023-06-15 bbb 2021-01-10 ccc <p1>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :absolute 100)))

;;; Test: absolute with negative index clamps to first

(carettest-tesmo-test cpo-date-chrono-test-absolute-clamp-low
  "aaa 2023-06-15 bbb <p1>2021-01-10 ccc <p0>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :absolute -5)))

;;; Test: position :end goes to end of date

(carettest-tesmo-test cpo-date-chrono-test-position-end
  "<p0>aaa 2023-06-15 bbb 2021-01-10<p1> ccc 2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :absolute 0 :position 'end)))

;;; Test: relative forward at the chronological end clamps

(carettest-tesmo-test cpo-date-chrono-test-forward-clamp-at-end
  "aaa 2023-06-15 bbb 2021-01-10 ccc <p0><p1>2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 1)))

;;; Test: relative backward at the chronological start clamps

(carettest-tesmo-test cpo-date-chrono-test-backward-clamp-at-start
  "aaa 2023-06-15 bbb <p0><p1>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'backward :relative 1)))

;;; Test: error when both :relative and :absolute given

(ert-deftest cpo-date-chrono-test-error-relative-and-absolute ()
  "Specifying both :relative and :absolute signals an error."
  (with-temp-buffer
    (insert cpo-date-chrono-test-text)
    (goto-char (point-min))
    (should-error (cpo-date-move-chronologically :relative 1 :absolute 0))))

;;; Test: no dates in buffer returns nil

(ert-deftest cpo-date-chrono-test-no-dates ()
  "When buffer has no dates, function returns nil."
  (with-temp-buffer
    (insert "no dates here at all")
    (goto-char (point-min))
    (let ((result (cpo-date-move-chronologically :direction 'forward)))
      (should-not result))))

;;; Test: default direction and relative (forward by 1)

(carettest-tesmo-test cpo-date-chrono-test-default-args
  "aaa 2023-06-15 bbb <p0>2021-01-10 ccc 2024-12-01 ddd <p1>2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically)))

;;; Test: datetime format

(defconst cpo-date-chrono-test-datetime-text
  "log 2023-01-15T10:30:00 then 2021-06-01 09:00 and 2024-03-10T18:22:45 end")

(ert-deftest cpo-date-chrono-test-datetime-collect ()
  "Collecting datetime format finds datetime strings in chronological order."
  (with-temp-buffer
    (insert cpo-date-chrono-test-datetime-text)
    (let ((dates (cpo-date--collect-dates-in-buffer 'datetime)))
      (should (= 3 (length dates)))
      ;; Chronological order: 2021-06-01 09:00, 2023-01-15T10:30:00, 2024-03-10T18:22:45
      (should (string-prefix-p "2021-06-01" (car (nth 0 dates))))
      (should (string-prefix-p "2023-01-15" (car (nth 1 dates))))
      (should (string-prefix-p "2024-03-10" (car (nth 2 dates)))))))

(carettest-tesmo-test cpo-date-chrono-test-datetime-forward
  "<p0>log 2023-01-15T10:30:00 then <p1>2021-06-01 09:00 and 2024-03-10T18:22:45 end"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 1 :format 'datetime)))

(carettest-tesmo-test cpo-date-chrono-test-datetime-absolute-last
  "<p0>log 2023-01-15T10:30:00 then 2021-06-01 09:00 and <p1>2024-03-10T18:22:45 end"
  (lambda () (cpo-date-move-chronologically :absolute 2 :format 'datetime)))

;;; Test: direction backward with negative relative (double negative = forward)

(carettest-tesmo-test cpo-date-chrono-test-backward-negative-relative
  "aaa <p1>2023-06-15 bbb <p0>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'backward :relative -2)))

;;; Test: point inside a date, relative forward

(carettest-tesmo-test cpo-date-chrono-test-inside-date-forward
  "aaa 2023-06-15 bbb 2021<p0>-01-10 ccc 2024-12-01 ddd <p1>2022-03-20 eee"
  (lambda () (cpo-date-move-chronologically :direction 'forward :relative 1)))

;;; test-cpo-date-chronological.el ends here
