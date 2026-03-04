;;; test-cpo-date-transpose.el --- Tests for date/datetime transpose functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'carettest-tesmut)
(require 'cpo-date-object)

;;;; Normal (buffer-order) date transpose

;; Forward transpose: swap date at point with the next date in buffer order.
(carettest-tesmut-test
 test-transpose-date-forward_basic
 :before "aaa <p>2023-01-15 bbb 2024-06-20 ccc"
 :after "aaa 2024-06-20 bbb <p>2023-01-15 ccc"
 :function 'cpo-transpose-date-forward)

;; Forward transpose from middle of date.
(carettest-tesmut-test
 test-transpose-date-forward_from-middle
 :before "aaa 2023<p>-01-15 bbb 2024-06-20 ccc"
 :after "aaa 2024-06-20 bbb 2023<p>-01-15 ccc"
 :function 'cpo-transpose-date-forward)

;; Backward transpose: swap date at point with previous date in buffer order.
(carettest-tesmut-test
 test-transpose-date-backward_basic
 :before "aaa 2023-01-15 bbb <p>2024-06-20 ccc"
 :after "aaa <p>2024-06-20 bbb 2023-01-15 ccc"
 :function 'cpo-transpose-date-backward)

;; Backward transpose from middle of date.
(carettest-tesmut-test
 test-transpose-date-backward_from-middle
 :before "aaa 2023-01-15 bbb 2024<p>-06-20 ccc"
 :after "aaa 2024<p>-06-20 bbb 2023-01-15 ccc"
 :function 'cpo-transpose-date-backward)

;;;; Normal (buffer-order) datetime transpose

(carettest-tesmut-test
 test-transpose-datetime-forward_basic
 :before "log <p>2023-01-15T10:30 then 2024-06-20T14:00 end"
 :after "log 2024-06-20T14:00 then <p>2023-01-15T10:30 end"
 :function 'cpo-transpose-datetime-forward)

(carettest-tesmut-test
 test-transpose-datetime-forward_with-seconds
 :before "log <p>2023-01-15T10:30:00 then 2024-06-20T14:00:59 end"
 :after "log 2024-06-20T14:00:59 then <p>2023-01-15T10:30:00 end"
 :function 'cpo-transpose-datetime-forward)

(carettest-tesmut-test
 test-transpose-datetime-backward_basic
 :before "log 2023-01-15T10:30 then <p>2024-06-20T14:00 end"
 :after "log <p>2024-06-20T14:00 then 2023-01-15T10:30 end"
 :function 'cpo-transpose-datetime-backward)

;;;; Chronological date transpose
;; Buffer order:  2023-06-15  2021-01-10  2024-12-01  2022-03-20
;; Chrono order:  2021-01-10(0) 2022-03-20(1) 2023-06-15(2) 2024-12-01(3)

;; Forward chronological transpose from 2021-01-10 (chrono idx 0) swaps with
;; 2022-03-20 (chrono idx 1).  These are not buffer-adjacent.
(carettest-tesmut-test
 test-transpose-date-forward-chronological_basic
 :before "aaa 2023-06-15 bbb <p>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
 :after "aaa 2023-06-15 bbb 2022-03-20 ccc 2024-12-01 ddd <p>2021-01-10 eee"
 :function 'cpo-transpose-date-forward-chronological)

;; Backward chronological transpose from 2024-12-01 (chrono idx 3) swaps with
;; 2023-06-15 (chrono idx 2).
(carettest-tesmut-test
 test-transpose-date-backward-chronological_basic
 :before "aaa 2023-06-15 bbb 2021-01-10 ccc <p>2024-12-01 ddd 2022-03-20 eee"
 :after "aaa <p>2024-12-01 bbb 2021-01-10 ccc 2023-06-15 ddd 2022-03-20 eee"
 :function 'cpo-transpose-date-backward-chronological)

;; Forward chronological from middle of date.
(carettest-tesmut-test
 test-transpose-date-forward-chronological_from-middle
 :before "aaa 2023-06-15 bbb 2021<p>-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
 :after "aaa 2023-06-15 bbb 2022-03-20 ccc 2024-12-01 ddd 2021<p>-01-10 eee"
 :function 'cpo-transpose-date-forward-chronological)

;; Backward chronological from 2022-03-20 (chrono idx 1) swaps with
;; 2021-01-10 (chrono idx 0).
(carettest-tesmut-test
 test-transpose-date-backward-chronological_from-last-buffer
 :before "aaa 2023-06-15 bbb 2021-01-10 ccc 2024-12-01 ddd <p>2022-03-20 eee"
 :after "aaa 2023-06-15 bbb <p>2022-03-20 ccc 2024-12-01 ddd 2021-01-10 eee"
 :function 'cpo-transpose-date-backward-chronological)

;; No-op at chronological end (2024-12-01 is chrono last, can't go forward).
(carettest-tesmut-test
 test-transpose-date-forward-chronological_at-end
 :before "aaa 2023-06-15 bbb 2021-01-10 ccc <p>2024-12-01 ddd 2022-03-20 eee"
 :after "aaa 2023-06-15 bbb 2021-01-10 ccc <p>2024-12-01 ddd 2022-03-20 eee"
 :function 'cpo-transpose-date-forward-chronological)

;; No-op at chronological start (2021-01-10 is chrono first, can't go backward).
(carettest-tesmut-test
 test-transpose-date-backward-chronological_at-start
 :before "aaa 2023-06-15 bbb <p>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
 :after "aaa 2023-06-15 bbb <p>2021-01-10 ccc 2024-12-01 ddd 2022-03-20 eee"
 :function 'cpo-transpose-date-backward-chronological)

;;;; Chronological datetime transpose

;; Buffer order:  2023-01-15T10:30  2021-06-01 09:00  2024-03-10T18:22:45
;; Chrono order:  2021-06-01 09:00(0) 2023-01-15T10:30(1) 2024-03-10T18:22:45(2)
(carettest-tesmut-test
 test-transpose-datetime-forward-chronological_basic
 :before "log <p>2023-01-15T10:30 then 2021-06-01 09:00 and 2024-03-10T18:22:45 end"
 :after "log 2024-03-10T18:22:45 then 2021-06-01 09:00 and <p>2023-01-15T10:30 end"
 :function 'cpo-transpose-datetime-forward-chronological)

;; Backward chronological from 2023-01-15T10:30 (chrono idx 1) swaps with
;; 2021-06-01 09:00 (chrono idx 0).  Point follows the original date.
(carettest-tesmut-test
 test-transpose-datetime-backward-chronological_basic
 :before "log <p>2023-01-15T10:30 then 2021-06-01 09:00 and 2024-03-10T18:22:45 end"
 :after "log 2021-06-01 09:00 then <p>2023-01-15T10:30 and 2024-03-10T18:22:45 end"
 :function 'cpo-transpose-datetime-backward-chronological)

;; Backward chronological from 2024-03-10T18:22:45 (chrono idx 2) swaps with
;; 2023-01-15T10:30 (chrono idx 1).
(carettest-tesmut-test
 test-transpose-datetime-backward-chronological_last
 :before "log 2023-01-15T10:30 then 2021-06-01 09:00 and <p>2024-03-10T18:22:45 end"
 :after "log <p>2024-03-10T18:22:45 then 2021-06-01 09:00 and 2023-01-15T10:30 end"
 :function 'cpo-transpose-datetime-backward-chronological)

;;; test-cpo-date-transpose.el ends here
