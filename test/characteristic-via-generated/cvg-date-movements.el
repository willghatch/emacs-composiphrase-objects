;;; cvg-date-movements.el --- Characteristic tests for date and datetime movements -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-date-object)

;;; cpo-forward-date-beginning

;; Before first date: moves to beginning of nearest next date.
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-beginning__before-first
 "firs<p0>t <p1>2021-03-05 here\nsecond 2023-01-15 end"
 'cpo-forward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-date: moves forward to beginning of next date (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-beginning__cross-line
 "first 2021-03-0<p0>5 here\nsecond <p1>2023-01-15 end"
 'cpo-forward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After last date: no next date, stays in place.
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-beginning__at-last
 "first 2021-03-05 here\nsecond 2023-01-15 en<p1><p0>d"
 'cpo-forward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-date-beginning

;; After last date: moves to beginning of the last date.
(carettest-tesmo-test
 test-date-movements-cpo-backward-date-beginning__after-last
 "first 2021-03-05 here\nsecond <p1>2023-01-15 en<p0>d"
 'cpo-backward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-second-date: moves back to beginning of first date (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-backward-date-beginning__cross-line
 "first <p1>2021-03-05 here\nsecond 2023-01-1<p0>5 end"
 'cpo-backward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside first date: moves to beginning of current date.
(carettest-tesmo-test
 test-date-movements-cpo-backward-date-beginning__inside-first
 "first <p1><p0>2021-03-05 here\nsecond 2023-01-15 end"
 'cpo-backward-date-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-date-end

;; Before first date: moves to end of first date.
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-end__before-first
 "firs<p0>t 2021-03-05<p1> here\nsecond 2023-01-15 end"
 'cpo-forward-date-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After first date end: moves to end of next date (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-end__cross-line
 "first 2021-03-05<p0> here\nsecond 2023-01-15<p1> end"
 'cpo-forward-date-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside last date: no next date, stays in place.
(carettest-tesmo-test
 test-date-movements-cpo-forward-date-end__at-last
 "first 2021-03-05 here\nsecond 2023-01-1<p1><p0>5 end"
 'cpo-forward-date-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-date-end

;; Mid-second-date: moves back to end of first date (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-backward-date-end__cross-line
 "first 2021-03-05<p1> here\nsecond 2023-01-1<p0>5 end"
 'cpo-backward-date-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After second date: moves to end of second date.
(carettest-tesmo-test
 test-date-movements-cpo-backward-date-end__after-last
 "first 2021-03-05 here\nsecond 2023-01-15<p1> en<p0>d"
 'cpo-backward-date-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-date-beginning-2 (count=2)

;; From before first date: skips first, lands at beginning of second.
(carettest-tesmo-test
 test-date-movements-forward-date-beginning-2__skip-first
 "star<p0>t 2021-03-05 mid <p1>2022-06-30 end\nnext 2023-01-15 here"
 (lambda nil
   (cpo-forward-date-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From mid-first: skips remainder of first's vicinity, lands at third.
(carettest-tesmo-test
 test-date-movements-forward-date-beginning-2__cross-line
 "start 2021-03-05 mid 2022-06-3<p0>0 end\nnext <p1>2023-01-15 here"
 (lambda nil
   (cpo-forward-date-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-date-beginning-2 (count=2)

;; From after third date: moves back two dates, to beginning of first.
(carettest-tesmo-test
 test-date-movements-backward-date-beginning-2__skip-one
 "start <p1>2021-03-05 mid 2022-06-30 end\nnext 2023-01-1<p0>5 here"
 (lambda nil
   (cpo-backward-date-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From end of third: moves back two, to beginning of second (cross-line).
(carettest-tesmo-test
 test-date-movements-backward-date-beginning-2__cross-line
 "start 2021-03-05 mid <p1>2022-06-30 end\nnext 2023-01-15<p0> here"
 (lambda nil
   (cpo-backward-date-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-date-end-2 (count=2)

;; From before first date: skips end of first, lands at end of second.
(carettest-tesmo-test
 test-date-movements-forward-date-end-2__skip-first
 "star<p0>t 2021-03-05 mid 2022-06-30<p1> end\nnext 2023-01-15 here"
 (lambda nil
   (cpo-forward-date-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From mid-first: lands at end of third date (cross-line).
(carettest-tesmo-test
 test-date-movements-forward-date-end-2__cross-line
 "start 2021-03-05 mid 2022-06-3<p0>0 end\nnext 2023-01-15<p1> here"
 (lambda nil
   (cpo-forward-date-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-date-end-2 (count=2)

;; From after third date: moves back two date-ends, to end of first.
(carettest-tesmo-test
 test-date-movements-backward-date-end-2__skip-one
 "start 2021-03-05<p1> mid 2022-06-30 end\nnext 2023-01-1<p0>5 here"
 (lambda nil
   (cpo-backward-date-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From end of third date: moves back two date-ends, to end of first.
(carettest-tesmo-test
 test-date-movements-backward-date-end-2__cross-line
 "start 2021-03-05<p1> mid 2022-06-30 end\nnext 2023-01-15<p0> here"
 (lambda nil
   (cpo-backward-date-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-datetime-beginning

;; Before first datetime: moves to beginning of nearest next datetime.
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-beginning__before-first
 "lo<p0>g <p1>2023-01-15T10:30:00 here\nstamp 2024-03-10T18:22:45 end"
 'cpo-forward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-first-datetime: moves to beginning of next datetime (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-beginning__cross-line
 "log 2023-01-15T10:30:<p0>00 here\nstamp <p1>2024-03-10T18:22:45 end"
 'cpo-forward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After last datetime: no next datetime, stays in place.
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-beginning__at-last
 "log 2023-01-15T10:30:00 here\nstamp 2024-03-10T18:22:45 en<p1><p0>d"
 'cpo-forward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-datetime-beginning

;; After last datetime: moves to beginning of last datetime.
(carettest-tesmo-test
 test-date-movements-cpo-backward-datetime-beginning__after-last
 "log 2023-01-15T10:30:00 here\nstamp <p1>2024-03-10T18:22:45 en<p0>d"
 'cpo-backward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside second-datetime: moves to beginning of same (second) datetime.
(carettest-tesmo-test
 test-date-movements-cpo-backward-datetime-beginning__cross-line
 "log 2023-01-15T10:30:00 here\nstamp <p1>2024-03-10T18:22:<p0>45 end"
 'cpo-backward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Inside first datetime: stays at beginning of current datetime.
(carettest-tesmo-test
 test-date-movements-cpo-backward-datetime-beginning__inside-first
 "log <p1><p0>2023-01-15T10:30:00 here\nstamp 2024-03-10T18:22:45 end"
 'cpo-backward-datetime-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-datetime-end

;; Before first datetime: moves to end of first datetime.
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-end__before-first
 "lo<p0>g 2023-01-15T10:30:00<p1> here\nstamp 2024-03-10T18:22:45 end"
 'cpo-forward-datetime-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After first datetime end: moves to end of second datetime (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-end__cross-line
 "log 2023-01-15T10:30:00<p0> here\nstamp 2024-03-10T18:22:45<p1> end"
 'cpo-forward-datetime-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After last datetime: no next, stays in place.
(carettest-tesmo-test
 test-date-movements-cpo-forward-datetime-end__at-last
 "log 2023-01-15T10:30:00 here\nstamp 2024-03-10T18:22:45<p1><p0> end"
 'cpo-forward-datetime-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-datetime-end

;; Mid-second-datetime: moves to end of first datetime (cross-line).
(carettest-tesmo-test
 test-date-movements-cpo-backward-datetime-end__cross-line
 "log 2023-01-15T10:30:00<p1> here\nstamp 2024-03-10T18:22:<p0>45 end"
 'cpo-backward-datetime-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After second datetime: moves to end of second datetime.
(carettest-tesmo-test
 test-date-movements-cpo-backward-datetime-end__after-last
 "log 2023-01-15T10:30:00 here\nstamp 2024-03-10T18:22:45<p1> en<p0>d"
 'cpo-backward-datetime-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-datetime-beginning-3 (count=3)

;; From before first datetime: skips two, lands at third.
(carettest-tesmo-test
 test-date-movements-forward-datetime-beginning-3__skip-two
 "a<p0> 2023-01-15T10:30:00 b\nc 2022-12-01T09:15:30 d <p1>2024-03-10T18:22:45 end"
 (lambda nil
   (cpo-forward-datetime-beginning 3))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From mid-first: skips next two, lands at third (cross-line).
(carettest-tesmo-test
 test-date-movements-forward-datetime-beginning-3__cross-line
 "a 2023-01-15T10:30:<p0>00 b\nc 2022-12-01T09:15:30 d <p1>2024-03-10T18:22:45 end"
 (lambda nil
   (cpo-forward-datetime-beginning 3))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-datetime-end-2 (count=2)

;; From after third datetime: moves back two ends, to end of first.
(carettest-tesmo-test
 test-date-movements-backward-datetime-end-2__skip-one
 "a 2023-01-15T10:30:00<p1> b\nc 2022-12-01T09:15:30 d 2024-03-10T18:22:<p0>45 end"
 (lambda nil
   (cpo-backward-datetime-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From end of third datetime: moves back two ends, to end of first.
(carettest-tesmo-test
 test-date-movements-backward-datetime-end-2__from-end
 "a 2023-01-15T10:30:00<p1> b\nc 2022-12-01T09:15:30 d 2024-03-10T18:22:45<p0> end"
 (lambda nil
   (cpo-backward-datetime-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-datetime-end-2 (count=2)

;; From before first: skips end of first, lands at end of second.
(carettest-tesmo-test
 test-date-movements-forward-datetime-end-2__skip-first
 "a<p0> 2023-01-15T10:30:00 b\nc 2022-12-01T09:15:30<p1> d 2024-03-10T18:22:45 end"
 (lambda nil
   (cpo-forward-datetime-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From mid-second: skips remaining of second, lands at end of third.
(carettest-tesmo-test
 test-date-movements-forward-datetime-end-2__cross-line
 "a 2023-01-15T10:30:00 b\nc 2022-12-01T09:15:<p0>30 d 2024-03-10T18:22:45<p1> end"
 (lambda nil
   (cpo-forward-datetime-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-datetime-beginning-2 (count=2)

;; From after third datetime: moves back two beginnings, to start of second.
(carettest-tesmo-test
 test-date-movements-backward-datetime-beginning-2__skip-one
 "a 2023-01-15T10:30:00 b\nc <p1>2022-12-01T09:15:30 d 2024-03-10T18:22:45 en<p0>d"
 (lambda nil
   (cpo-backward-datetime-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; From mid-third: moves back two, to start of second (cross-line).
(carettest-tesmo-test
 test-date-movements-backward-datetime-beginning-2__cross-line
 "a 2023-01-15T10:30:00 b\nc <p1>2022-12-01T09:15:30 d 2024-03-10T18:22:<p0>45 end"
 (lambda nil
   (cpo-backward-datetime-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-expand-region-to-cpo-date

;; Point inside date, no initial region: expands to cover the full date.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-date__inside-date
 "text <m1>2023-01-<p0>15<p1> more text"
 'cpo-expand-region-to-cpo-date
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Point just past end of date, no initial region: still expands to cover the full date.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-date__point-after-end
 "text <m1>2023-01-15<p1><p0> more text"
 'cpo-expand-region-to-cpo-date
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Both point and mark inside same date: expands to cover the full date.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-date__mark-and-point-inside
 "text <m1>2023-<m0>01-<p0>15<p1> more text"
 'cpo-expand-region-to-cpo-date
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; cpo-expand-region-to-cpo-datetime

;; Point inside datetime, no initial region: expands to cover the full datetime.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-datetime__inside-datetime
 "log <m1>2023-01-15T10:30:<p0>00<p1> here"
 'cpo-expand-region-to-cpo-datetime
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Point just past end of datetime, no initial region: still expands to cover the full datetime.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-datetime__point-after-end
 "log <m1>2023-01-15T10:30:00<p1><p0> here"
 'cpo-expand-region-to-cpo-datetime
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Both point and mark inside same datetime: expands to cover the full datetime.
(carettest-tesmo-test
 test-date-movements-cpo-expand-region-to-cpo-datetime__mark-and-point-inside
 "log <m1>2023-01-15T<m0>10:30:<p0>00<p1> here"
 'cpo-expand-region-to-cpo-datetime
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(provide 'cvg-date-movements)
;;; cvg-date-movements.el ends here
