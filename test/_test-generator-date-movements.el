#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-date-movements.el --- Generator for date movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'cpo-date-object)

;; Generate tests for date movement functions
(carettest-tesmo-generate-tests
 "2021-03-05 is the first date; see also (2023-01-15) and 2023-12-25.
Multiple lines with dates like 2022-06-30 and random text.
Final date here: 2024-03-10 at the end."
 15  ; number of test positions
 '(cpo-forward-date-beginning
   cpo-backward-date-beginning
   cpo-forward-date-end
   cpo-backward-date-end
   ("forward-date-beginning-2" (lambda () (cpo-forward-date-beginning 2)))
   ("backward-date-beginning-2" (lambda () (cpo-backward-date-beginning 2)))
   ("forward-date-end-2" (lambda () (cpo-forward-date-end 2)))
   ("backward-date-end-2" (lambda () (cpo-backward-date-end 2))))
 "test-date-movements"
 :output-file "_generated-tests-date-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.8)

;; Generate tests for datetime movement functions
(carettest-tesmo-generate-tests
 "2023-01-15T10:30:00 starts the log; error at (2023-02-20 14:45) noted.
Log entry: 2022-12-01T09:15:30 - System started successfully.
Final timestamp: 2024-03-10T18:22:45 in the logs."
 12  ; number of test positions
 '(cpo-forward-datetime-beginning
   cpo-backward-datetime-beginning
   cpo-forward-datetime-end
   cpo-backward-datetime-end
   ("forward-datetime-beginning-3" (lambda () (cpo-forward-datetime-beginning 3)))
   ("backward-datetime-end-2" (lambda () (cpo-backward-datetime-end 2)))
   ("forward-datetime-end-2" (lambda () (cpo-forward-datetime-end 2)))
   ("backward-datetime-beginning-2" (lambda () (cpo-backward-datetime-beginning 2))))
 "test-datetime-movements"
 :output-file "_generated-tests-datetime-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.9)
