#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-text-object-movements.el --- Generator for text object movements function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'carettest-tesmut-generator)
(require 'cpo-text-object-stuff)

;; Has mix of sentences on different or same line.  Multiple paragraphs.  But short sentences.  Very bad gutted lorem ipsum.
(setq epsom
      "Lorem Epsom dolor sit amet, consectetur adipiscing elit.
Sed do et dolore magna aliqua. Ut enim veniam, quis consequat.

Duis aute irure dolor. Excepteur sint occaecat cupidatat non proident, sunt in culpa. Pellentesque habitant.

Vestibulum ante ipsum cubilia curae; Mauris sit amet massa. Suspendisse potenti nisi lacus.
Sed facilisis gravida neque convallis.

")

(setq movements
      '(cpo-forward-word-beginning
        cpo-backward-word-beginning
        cpo-forward-word-end
        cpo-backward-word-end
        cpo-forward-symbol-beginning
        cpo-backward-symbol-beginning
        cpo-forward-symbol-end
        cpo-backward-symbol-end
        ("forward-word-beginning-3" (lambda () (cpo-forward-word-beginning 3)))
        ("backward-symbol-end-2" (lambda () (cpo-backward-symbol-end 2)))
        cpo-forward-line-beginning
        cpo-backward-line-beginning
        cpo-forward-line-end
        cpo-backward-line-end
        cpo-next-line
        cpo-prev-line
        ("forward-line-beginning-2" (lambda () (cpo-forward-line-beginning 2)))
        ("prev-line-3" (lambda () (cpo-prev-line 3)))
        cpo-forward-sentence-beginning
        cpo-backward-sentence-beginning
        cpo-forward-sentence-end
        cpo-backward-sentence-end
        cpo-forward-paragraph-beginning
        cpo-backward-paragraph-beginning
        cpo-forward-paragraph-end
        cpo-backward-paragraph-end
        ("forward-sentence-beginning-2" (lambda () (cpo-forward-sentence-beginning 2)))
        ("backward-paragraph-end-2" (lambda () (cpo-backward-paragraph-end 2)))))

(carettest-tesmo-generate-tests
 epsom
 5  ; number of test positions
 movements
 "test-text-object-movements"
 :output-file "_generated-tests-text-object-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.9)

(carettest-tesmo-generate-tests
 "The quick_brown fox jumps over 42 lazy-dogs.
camelCaseVar and some_symbol: HTTP_REQUEST_TIMEOUT = 30.
Final line with (test123) and \"quoted-text\" for API testing."
 5  ; number of test positions
 movements
 "test-text-object-movements"
 :output-file "_generated-tests-text-object-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.9)

;; Generate tests for region operation functions
(carettest-tesmut-generate-tests
 epsom
 5  ; number of test positions
 '(("expand-region-with-final-newline" (lambda () (cpo-expand-region-to-fill-lines t)))
   ("expand-region-without-final-newline" (lambda () (cpo-expand-region-to-fill-lines nil))))
 "test-region-operations"
 :output-file "_generated-tests-region-operations.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.8
 :transient-mark-mode-prob 1.0)



