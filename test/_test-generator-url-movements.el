#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-url-movements.el --- Generator for url movements function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'carettest-tesmut-generator)
(require 'cpo-url-object)

(setq urls-test-string
      "
Check out http://test.org and ftp://files.example.com/data.
See https://duckduckgo.com/?t=ffab&q=aardvark&ia=images&iax=images for details.
Email me at user@domain.com for questions.
Final URL: https://github.com/user/repo.git for code.
")

;; Generate tests for URL movement functions
(carettest-tesmo-generate-tests
 urls-test-string
 12  ; number of test positions
 '(cpo-forward-url-beginning
   cpo-backward-url-beginning
   cpo-forward-url-end
   cpo-backward-url-end
   cpo-expand-region-to-url
   ("forward-url-beginning-2" (lambda () (cpo-forward-url-beginning 2)))
   ("backward-url-end-2" (lambda () (cpo-backward-url-end 2))))
 "test-url-movements"
 :output-file "_generated-tests-url-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)

;; Generate tests for URL modification functions using cpo-tesmut
(carettest-tesmut-generate-tests
 urls-test-string
 10  ; number of test positions
 '(cpo-transpose-url-forward
   cpo-transpose-url-backward)
 "test-url-modifications"
 :output-file "_generated-tests-url-modifications.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.5
 :transient-mark-mode-prob 0.8)
