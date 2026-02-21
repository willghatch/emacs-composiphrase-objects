#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-outline-movements.el --- Generator for outline movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'cpo-outline)

;; Generate tests for outline movement functions
(carettest-tesmo-generate-tests
 "* First heading
Some body text under first heading.
** Child heading 1
#+begin_src
hello
#+end_src
*** Grandchild 1
******** Great-grandchild half sibling A
text here
******* Great-grandchild half sibling B
   indented text here
**** Great-gandchild
*** Grandchild 2
** Child heading 2


**** Deep child
* Second heading
*** half-sibling situation
** Another child
*** Grandchild here
* Third heading"
 18  ; number of test positions
 '(cpo-outline-down-to-first-child
   cpo-outline-down-to-last-child
   cpo-outline-forward-half-or-full-sibling
   cpo-outline-backward-half-or-full-sibling
   cpo-outline-up-to-root
   ("forward-sibling-2" (lambda () (cpo-outline-forward-half-or-full-sibling 2)))
   ("backward-sibling-2" (lambda () (cpo-outline-backward-half-or-full-sibling 2))))
 "test-outline-movements"
 :output-file "_generated-tests-outline-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)
