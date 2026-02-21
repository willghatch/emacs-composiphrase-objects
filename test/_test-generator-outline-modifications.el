#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-outline-modifications.el --- Generator for outline modification function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmut-generator)
(require 'cpo-outline)

;; Generate tests for outline modification functions
(carettest-tesmut-generate-tests
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
 12  ; number of test positions
 '(cpo-outline-add-heading-above
   cpo-outline-add-heading-below
   cpo-outline-add-child-heading
   ("add-child-at-index-1" (lambda () (cpo-outline-add-child-heading 1)))
   cpo-outline-forward-slurp-heading
   cpo-outline-forward-barf-heading
   ("add-ancestor-sibling" (lambda () (cpo-outline-add-ancestor-next-sibling-heading 1)))
   cpo-outline-transpose-sibling-forward
   cpo-outline-transpose-sibling-backward
   ;; Sequence tests for complex operations
   (list cpo-outline-add-heading-below cpo-outline-forward-slurp-heading)
   (list cpo-outline-add-child-heading cpo-outline-forward-barf-heading))
 "test-outline-modifications"
 :output-file "_generated-tests-outline-modifications.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.7)
