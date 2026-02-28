;;; test-cpo-org-structure-block.el --- Tests for cpo-org-structure-block -*- lexical-binding: t; -*-

(require 'cpo-org-structure-block)
(require 'carettest-tesmo)
(require 'carettest-tesmut)

;;; Forward-beginning movement tests

(carettest-tesmo-test
 test-osb-forward-beginning-basic
 "<p0>Some text before
<p1>#+begin_src emacs-lisp
  (message \"hello\")
#+end_src
Some text after
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-beginning-from-first-to-second
 "<p0>#+begin_src python
print('first')
#+end_src

<p1>#+begin_quote
A wise quote.
#+end_quote
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-beginning-from-inside-block
 "#+begin_src python
print<p0>('first')
#+end_src

<p1>#+begin_quote
A wise quote.
#+end_quote
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-beginning-multiple-types_first-step
 "<p0>#+begin_example
example content
#+end_example

<p1>#+begin_src ruby
puts 'hello'
#+end_src

#+begin_quote
wise words
#+end_quote
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-beginning-multiple-types_second-step
 "#+begin_example
example content
#+end_example

<p0>#+begin_src ruby
puts 'hello'
#+end_src

<p1>#+begin_quote
wise words
#+end_quote
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

;;; Backward-beginning movement tests

(carettest-tesmo-test
 test-osb-backward-beginning-basic
 "#+begin_src python
print('first')
#+end_src

<p1>#+begin_quote
A wise quote.
#+end_quote
<p0>"
 'cpo-org-structure-block-backward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-beginning-from-second-to-first
 "<p1>#+begin_src python
print('first')
#+end_src

<p0>#+begin_quote
A wise quote.
#+end_quote
"
 'cpo-org-structure-block-backward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-beginning-from-inside-block_to-current
 "#+begin_src python
print('first')
#+end_src

<p1>#+begin_quote
A wise<p0> quote.
#+end_quote
"
 'cpo-org-structure-block-backward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-beginning-from-inside-block_to-previous
 "<p1>#+begin_src python
print('first')
#+end_src

<p0>#+begin_quote
A wise quote.
#+end_quote
"
 'cpo-org-structure-block-backward-beginning
 :setup (org-mode))

;;; Forward-end movement tests

(carettest-tesmo-test
 test-osb-forward-end-basic
 "<p0>Some text before\n#+begin_src emacs-lisp\n  (message \"hello\")\n#+end_src\n<p1>Some text after\n"
 'cpo-org-structure-block-forward-end
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-end-from-inside-block
 "#+begin_src python\nprint<p0>('first')\n#+end_src\n<p1>\n#+begin_quote\nA wise quote.\n#+end_quote\n"
 'cpo-org-structure-block-forward-end
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-end-from-first-to-second
 "#+begin_src python\nprint('first')\n#+end_src\n<p0>\n#+begin_quote\nA wise quote.\n#+end_quote\n<p1>"
 'cpo-org-structure-block-forward-end
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-end-at-end-of-buffer
 "Some text before\n#+begin_src emacs-lisp\n  (message \"hello\")\n#+end_src\nSome text after\n<p0><p1>"
 'cpo-org-structure-block-forward-end
 :setup (org-mode))

;;; Backward-end movement tests

(carettest-tesmo-test
 test-osb-backward-end-basic
 "#+begin_src python\nprint('first')\n#+end_src\n<p1>\n#+begin_quote\nA wise quote.\n#+end_quote\n<p0>"
 'cpo-org-structure-block-backward-end
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-end-from-between-blocks
 "#+begin_src python\nprint('first')\n#+end_src\n<p1><p0>\n#+begin_quote\nA wise quote.\n#+end_quote\n"
 'cpo-org-structure-block-backward-end
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-end-at-beginning-of-buffer
 "<p0><p1>Some text before\n#+begin_src emacs-lisp\n  (message \"hello\")\n#+end_src\nSome text after\n"
 'cpo-org-structure-block-backward-end
 :setup (org-mode))

;;; Expand region tests

(carettest-tesmo-test
 test-osb-expand-region
 "Some text before
<p1>#+begin_src emacs-lisp
  (message<p0> \"hello\")
#+end_src
<m1>Some text after
"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-expand-region-from-begin-line
 "Some text before
<p0><p1>#+begin_src emacs-lisp
  (message \"hello\")
#+end_src
<m1>Some text after
"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-expand-region-from-end-line
 "Some text before
<p1>#+begin_src emacs-lisp
  (message \"hello\")
<p0>#+end_src
<m1>Some text after
"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

;;; Inner expand region tests

(carettest-tesmo-test
 test-osb-expand-region-inner
 "Some text before
#+begin_src emacs-lisp
<p1>  (message<p0> \"hello\")
<m1>#+end_src
Some text after
"
 'cpo-org-structure-block-expand-region-inner
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-expand-region-inner-empty-block
 "<p0>#+begin_src
<p1><m1>#+end_src
"
 'cpo-org-structure-block-expand-region-inner
 :setup (org-mode))

;;; Nested block tests

(carettest-tesmo-test
 test-osb-nested-expand-region-from-inner
 "#+begin_src org
Some org content
<p1>#+begin_src emacs-lisp
  (+ 1 2)<p0>
#+end_src
<m1>More org content
#+end_src
"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-nested-expand-region-from-between
 "<p1>#+begin_src org
Some org content<p0>
#+begin_src emacs-lisp
  (+ 1 2)
#+end_src
More org content
#+end_src
<m1>"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-nested-forward-beginning-finds-block
 "<p0><p1>#+begin_src org
Some org content
#+begin_src emacs-lisp
  (+ 1 2)
#+end_src
More org content
#+end_src
"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

;;; Open function tests

(carettest-tesmut-test
 test-osb-open-basic
 :before
 "<p>"
 :after
 "#+begin_src
<p>
#+end_src
"
 :function (lambda () (cpo-org-structure-block-open))
 :setup (org-mode))

(carettest-tesmut-test
 test-osb-open-custom-type
 :before
 "<p>"
 :after
 "#+begin_quote
<p>
#+end_quote
"
 :function (lambda () (cpo-org-structure-block-open "quote"))
 :setup (org-mode))

(carettest-tesmut-test
 test-osb-open-preserves-surrounding-text
 :before
 "Some existing text
<p>"
 :after
 "Some existing text
#+begin_src
<p>
#+end_src
"
 :function (lambda () (cpo-org-structure-block-open))
 :setup (org-mode))

;;; Edge case tests

(carettest-tesmo-test
 test-osb-no-block-at-point-expand-region
 "<p0><p1>Just some text with no blocks.
"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-no-block-at-point-expand-region-inner
 "<p0><p1>Just some text with no blocks.
"
 'cpo-org-structure-block-expand-region-inner
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-forward-beginning-at-end-of-buffer
 "Some text before
#+begin_src emacs-lisp
  (message \"hello\")
#+end_src
Some text after
<p0><p1>"
 'cpo-org-structure-block-forward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-backward-beginning-at-beginning-of-buffer
 "<p0><p1>Some text before
#+begin_src emacs-lisp
  (message \"hello\")
#+end_src
Some text after
"
 'cpo-org-structure-block-backward-beginning
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-different-block-types-respected
 "<p0><p1>#+begin_src
content
#+end_quote
more
#+end_src
<m1>"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(carettest-tesmo-test
 test-osb-case-insensitive-begin-end
 "<p0><p1>#+begin_SRC
content
#+end_src
<m1>"
 'cpo-org-structure-block-expand-region
 :setup (org-mode))

(provide 'test-cpo-org-structure-block)
;;; test-cpo-org-structure-block.el ends here
