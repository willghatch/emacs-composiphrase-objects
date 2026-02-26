;;; cvg-text-modifications.el --- Characteristic tests for text modifications -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'cpo-text-object-stuff)

;;; cpo-transpose-word-forward

;; Cursor at end of first word, swaps with next word.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-forward__at-mid
 :before "foo<p> bar baz"
 :after "bar foo<p> baz"
 :function 'cpo-transpose-word-forward
 :transient-mark-mode nil)

;; Cursor inside a word mid-buffer, swaps current word with next.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-forward__inside-word
 :before "one tw<p>o three"
 :after "one three tw<p>o"
 :function 'cpo-transpose-word-forward
 :transient-mark-mode nil)

;; Cursor inside second-to-last word, swaps with last word.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-forward__near-end
 :before "alpha be<p>ta gamma."
 :after "alpha gamma be<p>ta."
 :function 'cpo-transpose-word-forward
 :transient-mark-mode nil)

;;; cpo-transpose-word-backward

;; Cursor after a word, swaps it with previous.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-backward__at-mid
 :before "foo bar<p> baz"
 :after "bar<p> foo baz"
 :function 'cpo-transpose-word-backward
 :transient-mark-mode nil)

;; Cursor inside a word, swaps it with preceding word.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-backward__inside-word
 :before "one two thr<p>ee"
 :after "one thr<p>ee two"
 :function 'cpo-transpose-word-backward
 :transient-mark-mode nil)

;; Cursor after last word, swaps it with second-to-last.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-word-backward__at-last
 :before "alpha beta gamma<p>"
 :after "alpha gamma<p> beta"
 :function 'cpo-transpose-word-backward
 :transient-mark-mode nil)

;;; cpo-transpose-symbol-forward

;; Cursor inside a symbol, swaps it with next symbol.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-forward__inside-symbol
 :before "The quick_b<p>rown fox"
 :after "The fox quick_b<p>rown"
 :function 'cpo-transpose-symbol-forward
 :transient-mark-mode nil)

;; Cursor at start of a symbol, swaps it with next.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-forward__at-start
 :before "alpha <p>beta gamma"
 :after "alpha gamma <p>beta"
 :function 'cpo-transpose-symbol-forward
 :transient-mark-mode nil)

;; Cursor inside second-to-last symbol, swaps with last symbol.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-forward__second-to-last
 :before "foo ba<p>r baz"
 :after "foo baz ba<p>r"
 :function 'cpo-transpose-symbol-forward
 :transient-mark-mode nil)

;;; cpo-transpose-symbol-backward

;; Cursor inside a symbol, swaps it with preceding symbol.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-backward__inside-symbol
 :before "The quick<p>_brown fox"
 :after "quick<p>_brown The fox"
 :function 'cpo-transpose-symbol-backward
 :transient-mark-mode nil)

;; Cursor at end of line after last symbol, swaps with previous.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-backward__at-end
 :before "foo bar baz<p>"
 :after "foo baz<p> bar"
 :function 'cpo-transpose-symbol-backward
 :transient-mark-mode nil)

;; Cursor inside middle symbol, swaps with previous.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-symbol-backward__mid-symbol
 :before "alpha beta gamm<p>a"
 :after "alpha gamm<p>a beta"
 :function 'cpo-transpose-symbol-backward
 :transient-mark-mode nil)

;;; cpo-transpose-sentence-forward

;; Cursor inside first sentence, swaps it with next sentence.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-sentence-forward__first-sentence
 :before "Hello world<p>.  Goodbye world."
 :after "Goodbye world.  Hello world<p>."
 :function 'cpo-transpose-sentence-forward
 :transient-mark-mode nil)

;; Cursor inside middle sentence, swaps with next.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-sentence-forward__mid-sentence
 :before "First.  Sec<p>ond.  Third."
 :after "First.  Third.  Sec<p>ond."
 :function 'cpo-transpose-sentence-forward
 :transient-mark-mode nil)

;;; cpo-transpose-sentence-backward

;; Cursor inside second sentence, swaps it with previous.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-sentence-backward__second-sentence
 :before "First.  Sec<p>ond."
 :after "Sec<p>ond.  First."
 :function 'cpo-transpose-sentence-backward
 :transient-mark-mode nil)

;; Cursor inside last of three sentences, swaps with previous.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-sentence-backward__last-sentence
 :before "First.  Second.  Thi<p>rd."
 :after "First.  Thi<p>rd.  Second."
 :function 'cpo-transpose-sentence-backward
 :transient-mark-mode nil)

;;; transpose-word-forward-2 (cpo-transpose-word-forward 2)

;; Cursor inside word, skips 2 words forward.
(carettest-tesmut-test
 test-text-modifications-transpose-word-forward-2__mid
 :before "one tw<p>o three four"
 :after "one three four tw<p>o"
 :function (lambda nil (cpo-transpose-word-forward 2))
 :transient-mark-mode nil)

;; Cursor inside first word, moves 2 ahead.
(carettest-tesmut-test
 test-text-modifications-transpose-word-forward-2__first
 :before "fo<p>o bar baz"
 :after "bar baz fo<p>o"
 :function (lambda nil (cpo-transpose-word-forward 2))
 :transient-mark-mode nil)

;;; transpose-symbol-backward-2 (cpo-transpose-symbol-backward 2)

;; Cursor inside symbol, swaps 2 symbols back.
(carettest-tesmut-test
 test-text-modifications-transpose-symbol-backward-2__mid
 :before "one two thr<p>ee"
 :after "thr<p>ee one two"
 :function (lambda nil (cpo-transpose-symbol-backward 2))
 :transient-mark-mode nil)

;; Cursor inside last symbol, swaps back 2.
(carettest-tesmut-test
 test-text-modifications-transpose-symbol-backward-2__last
 :before "alpha beta gamm<p>a"
 :after "gamm<p>a alpha beta"
 :function (lambda nil (cpo-transpose-symbol-backward 2))
 :transient-mark-mode nil)

;;; cpo-transpose-line-forward

;; Cursor on first line, swaps with second.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-line-forward__first-line
 :before "First line of tex<p>t
Second line with content
Third line here"
 :after "Second line with content
First line of tex<p>t
Third line here"
 :function 'cpo-transpose-line-forward
 :transient-mark-mode nil)

;; Cursor on second line, swaps with third.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-line-forward__mid-line
 :before "First line of text
Seco<p>nd line with content
Third line here
Fourth line"
 :after "First line of text
Third line here
Seco<p>nd line with content
Fourth line"
 :function 'cpo-transpose-line-forward
 :transient-mark-mode nil)

;;; cpo-transpose-line-backward

;; Cursor on second line, swaps with first.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-line-backward__second-line
 :before "First line of text
S<p>econd line with content
Third line here"
 :after "S<p>econd line with content
First line of text
Third line here"
 :function 'cpo-transpose-line-backward
 :transient-mark-mode nil)

;; Cursor on third line, swaps with second.
(carettest-tesmut-test
 test-text-modifications-cpo-transpose-line-backward__third-line
 :before "First line of text
Second line with content
Third line her<p>e
Fourth line"
 :after "First line of text
Third line her<p>e
Second line with content
Fourth line"
 :function 'cpo-transpose-line-backward
 :transient-mark-mode nil)

;;; cpo-open-line-below

;; Open new line below first line, cursor moves to new empty line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-below__first-line
 :before "Fi<p>rst line of text
Second line with content
Third line here"
 :after "First line of text
<p>
Second line with content
Third line here"
 :function 'cpo-open-line-below
 :transient-mark-mode nil)

;; Open new line below second line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-below__mid-line
 :before "First line of text
S<p>econd line with content
Third line here"
 :after "First line of text
Second line with content
<p>
Third line here"
 :function 'cpo-open-line-below
 :transient-mark-mode nil)

;; Open new line below last line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-below__last-line
 :before "First line of text
Second line with content
Third line for test<p>ing"
 :after "First line of text
Second line with content
Third line for testing
<p>"
 :function 'cpo-open-line-below
 :transient-mark-mode nil)

;;; cpo-open-line-above

;; Open new line above first line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-above__first-line
 :before "Fi<p>rst line of text
Second line with content
Third line here"
 :after "<p>
First line of text
Second line with content
Third line here"
 :function 'cpo-open-line-above
 :transient-mark-mode nil)

;; Open new line above second line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-above__mid-line
 :before "First line of text
Sec<p>ond line with content
Third line here"
 :after "First line of text
<p>
Second line with content
Third line here"
 :function 'cpo-open-line-above
 :transient-mark-mode nil)

;; Open new line above last line.
(carettest-tesmut-test
 test-text-modifications-cpo-open-line-above__last-line
 :before "First line of text
Second line with content
Thi<p>rd line here"
 :after "First line of text
Second line with content
<p>
Third line here"
 :function 'cpo-open-line-above
 :transient-mark-mode nil)

;;; transpose-line-forward-2 (cpo-transpose-line-forward 2)

;; Cursor on first line, moves it 2 lines forward.
(carettest-tesmut-test
 test-text-modifications-transpose-line-forward-2__first-line
 :before "First line <p>of text
Second line with content
Third line here
Fourth line for testing"
 :after "Second line with content
Third line here
First line <p>of text
Fourth line for testing"
 :function (lambda nil (cpo-transpose-line-forward 2))
 :transient-mark-mode nil)

;; Cursor on second line, moves it 2 forward (hits end of buffer).
(carettest-tesmut-test
 test-text-modifications-transpose-line-forward-2__mid-line
 :before "First line of text
Second line with co<p>ntent
Third line here
Fourth line for testing"
 :after "First line of text
Third line here
Fourth line for testingSecond line with co<p>ntent
"
 :function (lambda nil (cpo-transpose-line-forward 2))
 :transient-mark-mode nil)

(provide 'cvg-text-modifications)
;;; cvg-text-modifications.el ends here
