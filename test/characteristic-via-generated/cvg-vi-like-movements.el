;;; cvg-vi-like-movements.el --- Characteristic tests for vi-like word movements -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-text-object-stuff)

;;; cpo-forward-cpo-vi-like-word-beginning
;;
;; Vi-like words treat sequences of word characters (letters/digits) and
;; sequences of non-word non-space characters (punctuation, hyphens,
;; underscores) as separate "words".  Only whitespace is a separator.
;;
;; "foo-bar" => three vi-like words: "foo", "-", "bar"
;; This contrasts with regular words where "-" is a separator and is skipped.

;; Mid word-char sequence: forward from inside "foo" goes to "-" (next unit).
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-beginning__mid-word
 "f<p0>oo<p1>- bar baz"
 'cpo-forward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; End of word-char sequence: forward from end of "foo" skips space to "bar".
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-beginning__cross-space
 "fo<p0>o <p1>bar baz"
 'cpo-forward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: forward from end of last word on a line jumps to next line.
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-beginning__cross-line
 "foo = ba<p0>r
<p1>
    count42"
 'cpo-forward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Punct-unit: from inside a punct sequence "=", forward goes to next word.
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-beginning__punct-unit
 "foo <p0>= <p1>bar baz"
 'cpo-forward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-cpo-vi-like-word-beginning

;; Mid word: backward from inside "bar" goes to beginning of "bar".
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-beginning__mid-word
 "foo - <p1>ba<p0>r baz"
 'cpo-backward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-space: backward from start of "bar" goes to "=".
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-beginning__cross-space
 "foo <p1>=<p0> bar"
 'cpo-backward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: backward from beginning of a line goes to prev line's last word start.
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-beginning__cross-line
 "foo - bar
<p1>count42<p0> = baz"
 'cpo-backward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Punct backward: backward from "bar" over space to start of "-" (punct unit).
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-beginning__punct-unit
 "foo <p1>- <p0>bar"
 'cpo-backward-cpo-vi-like-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-cpo-vi-like-word-end

;; Mid word: forward from inside "foo" goes to end of "foo".
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-end__mid-word
 "f<p0>oo<p1> - bar baz"
 'cpo-forward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-space: forward from after "foo" skips space to end of "-".
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-end__cross-space
 "foo<p0> -<p1> bar"
 'cpo-forward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: forward from end of a line goes to end of next line's first word.
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-end__cross-line
 "foo = bar<p0>
foo<p1> bar"
 'cpo-forward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Punct-end: forward from start of a word goes to end of word chars, not past punct.
(carettest-tesmo-test
 test-vi-like-movements-cpo-forward-cpo-vi-like-word-end__punct-end
 "foo <p0>-<p1> bar"
 'cpo-forward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-cpo-vi-like-word-end

;; Mid word: backward from inside "bar" goes to end of "-" (previous vi-like word end).
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-end__mid-word
 "foo -<p1> ba<p0>r"
 'cpo-backward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-space: backward from start of a punct unit goes to end of previous word.
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-end__cross-space
 "foo<p1> <p0>- bar"
 'cpo-backward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: backward from start of line 2 lands inside last word of line 1.
(carettest-tesmo-test
 test-vi-like-movements-cpo-backward-cpo-vi-like-word-end__cross-line
 "foo = ba<p1>r
<p0>count42 = baz"
 'cpo-backward-cpo-vi-like-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-cpo-vi-like-word-beginning-2 (2 vi-like word beginnings forward)

;; Count 2: from inside "foo", skip two units forward to "bar".
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning-2__mid
 "f<p0>oo - <p1>bar baz"
 (lambda nil
   (cpo-forward-cpo-vi-like-word-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 2 vi-like word beginnings forward, crossing a newline.
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-beginning-2__cross-line
 "fo<p0>o bar
<p1>baz"
 (lambda nil
   (cpo-forward-cpo-vi-like-word-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-cpo-vi-like-word-end-2 (2 vi-like word ends forward)

;; Count 2: from inside "foo", advance two ends: end of "foo", then end of "-".
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-end-2__mid
 "f<p0>oo -<p1> bar"
 (lambda nil
   (cpo-forward-cpo-vi-like-word-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 2 vi-like word ends forward, crossing a newline.
(carettest-tesmo-test
 test-vi-like-movements-forward-cpo-vi-like-word-end-2__cross-line
 "foo<p0> bar
baz<p1> qux"
 (lambda nil
   (cpo-forward-cpo-vi-like-word-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-cpo-vi-like-word-beginning-2 (2 vi-like word beginnings backward)

;; Count 2: from inside "bar", go back two beginnings: "bar", then "-".
(carettest-tesmo-test
 test-vi-like-movements-backward-cpo-vi-like-word-beginning-2__mid
 "foo <p1>- ba<p0>r baz"
 (lambda nil
   (cpo-backward-cpo-vi-like-word-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 2 vi-like word beginnings backward, crossing a newline.
;; From mid-"baz" going back 2 lands before "r" in "bar" on line 1.
(carettest-tesmo-test
 test-vi-like-movements-backward-cpo-vi-like-word-beginning-2__cross-line
 "foo ba<p1>r
ba<p0>z"
 (lambda nil
   (cpo-backward-cpo-vi-like-word-beginning 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-cpo-vi-like-word-end-2 (2 vi-like word ends backward)

;; Count 2: from inside "bar", go back two ends: end of "-", then end of "foo".
(carettest-tesmo-test
 test-vi-like-movements-backward-cpo-vi-like-word-end-2__mid
 "foo<p1> - ba<p0>r baz"
 (lambda nil
   (cpo-backward-cpo-vi-like-word-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 2 vi-like word ends backward, crossing a newline.
;; From mid-"bar" on line 2 going back 2 ends lands before "r" in "bar" on line 1.
(carettest-tesmo-test
 test-vi-like-movements-backward-cpo-vi-like-word-end-2__cross-line
 "foo ba<p1>r
baz ba<p0>r"
 (lambda nil
   (cpo-backward-cpo-vi-like-word-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-transpose-cpo-vi-like-word-forward
;;
;; Swaps the current vi-like word with the next vi-like word.
;; Since vi-like words include punctuation units, "foo-bar" has three words.

;; Cursor at end of "foo" word: swaps "foo" with "bar" (next word after space).
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-forward__at-word-end
 :before "foo<p> bar baz"
 :after "bar foo<p> baz"
 :function 'cpo-transpose-cpo-vi-like-word-forward
 :transient-mark-mode
 nil)

;; Cursor inside a word: swaps current word with next word.
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-forward__inside-word
 :before "fo<p>o bar baz"
 :after "bar fo<p>o baz"
 :function 'cpo-transpose-cpo-vi-like-word-forward
 :transient-mark-mode
 nil)

;; Cursor inside second-to-last word: swaps it with last word.
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-forward__near-end
 :before "foo ba<p>r baz"
 :after "foo baz ba<p>r"
 :function 'cpo-transpose-cpo-vi-like-word-forward
 :transient-mark-mode
 nil)

;;; cpo-transpose-cpo-vi-like-word-backward
;;
;; Swaps the current vi-like word with the previous vi-like word.

;; Cursor after last word: swaps last word with previous.
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-backward__at-end
 :before "foo bar baz<p>"
 :after "foo baz<p> bar"
 :function 'cpo-transpose-cpo-vi-like-word-backward
 :transient-mark-mode
 nil)

;; Cursor inside a word: swaps current word with previous.
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-backward__inside-word
 :before "foo ba<p>r baz"
 :after "ba<p>r foo baz"
 :function 'cpo-transpose-cpo-vi-like-word-backward
 :transient-mark-mode
 nil)

;; Cursor inside second word: swaps with first word.
(carettest-tesmut-test
 test-vi-like-movements-cpo-transpose-cpo-vi-like-word-backward__second-word
 :before "foo ba<p>r"
 :after "ba<p>r foo"
 :function 'cpo-transpose-cpo-vi-like-word-backward
 :transient-mark-mode
 nil)

(provide 'cvg-vi-like-movements)
;;; cvg-vi-like-movements.el ends here
