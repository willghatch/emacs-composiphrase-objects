;;; cvg-search-movements.el --- Characteristic tests for find-char search movement -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-search-movements)

;;; find-char-a-forward
;; (cpo--find-char-in-line/impl ?a 1 'beginning)
;; Searches forward on current line for 'a'; moves to its beginning.
;; No match: backward-char 1 quirk (forward+beginning style).

;; Typical: 'a' found mid-line ahead.
(carettest-tesmo-test
 test-search-movements-find-char-a-forward__mid
 "hello <p0>world bar baz"
 (lambda nil
   (let ((char ?a))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds first 'a' ahead when multiple 'a' chars exist on the line.
(carettest-tesmo-test
 test-search-movements-find-char-a-forward__multiple
 "This line.has repeated<p0>.letters like <p1>aaa and bbb."
 (lambda nil
   (let ((char ?a))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds 'a' in "ation" near the end of the line.
(carettest-tesmo-test
 test-search-movements-find-char-a-forward__near-end
 "Final <p0>line with punctu<p1>ation: .,;!? for testing. "
 (lambda nil
   (let ((char ?a))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-e-backward
;; (cpo--find-char-in-line/impl ?e -1 'beginning)
;; Searches backward on current line for 'e'; moves to its beginning.
;; No match: point stays (no adjustment for backward+beginning).

;; Typical: 'e' found backward mid-line.
(carettest-tesmo-test
 test-search-movements-find-char-e-backward__mid
 "The quick brown fox jumps over th<p1>e lazy <p0>dog."
 (lambda nil
   (let ((char ?e))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds 'e' in "letters" searching backward.
(carettest-tesmo-test
 test-search-movements-find-char-e-backward__letters
 "This line.has repeated.lett<p1>ers l<p0>ike aaa and bbb."
 (lambda nil
   (let ((char ?e))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; No 'e' to the left on this line; point stays.
(carettest-tesmo-test
 test-search-movements-find-char-e-backward__no-match
 "T<p1><p0>his line.has repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char ?e))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-o-end-forward
;; (cpo--find-char-in-line/impl ?o 1 'end)
;; Searches forward on current line for 'o'; moves to just after it.
;; No match: point stays (forward+end style has no extra adjustment).

;; Typical: finds 'o' in "over", moves after it.
(carettest-tesmo-test
 test-search-movements-find-char-o-end-forward__mid
 "The quick brown fox<p0> jumps o<p1>ver the lazy dog."
 (lambda nil
   (let ((char ?o))
     (cpo--find-char-in-line/impl char 1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds 'o' in "bro", moves after it.
(carettest-tesmo-test
 test-search-movements-find-char-o-end-forward__bro
 "T<p0>he quick bro<p1>wn fox jumps over the lazy dog."
 (lambda nil
   (let ((char ?o))
     (cpo--find-char-in-line/impl char 1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; No 'o' on this line (empty line); point stays.
(carettest-tesmo-test
 test-search-movements-find-char-o-end-forward__no-match
 "The quick brown fox jumps over the lazy dog.
<p1><p0>
This line.has repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char ?o))
     (cpo--find-char-in-line/impl char 1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-t-end-backward
;; (cpo--find-char-in-line/impl ?t -1 'end)
;; Searches backward on current line for 't'; moves to just after it.
;; No match: point stays (forward-char+backward-char net zero).

;; Typical: finds 't' in "over t", moves to just after it.
(carettest-tesmo-test
 test-search-movements-find-char-t-end-backward__mid
 "The quick brown fox jumps over t<p1>he l<p0>azy dog."
 (lambda nil
   (let ((char ?t))
     (cpo--find-char-in-line/impl char -1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds 't' in "lett" searching backward.
(carettest-tesmo-test
 test-search-movements-find-char-t-end-backward__lett
 "This line.has repeated.lett<p1>ers like aaa an<p0>d bbb."
 (lambda nil
   (let ((char ?t))
     (cpo--find-char-in-line/impl char -1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; No lowercase 't' to the left; point stays.
(carettest-tesmo-test
 test-search-movements-find-char-t-end-backward__no-match
 "T<p1><p0>his line.has repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char ?t))
     (cpo--find-char-in-line/impl char -1 'end)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-space-forward
;; (cpo--find-char-in-line/impl 32 1 'beginning)
;; Searches forward on current line for a space; moves to it.

;; From start of line, finds first space.
(carettest-tesmo-test
 test-search-movements-find-char-space-forward__line-start
 "<p0>Final<p1> line with punctuation: .,;!? for testing. "
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Mid-line: finds next space ahead.
(carettest-tesmo-test
 test-search-movements-find-char-space-forward__mid
 "This line.has repeate<p0>d.letters<p1> like aaa and bbb."
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Skips to next space past ".,;!?".
(carettest-tesmo-test
 test-search-movements-find-char-space-forward__past-punct
 "Final line with punctuation:<p0> .,;!?<p1> for testing. "
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-space-backward
;; (cpo--find-char-in-line/impl 32 -1 'beginning)
;; Searches backward on current line for a space; moves to it.
;; No match: point stays.

;; Typical: finds space before "over".
(carettest-tesmo-test
 test-search-movements-find-char-space-backward__mid
 "The quick brown fox jumps<p1> ove<p0>r the lazy dog."
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds space before "dog" near end.
(carettest-tesmo-test
 test-search-movements-find-char-space-backward__near-end
 "The quick brown fox jumps over the lazy<p1> dog.<p0>"
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; No space to the left; point stays.
(carettest-tesmo-test
 test-search-movements-find-char-space-backward__no-match
 "Fin<p1><p0>al line with punctuation: .,;!? for testing. "
 (lambda nil
   (let ((char 32))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-period-forward
;; (cpo--find-char-in-line/impl 46 1 'beginning)
;; Searches forward on current line for '.'; moves to it.

;; Typical: finds '.' in "ne.has".
(carettest-tesmo-test
 test-search-movements-find-char-period-forward__mid
 "This li<p0>ne<p1>.has repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds '.' at end of line from far back.
(carettest-tesmo-test
 test-search-movements-find-char-period-forward__end-of-line
 "The quick brown <p0>fox jumps over the lazy dog<p1>."
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds '.' in "punctuation: .," (first '.' in sequence).
(carettest-tesmo-test
 test-search-movements-find-char-period-forward__punct-sequence
 "Final line with<p0> punctuation: <p1>.,;!? for testing. "
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char 1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; find-char-period-backward
;; (cpo--find-char-in-line/impl 46 -1 'beginning)
;; Searches backward on current line for '.'; moves to it.
;; No match: point stays.

;; Typical: finds '.' in "repeated." searching backward.
(carettest-tesmo-test
 test-search-movements-find-char-period-backward__mid
 "This line.has repeated<p1>.lette<p0>rs like aaa and bbb."
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Finds '.' in "line." searching backward.
(carettest-tesmo-test
 test-search-movements-find-char-period-backward__line-dot
 "This line<p1>.has<p0> repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; No '.' to the left; point stays.
(carettest-tesmo-test
 test-search-movements-find-char-period-backward__no-match
 "This l<p1><p0>ine.has repeated.letters like aaa and bbb."
 (lambda nil
   (let ((char 46))
     (cpo--find-char-in-line/impl char -1 'beginning)))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(provide 'cvg-search-movements)
;;; cvg-search-movements.el ends here
