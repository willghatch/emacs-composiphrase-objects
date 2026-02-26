;;; cvg-word-symbol-movements.el --- Characteristic tests for word and symbol movements -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-text-object-stuff)

;;; cpo-forward-word-beginning

;; Mid-word: moves to beginning of next word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-beginning__mid
 "hello wor<p0>ld <p1>foo bar baz."
 'cpo-forward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After punctuation: moves across comma+space to next word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-beginning__cross-punct
 "hello world, <p0>foo <p1>bar baz."
 'cpo-forward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: moves from end-of-line area to next line's first word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-beginning__cross-line
 "hello wor<p0>ld.
<p1>foo bar baz."
 'cpo-forward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-word-beginning

;; Mid-word: moves to the beginning of the current word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-beginning__mid
 "hello <p1>wor<p0>ld foo bar baz."
 'cpo-backward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: moves from start of line to previous line's word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-beginning__cross-line
 "hello world.
<p1>foo<p0> bar baz."
 'cpo-backward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Across hyphen: hyphen splits words, so lazy-dogs has two words.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-beginning__hyphen
 "over 42 lazy-<p1>dogs<p0>."
 'cpo-backward-word-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-word-end

;; Mid-word: moves to end of current word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-end__mid
 "hello wo<p0>rld<p1> foo bar baz."
 'cpo-forward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; After punctuation: crosses punctuation, finds end of next word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-end__cross-punct
 "hello world,<p0> foo<p1> bar baz."
 'cpo-forward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; camelCase: word boundary at capital letter transitions.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-word-end__camel
 "camelCa<p0>seVar<p1> and some_symbol."
 'cpo-forward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-word-end

;; Mid-word: moves back to end of previous word.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-end__mid
 "hello world<p1>, fo<p0>o bar baz."
 'cpo-backward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross punctuation: skips past punctuation to previous word end.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-end__cross-punct
 "hello world<p1>. U<p0>t enim veniam."
 'cpo-backward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross line: moves from mid-word back across words to a prior word end.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-word-end__cross-line
 "hello world.
foo bar<p1> ba<p0>z."
 'cpo-backward-word-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-symbol-beginning
;; Symbols treat underscores and hyphens as word characters (no split).

;; Mid-symbol: moves to beginning of next symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-beginning__mid
 "hello wor<p0>ld <p1>foo bar baz."
 'cpo-forward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Underscore: quick_brown is one symbol; moves past it to next symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-beginning__underscore
 "The q<p0>uick_brown <p1>fox jumps over 42 lazy-dogs."
 'cpo-forward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: moves to first symbol of next line.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-beginning__cross-line
 "hello wor<p0>ld.
<p1>foo bar baz."
 'cpo-forward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-symbol-beginning

;; Mid-symbol: moves to the beginning of the current symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-beginning__mid
 "hello <p1>wor<p0>ld foo bar baz."
 'cpo-backward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Underscore-symbol: HTTP_REQUEST_TIMEOUT is one symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-beginning__underscore
 "some_symbol: <p1>HTTP_REQUEST_TIMEOU<p0>T = 30."
 'cpo-backward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: moves from start of line back to previous line's symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-beginning__cross-line
 "hello world.
<p1>foo<p0> bar baz."
 'cpo-backward-symbol-beginning
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-forward-symbol-end

;; Mid-symbol: moves to end of current symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-end__mid
 "hello wo<p0>rld<p1> foo bar baz."
 'cpo-forward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Underscore: quick_brown is one symbol, end is after 'n'.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-end__underscore
 "The quick_brow<p0>n<p1> fox jumps over 42 lazy-dogs."
 'cpo-forward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Hyphen-symbol: lazy-dogs is one symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-forward-symbol-end__hyphen
 "over 42 lazy-d<p0>ogs<p1>."
 'cpo-forward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; cpo-backward-symbol-end

;; Mid-symbol: moves to end of previous symbol.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-end__mid
 "camelCaseVar<p1> an<p0>d some_symbol."
 'cpo-backward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-punct: skips colon separator to previous symbol end.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-end__cross-punct
 "camelCaseVar and some_symbol<p1>: HTTP_RE<p0>QUEST_TIMEOUT = 30."
 'cpo-backward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: moves back one symbol end, crossing a line boundary.
(carettest-tesmo-test
 test-word-symbol-movements-cpo-backward-symbol-end__cross-line
 "hello world.
foo<p1> ba<p0>z."
 'cpo-backward-symbol-end
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; forward-word-beginning with count 3

;; Mid content: 3 word beginnings forward, crosses hyphen boundaries.
(carettest-tesmo-test
 test-word-symbol-movements-forward-word-beginning-3__mid
 "The quick_brown fox j<p0>umps over 42 <p1>lazy-dogs."
 (lambda nil
   (cpo-forward-word-beginning 3))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 3 word beginnings forward, crossing a line boundary.
(carettest-tesmo-test
 test-word-symbol-movements-forward-word-beginning-3__cross-line
 "hello wo<p0>rld foo.
bar <p1>baz qux."
 (lambda nil
   (cpo-forward-word-beginning 3))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;;; backward-symbol-end with count 2

;; Mid content: 2 symbol ends backward.
(carettest-tesmo-test
 test-word-symbol-movements-backward-symbol-end-2__mid
 "camelCaseVar and some_symbol<p1>: HTTP_REQUEST_TIMEOUT =<p0> 30."
 (lambda nil
   (cpo-backward-symbol-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

;; Cross-line: 2 symbol ends backward, crossing a line boundary.
(carettest-tesmo-test
 test-word-symbol-movements-backward-symbol-end-2__cross-line
 "hello world<p1>.
foo ba<p0>z."
 (lambda nil
   (cpo-backward-symbol-end 2))
 :transient-mark-mode
 nil
 :points
 ("<p0>" "<p1>")
 :marks
 ("<m0>" "<m1>"))

(provide 'cvg-word-symbol-movements)
;;; cvg-word-symbol-movements.el ends here
