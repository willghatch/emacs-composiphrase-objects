;;; test-cpo-camel-case-sub-word.el --- Tests for cpo-camel-case-sub-word -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-camel-case-sub-word)


;;; Test: forward-beginning movement


(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-first
                      "<p0>camel<p1>CaseWord"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-middle
                      "camel<p0>Case<p1>Word"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-inside
                      "ca<p0>mel<p1>CaseWord"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-across-words
                      "camelCase<p0>Word <p1>anotherWord"
                      'cpo-forward-camel-case-sub-word-beginning)

;;; Test: backward-beginning movement

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-last
                      "camel<p1>Case<p0>Word"
                      'cpo-backward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-middle
                      "camel<p1>Ca<p0>seWord"
                      'cpo-backward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-to-first
                      "<p1>camel<p0>CaseWord"
                      'cpo-backward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-across-words
                      "camelCase<p1>Word <p0>anotherThing"
                      'cpo-backward-camel-case-sub-word-beginning)

;;; Test: forward-end movement

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-first
                      "<p0>camel<p1>CaseWord"
                      'cpo-forward-camel-case-sub-word-end)

;; Forward-end with count 2: from beginning of "Word", goes to end of "Word"
;; then to end of first sub-word of next word.
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-count-2
                      "camelCase<p0>Word last<p1>Part"
                      (lambda () (cpo-forward-camel-case-sub-word-end 2)))

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-middle
                      "camel<p0>Case<p1>Word"
                      'cpo-forward-camel-case-sub-word-end)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-to-end
                      "camelCase<p0>Word<p1>"
                      'cpo-forward-camel-case-sub-word-end)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-across-words
                      "camelCaseWord<p0> another<p1>Word"
                      'cpo-forward-camel-case-sub-word-end)

;;; Test: backward-end movement

;; From inside "Word", backward-end goes to end of "Case" (= beginning of "Word").
;; But since those positions coincide in camelCase, it must go further to end of "camel".
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-end-from-beginning
                      "camel<p1>Case<p0>Word"
                      'cpo-backward-camel-case-sub-word-end)

;; From inside "Case" (not at boundary), backward-end goes to end of "camel".
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-end-middle
                      "camel<p1>Ca<p0>seWord"
                      'cpo-backward-camel-case-sub-word-end)

;; From inside the first sub-word of a word, backward-end goes to end of previous word's last sub-word.
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-end-across-words
                      "camelCaseWord<p1> <p0>anotherThing"
                      'cpo-backward-camel-case-sub-word-end)

;;; Test: expand region

;; Expand from inside "camel" selects the whole "camel" sub-word.
(carettest-tesmo-test cpo-camel-case-sub-word-test-expand-region-first
                      "<m1>ca<p0>mel<p1>CaseWord"
                      'cpo-expand-region-to-camel-case-sub-word)

;; Expand from inside "Case" selects the whole "Case" sub-word.
(carettest-tesmo-test cpo-camel-case-sub-word-test-expand-region-middle
                      "camel<m1>Ca<p0>se<p1>Word"
                      'cpo-expand-region-to-camel-case-sub-word)

;;; Test: transpose forward

;; When the first sub-word is lowercase, the new first should also be lowercase.
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-first-lowercase
                       "<p>camelCaseWord"
                       "case<p>CamelWord"
                       'cpo-transpose-camel-case-sub-word-forward)

(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-middle
                       "camel<p>CaseWord"
                       "camelWord<p>Case"
                       'cpo-transpose-camel-case-sub-word-forward)

(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-first-stays-lower
                       "<p>getThisValue"
                       "this<p>GetValue"
                       'cpo-transpose-camel-case-sub-word-forward)

;;; Test: transpose backward

(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-bwd-basic
                       "camelCase<p>Word"
                       "camel<p>WordCase"
                       'cpo-transpose-camel-case-sub-word-backward)

;; When transposing backward to first position, old first becomes capitalized,
;; and the new first gets old first's case.  Cursor follows the moved element.
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-bwd-to-first
                       "camel<p>CaseWord"
                       "<p>caseCamelWord"
                       'cpo-transpose-camel-case-sub-word-backward)

;;; Test: transpose forward -- capitalization preservation across multiple

(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-twice
                       :before "<p>getThisValue"
                       :after "thisValue<p>Get"
                       :function (lambda () (cpo-transpose-camel-case-sub-word-forward 2)))

;;; Test: transpose -- PascalCase (first word is uppercase)

(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-pascal
                       "<p>GetThisValue"
                       "This<p>GetValue"
                       'cpo-transpose-camel-case-sub-word-forward)

;;; Test: open forward

(carettest-tesmo-test cpo-camel-case-sub-word-test-open-fwd-first
                      "<p0>camel<p1>CaseWord"
                      'cpo-camel-case-sub-word-open-forward)

(carettest-tesmo-test cpo-camel-case-sub-word-test-open-fwd-middle
                      "camel<p0>Case<p1>Word"
                      'cpo-camel-case-sub-word-open-forward)

;;; Test: open backward

;; Opening backward before the first sub-word capitalizes the old first.
(carettest-tesmut-test cpo-camel-case-sub-word-test-open-bwd-first
                       "<p>camelCaseWord"
                       "<p>CamelCaseWord"
                       'cpo-camel-case-sub-word-open-backward)

;; Opening backward before a non-first sub-word: cursor goes to
;; beginning of current sub-word (no capitalization change needed).
(carettest-tesmo-test cpo-camel-case-sub-word-test-open-bwd-middle
                      "camel<p0><p1>CaseWord"
                      'cpo-camel-case-sub-word-open-backward)

(carettest-tesmo-test cpo-camel-case-sub-word-test-open-bwd-last
                      "camelCase<p0><p1>Word"
                      'cpo-camel-case-sub-word-open-backward)

;;; Test: punctuation is not part of the word

(carettest-tesmo-test cpo-camel-case-sub-word-test-punctuation-not-included
                      "foo.<p0>camel<p1>CaseWord"
                      'cpo-forward-camel-case-sub-word-beginning)

(ert-deftest cpo-camel-case-sub-word-test-punctuation-bounds ()
  "Punctuation is not included in word bounds."
  (with-temp-buffer
    (insert "foo.camelCaseWord")
    (goto-char (+ (point-min) 4)) ;; on 'c' of 'camel'
    (let ((bounds (cpo-camel-case-sub-word--word-bounds-at-point)))
      (should bounds)
      (should (equal "camelCaseWord"
                     (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

;;; Test: multiple words separated by punctuation

(carettest-tesmo-test cpo-camel-case-sub-word-test-across-dot
                      "camelCase<p0>Word.<p1>anotherThing"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-back-across-dot
                      "camelCase<p1>Word.<p0>anotherThing"
                      'cpo-backward-camel-case-sub-word-beginning)

;;; Test: movement with count

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-count-2
                      "<p0>camelCase<p1>Word"
                      (lambda () (cpo-forward-camel-case-sub-word-beginning 2)))

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-count-2
                      "<p1>camelCase<p0>Word"
                      (lambda () (cpo-backward-camel-case-sub-word-beginning 2)))

;;; Test: consecutive uppercase (XMLParser, someYAMLParser)

;; forward-beginning across consecutive-uppercase sub-words.
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-xml
                      "<p0>XML<p1>Parser someYAMLParser"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-xml-across-words
                      "XML<p0>Parser <p1>someYAMLParser"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-yaml
                      "XMLParser <p0>some<p1>YAMLParser"
                      'cpo-forward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-yaml-to-parser
                      "XMLParser some<p0>YAML<p1>Parser"
                      'cpo-forward-camel-case-sub-word-beginning)

;; backward-beginning across consecutive-uppercase sub-words.
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-xml
                      "<p1>XML<p0>Parser someYAMLParser"
                      'cpo-backward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-yaml
                      "XMLParser <p1>some<p0>YAMLParser"
                      'cpo-backward-camel-case-sub-word-beginning)

(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-yaml-parser
                      "XMLParser some<p1>YAML<p0>Parser"
                      'cpo-backward-camel-case-sub-word-beginning)

;; forward-end within consecutive-uppercase words.
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-xml
                      "<p0>XML<p1>Parser someYAMLParser"
                      'cpo-forward-camel-case-sub-word-end)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-xml-parser
                      "XML<p0>Parser<p1> someYAMLParser"
                      'cpo-forward-camel-case-sub-word-end)

(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-yaml
                      "XMLParser some<p0>YAML<p1>Parser"
                      'cpo-forward-camel-case-sub-word-end)

;; backward-end within consecutive-uppercase words.
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-end-yaml-parser
                      "XMLParser some<p1>YAML<p0>Parser"
                      'cpo-backward-camel-case-sub-word-end)

;; expand-region within consecutive-uppercase words.
(carettest-tesmo-test cpo-camel-case-sub-word-test-expand-region-xml
                      "<m1>X<p0>ML<p1>Parser someYAMLParser"
                      'cpo-expand-region-to-camel-case-sub-word)

(carettest-tesmo-test cpo-camel-case-sub-word-test-expand-region-yaml
                      "XMLParser some<m1>YA<p0>ML<p1>Parser"
                      'cpo-expand-region-to-camel-case-sub-word)

;; Transpose forward with consecutive uppercase: XMLParser -> ParserXML.
;; First sub-word "XML" is uppercase, so new first "Parser" stays uppercase.
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-xml
                       "<p>XMLParser"
                       "Parser<p>XML"
                       'cpo-transpose-camel-case-sub-word-forward)

;; Transpose forward from lowercase "some" swaps with "YAML" in someYAMLParser.
;; First sub-word was lowercase, so new first "yAML" gets lowercased initial,
;; and displaced "some" becomes "Some".
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-some-yaml
                       "XMLParser <p>someYAMLParser"
                       "XMLParser yAML<p>SomeParser"
                       'cpo-transpose-camel-case-sub-word-forward)

;; Transpose backward from "YAML" to first position in someYAMLParser.
;; "some" was lowercase first; new first gets lowercased initial only,
;; so "YAML" becomes "yAML", and "some" becomes "Some".
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-bwd-yaml-to-first
                       "XMLParser some<p>YAMLParser"
                       "XMLParser <p>yAMLSomeParser"
                       'cpo-transpose-camel-case-sub-word-backward)

;; Open forward after "XML" in XMLParser.
(carettest-tesmo-test cpo-camel-case-sub-word-test-open-fwd-xml
                      "<p0>XML<p1>Parser"
                      'cpo-camel-case-sub-word-open-forward)

;; Open backward before "XML" in XMLParser capitalizes old first.
(carettest-tesmut-test cpo-camel-case-sub-word-test-open-bwd-xml
                       "<p>XMLParser"
                       "<p>XMLParser"
                       'cpo-camel-case-sub-word-open-backward)

;;; Test: numbers in identifiers (number2String)

;; forward-beginning: from "number2" to "String".
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-beg-number
                      "<p0>number2<p1>String"
                      'cpo-forward-camel-case-sub-word-beginning)

;; backward-beginning: from "String" to "number2".
(carettest-tesmo-test cpo-camel-case-sub-word-test-bwd-beg-number
                      "<p1>number2<p0>String"
                      'cpo-backward-camel-case-sub-word-beginning)

;; forward-end: from beginning of "number2" to end of "number2".
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-number
                      "<p0>number2<p1>String"
                      'cpo-forward-camel-case-sub-word-end)

;; forward-end: from beginning of "String" to end of "String".
(carettest-tesmo-test cpo-camel-case-sub-word-test-fwd-end-string
                      "number2<p0>String<p1>"
                      'cpo-forward-camel-case-sub-word-end)

;; expand-region from inside "number2".
(carettest-tesmo-test cpo-camel-case-sub-word-test-expand-region-number
                      "<m1>num<p0>ber2<p1>String"
                      'cpo-expand-region-to-camel-case-sub-word)

;; Transpose forward: "number2" swaps with "String".
;; First sub-word was lowercase, so new first "string" gets lowercased initial,
;; and "number2" becomes "Number2".
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-fwd-number
                       "<p>number2String"
                       "string<p>Number2"
                       'cpo-transpose-camel-case-sub-word-forward)

;; Transpose backward: from "String" back to first position.
(carettest-tesmut-test cpo-camel-case-sub-word-test-transpose-bwd-number
                       "number2<p>String"
                       "<p>stringNumber2"
                       'cpo-transpose-camel-case-sub-word-backward)

;;; test-cpo-camel-case-sub-word.el ends here
