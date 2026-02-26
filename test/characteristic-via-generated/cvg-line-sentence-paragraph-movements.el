;;; cvg-line-sentence-paragraph-movements.el --- Characteristic tests for line, sentence, and paragraph movement -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'cpo-text-object-stuff)


;;; cpo-forward-line-beginning

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-beginning__mid-line
 "First li<p0>ne here.
<p1>Second line here.
Third line here.
"
 'cpo-forward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-beginning__at-line-end
 "First line here.<p0>
<p1>Second line here.
Third line here.
"
 'cpo-forward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-beginning__mid-second-line
 "First line here.
Second li<p0>ne here.
<p1>Third line here.
"
 'cpo-forward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-line-beginning

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-beginning__mid-line-to-same-beginning
 "<p1>First li<p0>ne here.
Second line here.
Third line here.
"
 'cpo-backward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-beginning__mid-second-to-first
 "<p1>First line here.
Second li<p0>ne here.
Third line here.
"
 'cpo-backward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-beginning__mid-third-to-second
 "First line here.
<p1>Second line here.
Third li<p0>ne here.
"
 'cpo-backward-line-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-line-end

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-end__mid-first-line
 "First li<p0>ne here.
<p1>Second line here.
Third line here.
"
 'cpo-forward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-end__mid-second-line
 "First line here.
Second li<p0>ne here.
<p1>Third line here.
"
 'cpo-forward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-forward-line-end__near-end-of-line
 "First line her<p0>e.
<p1>Second line here.
Third line here.
"
 'cpo-forward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-line-end

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-end__mid-second-line
 "First line here.
<p1>Second li<p0>ne here.
Third line here.
"
 'cpo-backward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-end__mid-third-line
 "First line here.
Second line here.
<p1>Third li<p0>ne here.
"
 'cpo-backward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-backward-line-end__early-in-line
 "Fir<p0>st line here.
<p1>Second line here.
Third line here.
"
 'cpo-backward-line-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-next-line

(carettest-tesmo-test
 test-line-movements-cpo-next-line__first-to-second
 "First li<p0>ne here.
Second l<p1>ine here.
Third line here.
"
 'cpo-next-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-next-line__second-to-third
 "First line here.
Second li<p0>ne here.
Third lin<p1>e here.
"
 'cpo-next-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-next-line__across-blank-line
 "First li<p0>ne here.
<p1>
Third line here.
"
 'cpo-next-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-prev-line

(carettest-tesmo-test
 test-line-movements-cpo-prev-line__second-to-first
 "First lin<p1>e here.
Second li<p0>ne here.
Third line here.
"
 'cpo-prev-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-prev-line__third-to-second
 "First line here.
Second l<p1>ine here.
Third li<p0>ne here.
"
 'cpo-prev-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-cpo-prev-line__across-blank-line
 "First line here.
<p1>
Third li<p0>ne here.
"
 'cpo-prev-line
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; forward-line-beginning-2 (lambda: (cpo-forward-line-beginning 2))

(carettest-tesmo-test
 test-line-movements-forward-line-beginning-2__first-to-third
 "First li<p0>ne here.
Second line here.
<p1>Third line here.
"
 (lambda nil (cpo-forward-line-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-forward-line-beginning-2__skip-blank
 "First li<p0>ne here.
Second line here.
<p1>
"
 (lambda nil (cpo-forward-line-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; prev-line-3 (lambda: (cpo-prev-line 3))

(carettest-tesmo-test
 test-line-movements-prev-line-3__back-three
 "First lin<p1>e here.
Second line here.
Third line here.
Fourth li<p0>ne here.
"
 (lambda nil (cpo-prev-line 3))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-line-movements-prev-line-3__across-blank
 "First line<p1> here.
Second line here.

Fourth lin<p0>e here.
"
 (lambda nil (cpo-prev-line 3))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-sentence-beginning

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-beginning__mid-sentence
 "Hello world. Foo bar<p0> baz.
<p1>Next sentence here. Final one.
"
 'cpo-forward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-beginning__cross-blank-line
 "Hello world foo ba<p0>r baz.

<p1>Second paragraph here.
"
 'cpo-forward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-beginning__next-line
 "Hello world foo ba<p0>r baz.
<p1>Next sentence here.
"
 'cpo-forward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-sentence-beginning

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-beginning__mid-sentence
 "<p1>Hello world foo bar b<p0>az.
Next sentence here.
"
 'cpo-backward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-beginning__to-prev-line
 "<p1>Hello world foo bar baz.
<p0>Next sentence here.
"
 'cpo-backward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-beginning__cross-blank-line
 "Hello world.

<p1>Second par<p0>agraph here. Another sentence.
"
 'cpo-backward-sentence-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-sentence-end

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-end__mid-sentence
 "Hello wor<p0>ld.<p1>
Foo bar baz. Next sentence.
"
 'cpo-forward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-end__within-line
 "Hello wo<p0>rld. Foo bar baz.<p1>
Next sentence here.
"
 'cpo-forward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-forward-sentence-end__near-period
 "Hello world. Foo ba<p0>r baz.<p1>
Next sentence here.
"
 'cpo-forward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-sentence-end

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-end__mid-second-sentence
 "Hello world.<p1>
Foo bar b<p0>az. Next sentence.
"
 'cpo-backward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-end__at-start-of-sentence
 "Hello world. Foo bar baz.<p1>
<p0>Next sentence here.
"
 'cpo-backward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-cpo-backward-sentence-end__near-period
 "Hello world. Foo bar baz.<p1>
Next sentenc<p0>e here.
"
 'cpo-backward-sentence-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-paragraph-beginning

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-beginning__mid-first-para
 "First paragra<p0>ph text.
More first paragraph.
<p1>
Second paragraph text.
More second paragraph.
"
 'cpo-forward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-beginning__end-of-first-para
 "First paragraph text.
More first paragrap<p0>h.
<p1>
Second paragraph text.
"
 'cpo-forward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-beginning__mid-second-para
 "First paragraph text.
More first paragraph.

Second paragrap<p0>h text.
More second paragraph.
<p1>
Third paragraph text.
"
 'cpo-forward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-paragraph-beginning

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-beginning__mid-first-para
 "<p1>First paragra<p0>ph text.
More first paragraph.

Second paragraph text.
"
 'cpo-backward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-beginning__mid-second-para
 "First paragraph text.
More first paragraph.
<p1>
Second paragra<p0>ph text.
More second paragraph.
"
 'cpo-backward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-beginning__start-of-second-para
 "First paragraph text.
More first paragraph.
<p1>
<p0>Second paragraph text.
More second paragraph.
"
 'cpo-backward-paragraph-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-paragraph-end

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-end__mid-first-para
 "First paragra<p0>ph text.
More first paragraph.
<p1>
Second paragraph text.
"
 'cpo-forward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-end__near-end-of-para
 "First paragraph text.
More first paragrap<p0>h.
<p1>
Second paragraph text.
"
 'cpo-forward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-forward-paragraph-end__mid-second-para
 "First paragraph text.
More first paragraph.

Second paragrap<p0>h text.
More second paragraph.
<p1>
Third paragraph text.
"
 'cpo-forward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-paragraph-end

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-end__mid-second-para
 "First paragraph text.
More first paragraph.
<p1>
Second paragra<p0>ph text.
More second paragraph.
"
 'cpo-backward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-end__mid-third-para
 "First paragraph text.
More first paragraph.

Second paragraph text.
More second paragraph.
<p1>
Third paragra<p0>ph text.
"
 'cpo-backward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-cpo-backward-paragraph-end__near-blank-line
 "First paragraph text.
More first paragraph.
<p1>
Second paragraph<p0> text.
More second paragraph.
"
 'cpo-backward-paragraph-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; forward-sentence-beginning-2 (lambda: (cpo-forward-sentence-beginning 2))

(carettest-tesmo-test
 test-sentence-movements-forward-sentence-beginning-2__mid-first
 "Hello wor<p0>ld. Foo bar baz.
<p1>Next sentence here. Final one.
"
 (lambda nil (cpo-forward-sentence-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-sentence-movements-forward-sentence-beginning-2__cross-para
 "Hello world. Foo bar<p0> baz.

<p1>Second paragraph here. Another sentence.
"
 (lambda nil (cpo-forward-sentence-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; backward-paragraph-end-2 (lambda: (cpo-backward-paragraph-end 2))

(carettest-tesmo-test
 test-paragraph-movements-backward-paragraph-end-2__third-to-first
 "First paragraph text.
More first paragraph.
<p1>
Second paragraph text.
More second paragraph.

Third paragra<p0>ph text.
"
 (lambda nil (cpo-backward-paragraph-end 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-paragraph-movements-backward-paragraph-end-2__mid-third
 "First paragraph text.
More first paragraph.
<p1>
Second paragraph text.
More second paragraph.

Third paragrap<p0>h text.
"
 (lambda nil (cpo-backward-paragraph-end 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


(provide 'cvg-line-sentence-paragraph-movements)
;;; cvg-line-sentence-paragraph-movements.el ends here
