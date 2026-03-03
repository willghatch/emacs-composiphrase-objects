;;; cvg-url-movements.el --- Characteristic tests for URL movement and transposition -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-url-object)


;;; cpo-forward-url-beginning

;; Cursor before first URL (at "Vis"), moves to start of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-beginning__before-first
 "
Vis<p0>it <p1>http://example.com or https://other.org for info.
"
 'cpo-forward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside first URL, moves to start of second URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-beginning__inside-first
 "
Visit http://exam<p0>ple.com or <p1>https://other.org for info.
"
 'cpo-forward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor between URLs, moves to start of next URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-beginning__between-urls
 "
Visit http://example.com or<p0> <p1>https://other.org for info.
"
 'cpo-forward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-url-beginning

;; Cursor after second URL, moves to start of second URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-beginning__after-second
 "
Visit http://example.com or <p1>https://other.org for<p0> info.
"
 'cpo-backward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside second URL, moves to start of second URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-beginning__inside-second
 "
Visit http://example.com or <p1>https://oth<p0>er.org for info.
"
 'cpo-backward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor between URLs, moves to start of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-beginning__between-urls
 "
Visit <p1>http://example.com or <p0>https://other.org for info.
"
 'cpo-backward-url-beginning
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-forward-url-end

;; Cursor before first URL, moves to end of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-end__before-first
 "
Visit <p0>http://example.com<p1> or https://other.org for info.
"
 'cpo-forward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside first URL, moves to end of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-end__inside-first
 "
Visit http://exam<p0>ple.com<p1> or https://other.org for info.
"
 'cpo-forward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor after first URL end, moves to end of second URL.
(carettest-tesmo-test
 test-url-movements-cpo-forward-url-end__after-first-end
 "
Visit http://example.com or<p0> https://other.org<p1> for info.
"
 'cpo-forward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-backward-url-end

;; Cursor after second URL end, moves to end of second URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-end__after-second
 "
Visit http://example.com or https://other.org<p1> for<p0> info.
"
 'cpo-backward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside second URL, moves to end of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-end__inside-second
 "
Visit http://example.com<p1> or https://oth<p0>er.org for info.
"
 'cpo-backward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor between URLs, moves to end of first URL.
(carettest-tesmo-test
 test-url-movements-cpo-backward-url-end__between-urls
 "
Visit http://example.com<p1> or <p0>https://other.org for info.
"
 'cpo-backward-url-end
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-expand-region-to-url

;; Point and mark both inside same URL, expands to cover the full URL.
;; Default position puts point at beginning of region.
(carettest-tesmo-test
 test-url-movements-cpo-expand-region-to-url__inside-same-url
 "
Visit <p1>http://exam<p0>ple.com<m1> or https://other.org for info.
"
 'cpo-expand-region-to-url
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Point after URL end, mark inside URL, expands to cover full URL.
(carettest-tesmo-test
 test-url-movements-cpo-expand-region-to-url__mark-inside-point-after
 "
Visit <p1>http://example.com<m1><p0> or https://other.org for info.
"
 'cpo-expand-region-to-url
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; forward-url-beginning-2 (lambda: (cpo-forward-url-beginning 2))

;; Cursor before both URLs, skips to start of second URL.
(carettest-tesmo-test
 test-url-movements-forward-url-beginning-2__before-both
 "
Visit <p0>http://example.com or <p1>https://other.org for info.
"
 (lambda nil (cpo-forward-url-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside first URL, skips 2 more URL starts (lands at start of second URL on next line).
(carettest-tesmo-test
 test-url-movements-forward-url-beginning-2__inside-first-three-urls
 "
Visit http://exam<p0>ple.com or https://other.org for info.
See <p1>ftp://more.example.net for more.
"
 (lambda nil (cpo-forward-url-beginning 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; backward-url-end-2 (lambda: (cpo-backward-url-end 2))

;; Cursor after both URLs, moves back 2 URL ends to end of first URL.
(carettest-tesmo-test
 test-url-movements-backward-url-end-2__after-both
 "
Visit http://example.com<p1> or https://other.org for<p0> info.
"
 (lambda nil (cpo-backward-url-end 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Cursor inside third URL, moves back 2 URL ends to end of first URL.
(carettest-tesmo-test
 test-url-movements-backward-url-end-2__inside-third
 "
Visit http://example.com<p1> or https://other.org for info.
See ftp://m<p0>ore.example.net for more.
"
 (lambda nil (cpo-backward-url-end 2))
 :transient-mark-mode nil
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


;;; cpo-transpose-url-forward

;; Cursor inside first URL, swaps it with next URL.
(carettest-tesmut-test
 test-url-modifications-cpo-transpose-url-forward__first-url
 :before
 "
Visit http://exam<p>ple.com or https://other.org for info.
"
 :after
 "
Visit https://other.org or <p>http://example.com for info.
"
 :function 'cpo-transpose-url-forward
 :transient-mark-mode nil)

;; Cursor inside second URL, swaps it with next URL on the next line.
(carettest-tesmut-test
 test-url-modifications-cpo-transpose-url-forward__second-url
 :before
 "
Visit http://example.com or https://other<p>.org for info.
See ftp://more.example.net for more.
"
 :after
 "
Visit http://example.com or ftp://more.example.net for info.
See <p>https://other.org for more.
"
 :function 'cpo-transpose-url-forward
 :transient-mark-mode nil)


;;; cpo-transpose-url-backward

;; Cursor inside second URL, swaps it with previous URL.
(carettest-tesmut-test
 test-url-modifications-cpo-transpose-url-backward__second-url
 :before
 "
Visit http://example.com or https://oth<p>er.org for info.
"
 :after
 "
Visit <p>https://other.org or http://example.com for info.
"
 :function 'cpo-transpose-url-backward
 :transient-mark-mode nil)

;; Cursor inside third URL, swaps it with the second URL.
(carettest-tesmut-test
 test-url-modifications-cpo-transpose-url-backward__third-url
 :before
 "
Visit http://example.com or https://other.org for info.
See ftp://more.example<p>.net for more.
"
 :after
 "
Visit http://example.com or <p>ftp://more.example.net for info.
See https://other.org for more.
"
 :function 'cpo-transpose-url-backward
 :transient-mark-mode nil)


(provide 'cvg-url-movements)
;;; cvg-url-movements.el ends here
