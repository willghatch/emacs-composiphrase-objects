;;; cvg-region-operations.el --- Characteristic tests for region operations -*- lexical-binding: t; -*-

(require 'carettest-tesmut)
(require 'cpo-text-object-stuff)

;; expand-region-with-final-newline
;; Single line, no initial region: expands to cover the whole line including trailing newline.
(carettest-tesmut-test
 test-region-operations-expand-region-with-final-newline__single-line
 :before
 "line one\nline <p>two\nline three\n"
 :after
 "line one\n<m>line two\n<p>line three\n"
 :function
 (lambda nil
   (cpo-expand-region-to-fill-lines t))
 :transient-mark-mode
 t)

;; expand-region-with-final-newline
;; Region spanning multiple lines: mark and point move to cover full lines,
;; point ends after the final newline of the last line.
(carettest-tesmut-test
 test-region-operations-expand-region-with-final-newline__multi-line
 :before
 "line one\nS<m>ed two\nfour gra<p>vida\n"
 :after
 "line one\n<m>Sed two\nfour gravida\n<p>"
 :function
 (lambda nil
   (cpo-expand-region-to-fill-lines t))
 :transient-mark-mode
 t)

;; expand-region-without-final-newline
;; Region spanning two consecutive lines: expands to cover full lines,
;; but point stops at end of last line (before its newline).
(carettest-tesmut-test
 test-region-operations-expand-region-without-final-newline__two-lines
 :before
 "line one\npoten<m>ti two\nSed<p> three\nline four\n"
 :after
 "line one\n<m>potenti two\nSed three<p>\nline four\n"
 :function
 (lambda nil
   (cpo-expand-region-to-fill-lines nil))
 :transient-mark-mode
 t)

;; expand-region-without-final-newline
;; Point is before mark (reversed ordering): the function handles this case
;; by keeping point at the beginning of its line and mark at the end of its line.
(carettest-tesmut-test
 test-region-operations-expand-region-without-final-newline__point-before-mark
 :before
 "line <p>one\nline two\n\nDuis dolo<m>r three\nline four\n"
 :after
 "<p>line one\nline two\n\nDuis dolor three<m>\nline four\n"
 :function
 (lambda nil
   (cpo-expand-region-to-fill-lines nil))
 :transient-mark-mode
 t)

(provide 'cvg-region-operations)
;;; cvg-region-operations.el ends here
