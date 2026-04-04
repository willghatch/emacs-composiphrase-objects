;;; test-cpo-file-path-transpose.el --- Tests for file-path transpose functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'carettest-tesmut)
(require 'cpo-file-path-object)

;;;; Explicit (prefixed) file-path transpose

;; Forward transpose: swap prefixed path at point with next prefixed path.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_basic
 :before "aaa <p>/foo/bar bbb /baz/quux ccc"
 :after "aaa /baz/quux bbb <p>/foo/bar ccc"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Forward transpose from middle of path.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_from-middle
 :before "aaa /foo/<p>bar bbb /baz/quux ccc"
 :after "aaa /baz/quux bbb /foo/<p>bar ccc"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Forward transpose with ./ prefix.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_dot-slash
 :before "run <p>./script.sh and ./other.sh end"
 :after "run ./other.sh and <p>./script.sh end"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Forward transpose with ../ prefix.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_dotdot-slash
 :before "load <p>../lib/foo.el and ../lib/bar.el end"
 :after "load ../lib/bar.el and <p>../lib/foo.el end"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Forward transpose: mixed prefix types.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_mixed-prefixes
 :before "see <p>./local.el and /system/global.el done"
 :after "see /system/global.el and <p>./local.el done"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Forward transpose with quote and paren delimiters around the paths.
(carettest-tesmut-test
 test-transpose-file-path-explicit-forward_quoted-and-parenthesized
 :before "echo \"<p>/foo/bar\" (/baz/quux)"
 :after "echo \"/baz/quux\" (<p>/foo/bar)"
 :function 'cpo-transpose-cpo-file-path-object-explicit-forward)

;; Backward transpose: swap prefixed path at point with previous prefixed path.
(carettest-tesmut-test
 test-transpose-file-path-explicit-backward_basic
 :before "aaa /foo/bar bbb <p>/baz/quux ccc"
 :after "aaa <p>/baz/quux bbb /foo/bar ccc"
 :function 'cpo-transpose-cpo-file-path-object-explicit-backward)

;; Backward transpose from middle of path.
(carettest-tesmut-test
 test-transpose-file-path-explicit-backward_from-middle
 :before "aaa /foo/bar bbb /baz/<p>quux ccc"
 :after "aaa /baz/<p>quux bbb /foo/bar ccc"
 :function 'cpo-transpose-cpo-file-path-object-explicit-backward)

;; Backward transpose with ./ prefix.
(carettest-tesmut-test
 test-transpose-file-path-explicit-backward_dot-slash
 :before "run ./script.sh and <p>./other.sh end"
 :after "run <p>./other.sh and ./script.sh end"
 :function 'cpo-transpose-cpo-file-path-object-explicit-backward)

;;;; Bare (non-explicit) file-path transpose

;; Forward transpose: swap bare token at point with next bare token.
(carettest-tesmut-test
 test-transpose-file-path-bare-forward_basic
 :before "aaa <p>src/foo.el bbb src/bar.el ccc"
 :after "aaa bbb <p>src/foo.el src/bar.el ccc"
 :function 'cpo-transpose-cpo-file-path-object-forward)

;; Forward transpose from middle of bare token.
(carettest-tesmut-test
 test-transpose-file-path-bare-forward_from-middle
 :before "aaa src/fo<p>o.el bbb src/bar.el ccc"
 :after "aaa bbb src/fo<p>o.el src/bar.el ccc"
 :function 'cpo-transpose-cpo-file-path-object-forward)

;; Forward transpose: plain words (bare variant matches any non-whitespace token).
(carettest-tesmut-test
 test-transpose-file-path-bare-forward_plain-words
 :before "<p>hello world there"
 :after "world <p>hello there"
 :function 'cpo-transpose-cpo-file-path-object-forward)

;; Forward transpose: three tokens, from first.
(carettest-tesmut-test
 test-transpose-file-path-bare-forward_three-tokens-from-first
 :before "<p>aaa bbb ccc"
 :after "bbb <p>aaa ccc"
 :function 'cpo-transpose-cpo-file-path-object-forward)

;; Backward transpose: swap bare token at point with previous bare token.
(carettest-tesmut-test
 test-transpose-file-path-bare-backward_basic
 :before "aaa src/foo.el bbb <p>src/bar.el ccc"
 :after "aaa src/foo.el <p>src/bar.el bbb ccc"
 :function 'cpo-transpose-cpo-file-path-object-backward)

;; Backward transpose from middle of token.
(carettest-tesmut-test
 test-transpose-file-path-bare-backward_from-middle
 :before "aaa src/foo.el bbb src/b<p>ar.el ccc"
 :after "aaa src/foo.el src/b<p>ar.el bbb ccc"
 :function 'cpo-transpose-cpo-file-path-object-backward)

;; Backward transpose: plain words.
(carettest-tesmut-test
 test-transpose-file-path-bare-backward_plain-words
 :before "hello world <p>there"
 :after "hello <p>there world"
 :function 'cpo-transpose-cpo-file-path-object-backward)

;; Backward transpose: three tokens, from last.
(carettest-tesmut-test
 test-transpose-file-path-bare-backward_three-tokens-from-last
 :before "aaa bbb <p>ccc"
 :after "aaa <p>ccc bbb"
 :function 'cpo-transpose-cpo-file-path-object-backward)

;; Forward transpose with quote and paren delimiters around the tokens.
(carettest-tesmut-test
 test-transpose-file-path-bare-forward_quoted-and-parenthesized
 :before "\"<p>src/foo.el\" (src/bar.el)"
 :after "\"src/bar.el\" (<p>src/foo.el)"
 :function 'cpo-transpose-cpo-file-path-object-forward)

;;; test-cpo-file-path-transpose.el ends here
