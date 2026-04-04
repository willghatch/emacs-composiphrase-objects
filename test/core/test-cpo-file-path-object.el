;;; -*- lexical-binding: t; -*-
;;; test-cpo-file-path-object.el -- Tests for cpo-file-path-object

(require 'ert)
(require 'carettest-tesmo)
(require 'cpo-file-path-object)

;;; ===================================================================
;;; Tests for expand-region-to-cpo-file-path-object-explicit (prefixed paths)
;;; ===================================================================

;; Selection of an absolute path: point in the middle, selects whole path.
;; The regexp greedily matches all non-whitespace after the prefix,
;; so a trailing period at end of sentence IS included in the match.
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__absolute-path
 "See <p1>/foo/<p0>bar.txt<m1> for details."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Selection of a ./ path
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__dot-slash-path
 "Run <p1>./<p0>script.sh<m1> now."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Selection of a ../ path
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__dotdot-slash-path
 "Include <p1>../<p0>lib/utils.el<m1> here."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Selection starting at the beginning of a ./ path (point at prefix)
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__dot-slash-point-at-prefix
 "Run <p1><p0>./script.sh<m1> now."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))


(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__dot-slash-point-at-end
 "Run <p1>./script.sh<p0><m1> now."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Absolute path at start of line
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__absolute-path-at-line-start
 "<p1>/etc/ho<p0>sts<m1>
some other text"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path with multiple components.
;; The match includes the trailing period as part of the token since
;; the regexp matches all non-whitespace.
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__multi-component-path
 "Load from <p1>/home/user/.conf<p0>ig/foo.el.<m1>"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; ===================================================================
;;; Tests for expand-region-to-cpo-file-path-object (bare)
;;; ===================================================================

;; Bare variant selects any non-whitespace token
(carettest-tesmo-test
 test-file-path-object-expand-region__plain-word
 "hello <p1>wor<p0>ld<m1> there"
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

(carettest-tesmo-test
 test-file-path-object-expand-region__plain-word_at-end
 "hello <p1>world<p0><m1> there"
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Bare variant on a relative path without prefix
(carettest-tesmo-test
 test-file-path-object-expand-region__relative-path-no-prefix
 "Edit <p1>src/mai<p0>n.rs<m1> today."
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Bare variant on an absolute path (also matched by explicit variant)
(carettest-tesmo-test
 test-file-path-object-expand-region__absolute-path
 "See <p1>/etc/pas<p0>swd<m1> file."
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; ===================================================================
;;; Tests for forward/backward movement -- explicit variant
;;; ===================================================================

;; Forward movement from before a prefixed path lands at beginning
(carettest-tesmo-test
 test-file-path-object-explicit-forward-beginning__basic_at-last
 "See <p0><p1>/foo/bar.txt for details."
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;; Forward movement from inside first path lands at beginning of second path
(carettest-tesmo-test
 test-file-path-object-explicit-forward-beginning__skips-current
 "See <p0>/foo/bar.txt and then <p1>/baz/quux.el."
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;; Backward movement from after last path lands at beginning of last path
(carettest-tesmo-test
 test-file-path-object-explicit-backward-beginning__from-after-last-path
 "See /foo/bar.txt and then <p1>/baz/quux.el and now <p0>here."
 'cpo-backward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;; Backward movement from inside second path lands at start of that same path
(carettest-tesmo-test
 test-file-path-object-explicit-backward-beginning__from-inside-second-path
 "See /foo/bar.txt and then <p1>/baz/<p0>quux.el."
 'cpo-backward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))


;; backward to beginning with the normal version
(carettest-tesmo-test
 test-file-path-object-backward-beginning
 "then <p1>baz/q<p0>uux.el."
 'cpo-backward-cpo-file-path-object-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-file-path-object-backward-beginning-2
 "then <p1>baz/quux.el <p0>aoeu."
 'cpo-backward-cpo-file-path-object-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-file-path-object-backward-beginning-3
 "then <p1>/baz/quux.el <p0>aoeu."
 'cpo-backward-cpo-file-path-object-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

(carettest-tesmo-test
 test-file-path-object-backward-beginning-4
 "then <p1>../baz/quux.el <p0>aoeu."
 'cpo-backward-cpo-file-path-object-beginning
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;;; ===================================================================
;;; Tests for expand-region with position argument
;;; ===================================================================

;; Default position puts point at beginning of selection.
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__default-position-is-beginning
 "Run <p1>./scr<p0>ipt.sh<m1> now."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Position 'end puts point at end of selection
(carettest-tesmo-test
 test-file-path-object-explicit-expand-region__position-end
 "Run <m1>./scr<p0>ipt.sh<p1> now."
 (lambda () (cpo-expand-region-to-cpo-file-path-object-explicit :position 'end))
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; ===================================================================
;;; Tests for forward-end movement
;;; ===================================================================

;; Forward end movement from before a path lands at end of that path
(carettest-tesmo-test
 test-file-path-object-explicit-forward-end__basic
 "See <p0>./script.sh<p1> and /other/file.txt."
 'cpo-forward-cpo-file-path-object-explicit-end
 :transient-mark-mode t
 :points ("<p0>" "<p1>"))

;;; ===================================================================
;;; Tests for bash bounding characters -- explicit variant
;;; ===================================================================

;; Path ends before a double-quote (does not include the closing quote)
(carettest-tesmo-test
 test-file-path-object-explicit-bounded-by-double-quote
 "echo \"<p1>/foo/<p0>bar<m1>\" something"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before a single-quote
(carettest-tesmo-test
 test-file-path-object-explicit-bounded-by-single-quote
 "echo '<p1>/foo/<p0>bar<m1>' something"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before a backtick
(carettest-tesmo-test
 test-file-path-object-explicit-bounded-by-backtick
 "echo `<p1>/foo/<p0>bar<m1>` something"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before an open paren
(carettest-tesmo-test
 test-file-path-object-explicit-bounded-by-open-paren
 "foo(<p1>/foo/<p0>bar<m1>)"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Absolute path containing a dollar sign (env var component in path)
(carettest-tesmo-test
 test-file-path-object-explicit-dollar-sign-in-path
 "cp <p1>/$VAR/<p0>file<m1> dest"
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; ./ path where component contains a dollar sign
(carettest-tesmo-test
 test-file-path-object-explicit-dollar-sign-relative-path
 "Run <p1>./$SCRIPT_<p0>DIR/run.sh<m1> now."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path containing a colon (valid path character)
(carettest-tesmo-test
 test-file-path-object-explicit-colon-in-path
 "Use <p1>/foo:bar/<p0>baz<m1> here."
 'cpo-expand-region-to-cpo-file-path-object-explicit
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;;; ===================================================================
;;; Tests for bash bounding characters -- bare variant
;;; ===================================================================

;; Path ends before a double-quote
(carettest-tesmo-test
 test-file-path-object-bare-bounded-by-double-quote
 "echo \"<p1>src/foo/<p0>bar<m1>\" something"
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before a single-quote
(carettest-tesmo-test
 test-file-path-object-bare-bounded-by-single-quote
 "cp '<p1>src/foo/ba<p0>r<m1>' ."
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before a backtick
(carettest-tesmo-test
 test-file-path-object-bare-bounded-by-backtick
 "echo `<p1>src/<p0>bar<m1>` here"
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Path ends before an open bracket
(carettest-tesmo-test
 test-file-path-object-bare-bounded-by-open-bracket
 "[<p1>src/foo/<p0>bar<m1>]"
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Bare path containing a dollar sign (env var style)
(carettest-tesmo-test
 test-file-path-object-bare-dollar-sign-in-path
 "Run <p1>$HOME/bi<p0>n/foo<m1> now."
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))

;; Bare path containing a colon
(carettest-tesmo-test
 test-file-path-object-bare-colon-in-path
 "Use <p1>foo:bar/<p0>baz<m1> here."
 'cpo-expand-region-to-cpo-file-path-object
 :transient-mark-mode t
 :points ("<p0>" "<p1>")
 :marks ("<m0>" "<m1>"))
