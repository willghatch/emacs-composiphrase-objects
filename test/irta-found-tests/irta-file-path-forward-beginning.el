;;; -*- lexical-binding: t; -*-
;; IRTA tests for cpo-file-path-object: corrected forward-beginning tests.
;;
;; These tests were flagged from generated tests and simplified.

(require 'carettest-tesmo)
(require 'cpo-file-path-object)

;; p0 is inside /usr/share/doc/readme.txt; forward-beginning should skip to
;; the next standalone prefixed path, which is ./configure.
(carettest-tesmo-test
 irta-file-path-forward-beginning--from-inside-path-skips-to-next-standalone__syivxd
 "Load /usr/shar<p0>e/doc/readme.txt and\n<p1>./configure.\n"
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t :points ("<p0>" "<p1>") :marks ("<m0>" "<m1>"))

;; p0 is inside ./configure; forward-beginning should skip past
;; --prefix=/usr/local and land at ../scripts/build.sh.
(carettest-tesmo-test
 irta-file-path-forward-beginning--skips-interior-prefix-path__rpomxk
 "Run ./confi<p0>gure --prefix=/usr/local and\n<p1>../scripts/build.sh.\n"
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t :points ("<p0>" "<p1>") :marks ("<m0>" "<m1>"))

;; p0 is between paths, in the --prefix text before /usr/local.
(carettest-tesmo-test
 irta-file-path-forward-beginning--from-between-paths-skips-interior__mcroeh
 "Run ./configure <p0>--prefix=/usr/local and\n<p1>../scripts/build.sh.\n"
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t :points ("<p0>" "<p1>") :marks ("<m0>" "<m1>"))

;; p0 is inside $HOME; the embedded /bin and /foo matches should be skipped.
(carettest-tesmo-test
 irta-file-path-forward-beginning--skips-dollar-var-embedded-paths__dhvylv
 "Check $<p0>HOME/bin/my-script.sh and $XDG_CONFIG_HOME/foo/bar.conf.\nCopy '<p1>/tmp/test file' is not a path but /tmp/test-file is.\n"
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t :points ("<p0>" "<p1>") :marks ("<m0>" "<m1>"))

;; p0 is between paths; the quoted /tmp/test file should not be skipped.
(carettest-tesmo-test
 irta-file-path-forward-beginning--from-between-paths-skips-dollar-embedded__jkbnpc
 "Edit src/main.rs or lib/utils.el in the proje<p0>ct.\nCopy '<p1>/tmp/test file' is not a path but /tmp/test-file is.\n"
 'cpo-forward-cpo-file-path-object-explicit-beginning
 :transient-mark-mode t :points ("<p0>" "<p1>") :marks ("<m0>" "<m1>"))
