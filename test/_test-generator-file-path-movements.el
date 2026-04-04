#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-file-path-movements.el --- Generator for file-path movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'cpo-file-path-object)

;; Test text covering a variety of file path types:
;; - absolute paths (/foo/bar)
;; - ./ relative paths (./foo/bar)
;; - ../ relative paths (../foo/bar)
;; - bare relative paths without prefix (src/main.rs)
;; - $VAR-prefixed paths ($HOME/bin/foo)
;; - paths adjacent to punctuation and bounding characters
;; - multiple paths on one line
(setq cpo-file-path--test-string
      "
Load /etc/hosts and /usr/share/doc/readme.txt from the system.
Run ./configure --prefix=/usr/local and ../scripts/build.sh now.
Edit src/main.rs or lib/utils.el in the project.
Check $HOME/bin/my-script.sh and $XDG_CONFIG_HOME/foo/bar.conf here.
Copy '/tmp/test file' is not a path but /tmp/test-file is.
Multiple: /foo/bar /baz/quux ./rel/path ../up/one.
End with ../dotdot/file.txt and $VAR/subdir/deep.el here.
")

;; Generate tests for explicit (prefixed) file path movement functions
(carettest-tesmo-generate-tests
 cpo-file-path--test-string
 14  ; number of test positions
 '(cpo-forward-cpo-file-path-object-explicit-beginning
   cpo-backward-cpo-file-path-object-explicit-beginning
   cpo-forward-cpo-file-path-object-explicit-end
   cpo-backward-cpo-file-path-object-explicit-end
   cpo-expand-region-to-cpo-file-path-object-explicit
   ("forward-explicit-beginning-2" (lambda () (cpo-forward-cpo-file-path-object-explicit-beginning 2)))
   ("backward-explicit-beginning-2" (lambda () (cpo-backward-cpo-file-path-object-explicit-beginning 2)))
   ("forward-explicit-end-2" (lambda () (cpo-forward-cpo-file-path-object-explicit-end 2)))
   ("backward-explicit-end-2" (lambda () (cpo-backward-cpo-file-path-object-explicit-end 2))))
 "test-file-path-explicit-movements"
 :output-file "_generated-tests-file-path-explicit-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)

;; Generate tests for bare (any token) file path movement functions
(carettest-tesmo-generate-tests
 cpo-file-path--test-string
 14  ; number of test positions
 '(cpo-forward-cpo-file-path-object-beginning
   cpo-backward-cpo-file-path-object-beginning
   cpo-forward-cpo-file-path-object-end
   cpo-backward-cpo-file-path-object-end
   cpo-expand-region-to-cpo-file-path-object
   ("forward-bare-beginning-2" (lambda () (cpo-forward-cpo-file-path-object-beginning 2)))
   ("backward-bare-beginning-2" (lambda () (cpo-backward-cpo-file-path-object-beginning 2)))
   ("forward-bare-end-2" (lambda () (cpo-forward-cpo-file-path-object-end 2)))
   ("backward-bare-end-2" (lambda () (cpo-backward-cpo-file-path-object-end 2))))
 "test-file-path-bare-movements"
 :output-file "_generated-tests-file-path-bare-movements.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)
