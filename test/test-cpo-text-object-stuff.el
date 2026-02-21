;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cpo-text-object-stuff)

(setq some-words "The quick brown fox jumps over the lazy dog.")
(ert-deftest misc-tests ()
  (with-temp-buffer
    (insert some-words)
    (goto-char 6)
    (should (equal (cons 5 10)
                   (cpo-text-object-stuff--expanded-region-to-bounds-of-thing-at-point t nil 'word (cons 7 9))))))

