;;; -*- lexical-binding: t; -*-

(require 'cpo-smartparens)
(require 'ert)

;; TODO - actually write a bunch of tests
;; TODO - maybe use smartparens testing infrastructure, it has a lot of convenience definitions.  They are probably mostly internal, but they are probably sufficiently stable.

(defmacro should/region-equal (region-cons)
  `(progn
     (should (region-active-p))
     (should (equal ,region-cons
                    (cons (region-beginning) (region-end))))))
(defmacro should/looking-at (at-string)
  `(progn
     (let* ((str ,at-string)
            (result (looking-at-p str)))
       (when (not result)
         (message "Test failure, should look at: %s" str)
         (message "actually looking at: %s" (buffer-substring (point) (min (point-max) (+ (point) 10)))))
       (should result))))
(defmacro should/mark-looking-at (at-string)
  `(progn
     (save-mark-and-excursion
       (exchange-point-and-mark)
       (should/looking-at ,at-string))))

(ert-deftest test-expand-region-to-any-delimiter-after-last-child_smartparens ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (goto-char 1)
    (search-forward "))")
    (backward-char 5)
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")
    )

  )

(ert-deftest misc-cpo-smartparens-tests ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (goto-char 1)
    (transient-mark-mode 1)
    (emacs-lisp-mode)

    (search-forward "foo")
    (cpo-smartparens-expand-region)
    (should/looking-at "foo")
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")

    (set-mark nil)
    (goto-char 1)
    (search-forward "foo")
    (backward-char 1)
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")

    (set-mark nil)
    (goto-char 1)
    (search-forward "foo")
    (cpo-smartparens-expand-region-to-any-delimiter)
    (should/looking-at "(inner")
    )

  )
