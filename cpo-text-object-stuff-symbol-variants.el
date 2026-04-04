;;; -*- lexical-binding: t; -*-

(require 'cpo-text-object-stuff)

;; The built-in symbol object is mode-dependent.  These variants bind a syntax
;; table so symbol matching stays consistent even when the current mode would
;; interpret punctuation differently.  Keeping them in a separate feature lets
;; the core text-object helpers load without running mode hooks for every use.
(defconst cpo-text-object-stuff--elisp-like-symbol-syntax-table
  (with-temp-buffer
    (emacs-lisp-mode)
    (copy-syntax-table (syntax-table)))
  "Syntax table used to interpret elisp-like symbols.")
(defconst cpo-text-object-stuff--c-like-symbol-syntax-table
  (with-temp-buffer
    (c-mode)
    (copy-syntax-table (syntax-table)))
  "Syntax table used to interpret C-like symbols.")

(cl-defmacro cpo-text-object-stuff--def-symbol-variant (variant syntax-table)
  (let* ((variant-name (symbol-name variant))
         (fwd-beg (intern (format "cpo-forward-%s-beginning" variant-name)))
         (fwd-end (intern (format "cpo-forward-%s-end" variant-name)))
         (bwd-end (intern (format "cpo-backward-%s-end" variant-name)))
         (bwd-beg (intern (format "cpo-backward-%s-beginning" variant-name)))
         (transpose-fwd (intern (format "cpo-transpose-%s-forward" variant-name)))
         (transpose-bwd (intern (format "cpo-transpose-%s-backward" variant-name)))
         (expand (intern (format "cpo-expand-region-to-%s" variant-name))))
    `(progn
       (defun ,fwd-beg (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-forward-symbol-beginning count)))
       (defun ,fwd-end (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-forward-symbol-end count)))
       (defun ,bwd-end (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-backward-symbol-end count)))
       (defun ,bwd-beg (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-backward-symbol-beginning count)))
       (defun ,transpose-fwd (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-transpose-symbol-forward count)))
       (defun ,transpose-bwd (&optional count)
         (interactive "p")
         (with-syntax-table ,syntax-table
           (cpo-transpose-symbol-backward count)))
       (cl-defun ,expand (&key position)
         (interactive)
         (with-syntax-table ,syntax-table
           (cpo-expand-region-to-symbol :position position)))
       (with-eval-after-load 'repeatable-motion
         (repeatable-motion-define-pair ',fwd-beg ',bwd-beg)
         (repeatable-motion-define-pair ',fwd-end ',bwd-end)))))

(cpo-text-object-stuff--def-symbol-variant elisp-like-symbol
  cpo-text-object-stuff--elisp-like-symbol-syntax-table)
(cpo-text-object-stuff--def-symbol-variant c-like-symbol
  cpo-text-object-stuff--c-like-symbol-syntax-table)

(provide 'cpo-text-object-stuff-symbol-variants)
