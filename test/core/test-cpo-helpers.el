;;; test-cpo-helpers.el --- Shared test helper macros -*- lexical-binding: t; -*-

(require 'ert)

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

(defmacro should/region-equal (region-cons)
  `(progn
     (should (region-active-p))
     (should (equal ,region-cons
                    (cons (region-beginning) (region-end))))))

(provide 'test-cpo-helpers)
;;; test-cpo-helpers.el ends here
