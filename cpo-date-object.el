(require 'cpo-text-object-stuff)

;;;;;

;; Date and date-time text objects

;; Date format: YYYY-MM-DD
;; These date and datetime functions are all a bit sketchy, and don't really work right when inside a date/datetime rather than on the edge.  But they are good enough for now.
(defun cpo-text-object-stuff--forward-date-beginning ()
  (let ((start-point (point))
        (end-point nil)
        (regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
    (save-mark-and-excursion
      (let ((success (re-search-forward regexp nil t)))
        (when (and success (equal start-point (match-beginning 0)))
          (progn (forward-char 1)
                 (setq success (re-search-forward regexp nil t))))
        (when success
          (setq end-point (match-beginning 0)))))
    (when end-point (goto-char end-point))))

(defun cpo-text-object-stuff--backward-date-beginning ()
  (let* ((regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
         (success (re-search-backward regexp nil t)))
    (when success
      (goto-char (match-beginning 0)))))

(defun cpo-forward-date-beginning (&optional count)
  "Move forward to the beginning of a date in YYYY-MM-DD format, COUNT times.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (i count)
      (funcall (if fwd
                   'cpo-text-object-stuff--forward-date-beginning
                 'cpo-text-object-stuff--backward-date-beginning)))))

(defun cpo-backward-date-beginning (&optional count)
  "Like `cpo-forward-date-beginning' but backward."
  (interactive "p")
  (cpo-forward-date-beginning (- (or count 1))))

(defun cpo-backward-date-end (&optional count)
  "Move backward to the end of a date, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-forward-date-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (end-point nil))
        (save-mark-and-excursion
          (let* ((start-point (point))
                 (moved (cpo-tree-walk--motion-moved 'cpo-backward-date-beginning))
                 (bounds (and moved (cpo-text-object-stuff--date-bounds-at-point)))
                 (was-in-date (and bounds (<= start-point (cdr bounds)))))
            (cond
             (was-in-date (setq moved
                                (cpo-tree-walk--motion-moved
                                 (lambda () (dotimes (i count)
                                              (cpo-backward-date-beginning))))))
             (moved (dotimes (i (- count 1)) (cpo-backward-date-beginning)))
             (t nil))
            (when moved
              (let ((bounds (cpo-text-object-stuff--date-bounds-at-point)))
                (and bounds (setq end-point (cdr bounds)))))))
        (when end-point (goto-char end-point)))))))

(defun cpo-forward-date-end (&optional count)
  "Move forward to the end of a date, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-backward-date-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (started-in-date nil)
            (started-at-end-of-date nil)
            (end-point nil))
        (save-mark-and-excursion

          ;; Check the current bounds of the date
          (let ((bounds (cpo-text-object-stuff--date-bounds-at-point)))
            ;; If we are already in a date, go to its end
            (when (and bounds
                       (<= (car bounds) start-point (cdr bounds)))
              (if (equal start-point (cdr bounds))
                  (setq started-at-end-of-date t)
                (progn
                  (setq started-in-date t)
                  (goto-char (cdr bounds))))))

          ;; Go backward to find the start of a date to check more carefully
          (unless (or started-in-date started-at-end-of-date)
            (cpo-backward-date-beginning)
            (let ((bounds (cpo-text-object-stuff--date-bounds-at-point)))
              (when bounds
                (if (<= (car bounds) start-point (cdr bounds))
                    (progn
                      (goto-char (cdr bounds))
                      (if (equal start-point (cdr bounds))
                          (setq started-at-end-of-date t)
                        (setq started-in-date t)))
                  (goto-char start-point)))))

          (setq end-point (and started-in-date (point)))

          ;; Finally move forward by date then move to its end.
          (and (cpo-tree-walk--motion-moved
                (lambda ()
                  (dotimes (i (if started-in-date
                                  (- count 1)
                                count))
                    (cpo-forward-date-beginning))))
               (let ((final-bounds (cpo-text-object-stuff--date-bounds-at-point)))
                 (when final-bounds
                   (setq end-point (cdr final-bounds))))))
        (when end-point
          (goto-char end-point)))))))

(defun cpo-text-object-stuff--date-bounds-at-point ()
  "Get bounds of YYYY-MM-DD date at point."
  (let ((regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
    (save-excursion
      (let ((orig (point))
            (result nil))
        (when (looking-at regexp)
          (setq result (cons (match-beginning 0) (match-end 0))))
        (let ((limit (max (- orig 10) (point-min))))
          (while (and (not result) (> (point) limit))
            (backward-char 1)
            (when (and (looking-at regexp)
                       (<= orig (match-end 0)))
              (setq result (cons (match-beginning 0) (match-end 0))))))
        result))))

;; Date-time format: YYYY-MM-DD<sep>HH:MM or YYYY-MM-DD<sep>HH:MM:SS
;; where <sep> can be any character
(defun cpo-text-object-stuff--forward-datetime-beginning ()
  (let ((start-point (point))
        (end-point nil)
        (regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^0-9][0-9]\\{2\\}:[0-9]\\{2\\}\\(:[0-9]\\{2\\}\\)?\\b"))
    (save-mark-and-excursion
      (let ((success (re-search-forward regexp nil t)))
        (when (and success (equal start-point (match-beginning 0)))
          (progn (forward-char 1)
                 (setq success (re-search-forward regexp nil t))))
        (when success
          (setq end-point (match-beginning 0)))))
    (when end-point (goto-char end-point))))

(defun cpo-text-object-stuff--backward-datetime-beginning ()
  (let* ((regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^0-9][0-9]\\{2\\}:[0-9]\\{2\\}\\(:[0-9]\\{2\\}\\)?\\b")
         (success (re-search-backward regexp nil t)))
    (when success
      (goto-char (match-beginning 0)))))

(defun cpo-forward-datetime-beginning (&optional count)
  "Move forward to the beginning of a date-time, COUNT times.
Matches YYYY-MM-DD<sep>HH:MM or YYYY-MM-DD<sep>HH:MM:SS where <sep> is any non-digit.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (i count)
      (funcall (if fwd
                   'cpo-text-object-stuff--forward-datetime-beginning
                 'cpo-text-object-stuff--backward-datetime-beginning)))))

(defun cpo-backward-datetime-beginning (&optional count)
  "Like `cpo-forward-datetime-beginning' but backward."
  (interactive "p")
  (cpo-forward-datetime-beginning (- (or count 1))))

(defun cpo-backward-datetime-end (&optional count)
  "Move backward to the end of a date-time, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-forward-datetime-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (end-point nil))
        (save-mark-and-excursion
          (let* ((start-point (point))
                 (moved (cpo-tree-walk--motion-moved 'cpo-backward-datetime-beginning))
                 (bounds (and moved (cpo-text-object-stuff--datetime-bounds-at-point)))
                 (was-in-datetime (and bounds (<= start-point (cdr bounds)))))
            (cond
             (was-in-datetime (setq moved
                                    (cpo-tree-walk--motion-moved
                                     (lambda () (dotimes (i count)
                                                  (cpo-backward-datetime-beginning))))))
             (moved (dotimes (i (- count 1)) (cpo-backward-datetime-beginning)))
             (t nil))
            (when moved
              (let ((bounds (cpo-text-object-stuff--datetime-bounds-at-point)))
                (and bounds (setq end-point (cdr bounds)))))))
        (when end-point (goto-char end-point)))))))

(defun cpo-forward-datetime-end (&optional count)
  "Move forward to the end of a date-time, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-backward-datetime-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (started-in-datetime nil)
            (started-at-end-of-datetime nil)
            (end-point nil))
        (save-mark-and-excursion

          ;; Check the current bounds of the date-time
          (let ((bounds (cpo-text-object-stuff--datetime-bounds-at-point)))
            ;; If we are already in a date-time, go to its end
            (when (and bounds
                       (<= (car bounds) start-point (cdr bounds)))
              (if (equal start-point (cdr bounds))
                  (setq started-at-end-of-datetime t)
                (progn
                  (setq started-in-datetime t)
                  (goto-char (cdr bounds))))))

          ;; Go backward to find the start of a date-time to check more carefully
          (unless (or started-in-datetime started-at-end-of-datetime)
            (cpo-backward-datetime-beginning)
            (let ((bounds (cpo-text-object-stuff--datetime-bounds-at-point)))
              (when bounds
                (if (<= (car bounds) start-point (cdr bounds))
                    (progn
                      (goto-char (cdr bounds))
                      (if (equal start-point (cdr bounds))
                          (setq started-at-end-of-datetime t)
                        (setq started-in-datetime t)))
                  (goto-char start-point)))))

          (setq end-point (and started-in-datetime (point)))

          ;; Finally move forward by date-time then move to its end.
          (and (cpo-tree-walk--motion-moved
                (lambda ()
                  (dotimes (i (if started-in-datetime
                                  (- count 1)
                                count))
                    (cpo-forward-datetime-beginning))))
               (let ((final-bounds (cpo-text-object-stuff--datetime-bounds-at-point)))
                 (when final-bounds
                   (setq end-point (cdr final-bounds))))))
        (when end-point
          (goto-char end-point)))))))

(defun cpo-text-object-stuff--datetime-bounds-at-point ()
  "Get bounds of YYYY-MM-DD<sep>HH:MM[:SS] date-time at point."
  (let ((regexp "\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^0-9][0-9]\\{2\\}:[0-9]\\{2\\}\\(:[0-9]\\{2\\}\\)?\\b"))
    (save-excursion
      (let ((orig (point))
            (result nil))
        (when (looking-at regexp)
          (setq result (cons (match-beginning 0) (match-end 0))))
        (let ((limit (max (- orig 19) (point-min))))
          (while (and (not result) (> (point) limit))
            (backward-char 1)
            (when (and (looking-at regexp)
                       (<= orig (match-end 0)))
              (setq result (cons (match-beginning 0) (match-end 0))))))
        result))))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-forward-date-beginning 'cpo-backward-date-beginning)
  (repeatable-motion-define-pair 'cpo-forward-date-end 'cpo-backward-date-end)
  (repeatable-motion-define-pair 'cpo-forward-datetime-beginning 'cpo-backward-datetime-beginning)
  (repeatable-motion-define-pair 'cpo-forward-datetime-end 'cpo-backward-datetime-end))

(put 'cpo-date 'bounds-of-thing-at-point 'cpo-text-object-stuff--date-bounds-at-point)
(put 'cpo-datetime 'bounds-of-thing-at-point 'cpo-text-object-stuff--datetime-bounds-at-point)
(cpo-text-object-stuff--def-expand-region-to-thing cpo-date)
(cpo-text-object-stuff--def-expand-region-to-thing cpo-datetime)

;;;; TODO - these traanspose functions aren't working.
;; (cpo-text-object-stuff--define-transpose-funcs
;;  cpo-transpose-date-backward
;;  cpo-transpose-date-forward
;;  'cpo-text-object-stuff--date-bounds-at-point
;;  'cpo-backward-date-beginning
;;  'cpo-forward-date-beginning
;;  )

;; (cpo-text-object-stuff--define-transpose-funcs
;;  cpo-transpose-datetime-backward
;;  cpo-transpose-datetime-forward
;;  'cpo-text-object-stuff--datetime-bounds-at-point
;;  'cpo-backward-datetime-beginning
;;  'cpo-forward-datetime-beginning
;;  )





(provide 'cpo-date-object)
