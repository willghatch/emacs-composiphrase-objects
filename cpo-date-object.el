(require 'cpo-text-object-stuff)
(require 'cl-lib)

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

(cpo-text-object-stuff--define-transpose-funcs
 cpo-transpose-date-backward
 cpo-transpose-date-forward
 'cpo-text-object-stuff--date-bounds-at-point
 'cpo-backward-date-beginning
 'cpo-forward-date-beginning
 )

(cpo-text-object-stuff--define-transpose-funcs
 cpo-transpose-datetime-backward
 cpo-transpose-datetime-forward
 'cpo-text-object-stuff--datetime-bounds-at-point
 'cpo-backward-datetime-beginning
 'cpo-forward-datetime-beginning
 )


;;;; Chronological movement

(defun cpo-date--collect-dates-in-buffer (format)
  "Scan the entire buffer for date strings matching FORMAT.
FORMAT is either \\='date for YYYY-MM-DD or \\='datetime for
YYYY-MM-DD<sep>HH:MM[:SS].
Returns a list of (DATE-STRING BUFFER-START BUFFER-END), sorted
chronologically by the date string."
  (let* ((regexp (pcase format
                   ('date "\\b\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
                   ('datetime "\\b\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^0-9][0-9]\\{2\\}:[0-9]\\{2\\}\\(:[0-9]\\{2\\}\\)?\\)\\b")
                   (_ (error "cpo-date--collect-dates-in-buffer: unknown format %s" format))))
         (results nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((date-str (match-string-no-properties (if (eq format 'datetime) 0 1)))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (push (list date-str beg end) results))))
    ;; Sort chronologically by the date string.  For dates this is
    ;; lexicographic; for datetimes the separator character may vary,
    ;; so normalise by replacing the separator with a space before
    ;; comparing (the date portion is always 10 chars).
    (sort (nreverse results)
          (lambda (a b)
            (let ((sa (car a))
                  (sb (car b)))
              (when (eq format 'datetime)
                (setq sa (concat (substring sa 0 10) " " (substring sa 11)))
                (setq sb (concat (substring sb 0 10) " " (substring sb 11))))
              (string< sa sb))))))

(defun cpo-date--current-chronological-index (sorted-dates)
  "Return the index in SORTED-DATES of the date whose buffer position
is nearest to and at or before point, using chronological order.
If point is before all dates, returns -1.
If point is inside or on a date, returns that date's index.
Otherwise returns the index of the chronologically nearest date that
starts at or before point."
  (let ((pt (point))
        (best-idx -1)
        (idx 0))
    (dolist (entry sorted-dates)
      (let ((beg (nth 1 entry))
            (end (nth 2 entry)))
        (when (<= beg pt)
          ;; Among dates whose buffer start is at or before point,
          ;; pick the one that comes latest in chronological order
          ;; (highest index in the sorted list).  But prefer a date
          ;; that actually contains point over one that merely
          ;; starts earlier.
          (if (and (<= beg pt) (< pt end))
              ;; Point is inside this date -- strongest match.
              (setq best-idx idx)
            ;; Point is after this date's start.  Accept if we have
            ;; no match yet, or if this date's buffer start is closer
            ;; to point than the current best (break ties by
            ;; preferring higher chronological index).
            (when (or (= best-idx -1)
                      (> beg (nth 1 (nth best-idx sorted-dates)))
                      (and (= beg (nth 1 (nth best-idx sorted-dates)))
                           (> idx best-idx)))
              (setq best-idx idx)))))
      (setq idx (1+ idx)))
    best-idx))

(cl-defun cpo-date-move-chronologically (&key
                                          (direction 'forward)
                                          relative
                                          absolute
                                          (format 'date)
                                          (position 'beginning))
  "Move to a date in the buffer based on chronological ordering.

Scans the entire buffer for date strings (controlled by FORMAT,
which is \\='date or \\='datetime), sorts them chronologically,
and moves to one of them.

DIRECTION is \\='forward (default) or \\='backward.  It determines
the sign of relative movement: forward means toward later dates,
backward means toward earlier dates.

RELATIVE is an integer offset from the current chronological
position.  Default is 1 unless ABSOLUTE is given.  May be
negative (which reverses the effect of DIRECTION).

ABSOLUTE is a chronological index (0-based).  If non-nil and
RELATIVE is not given, go to that index directly.  It is an error
to specify both RELATIVE and ABSOLUTE.

FORMAT is \\='date (YYYY-MM-DD, default) or \\='datetime.

POSITION is \\='beginning (default) or \\='end, controlling whether
point lands at the start or end of the target date string."
  (when (and relative absolute)
    (error "cpo-date-move-chronologically: :relative and :absolute are mutually exclusive"))
  (let* ((sorted-dates (cpo-date--collect-dates-in-buffer format))
         (n (length sorted-dates)))
    (when (= n 0)
      (cl-return-from cpo-date-move-chronologically nil))
    (let* ((use-absolute (and absolute (not relative)))
           (target-idx
            (if use-absolute
                ;; Absolute positioning
                (let ((idx absolute))
                  (cond ((< idx 0) 0)
                        ((>= idx n) (1- n))
                        (t idx)))
              ;; Relative positioning
              (let* ((rel (or relative 1))
                     (signed-offset (if (eq direction 'backward) (- rel) rel))
                     (cur-idx (cpo-date--current-chronological-index sorted-dates))
                     (raw (+ cur-idx signed-offset)))
                (cond ((< raw 0) 0)
                      ((>= raw n) (1- n))
                      (t raw)))))
           (target (nth target-idx sorted-dates))
           (dest (if (eq position 'end)
                     (nth 2 target)
                   (nth 1 target))))
      (goto-char dest))))

(defun cpo-forward-date-beginning-chronological ()
  "Move forward chronologically to the beginning of the next date."
  (interactive)
  (cpo-date-move-chronologically :direction 'forward :relative 1
                                 :format 'date :position 'beginning))

(defun cpo-backward-date-beginning-chronological ()
  "Move backward chronologically to the beginning of the previous date."
  (interactive)
  (cpo-date-move-chronologically :direction 'backward :relative 1
                                 :format 'date :position 'beginning))

(defun cpo-forward-date-end-chronological ()
  "Move forward chronologically to the end of the next date."
  (interactive)
  (cpo-date-move-chronologically :direction 'forward :relative 1
                                 :format 'date :position 'end))

(defun cpo-backward-date-end-chronological ()
  "Move backward chronologically to the end of the previous date."
  (interactive)
  (cpo-date-move-chronologically :direction 'backward :relative 1
                                 :format 'date :position 'end))

(defun cpo-forward-datetime-beginning-chronological ()
  "Move forward chronologically to the beginning of the next datetime."
  (interactive)
  (cpo-date-move-chronologically :direction 'forward :relative 1
                                 :format 'datetime :position 'beginning))

(defun cpo-backward-datetime-beginning-chronological ()
  "Move backward chronologically to the beginning of the previous datetime."
  (interactive)
  (cpo-date-move-chronologically :direction 'backward :relative 1
                                 :format 'datetime :position 'beginning))

(defun cpo-forward-datetime-end-chronological ()
  "Move forward chronologically to the end of the next datetime."
  (interactive)
  (cpo-date-move-chronologically :direction 'forward :relative 1
                                 :format 'datetime :position 'end))

(defun cpo-backward-datetime-end-chronological ()
  "Move backward chronologically to the end of the previous datetime."
  (interactive)
  (cpo-date-move-chronologically :direction 'backward :relative 1
                                 :format 'datetime :position 'end))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-forward-date-beginning-chronological 'cpo-backward-date-beginning-chronological)
  (repeatable-motion-define-pair 'cpo-forward-date-end-chronological 'cpo-backward-date-end-chronological)
  (repeatable-motion-define-pair 'cpo-forward-datetime-beginning-chronological 'cpo-backward-datetime-beginning-chronological)
  (repeatable-motion-define-pair 'cpo-forward-datetime-end-chronological 'cpo-backward-datetime-end-chronological))


;;;; Chronological transpose

(defun cpo-date--transpose-chronological-once (format direction)
  "Swap the date at point with its chronologically adjacent date.
FORMAT is \\='date or \\='datetime.
DIRECTION is \\='forward or \\='backward.
Returns non-nil if a swap was performed."
  (let* ((original-point (point))
         (sorted-dates (cpo-date--collect-dates-in-buffer format))
         (n (length sorted-dates))
         (cur-idx (and (> n 0) (cpo-date--current-chronological-index sorted-dates))))
    (when (and cur-idx (>= cur-idx 0))
      (let* ((adj-idx (if (eq direction 'forward) (1+ cur-idx) (1- cur-idx))))
        (when (and (>= adj-idx 0) (< adj-idx n) (/= adj-idx cur-idx))
          (let* ((cur-entry (nth cur-idx sorted-dates))
                 (adj-entry (nth adj-idx sorted-dates))
                 (cur-beg (nth 1 cur-entry))
                 (cur-end (nth 2 cur-entry))
                 (adj-beg (nth 1 adj-entry))
                 (adj-end (nth 2 adj-entry))
                 (offset (- original-point cur-beg))
                 ;; Determine which is left/right in the buffer
                 (left-beg (min cur-beg adj-beg))
                 (left-end (if (= left-beg cur-beg) cur-end adj-end))
                 (right-beg (max cur-beg adj-beg))
                 (right-end (if (= right-beg cur-beg) cur-end adj-end))
                 (sl (buffer-substring-no-properties left-beg left-end))
                 (sr (buffer-substring-no-properties right-beg right-end)))
            ;; Swap regions (right first to preserve left positions)
            (atomic-change-group
              (delete-region right-beg right-end)
              (goto-char right-beg)
              (insert sl)
              (delete-region left-beg left-end)
              (goto-char left-beg)
              (insert sr))
            ;; Move point to where the current date ended up,
            ;; preserving the offset of point within the object.
            ;; If current was the left one, it moved to the right position
            ;; (adjusted for any length difference).
            ;; If current was the right one, it moved to the left position.
            (if (= cur-beg left-beg)
                (let ((len-diff (- (length sr) (length sl))))
                  (goto-char (+ right-beg len-diff offset)))
              (goto-char (+ left-beg offset)))
            (undo-boundary)
            t))))))

(defun cpo-transpose-date-forward-chronological (&optional count)
  "Swap date at point with the chronologically next date, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (cpo-date--transpose-chronological-once
       'date (if fwd 'forward 'backward)))))

(defun cpo-transpose-date-backward-chronological (&optional count)
  "Swap date at point with the chronologically previous date, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (cpo-date--transpose-chronological-once
       'date (if fwd 'backward 'forward)))))

(defun cpo-transpose-datetime-forward-chronological (&optional count)
  "Swap datetime at point with the chronologically next datetime, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (cpo-date--transpose-chronological-once
       'datetime (if fwd 'forward 'backward)))))

(defun cpo-transpose-datetime-backward-chronological (&optional count)
  "Swap datetime at point with the chronologically previous datetime, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (cpo-date--transpose-chronological-once
       'datetime (if fwd 'backward 'forward)))))

(provide 'cpo-date-object)
