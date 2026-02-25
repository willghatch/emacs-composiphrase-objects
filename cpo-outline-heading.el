;;; cpo-outline-heading.el --- Text object for outline-mode / org-mode headings -*- lexical-binding: t; -*-
;;
;; This provides a heading text object focused on the heading line itself and
;; its parts, as opposed to cpo-outline which treats headings as nodes of a
;; tree.  This object provides motions (forward/backward movement between
;; headings) and multiple selection modes:
;;
;; - Select: the entire heading line (including stars, space, and heading text)
;; - Select inner: just the heading text (after the asterisks and space)
;; - Select body: the body text under the heading, up to but not including
;;   the next heading
;; - Select prefix: just the asterisk/bullet prefix (the stars and trailing space)

(defconst cpo-outline-heading--regexp "^\\*+ "
  "Regexp matching an org-mode heading line prefix (asterisks followed by space).")

;;; Motion functions

(defun cpo-outline-heading--forward-beginning-single ()
  "Move forward to the beginning of a heading.  Return non-nil if moved."
  (let ((start (point)))
    (end-of-line)
    (if (re-search-forward cpo-outline-heading--regexp nil t)
        (progn (goto-char (match-beginning 0))
               (not (= start (point))))
      (goto-char start)
      nil)))

(defun cpo-outline-heading--backward-beginning-single ()
  "Move backward to the beginning of a heading.  Return non-nil if moved.
If point is in the middle of a heading line (past its beginning), move to
the beginning of that heading line instead of searching backward."
  (let ((start (point)))
    (beginning-of-line)
    (cond
     ;; On a heading line and we were past the beginning -- stay here.
     ((and (looking-at-p cpo-outline-heading--regexp)
           (not (= start (point))))
      t)
     ;; Otherwise, search backward for a heading.
     ((re-search-backward cpo-outline-heading--regexp nil t)
      (goto-char (match-beginning 0))
      (not (= start (point))))
     ;; No heading found.
     (t (goto-char start)
        nil))))

(defun cpo-outline-heading--forward-end-single ()
  "Move forward to the end of a heading line.  Return non-nil if moved.
If point is on a heading line but before its end, and there are more headings
after, move to the end of that heading line.  Otherwise, search forward for
the next heading and go to its end."
  (let ((start (point)))
    (if (and (cpo-outline-heading--on-heading-p)
             (not (eolp))
             (save-excursion
               (end-of-line)
               (re-search-forward cpo-outline-heading--regexp nil t)))
        ;; On a heading line before its end with more headings ahead.
        (progn (end-of-line)
               t)
      ;; At end of heading, not on a heading, or no more headings ahead.
      (end-of-line)
      (if (re-search-forward cpo-outline-heading--regexp nil t)
          (progn (goto-char (match-beginning 0))
                 (end-of-line)
                 (not (= start (point))))
        (goto-char start)
        nil))))

(defun cpo-outline-heading--backward-end-single ()
  "Move backward to the end of the previous heading line.  Return non-nil if moved."
  (let ((start (point)))
    (beginning-of-line)
    (if (re-search-backward cpo-outline-heading--regexp nil t)
        (progn (goto-char (match-beginning 0))
               (end-of-line)
               (not (= start (point))))
      (goto-char start)
      nil)))

(defun cpo-outline-heading-forward-beginning (&optional count)
  "Move forward to the beginning of the next heading, COUNT times.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (if fwd
          (cpo-outline-heading--forward-beginning-single)
        (cpo-outline-heading--backward-beginning-single)))))

(defun cpo-outline-heading-backward-beginning (&optional count)
  "Move backward to the beginning a heading, COUNT times.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-outline-heading-forward-beginning (- (or count 1))))

(defun cpo-outline-heading-forward-end (&optional count)
  "Move forward to the end of a heading line, COUNT times.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (if fwd
          (cpo-outline-heading--forward-end-single)
        (cpo-outline-heading--backward-end-single)))))

(defun cpo-outline-heading-backward-end (&optional count)
  "Move backward to the end of a heading line, COUNT times.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-outline-heading-forward-end (- (or count 1))))

;;; Bounds helper functions

(defun cpo-outline-heading--on-heading-p ()
  "Return non-nil if point is on a heading line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p cpo-outline-heading--regexp)))

(defun cpo-outline-heading--goto-current-heading ()
  "Move to the beginning of the current heading line.
If on a heading line, go to its beginning.  Otherwise, search backward.
Return non-nil if a heading was found."
  (if (cpo-outline-heading--on-heading-p)
      (progn (beginning-of-line) t)
    (cpo-outline-heading--backward-beginning-single)))

(defun cpo-outline-heading--bounds ()
  "Return the bounds of the entire heading line as (BEG . END).
The heading line includes the asterisks, space, and heading text,
but not the newline.  Returns nil if not on or near a heading."
  (save-excursion
    (when (cpo-outline-heading--goto-current-heading)
      (let ((beg (point)))
        (end-of-line)
        (cons beg (point))))))

(defun cpo-outline-heading--inner-bounds ()
  "Return the bounds of just the heading text as (BEG . END).
This is the text after the asterisks and space, up to end of line.
Returns nil if not on or near a heading."
  (save-excursion
    (when (cpo-outline-heading--goto-current-heading)
      (when (looking-at "\\*+ ")
        (let ((beg (match-end 0)))
          (end-of-line)
          (cons beg (point)))))))

(defun cpo-outline-heading--prefix-bounds ()
  "Return the bounds of just the asterisk/bullet prefix as (BEG . END).
This includes the asterisks and trailing space.
Returns nil if not on or near a heading."
  (save-excursion
    (when (cpo-outline-heading--goto-current-heading)
      (when (looking-at "\\*+ ")
        (cons (match-beginning 0) (match-end 0))))))

(defun cpo-outline-heading--body-bounds ()
  "Return the bounds of the body text under the heading as (BEG . END).
The body starts after the heading line (after its newline) and extends
up to but not including the next heading line.  If there is no body
text, returns nil."
  (save-excursion
    (when (cpo-outline-heading--goto-current-heading)
      (let ((body-beg (progn (end-of-line)
                             (if (eobp) nil
                               (forward-char 1)
                               (point)))))
        (when body-beg
          (let ((body-end (if (re-search-forward cpo-outline-heading--regexp nil t)
                              (match-beginning 0)
                            (point-max))))
            ;; Only return bounds if there is actual body content
            (when (< body-beg body-end)
              (cons body-beg body-end))))))))

;;; Selection functions

(defun cpo-outline-heading-select ()
  "Select the entire heading line."
  (interactive)
  (let ((bounds (cpo-outline-heading--bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark))))

(defun cpo-outline-heading-select-inner ()
  "Select just the heading text after the asterisks and space."
  (interactive)
  (let ((bounds (cpo-outline-heading--inner-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark))))

(defun cpo-outline-heading-select-body ()
  "Select the body text under the heading.  This is under cpo-outline-heading-
instead of cpo-outline- because cpo-outline handles trees, so it has an inner
selection that is about the whole outline tree.  The heading object, is
concerned with just one heading and not tree shape.
"
  (interactive)
  (let ((bounds (cpo-outline-heading--body-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark))))

(defun cpo-outline-heading-select-prefix ()
  "Select just the asterisk/bullet prefix (and the space after the bullets)."
  (interactive)
  (let ((bounds (cpo-outline-heading--prefix-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark))))

;;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-outline-heading-forward-beginning
                                 'cpo-outline-heading-backward-beginning)
  (repeatable-motion-define-pair 'cpo-outline-heading-forward-end
                                 'cpo-outline-heading-backward-end))

(provide 'cpo-outline-heading)
;;; cpo-outline-heading.el ends here
