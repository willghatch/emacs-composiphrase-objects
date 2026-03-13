;;; cpo-git-gutter.el --- Composiphrase text object for git-gutter hunks -*- lexical-binding: t; -*-
;;
;; A composiphrase text object for navigating and selecting git-gutter hunks.
;; Hunks are regions of the buffer that differ from some git revision.
;;
;; Dependencies:
;;   - git-gutter (https://github.com/emacsorphanage/git-gutter)
;;
;; git-gutter compares the current buffer against a base revision.  By default
;; it diffs against the index (staged content).  You can change the base
;; revision with `git-gutter:set-start-revision' to diff against any commit,
;; branch, or tag.  The variable `git-gutter:start-revision' holds the current
;; base revision (buffer-local, nil means use the index).
;;
;; This file also provides `cpo-git-gutter-set-merge-base-revision', which
;; sets the start revision to the merge-base of HEAD and a given branch,
;; making it easy to see only the changes on the current branch.
;;
;; Provides:
;;   - Forward/backward movement to hunk beginnings and ends
;;   - Bounds and selection of the hunk at point
;;   - Cross-file navigation to next/previous file with changes
;;   - Helper to set revision to merge-base of a branch

(require 'cl-lib)

;;; Internal helpers

(defun cpo-git-gutter--ensure-diffinfos ()
  "Return the current buffer's git-gutter diffinfos, or nil.
Does not trigger a refresh."
  (and (bound-and-true-p git-gutter-mode)
       (bound-and-true-p git-gutter:diffinfos)))

(defun cpo-git-gutter--line-point (line)
  "Return the buffer position of the beginning of LINE (1-based)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (point))))

(defun cpo-git-gutter--line-end-point (line)
  "Return the buffer position of the end of LINE (1-based)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (line-end-position))))

(defun cpo-git-gutter--hunk-start-pos (hunk)
  "Return the buffer position of the start of HUNK."
  (cpo-git-gutter--line-point (git-gutter-hunk-start-line hunk)))

(defun cpo-git-gutter--hunk-end-pos (hunk)
  "Return the buffer position of the end of HUNK.
For deleted hunks (type is `deleted'), the end equals the start
because no text was added to the buffer.
For added and modified hunks, the end is the position after the
trailing newline of the last line (i.e., the beginning of the next
line).  If the last line has no trailing newline (end of buffer
without a final newline), use the end of that line instead."
  (if (eq (git-gutter-hunk-type hunk) 'deleted)
      (cpo-git-gutter--line-point (git-gutter-hunk-end-line hunk))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- (git-gutter-hunk-end-line hunk)))
        (let ((eol (line-end-position)))
          (if (= eol (point-max))
              eol
            (1+ eol)))))))

(defun cpo-git-gutter--hunk-and-index-at-point ()
  "Return (INDEX . HUNK) for the git-gutter hunk at point, or nil.
Uses position-based comparison for the end boundary so that point
at the hunk end position (which may be on the line after the last
hunk line) still matches the hunk."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (pos (point)))
    (cl-loop for hunk in diffinfos
             for i from 0
             for start-pos = (cpo-git-gutter--hunk-start-pos hunk)
             for end-pos = (cpo-git-gutter--hunk-end-pos hunk)
             when (and (<= start-pos pos) (<= pos end-pos))
             return (cons i hunk))))

(defun cpo-git-gutter--hunk-at-point ()
  "Return the git-gutter hunk at point, or nil.
Uses position-based comparison for the end boundary so that point
at the hunk end position (which may be on the line after the last
hunk line) still matches the hunk."
  (cdr (cpo-git-gutter--hunk-and-index-at-point)))

(defun cpo-git-gutter--hunk-index-at-point ()
  "Return the index of the hunk at point in `git-gutter:diffinfos', or nil.
Uses position-based comparison for the end boundary so that point
at the hunk end position (which may be on the line after the last
hunk line) still matches the hunk."
  (car (cpo-git-gutter--hunk-and-index-at-point)))

(defun cpo-git-gutter--next-hunk-index-forward ()
  "Return the index of the first hunk that starts after point, or nil."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (cur-line (line-number-at-pos)))
    (cl-loop for hunk in diffinfos
             for i from 0
             for start = (git-gutter-hunk-start-line hunk)
             when (> start cur-line)
             return i)))

(defun cpo-git-gutter--next-hunk-index-backward ()
  "Return the index of the last hunk that ends before point, or nil."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (pos (point)))
    (cl-loop for hunk in (reverse diffinfos)
             for i downfrom (1- (length diffinfos))
             for end-pos = (cpo-git-gutter--hunk-end-pos hunk)
             when (< end-pos pos)
             return i)))

;;; Movement functions

(cl-defun cpo-git-gutter-hunk-forward-beginning (&key (count 1) cross-files)
  "Move forward to the beginning of the next hunk.
If currently in a hunk, move to the beginning of the next hunk.
If COUNT is negative, move backward.
When CROSS-FILES is non-nil and there is no next hunk in the current
buffer, move to the first hunk in the next changed file (or previous
file when going backward)."
  (interactive (list :count (prefix-numeric-value current-prefix-arg)))
  (let* ((fwd (>= count 0))
         (n (abs count)))
    (if fwd
        (dotimes (_i n)
          (unless (cpo-git-gutter--forward-beginning-single)
            (when cross-files
              (cpo-git-gutter-hunk-forward-file 1))))
      (dotimes (_i n)
        (unless (cpo-git-gutter--backward-beginning-single)
          (when cross-files
            (cpo-git-gutter-hunk-backward-file 1)))))))

(defun cpo-git-gutter--forward-beginning-single ()
  "Move forward to the beginning of the next hunk.  Return non-nil if moved."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (start (point)))
    (when diffinfos
      (let* ((cur-line (line-number-at-pos))
             ;; Find the first hunk whose start line is strictly after
             ;; the current line, OR whose start line is the current
             ;; line but we are not already at the beginning of it.
             (target-idx
              (cl-loop for hunk in diffinfos
                       for i from 0
                       for hunk-start = (git-gutter-hunk-start-line hunk)
                       when (> hunk-start cur-line)
                       return i
                       ;; If we're on the start line of a hunk but
                       ;; not at column 0, that hunk is still "ahead".
                       when (and (= hunk-start cur-line)
                                 (> (point) (cpo-git-gutter--line-point hunk-start)))
                       return i)))
        (when target-idx
          (let ((target (nth target-idx diffinfos)))
            (goto-char (cpo-git-gutter--hunk-start-pos target))
            (not (= start (point)))))))))

(defun cpo-git-gutter--backward-beginning-single ()
  "Move backward to the beginning of the previous hunk.  Return non-nil if moved.
If point is in the middle of a hunk (past its start), move to the start of that hunk."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (start (point)))
    (when diffinfos
      (let* ((cur-line (line-number-at-pos))
             ;; First check: are we in a hunk but not at its beginning?
             (in-hunk (cpo-git-gutter--hunk-at-point))
             (at-hunk-start (and in-hunk
                                 (= (point)
                                    (cpo-git-gutter--hunk-start-pos in-hunk)))))
        (cond
         ;; In a hunk but not at its start: go to start.
         ((and in-hunk (not at-hunk-start))
          (goto-char (cpo-git-gutter--hunk-start-pos in-hunk))
          (not (= start (point))))
         ;; At the start of a hunk, or not in a hunk: find previous hunk.
         (t
          (let ((target-idx
                 (cl-loop for hunk in (reverse diffinfos)
                          for i downfrom (1- (length diffinfos))
                          for hunk-start = (git-gutter-hunk-start-line hunk)
                          when (< hunk-start cur-line)
                          return i
                          when (and (= hunk-start cur-line)
                                    (< (cpo-git-gutter--line-point hunk-start) (point)))
                          return i)))
            (when target-idx
              (let ((target (nth target-idx diffinfos)))
                (goto-char (cpo-git-gutter--hunk-start-pos target))
                (not (= start (point))))))))))))

(cl-defun cpo-git-gutter-hunk-backward-beginning (&key (count 1) cross-files)
  "Move backward to the beginning of the previous hunk.
If at the beginning of a hunk, move to beginning of previous hunk.
If COUNT is negative, move forward.
When CROSS-FILES is non-nil and there is no previous hunk in the current
buffer, move to the last hunk in the previous changed file (or next file
when going backward with a negative count)."
  (interactive (list :count (prefix-numeric-value current-prefix-arg)))
  (cpo-git-gutter-hunk-forward-beginning
   :count (- count) :cross-files cross-files))

(cl-defun cpo-git-gutter-hunk-forward-end (&key (count 1) cross-files)
  "Move forward to the end of the current hunk if in one, or end of next hunk.
If COUNT is negative, move backward.
When CROSS-FILES is non-nil and there is no next hunk in the current
buffer, move to the first hunk in the next changed file (or previous
file when going backward)."
  (interactive (list :count (prefix-numeric-value current-prefix-arg)))
  (let* ((fwd (>= count 0))
         (n (abs count)))
    (if fwd
        (dotimes (_i n)
          (unless (cpo-git-gutter--forward-end-single)
            (when cross-files
              (cpo-git-gutter-hunk-forward-file 1))))
      (dotimes (_i n)
        (unless (cpo-git-gutter--backward-end-single)
          (when cross-files
            (cpo-git-gutter-hunk-backward-file 1)))))))

(defun cpo-git-gutter--forward-end-single ()
  "Move forward to the end of the current or next hunk.  Return non-nil if moved.
If in a hunk and before its end, go to its end.
Otherwise, go to the end of the next hunk."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (start (point)))
    (when diffinfos
      (let* ((idx-and-hunk (cpo-git-gutter--hunk-and-index-at-point))
             (cur-idx (car idx-and-hunk))
             (in-hunk (cdr idx-and-hunk))
             (hunk-end (and in-hunk (cpo-git-gutter--hunk-end-pos in-hunk))))
        (cond
         ;; In a hunk and before its end: go to end.
         ((and in-hunk (< (point) hunk-end))
          (goto-char hunk-end)
          (not (= start (point))))
         ;; At end of hunk or not in a hunk: find next hunk.
         (t
          (let ((target-idx
                 (if in-hunk
                     ;; We're at the end of a hunk; find the next one.
                     (and cur-idx
                          (< (1+ cur-idx) (length diffinfos))
                          (1+ cur-idx))
                   ;; Not in a hunk: find the first hunk after point.
                   (cpo-git-gutter--next-hunk-index-forward))))
            (when target-idx
              (let ((target (nth target-idx diffinfos)))
                (goto-char (cpo-git-gutter--hunk-end-pos target))
                (not (= start (point))))))))))))

(defun cpo-git-gutter--backward-end-single ()
  "Move backward to the end of the previous hunk.  Return non-nil if moved."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos))
        (start (point)))
    (when diffinfos
      (let* ((pos (point))
             ;; Find the last hunk whose end position is strictly before point.
             (target-idx
              (cl-loop for hunk in (reverse diffinfos)
                       for i downfrom (1- (length diffinfos))
                       for hunk-end-pos = (cpo-git-gutter--hunk-end-pos hunk)
                       when (< hunk-end-pos pos)
                       return i)))
        (when target-idx
          (let ((target (nth target-idx diffinfos)))
            (goto-char (cpo-git-gutter--hunk-end-pos target))
            (not (= start (point)))))))))

(cl-defun cpo-git-gutter-hunk-backward-end (&key (count 1) cross-files)
  "Move backward to the end of the previous hunk.
If COUNT is negative, move forward.
When CROSS-FILES is non-nil and there is no previous hunk in the current
buffer, move to the last hunk in the previous changed file (or next file
when going backward with a negative count)."
  (interactive (list :count (prefix-numeric-value current-prefix-arg)))
  (cpo-git-gutter-hunk-forward-end
   :count (- count) :cross-files cross-files))

;;; Bounds and selection

(defun cpo-git-gutter-hunk-get-bounds ()
  "Return (BEG . END) cons of the hunk at point, or nil.
BEG is the beginning of the start line, END is the end of the end line."
  (let ((hunk (cpo-git-gutter--hunk-at-point)))
    (when hunk
      (cons (cpo-git-gutter--hunk-start-pos hunk)
            (cpo-git-gutter--hunk-end-pos hunk)))))

(cl-defun cpo-git-gutter-hunk-expand-region (&key position)
  "Expand region to the git-gutter hunk at point.
If the region is already active and covers the current hunk, this is
a no-op.  POSITION controls where point ends up: nil or \\='beginning
puts point at the beginning, \\='end puts point at the end.
When POSITION is nil and only one side of the region actually
expanded, point is placed at the end that moved."
  (interactive)
  (let* ((bounds (cpo-git-gutter-hunk-get-bounds))
         (orig-region (if (region-active-p)
                          (cons (region-beginning) (region-end))
                        (cons (point) (point)))))
    (when bounds
      (let* ((new-beg (min (car bounds) (car orig-region)))
             (new-end (max (cdr bounds) (cdr orig-region)))
             (beg-grew (< new-beg (car orig-region)))
             (end-grew (> new-end (cdr orig-region))))
        (when (or beg-grew end-grew)
          (let ((effective-position
                 (cond
                  (position position)
                  ;; When :position is nil and one end of the original
                  ;; region was already strictly beyond the hunk bounds
                  ;; but the other end moved, put point at the end that
                  ;; moved.
                  ((and end-grew (not beg-grew)
                        (< (car orig-region) (car bounds)))
                   'end)
                  ((and beg-grew (not end-grew)
                        (> (cdr orig-region) (cdr bounds)))
                   'beginning)
                  (t 'beginning))))
            (if (eq effective-position 'end)
                (progn
                  (set-mark new-beg)
                  (goto-char new-end))
              (set-mark new-end)
              (goto-char new-beg))
            (activate-mark)
            (cons new-beg new-end)))))))

;;; Cross-file navigation

(defun cpo-git-gutter--changed-files ()
  "Return a sorted list of files with changes relative to the start revision.
Uses `git diff --name-only' from `git-gutter:start-revision' (or HEAD if unset).
Returns paths relative to the repository root."
  (let* ((default-directory (or (locate-dominating-file default-directory ".git")
                                default-directory))
         (rev (if (and (bound-and-true-p git-gutter:start-revision)
                       (not (string-empty-p git-gutter:start-revision)))
                  git-gutter:start-revision
                nil))
         (args (if rev
                   (list "diff" "--name-only" rev)
                 (list "diff" "--name-only"))))
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil t nil args))
        (let ((files nil))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-empty-p line)
                (push line files)))
            (forward-line 1))
          (sort (nreverse files) #'string<))))))

(defun cpo-git-gutter--current-file-relative ()
  "Return the current buffer's file path relative to the git repo root, or nil."
  (let ((file (buffer-file-name))
        (root (locate-dominating-file default-directory ".git")))
    (when (and file root)
      (file-relative-name file root))))

(defun cpo-git-gutter-hunk-forward-file (&optional count)
  "Move to the first hunk in the next file that has changes, COUNT times.
Opens the file if not already open.  Uses `git diff --name-only'
relative to `git-gutter:start-revision'.
If COUNT is negative, move backward through files."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (>= count 0))
        (n (abs count)))
    (dotimes (_i n)
      (cpo-git-gutter--navigate-file (if fwd 1 -1)))))

(defun cpo-git-gutter-hunk-backward-file (&optional count)
  "Move to the first hunk in the previous file that has changes, COUNT times.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-git-gutter-hunk-forward-file (- (or count 1))))

(defun cpo-git-gutter--navigate-file (direction)
  "Navigate to the next (DIRECTION=1) or previous (DIRECTION=-1) changed file.
Opens the file and moves to its first hunk."
  (let* ((files (cpo-git-gutter--changed-files))
         (cur-file (cpo-git-gutter--current-file-relative))
         (root (locate-dominating-file default-directory ".git"))
         (n (length files)))
    (when (and files root (> n 0))
      (let* ((cur-idx (cl-position cur-file files :test #'string=))
             (target-idx (cond
                          ;; Current file is in the list: go to next/prev.
                          (cur-idx
                           (let ((raw (+ cur-idx direction)))
                             (when (and (>= raw 0) (< raw n))
                               raw)))
                          ;; Current file not in list: go to first/last.
                          ((> direction 0) 0)
                          (t (1- n)))))
        (when target-idx
          (let ((target-file (expand-file-name (nth target-idx files) root)))
            (find-file target-file)
            ;; Ensure git-gutter is active and give it a moment.
            (unless (bound-and-true-p git-gutter-mode)
              (git-gutter-mode 1))
            ;; Try to jump to first hunk once diffinfos are available.
            ;; git-gutter computes diffs asynchronously, so diffinfos may
            ;; not be ready yet.  If they are, jump immediately; otherwise
            ;; set up a one-shot timer to retry.
            (if (cpo-git-gutter--ensure-diffinfos)
                (cpo-git-gutter--goto-first-hunk)
              (run-with-idle-timer
               0.5 nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (cpo-git-gutter--goto-first-hunk))))
               (current-buffer)))))))))

(defun cpo-git-gutter--goto-first-hunk ()
  "Move to the first hunk in the current buffer, if any."
  (let ((diffinfos (cpo-git-gutter--ensure-diffinfos)))
    (when diffinfos
      (let ((first-hunk (car diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos first-hunk))))))

;;; Helper: set revision to merge-base

(defun cpo-git-gutter-set-merge-base-revision (branch)
  "Set `git-gutter:start-revision' to the merge-base of BRANCH and HEAD.
This makes git-gutter show only changes on the current branch relative
to where it diverged from BRANCH.  Prompts for a branch name,
defaulting to \"main\".  Then calls `git-gutter' to refresh."
  (interactive
   (list (read-string "Branch for merge-base (default main): " nil nil "main")))
  (let* ((default-directory (or (locate-dominating-file default-directory ".git")
                                default-directory))
         (merge-base
          (with-temp-buffer
            (if (zerop (process-file "git" nil t nil "merge-base" branch "HEAD"))
                (string-trim (buffer-string))
              nil))))
    (if merge-base
        (progn
          (setq git-gutter:start-revision merge-base)
          (git-gutter)
          (message "git-gutter revision set to merge-base: %s" (substring merge-base 0 (min 12 (length merge-base)))))
      (error "Could not determine merge-base of %s and HEAD" branch))))

;;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-git-gutter-hunk-forward-beginning
                                 'cpo-git-gutter-hunk-backward-beginning)
  (repeatable-motion-define-pair 'cpo-git-gutter-hunk-forward-end
                                 'cpo-git-gutter-hunk-backward-end)
  (repeatable-motion-define-pair 'cpo-git-gutter-hunk-forward-file
                                 'cpo-git-gutter-hunk-backward-file))

(provide 'cpo-git-gutter)
;;; cpo-git-gutter.el ends here
