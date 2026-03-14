;;; cpo-org-structure-block.el --- Composiphrase text object for org-mode structural blocks -*- lexical-binding: t; -*-

;; Org-mode structural blocks are delimited by #+begin_X and #+end_X lines.
;; This file provides motions, selection, and open functions for them.

(require 'cpo-tree-walk)

;;; Regexps

(defconst cpo-org-structure-block--begin-re
  "^[ \t]*#\\+begin_\\([^ \t\n]+\\)"
  "Regexp matching the beginning of an org structure block.
The first capture group is the block type (src, quote, example, etc).")

(defconst cpo-org-structure-block--end-re
  "^[ \t]*#\\+end_\\([^ \t\n]+\\)"
  "Regexp matching the end of an org structure block.")

;;; Internal helpers

(defun cpo-org-structure-block--find-matching-end (&optional bound)
  "From point on a #+begin_ line, find the matching #+end_ line.
Handles nesting of same-type blocks.
Returns the position of the end of the #+end_ line (including newline if present), or nil."
  (save-match-data
    (let ((begin-type (and (looking-at cpo-org-structure-block--begin-re)
                           (downcase (match-string-no-properties 1))))
          (depth 1)
          (found nil))
      (when begin-type
        (let ((type-begin-re (concat "^[ \t]*#\\+begin_" (regexp-quote begin-type) "\\b"))
              (type-end-re (concat "^[ \t]*#\\+end_" (regexp-quote begin-type) "\\b")))
          (save-excursion
            (forward-line 1)
            (while (and (not found) (> depth 0) (not (eobp)))
              (cond
               ((looking-at type-begin-re)
                (setq depth (1+ depth))
                (forward-line 1))
               ((looking-at type-end-re)
                (setq depth (1- depth))
                (if (= depth 0)
                    (progn
                      (setq found (point))
                      ;; Include the full end line
                      (forward-line 1)
                      (setq found (point)))
                  (forward-line 1)))
               (t (forward-line 1))))))
        found))))

(defun cpo-org-structure-block--find-matching-begin (&optional bound)
  "From point on a #+end_ line, find the matching #+begin_ line.
Handles nesting of same-type blocks.
Returns the position of the beginning of the #+begin_ line, or nil."
  (save-match-data
    (let ((end-type (and (save-excursion
                           (beginning-of-line)
                           (looking-at cpo-org-structure-block--end-re))
                         (downcase (match-string-no-properties 1))))
          (depth 1)
          (found nil))
      (when end-type
        (let ((type-begin-re (concat "^[ \t]*#\\+begin_" (regexp-quote end-type) "\\b"))
              (type-end-re (concat "^[ \t]*#\\+end_" (regexp-quote end-type) "\\b")))
          (save-excursion
            (forward-line -1)
            (while (and (not found) (> depth 0))
              (cond
               ((looking-at type-end-re)
                (setq depth (1+ depth))
                (if (bobp)
                    (setq depth 0)
                  (forward-line -1)))
               ((looking-at type-begin-re)
                (setq depth (1- depth))
                (if (= depth 0)
                    (setq found (point))
                  (if (bobp)
                      (setq depth 0)
                    (forward-line -1))))
               (t
                (if (bobp)
                    (setq depth 0)
                  (forward-line -1)))))))
        found))))

(defun cpo-org-structure-block--on-begin-line-p ()
  "Return non-nil if point is on a #+begin_ line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p cpo-org-structure-block--begin-re)))

(defun cpo-org-structure-block--on-end-line-p ()
  "Return non-nil if point is on a #+end_ line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p cpo-org-structure-block--end-re)))

(defun cpo-org-structure-block--goto-begin-of-current-block ()
  "If inside or on a block, move to the beginning of its #+begin_ line.
Returns the position or nil if not in a block."
  (cond
   ;; Already on a begin line
   ((cpo-org-structure-block--on-begin-line-p)
    (beginning-of-line)
    (point))
   ;; On an end line -- find the matching begin
   ((cpo-org-structure-block--on-end-line-p)
    (beginning-of-line)
    (let ((pos (cpo-org-structure-block--find-matching-begin)))
      (when pos
        (goto-char pos)
        pos)))
   ;; Inside a block -- search backward for a begin line, checking nesting
   (t
    (let ((orig (point))
          (found nil))
      (save-excursion
        (while (and (not found)
                    (re-search-backward cpo-org-structure-block--begin-re nil t))
          (let ((candidate (point))
                (end-pos (cpo-org-structure-block--find-matching-end)))
            (when (and end-pos (< orig end-pos))
              (setq found candidate)))))
      (when found
        (goto-char found)
        found)))))

;;; Bounds functions

(defun cpo-org-structure-block--bounds (&optional pt)
  "Get the bounds of the org structure block at or around point.
Bounds include the #+begin_ and #+end_ lines.
Returns (beg . end) or nil."
  (save-excursion
    (when pt (goto-char pt))
    (let ((begin-pos (cpo-org-structure-block--goto-begin-of-current-block)))
      (when begin-pos
        (goto-char begin-pos)
        (let ((end-pos (cpo-org-structure-block--find-matching-end)))
          (when end-pos
            (cons begin-pos end-pos)))))))

(defun cpo-org-structure-block--inner-bounds (&optional pt)
  "Get the inner bounds of the org structure block at or around point.
Inner bounds are the content between #+begin_ and #+end_ lines (excluding those lines).
Returns (beg . end) or nil."
  (save-excursion
    (when pt (goto-char pt))
    (let ((begin-pos (cpo-org-structure-block--goto-begin-of-current-block)))
      (when begin-pos
        (goto-char begin-pos)
        (let ((end-pos (cpo-org-structure-block--find-matching-end)))
          (when end-pos
            ;; Inner start: line after the begin line
            (goto-char begin-pos)
            (forward-line 1)
            (let ((inner-start (point)))
              ;; Inner end: beginning of the end line
              (goto-char end-pos)
              (forward-line -1)
              (end-of-line)
              ;; Actually, find the end_ line by searching backward from end-pos
              (goto-char end-pos)
              ;; end-pos is the line after the #+end_ line, so go back one line to get to its beginning
              (forward-line -1)
              (let ((inner-end (point)))
                (when (<= inner-start inner-end)
                  (cons inner-start inner-end))))))))))

;;; Movement functions

(defun cpo-org-structure-block-forward-beginning (&optional count)
  "Move forward to the beginning of the next org structure block.
With numeric prefix COUNT, move forward that many blocks."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (cpo-org-structure-block-backward-beginning (- count))
    (let ((orig (point)))
      (while (> count 0)
        ;; If on a begin line (at bol), skip past this block
        (when (and (bolp) (cpo-org-structure-block--on-begin-line-p))
          (let ((end-pos (cpo-org-structure-block--find-matching-end)))
            (if end-pos
                (goto-char end-pos)
              (forward-line 1))))
        ;; If inside a block (but not at the start of a begin line), skip past its end
        (unless (and (bolp) (cpo-org-structure-block--on-begin-line-p))
          (let ((bounds (save-excursion
                          (cpo-org-structure-block--bounds))))
            (when (and bounds (< (point) (cdr bounds)))
              (goto-char (cdr bounds)))))
        ;; Search forward for the next begin line
        (if (re-search-forward cpo-org-structure-block--begin-re nil t)
            (progn
              (beginning-of-line)
              (setq count (1- count)))
          ;; No more blocks found, restore original position and stop
          (goto-char orig)
          (setq count 0))))))

(defun cpo-org-structure-block-backward-beginning (&optional count)
  "Move backward to the beginning of the previous org structure block.
With numeric prefix COUNT, move backward that many blocks."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (cpo-org-structure-block-forward-beginning (- count))
    (dotimes (_ count)
      (let ((orig (point)))
        ;; If on a begin line, need to go before this line to find previous
        (when (cpo-org-structure-block--on-begin-line-p)
          (forward-line -1))
        ;; Search backward for a begin line
        (if (re-search-backward cpo-org-structure-block--begin-re nil t)
            (beginning-of-line)
          (goto-char orig))))))

(defun cpo-org-structure-block-forward-end (&optional count)
  "Move forward to the end of the next org structure block.
With numeric prefix COUNT, move forward that many blocks.
Moves to the end of the #+end_ line (after the newline)."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (cpo-org-structure-block-backward-end (- count))
    (let ((orig (point)))
      (while (> count 0)
        ;; If inside a block, go to its end
        (let ((bounds (save-excursion
                        (cpo-org-structure-block--bounds))))
          (if (and bounds (< (point) (cdr bounds)))
              (progn
                (goto-char (cdr bounds))
                (setq count (1- count)))
            ;; Otherwise search forward for next block and go to its end
            (if (re-search-forward cpo-org-structure-block--begin-re nil t)
                (progn
                  (beginning-of-line)
                  (let ((end-pos (cpo-org-structure-block--find-matching-end)))
                    (if end-pos
                        (progn
                          (goto-char end-pos)
                          (setq count (1- count)))
                      ;; Malformed block, skip past this begin line and retry
                      (forward-line 1))))
              ;; No more blocks found, restore original position and stop
              (goto-char orig)
              (setq count 0))))))))

(defun cpo-org-structure-block-backward-end (&optional count)
  "Move backward to the end of the previous org structure block.
With numeric prefix COUNT, move backward that many blocks.
Moves to the end of the #+end_ line (after the newline)."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (cpo-org-structure-block-forward-end (- count))
    (let ((orig (point)))
      (dotimes (_ count)
        ;; Search backward for an end line
        (let ((found nil))
          (save-excursion
            (while (and (not found)
                        (re-search-backward cpo-org-structure-block--end-re nil t))
              (beginning-of-line)
              ;; Find the matching begin to get proper bounds
              (let ((end-line-pos (point))
                    (begin-pos (cpo-org-structure-block--find-matching-begin)))
                (when begin-pos
                  ;; Get the full end position (after the newline)
                  (goto-char begin-pos)
                  (let ((end-pos (cpo-org-structure-block--find-matching-end)))
                    (when (and end-pos (< end-pos orig))
                      (setq found end-pos))))
                ;; If we didn't find it, continue searching from before this end line
                (unless found
                  (goto-char end-line-pos)))))
          (if found
              (progn
                (goto-char found)
                (setq orig found))
            (goto-char orig)))))))

;;; Expand region

(defun cpo-org-structure-block--set-region-with-position (bounds position)
  "Set the active region to BOUNDS with POSITION controlling point placement.
POSITION nil or \\='beginning puts point at beginning, \\='end puts point at end."
  (if (eq position 'end)
      (progn (goto-char (cdr bounds))
             (set-mark (car bounds)))
    (goto-char (car bounds))
    (set-mark (cdr bounds))))

(cl-defun cpo-org-structure-block-expand-region (&key strict position)
  "Expand region to the bounds of the org structure block at point.
When STRICT is non-nil, only expand if the new bounds are strictly larger
than the current region.
POSITION controls where point ends up: nil or \\='beginning puts point
at the beginning of the region, \\='end puts point at the end."
  (interactive)
  (let ((bounds (cpo-org-structure-block--bounds)))
    (when bounds
      (if strict
          (let ((current-region (if (region-active-p)
                                    (cons (region-beginning) (region-end))
                                  (cons (point) (point)))))
            (when (or (< (car bounds) (car current-region))
                      (> (cdr bounds) (cdr current-region)))
              (cpo-org-structure-block--set-region-with-position bounds position)
              (activate-mark)))
        (cpo-org-structure-block--set-region-with-position bounds position)
        (activate-mark)))))

(cl-defun cpo-org-structure-block-expand-region-inner (&key strict position)
  "Expand region to the inner bounds of the org structure block at point.
When STRICT is non-nil, only expand if the new bounds are strictly larger
than the current region.
POSITION controls where point ends up: nil or \\='beginning puts point
at the beginning of the region, \\='end puts point at the end."
  (interactive)
  (let ((bounds (cpo-org-structure-block--inner-bounds)))
    (when bounds
      (if strict
          (let ((current-region (if (region-active-p)
                                    (cons (region-beginning) (region-end))
                                  (cons (point) (point)))))
            (when (or (< (car bounds) (car current-region))
                      (> (cdr bounds) (cdr current-region)))
              (cpo-org-structure-block--set-region-with-position bounds position)
              (activate-mark)))
        (cpo-org-structure-block--set-region-with-position bounds position)
        (activate-mark)))))

;;; Open function

(defun cpo-org-structure-block-open (&optional block-type)
  "Insert a new org structure block and place point inside it.
BLOCK-TYPE defaults to \"src\" (the most common use case).
Inserts #+begin_TYPE / #+end_TYPE pair."
  (interactive)
  (let ((type (or block-type "src")))
    (let ((indent (current-indentation))
          (indent-str (make-string (current-indentation) ?\s)))
      ;; If not at the beginning of a line, go to end of line and add a newline
      (unless (bolp)
        (end-of-line)
        (newline))
      (insert indent-str "#+begin_" type "\n")
      (let ((content-pos (point)))
        (insert indent-str "\n")
        (insert indent-str "#+end_" type "\n")
        (goto-char content-pos)
        ;; Insert proper indentation for content line
        content-pos))))

;;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-org-structure-block-forward-beginning 'cpo-org-structure-block-backward-beginning)
  (repeatable-motion-define-pair 'cpo-org-structure-block-forward-end 'cpo-org-structure-block-backward-end))

(provide 'cpo-org-structure-block)
;;; cpo-org-structure-block.el ends here
