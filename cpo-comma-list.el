;;; cpo-comma-list.el --- Text objects for comma-separated list elements -*- lexical-binding: t; -*-

;; A generic text object for elements in comma-separated lists.
;; Respects balanced delimiters (parens, brackets, braces) so it works
;; correctly across programming languages and English prose.
;;
;;
;; Provides:
;; - Forward/backward movement between elements
;; - Transpose operations (swap element with neighbor)
;; - Selection: just the element content (trimmed whitespace)
;; - Outer selection: element plus trailing comma+space (or leading for last)
;; - Open: edit to open an item forward or backward in the list.

(require 'cpo-text-object-stuff)

;;; Configuration

(defvar-local cpo-comma-list-delimiter-pairs '((?\( ?\))
                                               (?\[ ?\])
                                               (?\{ ?\}))
  "Pairs of matching delimiters.
Each element is a list of length 2: (OPEN CLOSE).")

;;; Internal helpers

(defun cpo-comma-list--open-delimiter-p (ch)
  "If CH is an opening delimiter, return its pair, else nil."
  (assq ch cpo-comma-list-delimiter-pairs))

(defun cpo-comma-list--close-delimiter-p (ch)
  "If CH is a closing delimiter, return its pair, else nil."
  (cl-find ch cpo-comma-list-delimiter-pairs :key #'cadr))

(defun cpo-comma-list--matching-close (open-char)
  "Return the matching close delimiter for OPEN-CHAR."
  (cadr (assq open-char cpo-comma-list-delimiter-pairs)))

(defun cpo-comma-list--matching-open (close-char)
  "Return the matching open delimiter for CLOSE-CHAR."
  (car (cl-find close-char cpo-comma-list-delimiter-pairs :key #'cadr)))

(defun cpo-comma-list--forward-past-balanced (limit)
  "Move forward past balanced delimiters until hitting a comma or LIMIT.
Point is assumed to be inside a list context.
Returns the character that stopped the scan: ?, for comma, a close delimiter char,
or nil if we hit LIMIT."
  (let ((result nil))
    (while (and (< (point) limit) (not result))
      (let ((ch (char-after)))
        (cond
         ;; Hit a comma at this nesting level -- stop.
         ((eq ch ?,) (setq result ?,))
         ;; Opening delimiter -- skip balanced group.
         ((cpo-comma-list--open-delimiter-p ch)
          (let ((close (cpo-comma-list--matching-close ch)))
            (forward-char 1)
            (cpo-comma-list--skip-to-close close limit)))
         ;; Closing delimiter -- stop, we've hit the list end.
         ((cpo-comma-list--close-delimiter-p ch)
          (setq result ch))
         ;; Regular character.
         (t (forward-char 1)))))
    result))

(defun cpo-comma-list--skip-to-close (close-char limit)
  "Skip forward to the matching CLOSE-CHAR, respecting nesting, up to LIMIT.
Point should be just past the opening delimiter."
  (let ((depth 1)
        (open-char (cpo-comma-list--matching-open close-char)))
    (while (and (< (point) limit) (> depth 0))
      (let ((ch (char-after)))
        (cond
         ((eq ch close-char) (setq depth (1- depth)))
         ((eq ch open-char) (setq depth (1+ depth)))
         ;; Also handle other delimiters nested inside.
         ((cpo-comma-list--open-delimiter-p ch)
          (let ((inner-close (cpo-comma-list--matching-close ch)))
            (forward-char 1)
            (cpo-comma-list--skip-to-close inner-close limit)
            ;; skip-to-close leaves point on the close char, we need
            ;; to go past it, but only if we successfully found it.
            (when (and (< (point) limit) (eq (char-after) inner-close))
              (forward-char 1))
            ;; Don't advance again below.
            (setq ch nil))))
        (when ch (forward-char 1))))))

(defun cpo-comma-list--backward-past-balanced (limit)
  "Move backward past balanced delimiters until hitting a comma or LIMIT.
Returns the character that stopped the scan: ?, for comma, an open delimiter char,
or nil if we hit LIMIT."
  (let ((result nil))
    (while (and (> (point) limit) (not result))
      (backward-char 1)
      (let ((ch (char-after)))
        (cond
         ((eq ch ?,) (setq result ?,))
         ;; Closing delimiter -- skip backward past balanced group.
         ((cpo-comma-list--close-delimiter-p ch)
          (let ((open (cpo-comma-list--matching-open ch)))
            (cpo-comma-list--skip-to-open open limit)))
         ;; Opening delimiter -- stop, we've hit the list start.
         ((cpo-comma-list--open-delimiter-p ch)
          (setq result ch))
         (t nil))))
    result))

(defun cpo-comma-list--skip-to-open (open-char limit)
  "Skip backward to the matching OPEN-CHAR, respecting nesting, to LIMIT.
Point should be on the closing delimiter."
  (let ((depth 1)
        (close-char (cpo-comma-list--matching-close open-char)))
    (while (and (> (point) limit) (> depth 0))
      (backward-char 1)
      (let ((ch (char-after)))
        (cond
         ((eq ch open-char) (setq depth (1- depth)))
         ((eq ch close-char) (setq depth (1+ depth)))
         ;; Handle other nested delimiters.
         ((cpo-comma-list--close-delimiter-p ch)
          (let ((inner-open (cpo-comma-list--matching-open ch)))
            (cpo-comma-list--skip-to-open inner-open limit))))))))

(defun cpo-comma-list--find-list-bounds ()
  "Find the bounds of the enclosing comma-separated list context.
Returns (BEG . END) where BEG is after the opening delimiter (or beginning of line)
and END is before the closing delimiter (or end of line).
Returns nil if no suitable context is found."
  (save-excursion
    (let ((start (point))
          (beg nil)
          (end nil))
      ;; Search backward for an opening delimiter or beginning of line.
      (save-excursion
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (backward-char 1)
            (let ((ch (char-after)))
              (cond
               ((cpo-comma-list--open-delimiter-p ch)
                (setq beg (1+ (point)))
                (setq found t))
               ;; If we hit a close delimiter, skip backward past its group.
               ((cpo-comma-list--close-delimiter-p ch)
                (let ((open (cpo-comma-list--matching-open ch)))
                  (cpo-comma-list--skip-to-open open (point-min))))
               (t nil))))
          (unless found
            ;; Use beginning of line as boundary.
            (goto-char start)
            (beginning-of-line)
            (setq beg (point)))))
      ;; Search forward for the matching closing delimiter or end of line.
      (save-excursion
        (goto-char start)
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (let ((ch (char-after)))
              (cond
               ((cpo-comma-list--close-delimiter-p ch)
                (setq end (point))
                (setq found t))
               ((cpo-comma-list--open-delimiter-p ch)
                (let ((close (cpo-comma-list--matching-close ch)))
                  (forward-char 1)
                  (cpo-comma-list--skip-to-close close (point-max))
                  ;; Skip past close char.
                  (when (and (not (eobp))
                             (eq (char-after) close))
                    (forward-char 1))))
               (t (forward-char 1)))))
          (unless found
            (goto-char start)
            (end-of-line)
            (setq end (point)))))
      (when (and beg end (<= beg end))
        (cons beg end)))))

(defun cpo-comma-list--parse-elements (beg end)
  "Parse the comma-separated elements between BEG and END.
Returns a list of (ELEM-BEG . ELEM-END) pairs, one for each element.
These bounds include the raw whitespace around each element."
  (save-excursion
    (goto-char beg)
    (let ((elements nil)
          (elem-start beg))
      (while (< (point) end)
        (let ((stop-char (cpo-comma-list--forward-past-balanced end)))
          (cond
           ((eq stop-char ?,)
            ;; Found a comma: the element runs from elem-start to point.
            (push (cons elem-start (point)) elements)
            (forward-char 1)  ;; skip the comma
            (setq elem-start (point)))
           (t
            ;; Hit a close delimiter or end: last element.
            (push (cons elem-start (point)) elements)
            (goto-char end)))))
      ;; If we exited the loop with elem-start still pointing at
      ;; unconsumed content (or an empty trailing element after a comma),
      ;; add it.  This handles trailing commas and empty lists.
      ;; Note: elements is in reverse order here (most recent push first).
      (when (and (<= elem-start end)
                 ;; Don't duplicate: only add if we haven't already
                 ;; recorded this range as the most recently pushed element.
                 (or (null elements)
                     (not (and (= (cdr (car elements)) end)
                               (= (car (car elements)) elem-start)))))
        (push (cons elem-start end) elements))
      (nreverse elements))))

(defun cpo-comma-list--trim-bounds (beg end)
  "Return (BEG . END) trimmed of leading and trailing whitespace."
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \t\n" end)
    (let ((trimmed-beg (point)))
      (goto-char end)
      (skip-chars-backward " \t\n" trimmed-beg)
      (cons trimmed-beg (point)))))

(defun cpo-comma-list--element-at-point ()
  "Find the comma-list element at point.
Returns a plist (:elements ELEMS :index INDEX :list-bounds BOUNDS)
where ELEMS is a list of (BEG . END) for all elements,
INDEX is the 0-based index of the element containing point,
and BOUNDS is the (BEG . END) of the enclosing list context.
Returns nil if point is not in a comma-separated list."
  (let ((list-bounds (cpo-comma-list--find-list-bounds)))
    (when list-bounds
      (let* ((elements (cpo-comma-list--parse-elements (car list-bounds) (cdr list-bounds)))
             (pt (point))
             (index nil))
        ;; Find which element point is in.
        (cl-loop for elem in elements
                 for i from 0
                 when (and (<= (car elem) pt)
                           ;; For the last element, include the endpoint.
                           (or (<= pt (cdr elem))
                               (and (= i (1- (length elements)))
                                    (= pt (cdr elem)))))
                 do (setq index i)
                 and return nil)
        ;; If point is exactly on a comma (between elements), pick the element
        ;; before the comma.
        (when (and (null index) (eq (char-after) ?,))
          (cl-loop for elem in elements
                   for i from 0
                   when (= (cdr elem) pt)
                   do (setq index i)
                   and return nil))
        ;; If still no match, try to find nearest element.
        (when (and (null index) elements)
          ;; Point might be in whitespace after comma.  Find nearest.
          (let ((min-dist most-positive-fixnum))
            (cl-loop for elem in elements
                     for i from 0
                     do (let ((dist (min (abs (- pt (car elem)))
                                         (abs (- pt (cdr elem))))))
                          (when (< dist min-dist)
                            (setq min-dist dist)
                            (setq index i))))))
        (when index
          (list :elements elements
                :index index
                :list-bounds list-bounds))))))

;;; Inner/outer bounds

(defun cpo-comma-list--inner-bounds (&optional pt)
  "Return (BEG . END) for the inner bounds of the comma-list element at PT.
Inner bounds are the element content trimmed of whitespace."
  (save-excursion
    (when pt (goto-char pt))
    (let ((info (cpo-comma-list--element-at-point)))
      (when info
        (let* ((elements (plist-get info :elements))
               (index (plist-get info :index))
               (elem (nth index elements)))
          (cpo-comma-list--trim-bounds (car elem) (cdr elem)))))))

(defun cpo-comma-list--outer-bounds (&optional pt)
  "Return (BEG . END) for the outer bounds of the comma-list element at PT.
Outer bounds include the trailing comma+space, or leading comma+space for the last element."
  (save-excursion
    (when pt (goto-char pt))
    (let ((info (cpo-comma-list--element-at-point)))
      (when info
        (let* ((elements (plist-get info :elements))
               (index (plist-get info :index))
               (elem (nth index elements))
               (is-last (= index (1- (length elements))))
               (is-only (= (length elements) 1)))
          (cond
           ;; Only element: just the inner bounds (no commas to include).
           (is-only
            (cpo-comma-list--trim-bounds (car elem) (cdr elem)))
           ;; Not the last element: include trailing comma and space.
           ((not is-last)
            (let* ((next-elem (nth (1+ index) elements))
                   (trimmed-next (cpo-comma-list--trim-bounds
                                  (car next-elem) (cdr next-elem)))
                   (trimmed-cur (cpo-comma-list--trim-bounds
                                 (car elem) (cdr elem))))
              (cons (car trimmed-cur) (car trimmed-next))))
           ;; Last element: include leading comma and space.
           (t
            (let* ((prev-elem (nth (1- index) elements))
                   (trimmed-prev (cpo-comma-list--trim-bounds
                                  (car prev-elem) (cdr prev-elem)))
                   (trimmed-cur (cpo-comma-list--trim-bounds
                                 (car elem) (cdr elem))))
              (cons (cdr trimmed-prev) (cdr trimmed-cur))))))))))

;;; Bounds-of-thing-at-point integration

(defun cpo-comma-list--bounds-of-inner-at-point ()
  "Return bounds of inner comma-list element at point."
  (cpo-comma-list--inner-bounds))

(defun cpo-comma-list--bounds-of-outer-at-point ()
  "Return bounds of outer comma-list element at point."
  (cpo-comma-list--outer-bounds))

;;; Motion commands

(defun cpo-comma-list--forward-beginning ()
  "Move forward to the beginning of the next comma-list element.
Returns the new position, or nil if no next element."
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (next-index (1+ index)))
        (when (< next-index (length elements))
          (let* ((next-elem (nth next-index elements))
                 (trimmed (cpo-comma-list--trim-bounds (car next-elem) (cdr next-elem))))
            (goto-char (car trimmed))
            (car trimmed)))))))

(defun cpo-comma-list--backward-beginning ()
  "Move backward to the beginning of the previous comma-list element.
Returns the new position, or nil if no previous element."
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (prev-index (1- index)))
        (when (>= prev-index 0)
          (let* ((prev-elem (nth prev-index elements))
                 (trimmed (cpo-comma-list--trim-bounds (car prev-elem) (cdr prev-elem))))
            (goto-char (car trimmed))
            (car trimmed)))))))

(defun cpo-comma-list--forward-end ()
  "Move forward to the end of the next comma-list element.
The end is the position after the last character of the trimmed element,
before the comma.  Returns the new position, or nil if no next element."
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (cur-elem (nth index elements))
             (cur-trimmed (cpo-comma-list--trim-bounds (car cur-elem) (cdr cur-elem))))
        ;; If point is before the end of the current element, go to its end.
        ;; Otherwise, go to the end of the next element.
        (if (< (point) (cdr cur-trimmed))
            (progn
              (goto-char (cdr cur-trimmed))
              (cdr cur-trimmed))
          (let ((next-index (1+ index)))
            (when (< next-index (length elements))
              (let* ((next-elem (nth next-index elements))
                     (trimmed (cpo-comma-list--trim-bounds (car next-elem) (cdr next-elem))))
                (goto-char (cdr trimmed))
                (cdr trimmed)))))))))

(defun cpo-comma-list--backward-end ()
  "Move backward to the end of the previous comma-list element.
The end is the position after the last character of the trimmed element,
before the comma.  Returns the new position, or nil if no previous element."
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (cur-elem (nth index elements))
             (cur-trimmed (cpo-comma-list--trim-bounds (car cur-elem) (cdr cur-elem))))
        ;; If point is after the beginning of the current element,
        ;; and we're at or before its end, go to previous element's end.
        ;; If point is at or before current beginning, also go to previous.
        (if (and (> (point) (car cur-trimmed))
                 (<= (point) (cdr cur-trimmed)))
            ;; Point is inside current element but not at end -- go to previous end.
            (let ((prev-index (1- index)))
              (when (>= prev-index 0)
                (let* ((prev-elem (nth prev-index elements))
                       (trimmed (cpo-comma-list--trim-bounds (car prev-elem) (cdr prev-elem))))
                  (goto-char (cdr trimmed))
                  (cdr trimmed))))
          ;; Point is at or after current element end, or at/before its beginning.
          (let ((prev-index (1- index)))
            (when (>= prev-index 0)
              (let* ((prev-elem (nth prev-index elements))
                     (trimmed (cpo-comma-list--trim-bounds (car prev-elem) (cdr prev-elem))))
                (goto-char (cdr trimmed))
                (cdr trimmed)))))))))

;;;###autoload
(defun cpo-comma-list-forward-beginning (&optional count)
  "Move forward COUNT comma-list elements to the beginning.
If COUNT is negative, move backward."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (dotimes (_i n)
      (if fwd
          (cpo-comma-list--forward-beginning)
        (cpo-comma-list--backward-beginning)))))

;;;###autoload
(defun cpo-comma-list-backward-beginning (&optional count)
  "Move backward COUNT comma-list elements to the beginning.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-comma-list-forward-beginning (- (or count 1))))

;;;###autoload
(defun cpo-comma-list-forward-end (&optional count)
  "Move forward COUNT comma-list elements to the end.
If COUNT is negative, move backward."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (dotimes (_i n)
      (if fwd
          (cpo-comma-list--forward-end)
        (cpo-comma-list--backward-end)))))

;;;###autoload
(defun cpo-comma-list-backward-end (&optional count)
  "Move backward COUNT comma-list elements to the end.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-comma-list-forward-end (- (or count 1))))

;;; Selection commands

;;;###autoload
(defun cpo-comma-list-select ()
  "Select the content of the comma-list element at point."
  (interactive)
  (let ((bounds (cpo-comma-list--inner-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark)
      bounds)))

;;;###autoload
(defun cpo-comma-list-select-outer ()
  "Select the outer content of the comma-list element at point (including comma separator)."
  (interactive)
  (let ((bounds (cpo-comma-list--outer-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (activate-mark)
      bounds)))

;;; Expand region

(cl-defun cpo-comma-list-expand-region (&key position)
  "Expand region to comma-list element at point.
POSITION controls where point ends up: nil or \\='beginning puts point
at the beginning of the region, \\='end puts point at the end."
  (interactive)
  (cpo-text-object-stuff--expand-region-to-thing 'cpo-comma-list nil :position position))

(cl-defun cpo-comma-list-expand-region-outer (&key position)
  "Expand region to outer comma-list element at point.
POSITION controls where point ends up: nil or \\='beginning puts point
at the beginning of the region, \\='end puts point at the end."
  (interactive)
  (cpo-text-object-stuff--expand-region-to-thing 'cpo-comma-list-outer nil :position position))

;; Register bounds-of-thing-at-point for expand-region integration.
(put 'cpo-comma-list 'bounds-of-thing-at-point
     'cpo-comma-list--bounds-of-inner-at-point)
(put 'cpo-comma-list-outer 'bounds-of-thing-at-point
     'cpo-comma-list--bounds-of-outer-at-point)

;;; Transpose

(defun cpo-comma-list--transpose-once (direction)
  "Transpose the current comma-list element with an adjacent one.
DIRECTION is 1 for forward, -1 for backward.
Leaves point at the beginning of the moved element in its new position.
Returns non-nil on success."
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (other-index (+ index direction)))
        (when (and (>= other-index 0) (< other-index (length elements)))
          (let* ((elem-1 (nth index elements))
                 (elem-2 (nth other-index elements))
                 (trimmed-1 (cpo-comma-list--trim-bounds (car elem-1) (cdr elem-1)))
                 (trimmed-2 (cpo-comma-list--trim-bounds (car elem-2) (cdr elem-2)))
                 ;; Ensure earlier/later ordering for the swap.
                 (t1-is-earlier (< (car trimmed-1) (car trimmed-2)))
                 (earlier (if t1-is-earlier trimmed-1 trimmed-2))
                 (later (if t1-is-earlier trimmed-2 trimmed-1))
                 (s-earlier (buffer-substring-no-properties (car earlier) (cdr earlier)))
                 (s-later (buffer-substring-no-properties (car later) (cdr later)))
                 (region-was-active (region-active-p)))
            ;; Perform the swap.
            (atomic-change-group
              (delete-region (car later) (cdr later))
              (goto-char (car later))
              (insert s-earlier)
              (delete-region (car earlier) (cdr earlier))
              (goto-char (car earlier))
              (insert s-later))
            ;; Position cursor at the beginning of the original element
            ;; (trimmed-1) in its new location.
            (let* ((len-diff (- (length s-later) (length s-earlier))))
              (if t1-is-earlier
                  ;; Our element moved from earlier to later position.
                  (goto-char (+ (car later) len-diff))
                ;; Our element moved from later to earlier position.
                (goto-char (car earlier))))
            (when region-was-active
              (set-mark (+ (point) (length (buffer-substring-no-properties
                                            (car trimmed-1) (cdr trimmed-1)))))
              (activate-mark)
              (setq deactivate-mark nil))
            (undo-boundary)
            t))))))

(defun cpo-comma-list--transpose-region-once (direction)
  "Transpose the active region with an adjacent comma-list element.
DIRECTION is 1 for forward, -1 for backward.
Assumes the region is aligned on comma-list element boundaries.
Re-activates the region around the moved content.
Returns non-nil on success."
  (let* ((reg-beg (region-beginning))
         (reg-end (region-end))
         ;; Find the neighbor element.  For forward, look at the element
         ;; at/after the end of the region.  For backward, look at the
         ;; element at/before the start of the region.
         (neighbor-info (save-excursion
                          (if (> direction 0)
                              (progn
                                (goto-char reg-end)
                                ;; Skip comma and whitespace to get into the
                                ;; next element.
                                (skip-chars-forward ", \t\n")
                                (cpo-comma-list--inner-bounds))
                            (progn
                              (goto-char reg-beg)
                              ;; Skip backward past comma and whitespace to
                              ;; get into the previous element.
                              (skip-chars-backward ", \t\n")
                              (cpo-comma-list--inner-bounds))))))
    (when neighbor-info
      (let* ((neighbor-trimmed neighbor-info)
             (region-bounds (cons reg-beg reg-end))
             ;; Determine earlier/later ordering.
             (region-is-earlier (< reg-beg (car neighbor-trimmed)))
             (earlier (if region-is-earlier region-bounds neighbor-trimmed))
             (later (if region-is-earlier neighbor-trimmed region-bounds))
             (s-earlier (buffer-substring-no-properties (car earlier) (cdr earlier)))
             (s-later (buffer-substring-no-properties (car later) (cdr later)))
             (region-text-len (- reg-end reg-beg)))
        ;; Only proceed if the neighbor doesn't overlap the region.
        (when (or (<= (cdr earlier) (car later)))
          ;; Perform the swap.
          (atomic-change-group
            (delete-region (car later) (cdr later))
            (goto-char (car later))
            (insert s-earlier)
            (delete-region (car earlier) (cdr earlier))
            (goto-char (car earlier))
            (insert s-later))
          ;; Position point and mark around the region text in its new location.
          (let* ((len-diff (- (length s-later) (length s-earlier))))
            (if region-is-earlier
                ;; Region moved from earlier to later position.
                (goto-char (+ (car later) len-diff))
              ;; Region moved from later to earlier position.
              (goto-char (car earlier))))
          (set-mark (+ (point) region-text-len))
          (activate-mark)
          (setq deactivate-mark nil)
          (undo-boundary)
          t)))))

;;;###autoload
(defun cpo-comma-list-transpose-forward (&optional count)
  "Transpose the current comma-list element forward COUNT times.
When region is active, transpose the region as a block, assuming
it is aligned on comma-list element boundaries."
  (interactive "p")
  (setq count (or count 1))
  (let ((dir (if (> count 0) 1 -1))
        (n (abs count)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (if (region-active-p)
            (cpo-comma-list--transpose-region-once dir)
          (cpo-comma-list--transpose-once dir))))))

;;;###autoload
(defun cpo-comma-list-transpose-backward (&optional count)
  "Transpose the current comma-list element backward COUNT times.
When region is active, transpose the region as a block, assuming
it is aligned on comma-list element boundaries."
  (interactive "p")
  (cpo-comma-list-transpose-forward (- (or count 1))))

;;; List open commands

;;;###autoload
(defun cpo-comma-list-open-forward ()
  "Open a new empty comma-list slot after the current element.
Inserts a comma and space after the current element's content
and positions point in the new empty slot."
  (interactive)
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (elem (nth index elements))
             (trimmed (cpo-comma-list--trim-bounds (car elem) (cdr elem))))
        (goto-char (cdr trimmed))
        (insert ", ")))))

;;;###autoload
(defun cpo-comma-list-open-backward ()
  "Open a new empty comma-list slot before the current element.
Inserts a comma and space before the current element's content
and positions point in the new empty slot."
  (interactive)
  (let ((info (cpo-comma-list--element-at-point)))
    (when info
      (let* ((elements (plist-get info :elements))
             (index (plist-get info :index))
             (elem (nth index elements))
             (trimmed (cpo-comma-list--trim-bounds (car elem) (cdr elem))))
        (goto-char (car trimmed))
        (insert ", ")
        (goto-char (car trimmed))))))

;;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-comma-list-forward-beginning
                                 'cpo-comma-list-backward-beginning)
  (repeatable-motion-define-pair 'cpo-comma-list-forward-end
                                 'cpo-comma-list-backward-end))

(provide 'cpo-comma-list)
;;; cpo-comma-list.el ends here
