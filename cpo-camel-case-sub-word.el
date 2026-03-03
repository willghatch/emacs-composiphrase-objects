;;; cpo-camel-case-sub-word.el --- Text objects for camelCase sub-words -*- lexical-binding: t; -*-

;; A text object for camelCase sub-words within identifiers.
;; A "camelCase sub-word" is a segment of a camelCase identifier delimited
;; by transitions from lowercase to uppercase (or the word boundary).
;;
;; For example, in "camelCaseIdentifiersLikeThis":
;;   - "camel" "Case" "Identifiers" "Like" "This"
;;
;; Punctuation is treated as NOT part of the overall word -- this behaves
;; like a smaller "word" rather than a smaller "symbol".  That is, only
;; word-constituent characters (letters and digits) are considered part of
;; camelCase identifiers.
;;
;; The transpose and open operations are capitalization-aware:
;; - Transposing adjusts capitalization so the new first sub-word gets the
;;   capitalization style of the old first sub-word, and the displaced
;;   sub-word becomes capitalized (since it is no longer first).
;; - Opening a new sub-word before the first sub-word capitalizes the
;;   old first sub-word.
;;
;; Provides:
;; - cpo-forward-camel-case-sub-word-beginning
;; - cpo-forward-camel-case-sub-word-end
;; - cpo-backward-camel-case-sub-word-beginning
;; - cpo-backward-camel-case-sub-word-end
;; - cpo-expand-region-to-camel-case-sub-word
;; - cpo-transpose-camel-case-sub-word-forward
;; - cpo-transpose-camel-case-sub-word-backward
;; - cpo-camel-case-sub-word-open-forward
;; - cpo-camel-case-sub-word-open-backward

(require 'cpo-text-object-stuff)

;;; Internal helpers

(defun cpo-camel-case-sub-word--word-bounds-at-point ()
  "Return the bounds (BEG . END) of the word-constituent run at point.
A word here means a contiguous sequence of word-constituent characters
\(letters and digits), not a symbol.  Returns nil if point is not on
or immediately after a word character."
  (let ((on-word (and (not (eobp))
                      (let ((ch (char-after)))
                        (and ch (or (and (>= ch ?a) (<= ch ?z))
                                    (and (>= ch ?A) (<= ch ?Z))
                                    (and (>= ch ?0) (<= ch ?9)))))))
        (after-word (and (not (bobp))
                         (let ((ch (char-before)))
                           (and ch (or (and (>= ch ?a) (<= ch ?z))
                                       (and (>= ch ?A) (<= ch ?Z))
                                       (and (>= ch ?0) (<= ch ?9))))))))
    (when (or on-word after-word)
      (save-excursion
        ;; If not on a word character but after one, step back first.
        (when (and (not on-word) after-word)
          (backward-char 1))
        (let ((beg (progn (skip-chars-backward "[:alnum:]") (point)))
              (end (progn (skip-chars-forward "[:alnum:]") (point))))
          (when (< beg end)
            (cons beg end)))))))

(defun cpo-camel-case-sub-word--parse-sub-words (beg end)
  "Parse the sub-words in the region BEG to END.
Returns a list of (SUB-BEG . SUB-END) pairs for each camelCase sub-word.
Sub-words are split at transitions from lowercase to uppercase,
and at transitions from a run of uppercase to an uppercase followed by lowercase
\(e.g. \"XMLParser\" -> \"XML\" \"Parser\")."
  (save-excursion
    (goto-char beg)
    (let ((sub-words nil)
          (sub-beg beg))
      (while (< (point) end)
        (forward-char 1)
        (when (and (< (point) end)
                   (let ((prev-char (char-before))
                         (cur-char (char-after)))
                     (or
                      ;; lowercase -> uppercase: "camelCase" -> "camel" | "Case"
                      (and (characterp prev-char)
                           (characterp cur-char)
                           (or (and (>= prev-char ?a) (<= prev-char ?z))
                               (and (>= prev-char ?0) (<= prev-char ?9)))
                           (>= cur-char ?A) (<= cur-char ?Z))
                      ;; uppercase -> uppercase + lowercase: "XMLParser" -> "XML" | "Parser"
                      ;; We detect this when we're at the last uppercase before lowercase.
                      (and (characterp prev-char)
                           (characterp cur-char)
                           (>= prev-char ?A) (<= prev-char ?Z)
                           (>= cur-char ?A) (<= cur-char ?Z)
                           (< (1+ (point)) (1+ end))
                           (let ((next-char (char-after (1+ (point)))))
                             (and (characterp next-char)
                                  (>= next-char ?a) (<= next-char ?z)))))))
          (push (cons sub-beg (point)) sub-words)
          (setq sub-beg (point))))
      ;; Final sub-word
      (when (< sub-beg end)
        (push (cons sub-beg end) sub-words))
      (nreverse sub-words))))

(defun cpo-camel-case-sub-word--sub-word-at-point ()
  "Find the camelCase sub-word at point.
Returns a plist (:sub-words SUBS :index INDEX :word-bounds BOUNDS)
where SUBS is a list of (BEG . END), INDEX is the 0-based index of
the sub-word containing point, and BOUNDS is the word bounds.
Returns nil if point is not in a camelCase word."
  (let ((word-bounds (cpo-camel-case-sub-word--word-bounds-at-point)))
    (when word-bounds
      (let* ((sub-words (cpo-camel-case-sub-word--parse-sub-words
                         (car word-bounds) (cdr word-bounds)))
             (pt (point))
             (index nil))
        ;; Find which sub-word contains point.
        (cl-loop for sw in sub-words
                 for i from 0
                 when (and (<= (car sw) pt)
                           (or (< pt (cdr sw))
                               ;; At the very end of the last sub-word
                               (and (= i (1- (length sub-words)))
                                    (= pt (cdr sw)))))
                 do (setq index i)
                 and return nil)
        ;; If point is exactly at a boundary between sub-words, pick the one starting here.
        (when (null index)
          (cl-loop for sw in sub-words
                   for i from 0
                   when (= (car sw) pt)
                   do (setq index i)
                   and return nil))
        ;; If still no match, find nearest sub-word.
        (when (and (null index) sub-words)
          (let ((min-dist most-positive-fixnum))
            (cl-loop for sw in sub-words
                     for i from 0
                     do (let ((dist (min (abs (- pt (car sw)))
                                         (abs (- pt (cdr sw))))))
                          (when (< dist min-dist)
                            (setq min-dist dist)
                            (setq index i))))))
        (when index
          (list :sub-words sub-words
                :index index
                :word-bounds word-bounds))))))

(defun cpo-camel-case-sub-word--bounds-at-point ()
  "Return bounds of camelCase sub-word at point as (BEG . END)."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (nth (plist-get info :index)
           (plist-get info :sub-words)))))

;;; Motion commands

(defun cpo-camel-case-sub-word--forward-beginning ()
  "Move forward to the beginning of the next camelCase sub-word.
Returns the new position, or nil if no next sub-word."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (next-index (1+ index)))
        (if (< next-index (length sub-words))
            ;; Next sub-word within the same word.
            (let ((next-sw (nth next-index sub-words)))
              (goto-char (car next-sw))
              (car next-sw))
          ;; At the last sub-word; try to move to the next word's first sub-word.
          (let ((word-bounds (plist-get info :word-bounds))
                (found nil))
            (save-excursion
              (goto-char (cdr word-bounds))
              (when (re-search-forward "\\w" nil t)
                (backward-char 1)
                (let ((new-info (cpo-camel-case-sub-word--sub-word-at-point)))
                  (when new-info
                    (setq found (car (car (plist-get new-info :sub-words))))))))
            (when found
              (goto-char found)
              found)))))))

(defun cpo-camel-case-sub-word--backward-beginning ()
  "Move backward to the beginning of the previous camelCase sub-word.
Returns the new position, or nil if no previous sub-word."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (cur-sw (nth index sub-words)))
        ;; If we're past the beginning of the current sub-word, go to its beginning.
        (if (> (point) (car cur-sw))
            (progn
              (goto-char (car cur-sw))
              (car cur-sw))
          ;; At the beginning of the current sub-word.
          (let ((prev-index (1- index)))
            (if (>= prev-index 0)
                ;; Previous sub-word in the same word.
                (let ((prev-sw (nth prev-index sub-words)))
                  (goto-char (car prev-sw))
                  (car prev-sw))
              ;; At the first sub-word; try to move to previous word's last sub-word beginning.
              (let ((word-bounds (plist-get info :word-bounds))
                    (found nil))
                (save-excursion
                  (goto-char (car word-bounds))
                  (when (re-search-backward "\\w" nil t)
                    (let ((new-info (cpo-camel-case-sub-word--sub-word-at-point)))
                      (when new-info
                        (let* ((new-sws (plist-get new-info :sub-words))
                               (last-sw (car (last new-sws))))
                          (setq found (car last-sw)))))))
                (when found
                  (goto-char found)
                  found)))))))))

(defun cpo-camel-case-sub-word--forward-end ()
  "Move forward to the end of the current or next camelCase sub-word.
Returns the new position, or nil if no movement possible."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (cur-sw (nth index sub-words)))
        ;; If point is before the end of the current sub-word, go there.
        (if (< (point) (cdr cur-sw))
            (progn
              (goto-char (cdr cur-sw))
              (cdr cur-sw))
          ;; At or past the end; go to end of next sub-word.
          (let ((next-index (1+ index)))
            (if (< next-index (length sub-words))
                (let ((next-sw (nth next-index sub-words)))
                  (goto-char (cdr next-sw))
                  (cdr next-sw))
              ;; Last sub-word in this word; try next word.
              (let ((word-bounds (plist-get info :word-bounds))
                    (found nil))
                (save-excursion
                  (goto-char (cdr word-bounds))
                  (when (re-search-forward "\\w" nil t)
                    (backward-char 1)
                    (let ((new-info (cpo-camel-case-sub-word--sub-word-at-point)))
                      (when new-info
                        (let* ((new-sws (plist-get new-info :sub-words))
                               (first-sw (car new-sws)))
                          (setq found (cdr first-sw)))))))
                (when found
                  (goto-char found)
                  found)))))))))

(defun cpo-camel-case-sub-word--backward-end ()
  "Move backward to the end of the previous camelCase sub-word.
Returns the new position, or nil if no previous sub-word."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (pt (point))
             ;; Find the target: the nearest sub-word end strictly before point.
             ;; In camelCase, sub-word ends coincide with the next sub-word's start,
             ;; so we need to find an end position strictly less than current point.
             (target nil))
        ;; Search backwards through sub-words for an end position before point.
        (cl-loop for i downfrom index to 0
                 do (let ((sw (nth i sub-words)))
                      (when (< (cdr sw) pt)
                        (setq target (cdr sw))
                        (cl-return))))
        (if target
            (progn
              (goto-char target)
              target)
          ;; No sub-word end before point in this word; try previous word.
          (let ((word-bounds (plist-get info :word-bounds))
                (found nil))
            (save-excursion
              (goto-char (car word-bounds))
              (when (re-search-backward "\\w" nil t)
                (let ((new-info (cpo-camel-case-sub-word--sub-word-at-point)))
                  (when new-info
                    (let* ((new-sws (plist-get new-info :sub-words))
                           (last-sw (car (last new-sws))))
                      (setq found (cdr last-sw)))))))
            (when found
              (goto-char found)
              found)))))))

;;;###autoload
(defun cpo-forward-camel-case-sub-word-beginning (&optional count)
  "Move forward COUNT camelCase sub-words to the beginning.
If COUNT is negative, move backward."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (dotimes (_i n)
      (if fwd
          (cpo-camel-case-sub-word--forward-beginning)
        (cpo-camel-case-sub-word--backward-beginning)))))

;;;###autoload
(defun cpo-backward-camel-case-sub-word-beginning (&optional count)
  "Move backward COUNT camelCase sub-words to the beginning.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-forward-camel-case-sub-word-beginning (- (or count 1))))

;;;###autoload
(defun cpo-forward-camel-case-sub-word-end (&optional count)
  "Move forward COUNT camelCase sub-words to the end.
If COUNT is negative, move backward."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (dotimes (_i n)
      (if fwd
          (cpo-camel-case-sub-word--forward-end)
        (cpo-camel-case-sub-word--backward-end)))))

;;;###autoload
(defun cpo-backward-camel-case-sub-word-end (&optional count)
  "Move backward COUNT camelCase sub-words to the end.
If COUNT is negative, move forward."
  (interactive "p")
  (cpo-forward-camel-case-sub-word-end (- (or count 1))))

;;; Expand region

(put 'cpo-camel-case-sub-word 'bounds-of-thing-at-point
     'cpo-camel-case-sub-word--bounds-at-point)

(defun cpo-expand-region-to-camel-case-sub-word ()
  "Expand region to camelCase sub-word at point."
  (interactive)
  (cpo-text-object-stuff--expand-region-to-thing 'cpo-camel-case-sub-word t))

;;; Capitalization helpers for transpose and open

(defun cpo-camel-case-sub-word--lowercase-first-char (str)
  "Return STR with its first character lowercased."
  (if (and str (> (length str) 0))
      (concat (downcase (substring str 0 1)) (substring str 1))
    str))

(defun cpo-camel-case-sub-word--uppercase-first-char (str)
  "Return STR with its first character uppercased."
  (if (and str (> (length str) 0))
      (concat (upcase (substring str 0 1)) (substring str 1))
    str))

(defun cpo-camel-case-sub-word--first-char-lowercase-p (str)
  "Return non-nil if the first character of STR is lowercase."
  (and str (> (length str) 0)
       (let ((ch (aref str 0)))
         (and (>= ch ?a) (<= ch ?z)))))

;;; Transpose

(defun cpo-camel-case-sub-word--transpose-once (direction)
  "Transpose the current camelCase sub-word with an adjacent one.
DIRECTION is 1 for forward, -1 for backward.
Adjusts capitalization: the new first sub-word in the word gets the
capitalization style of the old first sub-word, and the old first
sub-word (now displaced) becomes capitalized.
Returns non-nil on success."
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (other-index (+ index direction))
             (word-bounds (plist-get info :word-bounds)))
        (when (and (>= other-index 0) (< other-index (length sub-words)))
          (let* ((sw-1 (nth index sub-words))
                 (sw-2 (nth other-index sub-words))
                 ;; Determine which is earlier and which is later.
                 (earlier-index (min index other-index))
                 (later-index (max index other-index))
                 (sw-earlier (nth earlier-index sub-words))
                 (sw-later (nth later-index sub-words))
                 (s-earlier (buffer-substring-no-properties (car sw-earlier) (cdr sw-earlier)))
                 (s-later (buffer-substring-no-properties (car sw-later) (cdr sw-later)))
                 ;; Adjust capitalization.
                 ;; The first sub-word (index 0) may be lowercase.
                 ;; All other sub-words should be uppercase-initial (camelCase).
                 ;; When transposing, if index 0 is involved:
                 ;; - The new first sub-word should get the original first sub-word's case.
                 ;; - The old first sub-word (now non-first) should be capitalized.
                 (new-s-earlier s-later)
                 (new-s-later s-earlier))
            ;; Adjust capitalization when index 0 is involved.
            (when (= earlier-index 0)
              (let ((first-was-lowercase (cpo-camel-case-sub-word--first-char-lowercase-p s-earlier)))
                ;; The text going to index 0 (new-s-earlier) gets the case of the old first word.
                (setq new-s-earlier (if first-was-lowercase
                                        (cpo-camel-case-sub-word--lowercase-first-char new-s-earlier)
                                      (cpo-camel-case-sub-word--uppercase-first-char new-s-earlier)))
                ;; The text leaving index 0 (new-s-later) becomes capitalized.
                (setq new-s-later (cpo-camel-case-sub-word--uppercase-first-char new-s-later))))
            ;; Perform the swap: replace later first (to preserve earlier positions).
            (atomic-change-group
              (delete-region (car sw-later) (cdr sw-later))
              (goto-char (car sw-later))
              (insert new-s-later)
              (delete-region (car sw-earlier) (cdr sw-earlier))
              (goto-char (car sw-earlier))
              (insert new-s-earlier))
            ;; Position cursor at the beginning of the moved sub-word (the one
            ;; that was originally at `index`) in its new location.
            (let ((len-diff (- (length new-s-earlier) (length s-earlier))))
              (if (< index other-index)
                  ;; Moved forward: our text is now at the later position.
                  (goto-char (+ (car sw-later) len-diff))
                ;; Moved backward: our text is now at the earlier position.
                (goto-char (car sw-earlier))))
            (undo-boundary)
            t))))))

;;;###autoload
(defun cpo-transpose-camel-case-sub-word-forward (&optional count)
  "Transpose the current camelCase sub-word forward COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (let ((dir (if (> count 0) 1 -1))
        (n (abs count)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (cpo-camel-case-sub-word--transpose-once dir)))))

;;;###autoload
(defun cpo-transpose-camel-case-sub-word-backward (&optional count)
  "Transpose the current camelCase sub-word backward COUNT times."
  (interactive "p")
  (cpo-transpose-camel-case-sub-word-forward (- (or count 1))))

;;; Open

;;;###autoload
(defun cpo-camel-case-sub-word-open-forward ()
  "Open a new empty camelCase sub-word slot after the current sub-word.
Inserts a capital letter placeholder position after the current sub-word.
Point is left where the user can type the new sub-word."
  (interactive)
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (cur-sw (nth index sub-words)))
        ;; Position at the end of the current sub-word.
        ;; The user will type a new sub-word here.
        ;; Since the next sub-word already starts with a capital letter,
        ;; just place the cursor at the boundary.
        (goto-char (cdr cur-sw))))))

;;;###autoload
(defun cpo-camel-case-sub-word-open-backward ()
  "Open a new empty camelCase sub-word slot before the current sub-word.
When opening before the first sub-word, the old first sub-word is
capitalized (since it is no longer first).
Point is left where the user can type the new sub-word."
  (interactive)
  (let ((info (cpo-camel-case-sub-word--sub-word-at-point)))
    (when info
      (let* ((sub-words (plist-get info :sub-words))
             (index (plist-get info :index))
             (cur-sw (nth index sub-words)))
        ;; When opening before index 0, capitalize the old first sub-word.
        (when (= index 0)
          (let ((first-char-pos (car cur-sw)))
            (when (and (< first-char-pos (cdr cur-sw))
                       (cpo-camel-case-sub-word--first-char-lowercase-p
                        (buffer-substring-no-properties first-char-pos (1+ first-char-pos))))
              (save-excursion
                (goto-char first-char-pos)
                (let ((ch (char-after)))
                  (delete-char 1)
                  (insert (upcase (char-to-string ch))))))))
        ;; Place cursor at the beginning of the current sub-word.
        (goto-char (car cur-sw))))))

;;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-forward-camel-case-sub-word-beginning
                                 'cpo-backward-camel-case-sub-word-beginning)
  (repeatable-motion-define-pair 'cpo-forward-camel-case-sub-word-end
                                 'cpo-backward-camel-case-sub-word-end))

(provide 'cpo-camel-case-sub-word)
;;; cpo-camel-case-sub-word.el ends here
