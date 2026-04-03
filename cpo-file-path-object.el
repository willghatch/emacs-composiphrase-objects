;;; -*- lexical-binding: t; -*-
;;; cpo-file-path-object.el -- File path text objects for composiphrase

;;; Commentary:
;;
;; Provides two file-path text objects:
;;
;; `cpo-file-path-object-explicit' -- matches file paths that begin with one of the
;; canonical prefixes: =./=, =../=, or =/=.  These are unambiguously
;; file-like, so movement is more reliable.
;;
;; `cpo-file-path-object' -- matches any non-whitespace sequence that
;; could be a relative file path (no prefix required).  Selection is
;; the primary use case; movement may be noisy because many tokens that
;; are not actually file names will also match.
;;
;; Both objects assume that file names contain no whitespace, no quotes (single,
;; double backtick), no parentheses, no braces, no brackets, similar to what you
;; would expect when using eg. a bash shell.

(require 'cpo-text-object-stuff)

;;;;;
;; Regexp helpers

(defconst cpo-file-path-object--prefixed-regexp
  ;; Matches an absolute or explicitly-relative path:
  ;;   /...    absolute
  ;;   ./...   explicitly current-directory-relative
  ;;   ../...  parent-directory-relative
  ;; The prefix must be followed by zero or more non-bounding characters.
  ;; (A bare "/" is a valid absolute path.)
  ;;
  ;; Bounding characters (shell filename delimiters): ' " ` ( ) [ ] { }
  ;; Whitespace also bounds.
  ;; Dollar signs and colons are NOT bounding -- valid in paths
  ;; (eg. $VAR/subdir or /path:alternative style).
  ;;
  ;; Character class breakdown (inside [^] ... ]):
  ;;   ]            -- close bracket (must be first after [^ to be literal)
  ;;   [            -- open bracket
  ;;   \t \n \r \f  -- whitespace
  ;;   (space)      -- space
  ;;   '            -- single quote
  ;;   \"           -- double quote  (escaped in Lisp string)
  ;;   `            -- backtick
  ;;   ( )          -- parens
  ;;   { }          -- curly braces
  ;;
  ;; Note: ] is placed first in the class (right after [^) so it is treated
  ;; as a literal character, not as the end of the class.  This is the
  ;; standard Emacs/POSIX regex idiom; \] does NOT work to include ] in a
  ;; class.
  "\\(\\.\\.?/\\|/\\)[^][ \t\n\r\f'\"\\`(){}]*"
  "Regexp for a file path with a required prefix (/ ./ or ../).")

(defconst cpo-file-path-object--bare-regexp
  ;; Any run of non-bounding characters.
  ;; See cpo-file-path-object--prefixed-regexp for bounding char details.
  ;; ] is first in class (right after [^) so it is treated as literal.
  "[^][ \t\n\r\f'\"\\`(){}]+"
  "Regexp for any non-whitespace/non-bounding token (bare file-path variant).")

;;;;;
;; Bounds-at-point functions

(defun cpo-file-path-object--bounds-at-point-for-regexp (regexp max-scan-back)
  "Return bounds (START . END) of REGEXP at point, or nil.
Searches backward up to MAX-SCAN-BACK characters for a potential
match start, then checks whether the match spans point.  Returns
the leftmost (earliest starting) match that spans point, so that
inner sub-paths (e.g. /bar in /foo/bar) are not preferred over the
full enclosing path /foo/bar."
  (save-excursion
    (let* ((orig (point))
           (limit (max (- orig max-scan-back) (point-min)))
           (result nil)
           (pos orig))
      ;; Scan from orig backward to limit.
      ;; At each position, check if regexp matches and spans orig.
      ;; We want the leftmost (earliest-starting) such match.
      (while (>= pos limit)
        (goto-char pos)
        (when (and (looking-at regexp)
                   (<= (match-beginning 0) orig)
                   (< orig (match-end 0)))
          ;; This match spans point.
          ;; Since we're scanning backward, earlier positions give
          ;; earlier starts, so always update result.
          (setq result (cons (match-beginning 0) (match-end 0))))
        (setq pos (1- pos)))
      result)))

(defun cpo-file-path-object--bounds-at-point ()
  "Get bounds of a prefixed file path at point.
Matches paths beginning with /, ./, or ../."
  ;; Max path before point we need to scan: longest realistic prefix
  ;; before we'd already be in whitespace is bounded by MAX-SCAN-BACK.
  ;; We use 4096 to be generous (paths can be long).
  (cpo-file-path-object--bounds-at-point-for-regexp
   cpo-file-path-object--prefixed-regexp 4096))

(defun cpo-file-path-object--bare-bounds-at-point ()
  "Get bounds of a bare (no-prefix) non-whitespace token at point."
  (cpo-file-path-object--bounds-at-point-for-regexp
   cpo-file-path-object--bare-regexp 4096))

;;;;;
;; Forward/backward beginning movement -- prefixed variant

(defun cpo-file-path-object--forward-beginning ()
  "Move forward to the beginning of the next prefixed file path."
  (let ((start-point (point))
        (end-point nil)
        (regexp cpo-file-path-object--prefixed-regexp))
    (save-mark-and-excursion
      (let ((success (re-search-forward regexp nil t)))
        ;; If we matched starting at point, skip past it and search again.
        (when (and success (equal start-point (match-beginning 0)))
          (forward-char 1)
          (setq success (re-search-forward regexp nil t)))
        (when success
          (setq end-point (match-beginning 0)))))
    (when end-point (goto-char end-point))))

(defun cpo-file-path-object--backward-beginning ()
  "Move backward to the beginning of the previous prefixed file path.
Unlike a raw re-search-backward, this correctly identifies the start
of the whitespace-delimited token rather than landing on an interior
slash (e.g. it finds /foo/bar not /bar in the middle of /foo/bar)."
  (let* ((start-point (point))
         (regexp cpo-file-path-object--prefixed-regexp)
         (found-point nil))
    (save-mark-and-excursion
      (let ((keep-going t))
        (while keep-going
          (let ((success (re-search-backward regexp nil t)))
            (if (not success)
                (setq keep-going nil)
              ;; Found a regexp match.  Check whether the character
              ;; before the match is non-whitespace: if so, we're inside
              ;; a larger token and this is an interior slash, not a real
              ;; path start.
              (let* ((match-start (match-beginning 0))
                     (char-before (and (> match-start (point-min))
                                       (char-before match-start))))
                (if (and char-before
                         (not (memq char-before '(?\s ?\t ?\n ?\r ?\f))))
                    ;; Interior match: skip past the start of the enclosing
                    ;; non-whitespace token and try again.
                    (progn
                      (goto-char match-start)
                      ;; Move to the beginning of this whitespace-delimited
                      ;; token by going backward past non-whitespace chars.
                      (skip-chars-backward "^ \t\n\r\f")
                      ;; Now check if the token at this position is a
                      ;; prefixed path (ie starts with / ./ ../).
                      (let ((token-start (point)))
                        (if (looking-at regexp)
                            (progn
                              ;; This token is a full prefixed path; use it.
                              (setq found-point token-start)
                              (setq keep-going nil))
                          ;; This token is not a prefixed path.  Continue
                          ;; searching backward from just before token-start.
                          (when (<= token-start (point-min))
                            (setq keep-going nil)))))
                  ;; The match start is preceded by whitespace or BOB:
                  ;; this is a genuine path start.
                  (setq found-point match-start)
                  (setq keep-going nil))))))))
    (when found-point (goto-char found-point))))

;;;;;
;; Forward/backward beginning movement -- bare variant

(defun cpo-file-path-object--bare-forward-beginning ()
  "Move forward to the beginning of the next bare non-whitespace token."
  (let ((start-point (point))
        (end-point nil)
        (regexp cpo-file-path-object--bare-regexp))
    (save-mark-and-excursion
      (let ((success (re-search-forward regexp nil t)))
        (when (and success (equal start-point (match-beginning 0)))
          (forward-char 1)
          (setq success (re-search-forward regexp nil t)))
        (when success
          (setq end-point (match-beginning 0)))))
    (when end-point (goto-char end-point))))

(defun cpo-file-path-object--bare-backward-beginning ()
  "Move backward to the beginning of the previous bare non-whitespace token."
  (let* ((regexp cpo-file-path-object--bare-regexp)
         (success (re-search-backward regexp nil t)))
    (when success
      (goto-char (match-beginning 0)))))

;;;;;
;; Public movement commands -- prefixed variant

(defun cpo-forward-cpo-file-path-object-explicit-beginning (&optional count)
  "Move forward to the beginning of a prefixed file path, COUNT times.
A prefixed file path starts with /, ./, or ../.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (funcall (if fwd
                   #'cpo-file-path-object--forward-beginning
                 #'cpo-file-path-object--backward-beginning)))))

(defun cpo-backward-cpo-file-path-object-explicit-beginning (&optional count)
  "Like `cpo-forward-cpo-file-path-object-explicit-beginning' but backward."
  (interactive "p")
  (cpo-forward-cpo-file-path-object-explicit-beginning (- (or count 1))))

(defun cpo-backward-cpo-file-path-object-explicit-end (&optional count)
  "Move backward to the end of a prefixed file path, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-forward-cpo-file-path-object-explicit-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (end-point nil))
        (save-mark-and-excursion
          (let* ((moved (cpo-tree-walk--motion-moved
                         #'cpo-backward-cpo-file-path-object-explicit-beginning))
                 (bounds (and moved (cpo-file-path-object--bounds-at-point)))
                 (was-in-path (and bounds (<= start-point (cdr bounds)))))
            (cond
             (was-in-path
              (setq moved
                    (cpo-tree-walk--motion-moved
                     (lambda ()
                       (dotimes (_i count)
                         (cpo-backward-cpo-file-path-object-explicit-beginning))))))
             (moved (dotimes (_i (- count 1))
                      (cpo-backward-cpo-file-path-object-explicit-beginning)))
             (t nil))
            (when moved
              (let ((bounds (cpo-file-path-object--bounds-at-point)))
                (and bounds (setq end-point (cdr bounds)))))))
        (when end-point (goto-char end-point)))))))

(defun cpo-forward-cpo-file-path-object-explicit-end (&optional count)
  "Move forward to the end of a prefixed file path, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-backward-cpo-file-path-object-explicit-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (started-in-path nil)
            (started-at-end-of-path nil)
            (end-point nil))
        (save-mark-and-excursion
          (let ((bounds (cpo-file-path-object--bounds-at-point)))
            (when (and bounds (<= (car bounds) start-point (cdr bounds)))
              (if (equal start-point (cdr bounds))
                  (setq started-at-end-of-path t)
                (progn
                  (setq started-in-path t)
                  (goto-char (cdr bounds))))))
          (unless (or started-in-path started-at-end-of-path)
            (cpo-backward-cpo-file-path-object-explicit-beginning)
            (let ((bounds (cpo-file-path-object--bounds-at-point)))
              (when bounds
                (if (<= (car bounds) start-point (cdr bounds))
                    (progn
                      (goto-char (cdr bounds))
                      (if (equal start-point (cdr bounds))
                          (setq started-at-end-of-path t)
                        (setq started-in-path t)))
                  (goto-char start-point)))))
          (setq end-point (and started-in-path (point)))
          (and (cpo-tree-walk--motion-moved
                (lambda ()
                  (dotimes (_i (if started-in-path (- count 1) count))
                    (cpo-forward-cpo-file-path-object-explicit-beginning))))
               (let ((final-bounds (cpo-file-path-object--bounds-at-point)))
                 (when final-bounds
                   (setq end-point (cdr final-bounds))))))
        (when end-point
          (goto-char end-point)))))))

;;;;;
;; Public movement commands -- bare variant

(defun cpo-forward-cpo-file-path-object-beginning (&optional count)
  "Move forward to the beginning of a bare non-whitespace token, COUNT times.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (funcall (if fwd
                   #'cpo-file-path-object--bare-forward-beginning
                 #'cpo-file-path-object--bare-backward-beginning)))))

(defun cpo-backward-cpo-file-path-object-beginning (&optional count)
  "Like `cpo-forward-cpo-file-path-object-beginning' but backward."
  (interactive "p")
  (cpo-forward-cpo-file-path-object-beginning (- (or count 1))))

(defun cpo-backward-cpo-file-path-object-end (&optional count)
  "Move backward to the end of a bare non-whitespace token, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-forward-cpo-file-path-object-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (end-point nil))
        (save-mark-and-excursion
          (let* ((moved (cpo-tree-walk--motion-moved
                         #'cpo-backward-cpo-file-path-object-beginning))
                 (bounds (and moved (cpo-file-path-object--bare-bounds-at-point)))
                 (was-in-token (and bounds (<= start-point (cdr bounds)))))
            (cond
             (was-in-token
              (setq moved
                    (cpo-tree-walk--motion-moved
                     (lambda ()
                       (dotimes (_i count)
                         (cpo-backward-cpo-file-path-object-beginning))))))
             (moved (dotimes (_i (- count 1))
                      (cpo-backward-cpo-file-path-object-beginning)))
             (t nil))
            (when moved
              (let ((bounds (cpo-file-path-object--bare-bounds-at-point)))
                (and bounds (setq end-point (cdr bounds)))))))
        (when end-point (goto-char end-point)))))))

(defun cpo-forward-cpo-file-path-object-end (&optional count)
  "Move forward to the end of a bare non-whitespace token, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-backward-cpo-file-path-object-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (started-in-token nil)
            (started-at-end-of-token nil)
            (end-point nil))
        (save-mark-and-excursion
          (let ((bounds (cpo-file-path-object--bare-bounds-at-point)))
            (when (and bounds (<= (car bounds) start-point (cdr bounds)))
              (if (equal start-point (cdr bounds))
                  (setq started-at-end-of-token t)
                (progn
                  (setq started-in-token t)
                  (goto-char (cdr bounds))))))
          (unless (or started-in-token started-at-end-of-token)
            (cpo-backward-cpo-file-path-object-beginning)
            (let ((bounds (cpo-file-path-object--bare-bounds-at-point)))
              (when bounds
                (if (<= (car bounds) start-point (cdr bounds))
                    (progn
                      (goto-char (cdr bounds))
                      (if (equal start-point (cdr bounds))
                          (setq started-at-end-of-token t)
                        (setq started-in-token t)))
                  (goto-char start-point)))))
          (setq end-point (and started-in-token (point)))
          (and (cpo-tree-walk--motion-moved
                (lambda ()
                  (dotimes (_i (if started-in-token (- count 1) count))
                    (cpo-forward-cpo-file-path-object-beginning))))
               (let ((final-bounds (cpo-file-path-object--bare-bounds-at-point)))
                 (when final-bounds
                   (setq end-point (cdr final-bounds))))))
        (when end-point
          (goto-char end-point)))))))

;;;;;
;; Register thing-at-point and expand-region

(put 'cpo-file-path-object-explicit 'bounds-of-thing-at-point #'cpo-file-path-object--bounds-at-point)
(put 'cpo-file-path-object 'bounds-of-thing-at-point #'cpo-file-path-object--bare-bounds-at-point)

(cpo-text-object-stuff--def-expand-region-to-thing cpo-file-path-object-explicit)
(cpo-text-object-stuff--def-expand-region-to-thing cpo-file-path-object)

;;;;;
;; Repeatable motion integration

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair
   'cpo-forward-cpo-file-path-object-explicit-beginning
   'cpo-backward-cpo-file-path-object-explicit-beginning)
  (repeatable-motion-define-pair
   'cpo-forward-cpo-file-path-object-explicit-end
   'cpo-backward-cpo-file-path-object-explicit-end)
  (repeatable-motion-define-pair
   'cpo-forward-cpo-file-path-object-beginning
   'cpo-backward-cpo-file-path-object-beginning)
  (repeatable-motion-define-pair
   'cpo-forward-cpo-file-path-object-end
   'cpo-backward-cpo-file-path-object-end))

(provide 'cpo-file-path-object)
