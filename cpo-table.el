;;; cpo-table.el --- Text objects for pipe-delimited tables -*- lexical-binding: t; -*-

;; Provides text objects and navigation for pipe-delimited tables in
;; both markdown and org-mode.
;;
;; The objects aren't a full recursive tree, but have a limited hierarchy:
;;   cpo-table          -- the whole table
;;   cpo-table-row      -- a single row line
;;   cpo-table-cell     -- a single cell (content between pipes)
;; Then there is one more that does not fit nicely in the hierarchy:
;;   cpo-table-column   -- a vertical slice
;;
;; Provides:
;;   - Table/row/cell bounds and expand-region
;;   - Forward/backward/up/down navigation
;;   - Expand-region iterative up the hierarchy (progressive: cell -> outer cell -> row -> table)
;;   - expand-region to cover a specific object (except for columns)
;;   - Open: initial scaffolding to start a table, insert row, insert column
;;   - Transpose: row, cell, column
;;   - Column delete
;;   - Repeatable-motion integration

(require 'cpo-text-object-stuff)
(require 'cl-lib)
(require 'subr-x)

;;; ---------------------------------------------------------------------------
;;; Backend detection and dispatch
;;; ---------------------------------------------------------------------------

(defun cpo-table--detect-backend ()
  "Determine the table backend for the current buffer.
Returns \\='org for org-mode and org-derived modes,
\\='markdown for markdown-mode and gfm-mode,
or \\='markdown as a generic fallback."
  (cond
   ((derived-mode-p 'org-mode) 'org)
   ((derived-mode-p 'markdown-mode) 'markdown)
   (t 'markdown)))

(defvar cpo-table-backends
  `((org . ((align . org-table-align)
            (separator-row-p . cpo-table--org-separator-row-p)
            (at-table-p . org-at-table-p)
            (table-begin . org-table-begin)
            (table-end . org-table-end)
            (next-field . org-table-next-field)
            (previous-field . org-table-previous-field)
            (insert-column . org-table-insert-column)
            (insert-row . org-table-insert-row)
            (beginning-of-field . org-table-beginning-of-field)
            (end-of-field . org-table-end-of-field)))
    (markdown . ((align . cpo-table--markdown-align)
                 (separator-row-p . cpo-table--markdown-separator-row-p)
                 (at-table-p . cpo-table--markdown-at-table-p)
                 (table-begin . cpo-table--markdown-table-begin)
                 (table-end . cpo-table--markdown-table-end)
                 (next-field . cpo-table--markdown-next-field)
                 (previous-field . cpo-table--markdown-previous-field)
                 (insert-column . cpo-table--markdown-insert-column)
                 (insert-row . cpo-table--markdown-insert-row)
                 (beginning-of-field . cpo-table--markdown-beginning-of-field)
                 (end-of-field . cpo-table--markdown-end-of-field))))
  "Alist mapping backend symbols to operation alists.")

(defun cpo-table--call (operation &rest args)
  "Call OPERATION for the current backend, passing ARGS.
Signals an error if the backend or operation is not found."
  (let* ((backend (cpo-table--detect-backend))
         (ops (alist-get backend cpo-table-backends))
         (func (alist-get operation ops)))
    (if func
        (apply func args)
      (error "cpo-table: operation %s not supported for backend %s"
             operation backend))))

;;; ---------------------------------------------------------------------------
;;; Parsing cache
;;; ---------------------------------------------------------------------------

(defvar-local cpo-table--cache nil
  "Cache for the most recently parsed table.
A plist (:tick TICK :pos POS :table TABLE) where TICK is
`buffer-chars-modified-tick' and POS is the buffer position
that was used to find the table.")

(defun cpo-table--cache-get (pos)
  "Return cached table if cache is valid for POS, else nil."
  (when cpo-table--cache
    (let ((tick (plist-get cpo-table--cache :tick))
          (cached-pos (plist-get cpo-table--cache :pos)))
      (when (and (= tick (buffer-chars-modified-tick))
                 (= cached-pos pos))
        (plist-get cpo-table--cache :table)))))

(defun cpo-table--cache-put (pos table)
  "Store TABLE in the cache for POS."
  (setq cpo-table--cache
        (list :tick (buffer-chars-modified-tick)
              :pos pos
              :table table)))

;;; ---------------------------------------------------------------------------
;;; Markdown backend helpers
;;; ---------------------------------------------------------------------------

(defconst cpo-table--line-start-regexp "^[ \t]*|"
  "Regexp matching the start of a table row (optional leading spaces/tabs then pipe).")

(defun cpo-table--markdown-at-table-p ()
  "Return non-nil if point is in a markdown table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p cpo-table--line-start-regexp)))

(defun cpo-table--markdown-separator-row-p ()
  "Return non-nil if the current line is a markdown separator row."
  (save-excursion
    (beginning-of-line)
    (looking-at-p
     "^[ \t]*|\\([ \t]*:?-+:?[ \t]*|\\)+[ \t]*$")))

(defun cpo-table--org-separator-row-p ()
  "Return non-nil if the current line is an org-mode table separator row."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*|-")))

(defun cpo-table--separator-row-p ()
  "Return non-nil if the current line is a separator row (backend-sensitive)."
  (cpo-table--call 'separator-row-p))

(defun cpo-table--markdown-table-begin ()
  "Return the beginning position of the markdown table at point."
  (save-excursion
    (beginning-of-line)
    ;; Walk backward as long as both the current line and the previous line
    ;; are table lines (start with optional whitespace then a pipe).
    (while (and (not (bobp))
                (looking-at-p cpo-table--line-start-regexp)
                (save-excursion
                  (forward-line -1)
                  (looking-at-p cpo-table--line-start-regexp)))
      (forward-line -1))
    (point)))

(defun cpo-table--markdown-table-end ()
  "Return the end position of the markdown table at point.
The end is the buffer position after the last row of the table."
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp))
                (looking-at-p cpo-table--line-start-regexp))
      (forward-line 1))
    (point)))

(defun cpo-table--markdown-align ()
  "Align the markdown table at point.
Delegates to `markdown-table-align' if available, otherwise
performs a simple alignment using the internal parser."
  (if (fboundp 'markdown-table-align)
      (markdown-table-align)
    (cpo-table--simple-align)))

(defun cpo-table--simple-align ()
  "Simple table alignment for markdown tables.
Pads each column to the maximum width found across all rows."
  (let* ((table (cpo-table--parse-table-at-point)))
    (when table
      (let* ((rows (plist-get table :rows))
             (n-cols (plist-get table :num-columns))
             ;; Compute column widths
             (col-widths (make-vector n-cols 3))
             (data-rows (cl-remove-if (lambda (r) (plist-get r :separator-p)) rows)))
        ;; Gather widths from all data rows
        (dolist (row data-rows)
          (let ((cells (plist-get row :cells)))
            (dolist (cell cells)
              (let* ((ci (plist-get cell :column-index))
                     (beg (plist-get cell :inner-begin))
                     (end (plist-get cell :inner-end))
                     (content (buffer-substring-no-properties beg end))
                     (w (length content)))
                (when (> w (aref col-widths ci))
                  (aset col-widths ci w))))))
        ;; Rewrite the table rows
        (let ((inhibit-modification-hooks nil))
          (with-undo-amalgamate
            (dolist (row (reverse rows))
              (let ((lbeg (plist-get row :line-begin))
                    (lend (plist-get row :line-end)))
                (if (plist-get row :separator-p)
                    ;; Rewrite separator row
                    (let ((sep-str "|"))
                      (dotimes (ci n-cols)
                        (setq sep-str (concat sep-str
                                              (make-string (+ 2 (aref col-widths ci)) ?-)
                                              "|")))
                      (delete-region lbeg lend)
                      (goto-char lbeg)
                      (insert sep-str))
                  ;; Rewrite data row: gather content for each column
                  (let ((cells (plist-get row :cells))
                        (cell-contents (make-vector n-cols "")))
                    (dolist (cell cells)
                      (let* ((ci (plist-get cell :column-index))
                             (beg (plist-get cell :inner-begin))
                             (end (plist-get cell :inner-end)))
                        (aset cell-contents ci
                              (string-trim (buffer-substring-no-properties beg end)))))
                    (let ((row-str "|"))
                      (dotimes (ci n-cols)
                        (let* ((content (aref cell-contents ci))
                               (w (aref col-widths ci))
                               (padded (concat " " content
                                               (make-string (- w (length content)) ?\s)
                                               " ")))
                          (setq row-str (concat row-str padded "|"))))
                      (delete-region lbeg lend)
                      (goto-char lbeg)
                      (insert row-str))))))))))))

;;; ---------------------------------------------------------------------------
;;; Core parser
;;; ---------------------------------------------------------------------------

(defun cpo-table--parse-row (line-begin line-end)
  "Parse a single table row from LINE-BEGIN to LINE-END.
Returns a row plist.  The caller provides the separator-p flag."
  (save-excursion
    (goto-char line-begin)
    (let ((sep-p (cpo-table--call 'separator-row-p))
          (cells nil)
          (col-idx 0))
      ;; Skip leading whitespace and opening pipe
      (skip-chars-forward " \t" line-end)
      (when (and (< (point) line-end) (eq (char-after) ?|))
        (forward-char 1))  ; skip opening pipe
      ;; Parse cells
      (while (< (point) line-end)
        (let ((cell-outer-begin (point))
              (cell-outer-end nil)
              (cell-inner-begin nil)
              (cell-inner-end nil))
          ;; Scan to next pipe or end of line
          (let ((scan-pos (point)))
            (while (and (< scan-pos line-end)
                        (not (eq (char-after scan-pos) ?|)))
              (setq scan-pos (1+ scan-pos)))
            (setq cell-outer-end scan-pos))
          ;; Compute inner (trimmed) bounds
          (save-excursion
            (goto-char cell-outer-begin)
            (skip-chars-forward " \t" cell-outer-end)
            (setq cell-inner-begin (point))
            (goto-char cell-outer-end)
            (skip-chars-backward " \t" cell-inner-begin)
            (setq cell-inner-end (point)))
          ;; Only add cell if we consumed something (avoid trailing empty cell after last |)
          (unless (and (= cell-outer-begin cell-outer-end)
                       (>= cell-outer-begin line-end))
            (push (list :outer-begin cell-outer-begin
                        :outer-end cell-outer-end
                        :inner-begin cell-inner-begin
                        :inner-end cell-inner-end
                        :column-index col-idx)
                  cells)
            (setq col-idx (1+ col-idx)))
          ;; Skip past the pipe
          (goto-char cell-outer-end)
          (when (and (< (point) line-end) (eq (char-after) ?|))
            (forward-char 1))))
      (list :line-begin line-begin
            :line-end line-end
            :separator-p sep-p
            :cells (nreverse cells)))))

(defun cpo-table--parse-table-at-point ()
  "Parse the table at point into a structured representation.
Returns a table plist or nil if not in a table.
Results are cached by buffer position and modification tick."
  (let* ((pos (point))
         (cached (cpo-table--cache-get pos)))
    (or cached
        (let ((result (cpo-table--do-parse-table-at-point)))
          (when result
            (cpo-table--cache-put pos result))
          result))))

(defun cpo-table--do-parse-table-at-point ()
  "Actually parse the table at point, no caching."
  (when (cpo-table--call 'at-table-p)
    (let* ((tbegin (save-excursion (cpo-table--call 'table-begin)))
           (tend (save-excursion (cpo-table--call 'table-end))))
      (when (and tbegin tend)
        (let ((rows nil)
              (num-columns 0))
          (save-excursion
            (goto-char tbegin)
            (while (< (point) tend)
              (let* ((lbegin (line-beginning-position))
                     (lend (line-end-position)))
                (when (save-excursion
                        (beginning-of-line)
                        (looking-at-p cpo-table--line-start-regexp))
                  (let ((row (cpo-table--parse-row lbegin lend)))
                    (let ((n (length (plist-get row :cells))))
                      (when (> n num-columns)
                        (setq num-columns n)))
                    (push row rows)))
                (forward-line 1))))
          (list :begin tbegin
                :end tend
                :rows (nreverse rows)
                :num-columns num-columns))))))

;;; ---------------------------------------------------------------------------
;;; Cell/row lookup at point
;;; ---------------------------------------------------------------------------

(defun cpo-table--row-at-point (&optional pt)
  "Return the row plist for the row at PT (default point).
Returns nil if not in a table row."
  (save-excursion
    (when pt (goto-char pt))
    (let ((table (cpo-table--parse-table-at-point)))
      (when table
        (let ((rows (plist-get table :rows))
              (pos (or pt (point))))
          (cl-find-if (lambda (row)
                        (and (<= (plist-get row :line-begin) pos)
                             (<= pos (plist-get row :line-end))))
                      rows))))))

(defun cpo-table--cell-at-point (&optional pt)
  "Return the cell plist for the cell at PT (default point).
Returns nil if not in a cell."
  (save-excursion
    (when pt (goto-char pt))
    (let ((pos (or pt (point)))
          (row (cpo-table--row-at-point pt)))
      (when (and row (not (plist-get row :separator-p)))
        (let ((cells (plist-get row :cells)))
          (cl-find-if (lambda (cell)
                        (and (<= (plist-get cell :outer-begin) pos)
                             (<= pos (plist-get cell :outer-end))))
                      cells))))))

(defun cpo-table--column-index-at-point (&optional pt)
  "Return the 0-based column index of the cell at PT (default point)."
  (let ((cell (cpo-table--cell-at-point pt)))
    (when cell (plist-get cell :column-index))))

(defun cpo-table--row-index-at-point (&optional pt)
  "Return the 0-based row index of the row at PT (default point)."
  (let ((table (cpo-table--parse-table-at-point))
        (row (cpo-table--row-at-point pt)))
    (when (and table row)
      (cl-position row (plist-get table :rows) :test #'equal))))

;;; ---------------------------------------------------------------------------
;;; Bounds functions
;;; ---------------------------------------------------------------------------

(defun cpo-table--table-bounds-at-point ()
  "Return (BEGIN . END) for the table at point, or nil."
  (when (cpo-table--call 'at-table-p)
    (let ((b (save-excursion (cpo-table--call 'table-begin)))
          (e (save-excursion (cpo-table--call 'table-end))))
      (when (and b e)
        (cons b e)))))

(defun cpo-table--cell-inner-bounds-at-point ()
  "Return (BEGIN . END) for the inner (trimmed) content of the cell at point."
  (let ((cell (cpo-table--cell-at-point)))
    (when cell
      (let ((ib (plist-get cell :inner-begin))
            (ie (plist-get cell :inner-end)))
        ;; For empty cells, return a zero-width region at the center of padding
        (if (= ib ie)
            (cons ib ie)
          (cons ib ie))))))

(defun cpo-table--cell-outer-bounds-at-point ()
  "Return (BEGIN . END) for the outer bounds of the cell at point.
Outer bounds span from after the left pipe to after the right pipe,
including whitespace padding and the closing delimiter."
  (let ((cell (cpo-table--cell-at-point)))
    (when cell
      (cons (plist-get cell :outer-begin)
            (min (1+ (plist-get cell :outer-end)) (point-max))))))

(defun cpo-table--row-inner-bounds-at-point ()
  "Return (BEGIN . END) for the inner bounds of the row at point.
Inner bounds span from the first non-whitespace character after the leading
pipe to the last non-whitespace character before the trailing pipe."
  (let ((row (cpo-table--row-at-point)))
    (when row
      (let* ((lbeg (plist-get row :line-begin))
             (lend (plist-get row :line-end)))
        (save-excursion
          (goto-char lbeg)
          (skip-chars-forward " \t")
          (when (eq (char-after) ?|) (forward-char 1))
          ;; Skip whitespace after the opening pipe
          (skip-chars-forward " \t" lend)
          (let ((inner-beg (point)))
            (goto-char lend)
            (when (eq (char-before) ?|) (backward-char 1))
            ;; Skip whitespace before the trailing pipe
            (skip-chars-backward " \t" inner-beg)
            (cons inner-beg (point))))))))

(defun cpo-table--row-outer-bounds-at-point ()
  "Return (BEGIN . END) for the outer bounds of the row at point.
Outer bounds span the full line including leading whitespace and trailing newline."
  (let ((row (cpo-table--row-at-point)))
    (when row
      (let ((lbeg (plist-get row :line-begin))
            (lend (plist-get row :line-end)))
        (cons lbeg (min (1+ lend) (point-max)))))))

;; Register with bounds-of-thing-at-point
(put 'cpo-table 'bounds-of-thing-at-point
     'cpo-table--table-bounds-at-point)
(put 'cpo-table-cell 'bounds-of-thing-at-point
     'cpo-table--cell-inner-bounds-at-point)
(put 'cpo-table-cell-outer 'bounds-of-thing-at-point
     'cpo-table--cell-outer-bounds-at-point)
(put 'cpo-table-row 'bounds-of-thing-at-point
     'cpo-table--row-inner-bounds-at-point)
(put 'cpo-table-row-outer 'bounds-of-thing-at-point
     'cpo-table--row-outer-bounds-at-point)

;;; ---------------------------------------------------------------------------
;;; Table-level navigation
;;; ---------------------------------------------------------------------------

(defun cpo-table--move-table (fwd-p beg-p &optional count)
  "Move across cpo-table bounds.
FWD-P determines direction, BEG-P selects beginning versus end, and COUNT
is the repeat count."
  (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion
   'cpo-table fwd-p beg-p count))

;;;###autoload
(defun cpo-table-forward-beginning (&optional count)
  "Move to the beginning of the next table, COUNT times."
  (interactive "p")
  (cpo-table--move-table t t count))

;;;###autoload
(defun cpo-table-forward-end (&optional count)
  "Move to the end of the next table, COUNT times."
  (interactive "p")
  (cpo-table--move-table t nil count))

;;;###autoload
(defun cpo-table-backward-end (&optional count)
  "Move to the end of the previous table, COUNT times."
  (interactive "p")
  (cpo-table--move-table nil nil count))

;;;###autoload
(defun cpo-table-backward-beginning (&optional count)
  "Move to the beginning of the previous table, COUNT times."
  (interactive "p")
  (cpo-table--move-table nil t count))

;;; ---------------------------------------------------------------------------
;;; Cell navigation
;;; ---------------------------------------------------------------------------

(defun cpo-table--next-data-row (table from-row-idx)
  "Return the row plist for the next data (non-separator) row after FROM-ROW-IDX.
Returns nil if no such row."
  (let ((rows (plist-get table :rows)))
    (cl-loop for i from (1+ from-row-idx) below (length rows)
             for row = (nth i rows)
             when (not (plist-get row :separator-p))
             return row)))

(defun cpo-table--prev-data-row (table from-row-idx)
  "Return the row plist for the previous data (non-separator) row before FROM-ROW-IDX.
Returns nil if no such row."
  (let ((rows (plist-get table :rows)))
    (cl-loop for i downfrom (1- from-row-idx) to 0
             for row = (nth i rows)
             when (not (plist-get row :separator-p))
             return row)))

(defun cpo-table--cell-in-row-at-col (row col-idx)
  "Return the cell plist in ROW at column COL-IDX, or nil."
  (cl-find col-idx (plist-get row :cells)
           :key (lambda (c) (plist-get c :column-index))))

(defun cpo-table--next-row (table from-row-idx)
  "Return the next physical row plist after FROM-ROW-IDX in TABLE."
  (let ((rows (plist-get table :rows)))
    (when (< (1+ from-row-idx) (length rows))
      (nth (1+ from-row-idx) rows))))

(defun cpo-table--prev-row (table from-row-idx)
  "Return the previous physical row plist before FROM-ROW-IDX in TABLE."
  (let ((rows (plist-get table :rows)))
    (when (>= (1- from-row-idx) 0)
      (nth (1- from-row-idx) rows))))

(defun cpo-table--goto-cell-beginning (cell)
  "Move point to the beginning of CELL, just after its left pipe."
  (goto-char (plist-get cell :outer-begin)))

(defun cpo-table--goto-cell-content-beginning (cell)
  "Move point to the beginning of CELL's trimmed content."
  (goto-char (plist-get cell :inner-begin)))

(defun cpo-table--goto-cell-end (cell)
  "Move point to the end of CELL's inner content."
  (goto-char (plist-get cell :inner-end)))

(defun cpo-table--goto-empty-cell-edit-point (cell)
  "Move point to the conventional edit position inside empty CELL."
  (goto-char (min (1+ (plist-get cell :outer-begin))
                  (plist-get cell :outer-end))))

(defun cpo-table--normalize-cell-move-direction (direction)
  "Return the canonical cell movement direction for DIRECTION."
  (pcase direction
    ((or 'forward 'right) 'forward)
    ((or 'backward 'left) 'backward)
    ('up 'up)
    ('down 'down)
    (_ (error "Unsupported cell movement direction: %S" direction))))

(defun cpo-table--invert-cell-move-direction (direction)
  "Return the opposite of canonical cell movement DIRECTION."
  (pcase direction
    ('forward 'backward)
    ('backward 'forward)
    ('up 'down)
    ('down 'up)))

(defun cpo-table--goto-cell-position (cell position)
  "Move point within CELL according to POSITION."
  (pcase position
    ('beginning (cpo-table--goto-cell-beginning cell))
    ('content-beginning (cpo-table--goto-cell-content-beginning cell))
    ('end (cpo-table--goto-cell-end cell))
    (_ (error "Unsupported cell movement position: %S" position))))

(defun cpo-table--cell-move-once (direction position)
  "Move one cell in canonical DIRECTION and place point at POSITION."
  (let ((before (point)))
    (pcase direction
      ('forward (cpo-table--cell-forward-beginning-once))
      ('backward (cpo-table--cell-backward-beginning-once))
      ('up (cpo-table--cell-up-once 'beginning))
      ('down (cpo-table--cell-down-once 'beginning)))
    (when (/= before (point))
      (let ((cell (cpo-table--cell-at-point)))
        (when cell
          (cpo-table--goto-cell-position cell position))))))

(defun cpo-table--parse-row-motion-args (args)
  "Return (COUNT IDEMPOTENT) parsed from row-motion ARGS."
  (let ((count 1)
        (idempotent nil))
    (when (numberp (car args))
      (setq count (or (pop args) 1)))
    (when args
      (setq idempotent (plist-get args :idempotent)))
    (list count idempotent)))

(defun cpo-table--capture-location ()
  "Capture the current point in table-relative terms."
  (let ((table (cpo-table--parse-table-at-point))
        (row (cpo-table--row-at-point))
        (cell (cpo-table--cell-at-point)))
    (cond
     (cell
      (let* ((point-pos (point))
             (row-index (cpo-table--row-index-at-point))
             (outer-begin (plist-get cell :outer-begin))
             (outer-limit (min (1+ (plist-get cell :outer-end)) (point-max)))
             (inner-begin (plist-get cell :inner-begin))
             (inner-end (plist-get cell :inner-end)))
        (if (and (>= point-pos inner-begin)
                 (<= point-pos inner-end))
            (list :kind 'cell
                  :row-index row-index
                  :col-index (plist-get cell :column-index)
                  :anchor 'inner
                  :offset (- point-pos inner-begin))
          (list :kind 'cell
                :row-index row-index
                :col-index (plist-get cell :column-index)
                :anchor 'outer
                :offset (- point-pos outer-begin)
                :limit (- outer-limit outer-begin)))))
     (row
      (list :kind 'row
            :row-index (cpo-table--row-index-at-point)
            :offset (- (point) (plist-get row :line-begin))))
     (table
      (list :kind 'table
            :offset (- (point) (plist-get table :begin)))))))

(defun cpo-table--restore-location (location &optional col-index-override)
  "Restore LOCATION in the current parsed table.
When COL-INDEX-OVERRIDE is non-nil, use it instead of LOCATION's
stored column index."
  (let ((table (cpo-table--parse-table-at-point)))
    (when (and location table)
      (pcase (plist-get location :kind)
        ('cell
         (let* ((row (nth (plist-get location :row-index)
                          (plist-get table :rows)))
                (col-index (or col-index-override
                               (plist-get location :col-index)))
                (cell (and row (cpo-table--cell-in-row-at-col row col-index))))
           (when cell
             (pcase (plist-get location :anchor)
               ('inner
                (goto-char (+ (plist-get cell :inner-begin)
                              (min (plist-get location :offset)
                                   (- (plist-get cell :inner-end)
                                      (plist-get cell :inner-begin))))))
               (_
                (let* ((outer-begin (plist-get cell :outer-begin))
                       (outer-limit (min (1+ (plist-get cell :outer-end))
                                         (point-max))))
                  (goto-char (+ outer-begin
                                (min (plist-get location :offset)
                                     (- outer-limit outer-begin)))))))))
         t)
        ('row
         (let ((row (nth (plist-get location :row-index)
                         (plist-get table :rows))))
           (when row
             (goto-char (+ (plist-get row :line-begin)
                           (min (plist-get location :offset)
                                (- (plist-get row :line-end)
                                   (plist-get row :line-begin)))))
             t)))
        ('table
         (goto-char (+ (plist-get table :begin)
                       (min (plist-get location :offset)
                            (- (plist-get table :end)
                               (plist-get table :begin)))))
         t)))))

;;;###autoload
(cl-defun cpo-table-cell-move (&key (count 1) (direction 'forward) (position 'beginning))
  "Move between table cells.
COUNT is the repeat count, DIRECTION is one of \\='forward, \\='backward,
\\='up, or \\='down, and POSITION is one of \\='beginning, \\='content-beginning,
or \\='end."
  (interactive)
  (setq count (or count 1))
  (let* ((direction (cpo-table--normalize-cell-move-direction direction))
         (effective-direction (if (< count 0)
                                  (cpo-table--invert-cell-move-direction direction)
                                direction))
         (n (abs count)))
    (dotimes (_i n)
      (cpo-table--cell-move-once effective-direction position))))

;;;###autoload
(defun cpo-table-cell-forward-beginning (&optional count)
  "Move to the beginning of the next cell (right), COUNT times.
Wraps to the first cell of the next row when past the last cell.
Skips separator rows."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'forward :position 'beginning))

;;;###autoload
(defun cpo-table-cell-backward-beginning (&optional count)
  "Move to the beginning of the previous cell (left), COUNT times."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'backward :position 'beginning))

(defun cpo-table--cell-forward-beginning-once ()
  "Move to beginning of next cell, wrapping to next row if needed."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and table row cell (not (plist-get row :separator-p)))
      (let* ((rows (plist-get table :rows))
             (col-idx (plist-get cell :column-index))
             (cells (plist-get row :cells))
             (next-cell (cl-find-if (lambda (c)
                                      (> (plist-get c :column-index) col-idx))
                                    cells)))
        (if next-cell
            (cpo-table--goto-cell-beginning next-cell)
          ;; Wrap to first cell of next data row
          (let* ((row-idx (cl-position row rows :test #'equal))
                 (next-row (cpo-table--next-data-row table row-idx)))
            (when next-row
              (let ((first-cell (car (plist-get next-row :cells))))
                (when first-cell
                  (cpo-table--goto-cell-beginning first-cell))))))))))

(defun cpo-table--cell-backward-beginning-once ()
  "Move to beginning of previous cell, wrapping to previous row if needed."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and table row cell (not (plist-get row :separator-p)))
      (let* ((rows (plist-get table :rows))
             (outer-begin (plist-get cell :outer-begin))
             (col-idx (plist-get cell :column-index))
             (cells (plist-get row :cells))
             (prev-cell (cl-find-if (lambda (c)
                                      (< (plist-get c :column-index) col-idx))
                                    (reverse cells)))
             (row-idx (cl-position row rows :test #'equal))
             (prev-row (and row-idx (cpo-table--prev-data-row table row-idx))))
        (when (or prev-cell prev-row)
          (if (> (point) outer-begin)
              (goto-char outer-begin)
            (if prev-cell
                (cpo-table--goto-cell-beginning prev-cell)
              (let ((last-cell (car (last (plist-get prev-row :cells)))))
                (when last-cell
                  (cpo-table--goto-cell-beginning last-cell))))))))))

;;;###autoload
(defun cpo-table-cell-forward-end (&optional count)
  "Move to the end of the next cell, COUNT times."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'forward :position 'end))

;;;###autoload
(defun cpo-table-cell-backward-end (&optional count)
  "Move to the end of the previous cell, COUNT times."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'backward :position 'end))

(defun cpo-table--cell-backward-end-once ()
  "Move to end of previous cell."
  (cpo-table--cell-backward-beginning-once)
  (let ((cell (cpo-table--cell-at-point)))
    (when cell (cpo-table--goto-cell-end cell))))

;;;###autoload
(cl-defun cpo-table-cell-up (&optional count &key (position 'beginning))
  "Move to same column in previous data row, COUNT times.
POSITION is \\='beginning or \\='end."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'up :position position))

;;;###autoload
(cl-defun cpo-table-cell-down (&optional count &key (position 'beginning))
  "Move to same column in next data row, COUNT times.
POSITION is \\='beginning or \\='end."
  (interactive "p")
  (cpo-table-cell-move :count count :direction 'down :position position))

(defun cpo-table--cell-up-once (position)
  "Move up one data row, preserving column index."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table row (not (plist-get row :separator-p)) col-idx)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (prev-row (cpo-table--prev-data-row table row-idx)))
        (when prev-row
          (let ((target-cell (cpo-table--cell-in-row-at-col prev-row col-idx)))
            (if target-cell
                (if (eq position 'end)
                    (cpo-table--goto-cell-end target-cell)
                  (cpo-table--goto-cell-beginning target-cell))
              ;; Column doesn't exist in target row -- go to last cell
              (let ((last-cell (car (last (plist-get prev-row :cells)))))
                (when last-cell
                  (if (eq position 'end)
                      (cpo-table--goto-cell-end last-cell)
                    (cpo-table--goto-cell-beginning last-cell)))))))))))

(defun cpo-table--cell-down-once (position)
  "Move down one data row, preserving column index."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table row (not (plist-get row :separator-p)) col-idx)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (next-row (cpo-table--next-data-row table row-idx)))
        (when next-row
          (let ((target-cell (cpo-table--cell-in-row-at-col next-row col-idx)))
            (if target-cell
                (if (eq position 'end)
                    (cpo-table--goto-cell-end target-cell)
                  (cpo-table--goto-cell-beginning target-cell))
              (let ((last-cell (car (last (plist-get next-row :cells)))))
                (when last-cell
                  (if (eq position 'end)
                      (cpo-table--goto-cell-end last-cell)
                    (cpo-table--goto-cell-beginning last-cell)))))))))))

;;;###autoload
(defun cpo-table-cell-beginning-of-content ()
  "Move to the beginning of the current cell's content."
  (interactive)
  (let ((cell (cpo-table--cell-at-point)))
    (when cell
      (cpo-table--goto-cell-content-beginning cell))))

;;;###autoload
(defun cpo-table-cell-end-of-content ()
  "Move to the end of the current cell's content."
  (interactive)
  (let ((cell (cpo-table--cell-at-point)))
    (when cell
      (cpo-table--goto-cell-end cell))))

;;; ---------------------------------------------------------------------------
;;; Row navigation
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun cpo-table-row-forward-beginning (&rest args)
  "Move to the beginning of the next data row, COUNT times."
  (interactive "p")
  (pcase-let ((`(,count ,idempotent)
               (cpo-table--parse-row-motion-args args)))
    (setq count (or count 1))
    (if idempotent
      (let ((row (cpo-table--row-at-point)))
        (when row
          (goto-char (plist-get row :line-begin))))
      (let ((fwd (> count 0))
            (n (abs count)))
        (dotimes (_i n)
          (if fwd
              (cpo-table--row-forward-beginning-once)
            (cpo-table--row-backward-beginning-once)))))))

;;;###autoload
(defun cpo-table-row-backward-beginning (&rest args)
  "Move to the beginning of the previous data row, COUNT times."
  (interactive "p")
  (pcase-let ((`(,count ,idempotent)
               (cpo-table--parse-row-motion-args args)))
    (if idempotent
      (let ((row (cpo-table--row-at-point)))
        (when (and row
                   (> (point) (plist-get row :line-begin)))
          (goto-char (plist-get row :line-begin))))
      (cpo-table-row-forward-beginning (- (or count 1))))))

;;;###autoload
(defun cpo-table-row-forward-end (&rest args)
  "Move to the end of the next data row, COUNT times."
  (interactive "p")
  (pcase-let ((`(,count ,idempotent)
               (cpo-table--parse-row-motion-args args)))
    (if idempotent
      (let ((row (cpo-table--row-at-point)))
        (when (and row
                   (> (point) (plist-get row :line-begin)))
          (goto-char (cpo-table--row-end-position row))))
      (setq count (or count 1))
      (let ((fwd (> count 0))
            (n (abs count)))
        (dotimes (_i n)
          (if fwd
              (cpo-table--row-forward-end-once)
            (cpo-table--row-backward-end-once)))))))

;;;###autoload
(defun cpo-table-row-backward-end (&rest args)
  "Move to the end of the previous data row, COUNT times."
  (interactive "p")
  (pcase-let ((`(,count ,idempotent)
               (cpo-table--parse-row-motion-args args)))
    (if idempotent
      (let ((row (cpo-table--row-at-point)))
        (when row
          (let* ((table (cpo-table--parse-table-at-point))
                 (row-idx (cpo-table--row-index-at-point))
                 (prev-row (and row-idx (cpo-table--prev-row table row-idx))))
            (when prev-row
              (goto-char (cpo-table--row-end-position prev-row))))))
      (cpo-table-row-forward-end (- (or count 1))))))

(defun cpo-table--row-first-cell-beginning (row)
  "Return the position of the first cell's content beginning in ROW."
  (let ((cells (plist-get row :cells)))
    (when cells
      (plist-get (car cells) :inner-begin))))

(defun cpo-table--row-last-cell-end (row)
  "Return the position of the last cell's content end in ROW."
  (let ((cells (plist-get row :cells)))
    (when cells
      (plist-get (car (last cells)) :inner-end))))

(defun cpo-table--row-forward-beginning-once ()
  "Move to the beginning of the next data row."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (next-row (cpo-table--next-data-row table row-idx)))
        (when next-row
          (let ((pos (plist-get next-row :line-begin)))
            (when pos (goto-char pos))))))))

(defun cpo-table--row-backward-beginning-once ()
  "Move to the beginning of the previous data row."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let ((row-begin (plist-get row :line-begin)))
        (if (> (point) row-begin)
            (goto-char row-begin)
          (let* ((rows (plist-get table :rows))
                 (row-idx (cl-position row rows :test #'equal))
                 (prev-row (cpo-table--prev-data-row table row-idx)))
            (when prev-row
              (let ((pos (plist-get prev-row :line-begin)))
                (when pos (goto-char pos))))))))))

(defun cpo-table--row-end-position (row)
  "Return the buffer position just after ROW's terminating newline."
  (min (1+ (plist-get row :line-end)) (point-max)))

(defun cpo-table--row-forward-end-once ()
  "Move to the end boundary of the current or next physical row."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let ((row-end (cpo-table--row-end-position row)))
        (if (< (point) row-end)
            (goto-char row-end)
          (let* ((rows (plist-get table :rows))
                 (row-idx (cl-position row rows :test #'equal))
                 (next-row (cpo-table--next-row table row-idx)))
            (when next-row
              (goto-char (cpo-table--row-end-position next-row)))))))))

(defun cpo-table--row-backward-end-once ()
  "Move to the end boundary of the previous physical row."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (prev-row (cpo-table--prev-row table row-idx)))
        (when prev-row
          (goto-char (cpo-table--row-end-position prev-row)))))))

;;; ---------------------------------------------------------------------------
;;; Expand-region commands
;;; ---------------------------------------------------------------------------

(cl-defun cpo-table--expand-region-to-bounds (bounds &key position)
  "Expand the active region to include BOUNDS.
POSITION controls where point ends up.  Nil or \\='beginning puts point
at the beginning, \\='end puts point at the end.  When POSITION is nil
and exactly one side of an active region grows, point moves to the side
that grew."
  (when bounds
    (let* ((had-region (region-active-p))
           (orig-region (if had-region
                            (cons (region-beginning) (region-end))
                          (cons (point) (point))))
           (new-bounds (if had-region
                           (cons (min (car bounds) (car orig-region))
                                 (max (cdr bounds) (cdr orig-region)))
                         bounds))
           (beg-grew (< (car new-bounds) (car orig-region)))
           (end-grew (> (cdr new-bounds) (cdr orig-region))))
      (when (or (not had-region) beg-grew end-grew)
        (let ((effective-position
               (cond
                (position position)
                ((and had-region
                      end-grew
                      (not beg-grew)
                      (< (car orig-region) (car bounds)))
                 'end)
                (t 'beginning))))
          (cpo-text-object-stuff--set-region new-bounds :position effective-position)
          (activate-mark)
          new-bounds)))))

;;;###autoload
(cl-defun cpo-table-cell-expand-region-inner (&key position)
  "Expand region to the current cell's inner bounds.
POSITION controls where point ends up."
  (interactive)
  (cpo-table--expand-region-to-bounds
   (cpo-table--cell-inner-bounds-at-point)
   :position position))

;;;###autoload
(cl-defun cpo-table-cell-expand-region (&key position)
  "Expand region to the current cell's outer bounds.
POSITION controls where point ends up."
  (interactive)
  (cpo-table--expand-region-to-bounds
   (cpo-table--cell-outer-bounds-at-point)
   :position position))

;;;###autoload
(cl-defun cpo-table-row-expand-region-inner (&key position)
  "Expand region to the current row's inner bounds.
POSITION controls where point ends up."
  (interactive)
  (cpo-table--expand-region-to-bounds
   (cpo-table--row-inner-bounds-at-point)
   :position position))

;;;###autoload
(cl-defun cpo-table-row-expand-region (&key position)
  "Expand region to the current row's outer bounds.
POSITION controls where point ends up."
  (interactive)
  (cpo-table--expand-region-to-bounds
   (cpo-table--row-outer-bounds-at-point)
   :position position))

;;; Expand-region integration

(defun cpo-table--region-equals-bounds-p (bounds)
  "Return non-nil if the active region equals BOUNDS."
  (and bounds
       (region-active-p)
       (= (region-beginning) (car bounds))
       (= (region-end) (cdr bounds))))

;;;###autoload
(cl-defun cpo-table-expand-region (&key position)
  "Expand region to the whole table at point.
POSITION controls where point ends up."
  (interactive)
  (cpo-table--expand-region-to-bounds
   (cpo-table--table-bounds-at-point)
   :position position))

;;;###autoload
(cl-defun cpo-table-expand-region-through-hierarchy (&key position)
  "Expand region through the table hierarchy.
The progression is cell inner -> cell outer -> row outer -> table."
  (interactive)
  (let ((cell-inner (cpo-table--cell-inner-bounds-at-point))
        (cell-outer (cpo-table--cell-outer-bounds-at-point))
        (row-outer (cpo-table--row-outer-bounds-at-point))
        (table-b (cpo-table--table-bounds-at-point)))
    (cond
     ;; Not in a cell: try row, then table
     ((not cell-inner)
      (cond
       ((cpo-table--region-equals-bounds-p row-outer)
        (when table-b
          (cpo-table--expand-region-to-bounds table-b :position position)))
       (row-outer
        (cpo-table--expand-region-to-bounds row-outer :position position))
       (table-b
        (cpo-table--expand-region-to-bounds table-b :position position))))
     ;; Region equals table: done
     ((cpo-table--region-equals-bounds-p table-b) nil)
     ;; Region equals row: expand to table
     ((cpo-table--region-equals-bounds-p row-outer)
      (when table-b
        (cpo-table--expand-region-to-bounds table-b :position position)))
     ;; Region equals cell outer: expand to row
     ((cpo-table--region-equals-bounds-p cell-outer)
      (when row-outer
        (cpo-table--expand-region-to-bounds row-outer :position position)))
     ;; Region equals cell inner: expand to cell outer
     ((cpo-table--region-equals-bounds-p cell-inner)
      (when cell-outer
        (cpo-table--expand-region-to-bounds cell-outer :position position)))
     ;; No region or region smaller than cell inner: select cell inner
     (t
      (when cell-inner
        (cpo-table--expand-region-to-bounds cell-inner :position position))))))

;;; ---------------------------------------------------------------------------
;;; Markdown backend: field navigation
;;; ---------------------------------------------------------------------------

(defun cpo-table--markdown-next-field ()
  "Move to the next field in a markdown table."
  (cpo-table--cell-forward-beginning-once))

(defun cpo-table--markdown-previous-field ()
  "Move to the previous field in a markdown table."
  (cpo-table--cell-backward-beginning-once))

(defun cpo-table--markdown-beginning-of-field ()
  "Move to the beginning of the current markdown table field."
  (cpo-table-cell-beginning-of-content))

(defun cpo-table--markdown-end-of-field ()
  "Move to the end of the current markdown table field."
  (cpo-table-cell-end-of-content))

;;; ---------------------------------------------------------------------------
;;; Row insertion
;;; ---------------------------------------------------------------------------

(defun cpo-table--make-empty-row (n-cols)
  "Return a string for an empty row with N-COLS columns."
  (let ((row "|"))
    (dotimes (_i n-cols)
      (setq row (concat row "  |")))
    row))

(defun cpo-table--position-in-first-cell-of-line (line-begin)
  "Move point to the first cell content of the row starting at LINE-BEGIN.
For a row like \"|  |  |\", places point after \"| \" (inside first cell)."
  ;; The new row is "|  |  |" -- we want point at inner-begin of first cell.
  ;; Use the parser to find it precisely.
  (goto-char line-begin)
  (setq cpo-table--cache nil)
  (let ((table (cpo-table--do-parse-table-at-point)))
    (when table
      (let* ((rows (plist-get table :rows))
             (row (cl-find-if (lambda (r)
                                (= (plist-get r :line-begin) line-begin))
                              rows)))
        (when row
          (let ((first-cell (car (plist-get row :cells))))
            (when first-cell
              ;; inner-begin for empty cell is at inner-begin (same as inner-end)
              ;; For "  " cell content, inner-begin = inner-end = halfway point
              ;; For practical purposes, place at outer-begin+1 (first space)
              (goto-char (1+ (plist-get first-cell :outer-begin))))))))))

(defun cpo-table--insert-row-after (row-plist table)
  "Insert an empty row after the row at ROW-PLIST.
Positions point in the first cell of the new row."
  (let* ((n-cols (plist-get table :num-columns))
         (lend (plist-get row-plist :line-end))
         (new-row (cpo-table--make-empty-row n-cols)))
    (goto-char lend)
    (insert "\n" new-row)
    ;; Invalidate cache
    (setq cpo-table--cache nil)
    ;; Position in first cell content of new row
    (let ((new-line-begin (save-excursion
                            (goto-char (1+ lend))
                            (line-beginning-position))))
      (cpo-table--position-in-first-cell-of-line new-line-begin))))

(defun cpo-table--insert-row-before (row-plist table)
  "Insert an empty row before the row at ROW-PLIST.
Positions point in the first cell of the new row."
  (let* ((n-cols (plist-get table :num-columns))
         (lbeg (plist-get row-plist :line-begin))
         (new-row (cpo-table--make-empty-row n-cols)))
    (goto-char lbeg)
    (insert new-row "\n")
    (setq cpo-table--cache nil)
    ;; The new row starts at lbeg (we inserted before the original row)
    (cpo-table--position-in-first-cell-of-line lbeg)))

;;;###autoload
(defun cpo-table--markdown-insert-row ()
  "Insert an empty row below the current row in a markdown table."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let ((target-row row))
        ;; If on separator row, insert after the separator
        (when (plist-get row :separator-p)
          (let* ((rows (plist-get table :rows))
                 (row-idx (cl-position row rows :test #'equal))
                 (next (cpo-table--next-data-row table row-idx)))
            (when next (setq target-row next))))
        (cpo-table--insert-row-after target-row table)))))

;;;###autoload
(defun cpo-table-row-open-forward ()
  "Insert a new empty row below the current row."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (next-row (and row-idx (nth (1+ row-idx) rows)))
             (target-row
              (cond
               ((plist-get row :separator-p) row)
               ((and (= row-idx 0) next-row (plist-get next-row :separator-p))
                next-row)
               (t row))))
        (with-undo-amalgamate
          (cpo-table--insert-row-after target-row table))))))

;;;###autoload
(defun cpo-table-row-open-backward ()
  "Insert a new empty row above the current row."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row)
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (next-row (and row-idx (nth (1+ row-idx) rows)))
             (target-row
              (cond
               ((plist-get row :separator-p) row)
               ((and (= row-idx 0) next-row (plist-get next-row :separator-p))
                next-row)
               (t row))))
        (with-undo-amalgamate
          (if (or (plist-get row :separator-p)
                  (and (= row-idx 0) next-row (plist-get next-row :separator-p)))
              (cpo-table--insert-row-after target-row table)
            (cpo-table--insert-row-before target-row table)))))))

;;;###autoload
(defun cpo-table-cell-open-down ()
  "Insert a new empty row below the current row, point in the cell below."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table row col-idx)
      (with-undo-amalgamate
        (cpo-table--insert-row-after row table))
      (setq cpo-table--cache nil)
      (let* ((new-table (cpo-table--parse-table-at-point))
             (new-row (cpo-table--row-at-point)))
        (when new-row
          (let ((cell (cpo-table--cell-in-row-at-col new-row col-idx)))
            (when cell
              (cpo-table--goto-empty-cell-edit-point cell))))))))

;;;###autoload
(defun cpo-table-cell-open-up ()
  "Insert a new empty row above the current row, point in the cell above."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table row col-idx)
      (with-undo-amalgamate
        (cpo-table--insert-row-before row table))
      (setq cpo-table--cache nil)
      (let* ((new-table (cpo-table--parse-table-at-point))
             (new-row (cpo-table--row-at-point)))
        (when new-row
          (let ((cell (cpo-table--cell-in-row-at-col new-row col-idx)))
            (when cell
              (cpo-table--goto-empty-cell-edit-point cell))))))))

;;; ---------------------------------------------------------------------------
;;; Column insertion
;;; ---------------------------------------------------------------------------

(defun cpo-table--column-fragment-for-row (row)
  "Return the text fragment for a newly inserted column in ROW."
  (if (plist-get row :separator-p)
      " --- |"
    "  |"))

(defun cpo-table--insert-column-at (new-col-idx table)
  "Insert a new empty column at NEW-COL-IDX in all rows of TABLE.
Positions point in the current row's cell in the new column."
  (let ((rows (plist-get table :rows))
        (current-row-index (cpo-table--row-index-at-point)))
    (with-undo-amalgamate
      (dolist (row (reverse rows))
        (let* ((target-cell (cpo-table--cell-in-row-at-col row new-col-idx))
               (insert-pos (if target-cell
                               (plist-get target-cell :outer-begin)
                             (plist-get row :line-end))))
          (goto-char insert-pos)
          (insert (cpo-table--column-fragment-for-row row)))))
    (setq cpo-table--cache nil)
    (let* ((new-table (cpo-table--parse-table-at-point))
           (current-row (and current-row-index
                             (nth current-row-index
                                  (plist-get new-table :rows)))))
      (when current-row
        (let ((new-cell (cpo-table--cell-in-row-at-col current-row new-col-idx)))
          (when new-cell
            (cpo-table--goto-empty-cell-edit-point new-cell)))))))

(defun cpo-table--insert-column-after (col-idx table)
  "Insert a new empty column after COL-IDX in all rows of TABLE."
  (cpo-table--insert-column-at (1+ col-idx) table))

(defun cpo-table--insert-column-before (col-idx table)
  "Insert a new empty column before COL-IDX in all rows of TABLE."
  (cpo-table--insert-column-at col-idx table))

;;;###autoload
(defun cpo-table-cell-open-forward ()
  "Insert a new empty column after the current cell's column.
Point lands in the new column in the current row."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table col-idx)
      (cpo-table--insert-column-after col-idx table))))

;;;###autoload
(defun cpo-table-cell-open-backward ()
  "Insert a new empty column before the current cell's column.
Point lands in the new column in the current row."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table col-idx)
      (cpo-table--insert-column-before col-idx table))))

;;;###autoload
(defun cpo-table-column-open-forward ()
  "Insert a new empty column after the current column.
Point lands in the top cell of the new column."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table col-idx)
      (cpo-table--insert-column-after col-idx table)
      ;; Move to top data row of new column
      (setq cpo-table--cache nil)
      (let* ((new-table (cpo-table--parse-table-at-point))
             (rows (plist-get new-table :rows)))
        (let ((first-data-row (cl-find-if (lambda (r)
                                            (not (plist-get r :separator-p)))
                                          rows)))
          (when first-data-row
            (let ((new-cell (cpo-table--cell-in-row-at-col first-data-row (1+ col-idx))))
              (when new-cell
                (cpo-table--goto-empty-cell-edit-point new-cell)))))))))

;;;###autoload
(defun cpo-table-column-open-backward ()
  "Insert a new empty column before the current column.
Point lands in the top cell of the new column."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table col-idx)
      (cpo-table--insert-column-before col-idx table)
      (setq cpo-table--cache nil)
      (let* ((new-table (cpo-table--parse-table-at-point))
             (rows (plist-get new-table :rows)))
        (let ((first-data-row (cl-find-if (lambda (r)
                                            (not (plist-get r :separator-p)))
                                          rows)))
          (when first-data-row
            (let ((new-cell (cpo-table--cell-in-row-at-col first-data-row col-idx)))
              (when new-cell
                (cpo-table--goto-empty-cell-edit-point new-cell)))))))))

;;;###autoload
(defun cpo-table--markdown-insert-column ()
  "Insert a column after the current one in a markdown table."
  (cpo-table-cell-open-forward))

;;; ---------------------------------------------------------------------------
;;; New table creation
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun cpo-table-open-new ()
  "Insert a new 1x1 markdown table template at point.
Place point in the first data cell."
  (interactive)
  (let ((template "|  |\n|---|\n")
        (start (point)))
    (insert template)
    (goto-char (+ start 2))))

(defun cpo-table--cell-text (cell)
  "Return CELL's trimmed content as a string."
  (buffer-substring-no-properties (plist-get cell :inner-begin)
                                  (plist-get cell :inner-end)))

(defun cpo-table--row-cell-texts (row n-cols)
  "Return ROW's cell texts padded to N-COLS entries."
  (let ((texts (make-list n-cols (if (plist-get row :separator-p) "---" ""))))
    (dolist (cell (plist-get row :cells))
      (setf (nth (plist-get cell :column-index) texts)
            (cpo-table--cell-text cell)))
    texts))

(defun cpo-table--build-row-string (texts separator-p)
  "Build a row string from TEXTS.
When SEPARATOR-P is non-nil, TEXTS are separator cell markers."
  (concat "| "
          (mapconcat (lambda (text)
                       (if (and separator-p (string-empty-p text))
                           "---"
                         text))
                     texts
                     " | ")
          " |"))

(defun cpo-table--rewrite-row-from-texts (row texts)
  "Rewrite ROW using TEXTS."
  (let ((line-begin (plist-get row :line-begin))
        (line-end (plist-get row :line-end)))
    (delete-region line-begin line-end)
    (goto-char line-begin)
    (insert (cpo-table--build-row-string texts (plist-get row :separator-p)))))

(defun cpo-table--split-text-at-point (cell)
  "Return (LEFT RIGHT) by splitting CELL's content at point."
  (let* ((content (cpo-table--cell-text cell))
         (split-pos (min (max 0 (- (point) (plist-get cell :inner-begin)))
                         (length content))))
    (list (substring content 0 split-pos)
          (substring content split-pos))))

(defun cpo-table--list-insert-at (items index item)
  "Return ITEMS with ITEM inserted at INDEX."
  (append (cl-subseq items 0 index)
          (list item)
          (nthcdr index items)))

(defun cpo-table--list-delete-at (items index)
  "Return ITEMS with the entry at INDEX removed."
  (append (cl-subseq items 0 index)
          (nthcdr (1+ index) items)))

(defun cpo-table--find-next-table-bounds (bounds)
  "Return the bounds of the next table after BOUNDS, or nil."
  (save-excursion
    (goto-char (cdr bounds))
    (catch 'found
      (while (re-search-forward cpo-table--line-start-regexp nil t)
        (goto-char (match-beginning 0))
        (let ((next-bounds (cpo-table--table-bounds-at-point)))
          (when next-bounds
            (throw 'found next-bounds)))
        (forward-line 1))
      nil)))

(defun cpo-table--find-prev-table-bounds (bounds)
  "Return the bounds of the previous table before BOUNDS, or nil."
  (save-excursion
    (goto-char (car bounds))
    (catch 'found
      (while (re-search-backward cpo-table--line-start-regexp nil t)
        (goto-char (match-beginning 0))
        (let ((prev-bounds (cpo-table--table-bounds-at-point)))
          (when (and prev-bounds
                     (< (car prev-bounds) (car bounds)))
            (throw 'found prev-bounds)))
        (unless (bobp)
          (backward-char 1)))
      nil)))

(defun cpo-table--transpose-table-once (direction)
  "Swap the current table with the adjacent table in DIRECTION (+1/-1)."
  (let* ((current-bounds (cpo-table--table-bounds-at-point))
         (offset (and current-bounds (- (point) (car current-bounds))))
         (other-bounds (and current-bounds
                            (if (> direction 0)
                                (cpo-table--find-next-table-bounds current-bounds)
                              (cpo-table--find-prev-table-bounds current-bounds)))))
    (when (and current-bounds other-bounds)
      (let* ((current-begin (car current-bounds))
             (current-end (cdr current-bounds))
             (other-begin (car other-bounds))
             (other-end (cdr other-bounds))
             (current-first (< current-begin other-begin))
             (first-begin (if current-first current-begin other-begin))
             (first-end (if current-first current-end other-end))
             (second-begin (if current-first other-begin current-begin))
             (second-end (if current-first other-end current-end))
             (current-text (buffer-substring-no-properties current-begin current-end))
             (other-text (buffer-substring-no-properties other-begin other-end)))
        (atomic-change-group
          (delete-region second-begin second-end)
          (goto-char second-begin)
          (insert (if current-first current-text other-text))
          (delete-region first-begin first-end)
          (goto-char first-begin)
          (insert (if current-first other-text current-text)))
        (setq cpo-table--cache nil)
        (if current-first
            (goto-char (+ second-begin
                          (- (length other-text) (length current-text))
                          offset))
          (goto-char (+ first-begin offset)))))))

;;;###autoload
(defun cpo-table-transpose-forward (&optional count)
  "Swap the current table with the next table, COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (cpo-table--transpose-table-once (if fwd 1 -1))))))

;;;###autoload
(defun cpo-table-transpose-backward (&optional count)
  "Swap the current table with the previous table, COUNT times."
  (interactive "p")
  (cpo-table-transpose-forward (- (or count 1))))

;;;###autoload
(cl-defun cpo-table-column-split (&key (direction 'forward))
  "Split the current column in the current row at point."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and table row cell (not (plist-get row :separator-p)))
      (let* ((rows (plist-get table :rows))
             (n-cols (plist-get table :num-columns))
             (row-index (cpo-table--row-index-at-point))
             (col-index (plist-get cell :column-index))
             (parts (cpo-table--split-text-at-point cell))
             (left-text (car parts))
             (right-text (cadr parts))
             (insert-at (1+ col-index)))
        (with-undo-amalgamate
          (cl-loop for idx downfrom (1- (length rows)) to 0
                   for row* = (nth idx rows)
                   do
                   (let* ((texts (cpo-table--row-cell-texts row* n-cols))
                   (new-texts
                    (cond
                     ((plist-get row* :separator-p)
                      (cpo-table--list-insert-at texts insert-at "---"))
                     ((= idx row-index)
                      (cpo-table--list-insert-at
                       (cl-copy-list (progn
                                       (setf (nth col-index texts) left-text)
                                       texts))
                       insert-at
                       right-text))
                     (t
                      (cpo-table--list-insert-at texts insert-at "")))))
                     (cpo-table--rewrite-row-from-texts row* new-texts))))
        (setq cpo-table--cache nil)
        (let* ((new-table (cpo-table--parse-table-at-point))
               (new-row (nth row-index (plist-get new-table :rows))))
          (when new-row
            (if (eq direction 'backward)
                (let ((new-cell (cpo-table--cell-in-row-at-col new-row col-index)))
                  (when new-cell
                    (goto-char (+ (plist-get new-cell :inner-begin)
                                  (length left-text)))))
              (let ((new-cell (cpo-table--cell-in-row-at-col new-row (1+ col-index))))
                (when new-cell
                  (goto-char (plist-get new-cell :inner-begin)))))))))))

;;;###autoload
(cl-defun cpo-table-column-join (&key (direction 'backward))
  "Join the current column with an adjacent column."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and table row cell)
      (let* ((rows (plist-get table :rows))
             (n-cols (plist-get table :num-columns))
             (row-index (cpo-table--row-index-at-point))
             (col-index (plist-get cell :column-index))
             (left-col (if (eq direction 'backward) (1- col-index) col-index))
             (right-col (if (eq direction 'backward) col-index (1+ col-index))))
        (when (and (>= left-col 0)
                   (< right-col n-cols))
          (let* ((current-texts (cpo-table--row-cell-texts row n-cols))
                 (boundary (length (nth left-col current-texts))))
            (with-undo-amalgamate
              (cl-loop for idx downfrom (1- (length rows)) to 0
                       for row* = (nth idx rows)
                       do
                (let* ((texts (cpo-table--row-cell-texts row* n-cols))
                       (merged (if (plist-get row* :separator-p)
                                   "---"
                                 (concat (nth left-col texts)
                                         (nth right-col texts)))))
                  (setf (nth left-col texts) merged)
                  (cpo-table--rewrite-row-from-texts
                   row*
                   (cpo-table--list-delete-at texts right-col)))))
            (setq cpo-table--cache nil)
            (let* ((new-table (cpo-table--parse-table-at-point))
                   (new-row (nth row-index (plist-get new-table :rows)))
                   (new-cell (and new-row
                                  (cpo-table--cell-in-row-at-col new-row left-col))))
              (when new-cell
                (goto-char (+ (plist-get new-cell :inner-begin) boundary))))))))))

;;; ---------------------------------------------------------------------------
;;; Transpose operations
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun cpo-table-row-transpose-forward (&optional count)
  "Swap the current row with the next data row, COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (if fwd
            (cpo-table--row-transpose-once 1)
          (cpo-table--row-transpose-once -1))))))

;;;###autoload
(defun cpo-table-row-transpose-backward (&optional count)
  "Swap the current row with the previous data row, COUNT times."
  (interactive "p")
  (cpo-table-row-transpose-forward (- (or count 1))))

(defun cpo-table--row-transpose-once (direction)
  "Swap the current row with the adjacent data row in DIRECTION (+1/-1)."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point)))
    (when (and table row (not (plist-get row :separator-p)))
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (other-row (if (> direction 0)
                            (cpo-table--next-data-row table row-idx)
                          (cpo-table--prev-data-row table row-idx))))
        (when other-row
          (let* ((r1-beg (plist-get row :line-begin))
                 (r1-end (plist-get row :line-end))
                 (r2-beg (plist-get other-row :line-begin))
                 (r2-end (plist-get other-row :line-end))
                 (offset (- (point) r1-beg))
                 ;; Determine which is first/second in buffer
                 (first-beg (min r1-beg r2-beg))
                 (second-beg (max r1-beg r2-beg))
                 (is-r1-first (= r1-beg first-beg))
                 (first-end (if is-r1-first r1-end r2-end))
                 (second-end (if is-r1-first r2-end r1-end))
                 (s-first (buffer-substring-no-properties first-beg first-end))
                 (s-second (buffer-substring-no-properties second-beg second-end)))
            (atomic-change-group
              (delete-region second-beg second-end)
              (goto-char second-beg)
              (insert s-first)
              (delete-region first-beg first-end)
              (goto-char first-beg)
              (insert s-second))
            (setq cpo-table--cache nil)
            ;; Restore point in the moved row
            (if is-r1-first
                ;; r1 moved to second-beg position, adjusted for length diff
                (let ((len-diff (- (length s-second) (length s-first))))
                  (goto-char (+ second-beg len-diff offset)))
              (goto-char (+ first-beg offset)))
            (undo-boundary)))))))

(defun cpo-table--cell-transpose-vertical-once (direction)
  "Swap the current cell with the adjacent cell in DIRECTION vertically.
DIRECTION is +1 for down and -1 for up."
  (let* ((table (cpo-table--parse-table-at-point))
         (row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and table row cell (not (plist-get row :separator-p)))
      (let* ((rows (plist-get table :rows))
             (row-idx (cl-position row rows :test #'equal))
             (other-row (if (> direction 0)
                            (cpo-table--next-data-row table row-idx)
                          (cpo-table--prev-data-row table row-idx)))
             (col-idx (plist-get cell :column-index))
             (other-cell (and other-row
                              (cpo-table--cell-in-row-at-col other-row col-idx))))
        (when other-cell
          (let* ((c1-beg (plist-get cell :inner-begin))
                 (c1-end (plist-get cell :inner-end))
                 (c2-beg (plist-get other-cell :inner-begin))
                 (c2-end (plist-get other-cell :inner-end))
                 (offset (- (point) c1-beg))
                 (is-c1-first (< c1-beg c2-beg))
                 (first-beg (if is-c1-first c1-beg c2-beg))
                 (first-end (if is-c1-first c1-end c2-end))
                 (second-beg (if is-c1-first c2-beg c1-beg))
                 (second-end (if is-c1-first c2-end c1-end))
                 (s-first (buffer-substring-no-properties first-beg first-end))
                 (s-second (buffer-substring-no-properties second-beg second-end)))
            (atomic-change-group
              (delete-region second-beg second-end)
              (goto-char second-beg)
              (insert s-first)
              (delete-region first-beg first-end)
              (goto-char first-beg)
              (insert s-second))
            (setq cpo-table--cache nil)
            (if is-c1-first
                (let ((len-diff (- (length s-second) (length s-first))))
                  (goto-char (+ second-beg len-diff offset)))
              (goto-char (+ first-beg offset)))
            (undo-boundary)))))))

;;;###autoload
(defun cpo-table-cell-transpose (&rest args)
  "Swap the current cell with the adjacent cell in a given direction.
The optional first positional argument is COUNT.  Remaining keyword
arguments currently support only :direction, which may be one of
\\='left, \\='right, \\='up, or \\='down."
  (interactive "p")
  (let* ((count-arg-p (and args (numberp (car args))))
         (count (if count-arg-p (car args) 1))
         (keywords (if count-arg-p (cdr args) args))
         (direction (or (plist-get keywords :direction) 'right))
         (dir (pcase direction
                ((or 'forward 'right) 'right)
                ((or 'backward 'left) 'left)
                ('up 'up)
                ('down 'down)
                (_ (error "Unsupported cell transpose direction: %S" direction))))
         (n (abs count))
         (effective-dir (if (< count 0)
                            (pcase dir
                              ('right 'left)
                              ('left 'right)
                              ('up 'down)
                              ('down 'up))
                          dir)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (pcase effective-dir
          ('right (cpo-table--cell-transpose-horizontal-once 1))
          ('left (cpo-table--cell-transpose-horizontal-once -1))
          ('up (cpo-table--cell-transpose-vertical-once -1))
          ('down (cpo-table--cell-transpose-vertical-once 1)))))))

;;;###autoload
(defun cpo-table-cell-transpose-forward (&optional count)
  "Swap the current cell with the next cell (rightward), COUNT times."
  (interactive "p")
  (cpo-table-cell-transpose (or count 1) :direction 'right))

;;;###autoload
(defun cpo-table-cell-transpose-backward (&optional count)
  "Swap the current cell with the previous cell (leftward), COUNT times."
  (interactive "p")
  (cpo-table-cell-transpose (or count 1) :direction 'left))

(defun cpo-table--cell-transpose-horizontal-once (direction)
  "Swap the current cell with the adjacent cell in DIRECTION (+1/-1) horizontally."
  (let* ((row (cpo-table--row-at-point))
         (cell (cpo-table--cell-at-point)))
    (when (and row cell (not (plist-get row :separator-p)))
      (let* ((cells (plist-get row :cells))
             (col-idx (plist-get cell :column-index))
             (other-idx (+ col-idx direction))
             (other-cell (cpo-table--cell-in-row-at-col row other-idx)))
        (when other-cell
          (let* ((c1-beg (plist-get cell :inner-begin))
                 (c1-end (plist-get cell :inner-end))
                 (c2-beg (plist-get other-cell :inner-begin))
                 (c2-end (plist-get other-cell :inner-end))
                 (offset (- (point) c1-beg))
                 (is-c1-first (< c1-beg c2-beg))
                 (first-beg (if is-c1-first c1-beg c2-beg))
                 (first-end (if is-c1-first c1-end c2-end))
                 (second-beg (if is-c1-first c2-beg c1-beg))
                 (second-end (if is-c1-first c2-end c1-end))
                 (s-first (buffer-substring-no-properties first-beg first-end))
                 (s-second (buffer-substring-no-properties second-beg second-end)))
            (atomic-change-group
              (delete-region second-beg second-end)
              (goto-char second-beg)
              (insert s-first)
              (delete-region first-beg first-end)
              (goto-char first-beg)
              (insert s-second))
            (setq cpo-table--cache nil)
            (if is-c1-first
                (let ((len-diff (- (length s-second) (length s-first))))
                  (goto-char (+ second-beg len-diff offset)))
              (goto-char (+ first-beg offset)))
            (undo-boundary)))))))

;;;###autoload
(defun cpo-table-column-transpose-forward (&optional count)
  "Swap the current column with the next column (all rows), COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (n (abs count)))
    (with-undo-amalgamate
      (dotimes (_i n)
        (if fwd
            (cpo-table--column-transpose-once 1)
          (cpo-table--column-transpose-once -1))))))

;;;###autoload
(defun cpo-table-column-transpose-backward (&optional count)
  "Swap the current column with the previous column (all rows), COUNT times."
  (interactive "p")
  (cpo-table-column-transpose-forward (- (or count 1))))

(defun cpo-table--column-transpose-once (direction)
  "Swap the current column with the adjacent column in DIRECTION (+1/-1)."
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point))
         (location (cpo-table--capture-location)))
    (when (and table col-idx)
      (let ((other-col-idx (+ col-idx direction)))
        (when (and (>= other-col-idx 0)
                   (< other-col-idx (plist-get table :num-columns)))
          (let ((rows (plist-get table :rows)))
            (with-undo-amalgamate
              (dolist (row (reverse rows))
                (let* ((c1 (cpo-table--cell-in-row-at-col row col-idx))
                       (c2 (cpo-table--cell-in-row-at-col row other-col-idx)))
                  (when (and c1 c2)
                    (let* ((b1 (plist-get c1 :inner-begin))
                           (e1 (plist-get c1 :inner-end))
                           (b2 (plist-get c2 :inner-begin))
                           (e2 (plist-get c2 :inner-end))
                           (s1 (buffer-substring-no-properties b1 e1))
                           (s2 (buffer-substring-no-properties b2 e2))
                           (is-c1-first (< b1 b2)))
                      (if is-c1-first
                          (progn
                            (delete-region b2 e2)
                            (goto-char b2)
                            (insert s1)
                            (delete-region b1 e1)
                            (goto-char b1)
                            (insert s2))
                        (delete-region b1 e1)
                        (goto-char b1)
                        (insert s2)
                        (delete-region b2 e2)
                        (goto-char b2)
                        (insert s1))))))
              (setq cpo-table--cache nil)
              (cpo-table--restore-location location other-col-idx)
              (undo-boundary))))))))

;;; ---------------------------------------------------------------------------
;;; Column delete
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun cpo-table-column-delete ()
  "Delete the current column from all rows of the table."
  (interactive)
  (let* ((table (cpo-table--parse-table-at-point))
         (col-idx (cpo-table--column-index-at-point)))
    (when (and table col-idx)
      (let ((rows (plist-get table :rows)))
        (with-undo-amalgamate
          (dolist (row (reverse rows))
            (let* ((cell (cpo-table--cell-in-row-at-col row col-idx)))
              (when cell
                ;; Delete from outer-begin to outer-end, then the pipe
                (let* ((ob (plist-get cell :outer-begin))
                       (oe (plist-get cell :outer-end)))
                  ;; We also need to delete the pipe after the cell
                  (delete-region ob (min (1+ oe) (point-max)))))))
          (setq cpo-table--cache nil)
          (undo-boundary))))))

;;; ---------------------------------------------------------------------------
;;; Alignment command
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun cpo-table-align ()
  "Align the table at point using the mode-appropriate backend."
  (interactive)
  (let ((location (cpo-table--capture-location)))
    (with-undo-amalgamate
      (cpo-table--call 'align))
    (setq cpo-table--cache nil)
    (cpo-table--restore-location location)))

;;; ---------------------------------------------------------------------------
;;; Repeatable motion integration
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair
   'cpo-table-cell-forward-beginning 'cpo-table-cell-backward-beginning)
  (repeatable-motion-define-pair
   'cpo-table-cell-forward-end 'cpo-table-cell-backward-end)
  (repeatable-motion-define-pair
   'cpo-table-cell-up 'cpo-table-cell-down)
  (repeatable-motion-define-pair
   'cpo-table-row-forward-beginning 'cpo-table-row-backward-beginning)
  (repeatable-motion-define-pair
   'cpo-table-row-forward-end 'cpo-table-row-backward-end)
  (repeatable-motion-define-pair
   'cpo-table-forward-beginning 'cpo-table-backward-beginning)
  (repeatable-motion-define-pair
   'cpo-table-forward-end 'cpo-table-backward-end))

(provide 'cpo-table)
;;; cpo-table.el ends here
