;;; -*- lexical-binding: t; -*-
(require 'cpo-tree-walk)
(require 'outline)

(defun cpo-outline-forward-slurp-heading ()
  (interactive)
  (require 'org)
  (save-mark-and-excursion
    (when (cpo-tree-walk--motion-moved
           (lambda () (ignore-errors (outline-forward-same-level 1))))
      (org-demote-subtree))))

(defun cpo-outline-forward-barf-heading ()
  (interactive)
  (require 'org)
  (save-mark-and-excursion
    (when (cpo-tree-walk--motion-moved 'cpo-outline-down-to-last-child)
      (org-promote-subtree))))

(defun cpo-outline-add-heading-above ()
  (interactive)
  (outline-back-to-heading)
  (outline-insert-heading)
  (insert " "))

(defun cpo-outline-add-heading-below ()
  (interactive)
  (cpo-outline-add-heading-above)
  (outline-move-subtree-down 1)
  (end-of-line))

(defun cpo-outline-add-child-heading (&optional insert-index)
  "Insert a child heading.  If INSERT-INDEX is null, insert last child, otherwise insert at index, where 0 means insert before the current first child."
  (interactive "P")
  (with-undo-amalgamate
    (let* ((count (cond ((numberp insert-index) insert-index)
                        ((consp insert-index) (car insert-index))
                        (t 'last)))
           (point-at-child (cpo-tree-walk--motion-moved
                            (if (eq count 'last)
                                'cpo-outline-down-to-last-child
                              'cpo-outline-down-to-first-child))))
      (if point-at-child
          (if (eq count 'last)
              (cpo-outline-add-heading-below)
            (progn
              (outline-forward-same-level count)
              (cpo-outline-add-heading-above)))
        (progn
          (cpo-outline-add-heading-below)
          ;;(org-demote-subtree)
          ;; instead of using demote, which is screwing up history somehow, just insert an extra star...
          (backward-char 1)
          (insert "*")
          (forward-char 1)
          )))))

(defun cpo-outline-add-ancestor-next-sibling-heading (&optional ancestor-index)
  "Insert a next sibling heading for parent, or ancestor at ANCESTOR-INDEX.
This is useful when finishing a heading and wanting to start writing something at the parent (or ancestor) level."
  (interactive "p")
  (outline-up-heading (or ancestor-index 1))
  (cpo-outline-add-heading-below))


(defun cpo-outline-down-to-first-child ()
  (interactive)
  (let ((end-point nil))
    (save-mark-and-excursion
      (outline-back-to-heading)
      (let* ((orig-level (outline-level))
             (orig-heading-point (point))
             (moved (cpo-tree-walk--motion-moved 'outline-next-heading))
             (next-level (outline-level))
             (next-heading-point (point)))
        (when (< orig-level next-level)
          (setq end-point next-heading-point))))
    (when end-point (goto-char end-point))))

;; TODO - switch these to use outline-minor-mode if possible instead of org-mode
(defun cpo-org-down-element ()
  (interactive)
  (require 'org)
  ;; To not get an error part way through a complex movement where an intermediate movement is allowed to fail
  (ignore-errors (org-down-element)))
;; TODO - use outline-up-heading
(defun cpo-org-up-element ()
  (interactive)
  (require 'org)
  ;; To not get an error part way through a complex movement where an intermediate movement is allowed to fail
  (ignore-errors (org-up-element)))

(defun cpo-org-forward-to-last-sibling ()
  (require 'org)
  (while (cpo-tree-walk--motion-moved (lambda () (org-forward-heading-same-level 1)))))
(defun cpo-org-down-to-last-child ()
  ;; TODO - this and others should probably take a number argument
  (interactive)
  (require 'org)
  (and (cpo-tree-walk--motion-moved #'cpo-org-down-element)
       (cpo-org-forward-to-last-sibling)))

(defun cpo-outline--outline-at-anchor-point-p ()
  (save-mark-and-excursion
    (let ((start-point (point)))
      (outline-previous-heading)
      (outline-next-heading)
      (equal (point) start-point))))

(defun cpo-outline--forward-half-or-full-sibling-single (&optional half-sibling-only)
  (let ((start-point (point))
        (start-level (org-current-level))
        ;; Use 0 as a sentinel for root headings with no parent, so that the
        ;; logic below works uniformly: root-level headings (level 1 with no
        ;; parent) can still reach their forward siblings.
        (parent-level (or (save-mark-and-excursion
                            (and (ignore-errors (outline-up-heading 1))
                                 (org-current-level)))
                          0))
        (cur-level nil)
        (end-point nil))
    (and
     (save-mark-and-excursion
       (outline-back-to-heading)
       ;; Use the return value of outline-next-heading directly as the loop
       ;; condition: it returns the new position on success, nil on failure.
       ;; This avoids treating an outline-next-heading that moves to point-max
       ;; (without finding a real heading) as a successful move.
       (while (and (ignore-errors (outline-next-heading))
                   (setq cur-level (org-current-level))
                   (setq end-point (point))
                   (and cur-level (< start-level cur-level)))
         nil)
       t)
     (cond
      ((equal cur-level start-level)
       (when (not half-sibling-only)
         (goto-char end-point)))
      ((and cur-level (< parent-level cur-level start-level))
       (goto-char end-point))
      (t nil)))))

(defun cpo-outline--backward-half-or-full-sibling-single (&optional half-sibling-only)
  (let ((start-point (point))
        (start-level (org-current-level))
        ;; Use 0 as a sentinel for root headings with no parent, so that the
        ;; logic below works uniformly: root-level headings (level 1 with no
        ;; parent) can still reach their backward siblings.
        ;; Note: unlike the forward function, the naive code here used to call
        ;; (org-current-level) unconditionally after (outline-up-heading 1),
        ;; which would return the current heading's level on failure rather than
        ;; nil.  This caused it to use start-level as parent-level, breaking
        ;; backward sibling movement at root level.
        (parent-level (or (save-mark-and-excursion
                            (and (ignore-errors (outline-up-heading 1))
                                 (org-current-level)))
                          0))
        (keep-going t)
        (min-level nil)
        (min-level-point nil))
    (and
     (save-mark-and-excursion
       (outline-back-to-heading)
       (while (and keep-going
                   (cpo-tree-walk--motion-moved
                    (lambda () (ignore-errors (outline-previous-heading)))))
         (let ((cur-level (org-current-level)))
           (when (not min-level)
             (setq min-level cur-level)
             (setq min-level-point (point)))
           ;; Only update min-level-point for strictly shallower levels,
           ;; not for same-level headings.  This ensures backward
           ;; half-sibling goes to the nearest (last) heading in the
           ;; preceding half-sibling group rather than the first.
           (when (and (< parent-level cur-level)
                      (< cur-level min-level))
             (setq min-level cur-level)
             (setq min-level-point (point)))
           (when (<= cur-level start-level)
             (setq keep-going nil))))
       t)
     (cond
      ((and (equal min-level start-level) (not half-sibling-only))
       (goto-char min-level-point))
      ((<= min-level start-level)
       nil)
      ((not min-level)
       nil)
      (t (goto-char min-level-point))))))

(defun cpo-outline-forward-full-sibling (num)
  "Go forward to full sibling of outline-tree node at point NUM times, going backward for negative NUM.
Return the (positive) number of iterations that could NOT be done (IE returns 0 for full success).
Notably this will stop if it hits a half-sibling boundary, only moving to headings at exactly the same level."
  (interactive "p")
  (let* ((direction (if (< 0 num) 1 -1))
         (times (abs num))
         (index 0)
         (successful t))
    (while (and (< index times)
                successful)
      (let ((moved (cpo-tree-walk--motion-moved
                    (lambda ()
                      (ignore-errors
                        (if (< 0 direction)
                            (outline-forward-same-level 1)
                          (outline-backward-same-level 1)))))))
        (if moved
            (setq index (+ 1 index))
          (setq successful nil))))
    (- times index)))

(defun cpo-outline-backward-full-sibling (num)
  "The reverse of `cpo-outline-forward-full-sibling'."
  (interactive "p")
  (cpo-outline-forward-full-sibling (* -1 (or num 1))))

(defun cpo-outline-forward-to-last-full-sibling ()
  "Move forward to the last full sibling at the same outline level."
  (interactive)
  (while (cpo-tree-walk--motion-moved (lambda () (cpo-outline-forward-full-sibling 1)))))

(defun cpo-outline-backward-to-first-full-sibling ()
  "Move backward to the first full sibling at the same outline level."
  (interactive)
  (while (cpo-tree-walk--motion-moved (lambda () (cpo-outline-forward-full-sibling -1)))))

(defun cpo-outline-forward-half-or-full-sibling (&optional count half-sibling-only)
  "Move forward by half or full outline sibling.
If HALF-SIBLING-ONLY, only moves to a half sibling, and will stall on full siblings.
A half sibling arises when an outline heading has a first child that increases heading level by more than 1, then a later child that has a level that is between the first child and the parent level.
There can be arbitrarily many half siblings, since the depth difference between a parent and first child can be arbitrarily wide, then future children can have gradually less depth.
"
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (if fwd
        (dotimes (i count) (cpo-outline--forward-half-or-full-sibling-single half-sibling-only))
      (dotimes (i count) (cpo-outline--backward-half-or-full-sibling-single half-sibling-only)))))

(defun cpo-outline-backward-half-or-full-sibling (&optional count half-sibling-only)
  "Move backward by half or full outline sibling.
If HALF-SIBLING-ONLY, only moves to a half sibling, and will stall on full siblings.
A half sibling arises when an outline heading has a first child that increases heading level by more than 1, then a later child that has a level that is between the first child and the parent level.
There can be arbitrarily many half siblings, since the depth difference between a parent and first child can be arbitrarily wide, then future children can have gradually less depth.
"
  (interactive "p")
  (cpo-outline-forward-half-or-full-sibling (- (or count 1)) half-sibling-only))

(cpo-tree-walk-define-operations
 :def-inorder-forward cpo-outline-inorder-traversal-forward
 :def-inorder-backward cpo-outline-inorder-traversal-backward
 :def-down-to-last-descendant cpo-outline-down-to-last-descendant
 :def-down-to-last-child cpo-outline-down-to-last-child

 ;;:def-evil-inner-object-for-tree-with-no-end-delimiter cpo-outline-tree-inner
 ;;:def-evil-outer-object-for-tree-with-no-end-delimiter cpo-outline-tree-outer

 :def-bounds-for-tree-with-no-end-delimiter cpo-outline-tree-bounds
 :def-children-bounds-for-tree-with-no-end-delimiter cpo-outline-tree-children-bounds
 :def-expand-region cpo-outline-expand-region
 :def-expand-region-idempotent cpo-outline-expand-region-idempotent
 :def-select-children-once cpo-outline-region-to-children
 :def-expand-region-to-children/ancestor-generation cpo-outline-expand-region/children-region
 :def-transpose-sibling-forward cpo-outline-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-outline-transpose-sibling-backward
 :def-up-to-root cpo-outline-up-to-root
 :def-select-root cpo-outline-select-root
 :def-visual-modifier cpo-outline-estate-visual-modifier

 :use-object-name "outline-mode header (eg. in org-mode or outline-minor-mode)"

 :use-up-to-parent (lambda () (ignore-errors (outline-up-heading 1)))
 :use-down-to-first-child #'cpo-outline-down-to-first-child
 :use-next-sibling 'cpo-outline-forward-half-or-full-sibling
 :use-previous-sibling 'cpo-outline-backward-half-or-full-sibling
 :use-left-finalizer-for-tree-with-no-end-delimiter (lambda ()
                                                      (if (cpo-outline--outline-at-anchor-point-p)
                                                          (line-beginning-position)
                                                        (outline-previous-heading)))
 :use-right-finalizer-for-tree-with-no-end-delimiter (lambda ()
                                                       (outline-next-heading)
                                                       (unless (and (eobp) (not (bolp)))
                                                         (beginning-of-line))
                                                       (point))
 )

(defun cpo-outline-raise ()
  "Replace the parent heading's subtree with the current heading's subtree.
The child heading is promoted to take the parent's place, discarding
the parent and any siblings.  The heading level is adjusted so the
child takes the parent's heading level."
  (interactive)
  (require 'org)
  (let* ((child-bounds (cpo-outline-tree-bounds (point)))
         (parent-bounds (and child-bounds
                             (save-mark-and-excursion
                               (goto-char (car child-bounds))
                               (and (cpo-tree-walk--motion-moved
                                     (lambda () (ignore-errors (outline-up-heading 1))))
                                    (cpo-outline-tree-bounds (point)))))))
    (when (and child-bounds parent-bounds
               (cpo-tree-walk--region-strictly-less child-bounds parent-bounds))
      (let* ((parent-level (save-mark-and-excursion
                             (goto-char (car parent-bounds))
                             (org-current-level)))
             (child-level (save-mark-and-excursion
                            (goto-char (car child-bounds))
                            (org-current-level)))
             (level-diff (- child-level parent-level))
             (child-text (buffer-substring-no-properties (car child-bounds)
                                                          (cdr child-bounds))))
        ;; Adjust heading levels: reduce each heading's level by level-diff
        (let ((adjusted-text
               (with-temp-buffer
                 (insert child-text)
                 (goto-char (point-min))
                 (while (re-search-forward "^\\(\\*+\\)" nil t)
                   (let* ((stars (match-string 1))
                          (new-level (max 1 (- (length stars) level-diff)))
                          (new-stars (make-string new-level ?*)))
                     (replace-match new-stars nil nil nil 1)))
                 (buffer-string))))
          (atomic-change-group
            (delete-region (car parent-bounds) (cdr parent-bounds))
            (goto-char (car parent-bounds))
            (insert adjusted-text))
          (goto-char (car parent-bounds)))))))

;;; Forward/backward to beginning/end of outline tree nodes.
;;; These movements respect tree structure -- they only move between
;;; sibling nodes (headings at the same level under the same parent),
;;; not to parent or uncle nodes.
;;; An outline tree node consists of a heading line plus its body text
;;; (everything up to the next heading of the same or higher level).
;;; The "beginning" of a node is the start of its heading line.
;;; The "end" of a node is the last position before the next heading
;;; (or end of buffer).

(defun cpo-outline--at-heading-beginning-p ()
  "Return non-nil if point is at the beginning of an outline heading line."
  (and (bolp)
       (looking-at-p outline-regexp)))

(defun cpo-outline--tree-forward-beginning-single ()
  "Move forward to the beginning of the next sibling outline tree node.
Only moves to a sibling at the same outline level under the same
parent, not to child or uncle nodes.  Return non-nil if moved."
  (let ((start (point)))
    (ignore-errors (outline-back-to-heading t))
    (if (ignore-errors (outline-forward-same-level 1) t)
        (not (= start (point)))
      (goto-char start)
      nil)))

(defun cpo-outline--tree-backward-beginning-single ()
  "Move backward to the beginning of the current or previous sibling outline tree node.
If point is inside a heading's content (on the heading line past its
beginning, or in the body text), move to the beginning of that heading.
If already at the beginning of a heading, move to the beginning of the
previous sibling heading (same level under same parent).
Return non-nil if moved."
  (let ((start (point)))
    (cond
     ;; At the beginning of a heading -- go to previous sibling heading.
     ((cpo-outline--at-heading-beginning-p)
      (if (ignore-errors (outline-backward-same-level 1) t)
          (not (= start (point)))
        (goto-char start)
        nil))
     ;; Somewhere inside a node -- go to its heading beginning.
     (t
      (ignore-errors (outline-back-to-heading t))
      (not (= start (point)))))))

(defun cpo-outline--tree-forward-end-single ()
  "Move forward to the end of the current or next sibling outline tree node.
If point is inside a node and not at its end, move to the end of that
node.  If already at the end, move to the end of the next sibling node.
The end of a node is the position just before the next heading line
\(or end of buffer).  Return non-nil if moved."
  (let ((start (point))
        (bounds (cpo-outline-tree-bounds (point))))
    (if (and bounds (< (point) (cdr bounds)))
        ;; Inside a node and not at its end -- go to end.
        (progn
          (goto-char (cdr bounds))
          (not (= start (point))))
      ;; At or past the end -- move to next sibling node's end.
      (when (cpo-outline--tree-forward-beginning-single)
        (let ((next-bounds (cpo-outline-tree-bounds (point))))
          (if next-bounds
              (progn
                (goto-char (cdr next-bounds))
                (not (= start (point))))
            (goto-char start)
            nil))))))

(defun cpo-outline--tree-backward-end-single ()
  "Move backward to the end of the previous sibling outline tree node.
Return non-nil if moved.
When the immediate previous sibling\'s subtree end equals the current
position (which happens when point is at the beginning of a heading,
since that is also the end boundary of the prior sibling), keep
stepping backward through siblings until a sibling with a strictly
earlier end is found, or no more siblings remain."
  (let ((start (point))
        (result-point nil))
    ;; Go to the beginning of the current heading first, then walk backward
    ;; through siblings looking for one whose subtree end is before start.
    (save-excursion
      (ignore-errors (outline-back-to-heading t))
      (let ((keep-going t))
        (while (and keep-going
                    (ignore-errors (outline-backward-same-level 1) t)
                    (not (= (point) start)))
          (let ((bounds (cpo-outline-tree-bounds (point))))
            (if bounds
                (let ((end-pos (cdr bounds)))
                  (if (< end-pos start)
                      ;; Found a sibling whose end is strictly before start.
                      (progn
                        (setq result-point end-pos)
                        (setq keep-going nil))
                    ;; This sibling\'s end is at or after start -- keep going.
                    nil))
              (setq keep-going nil))))))
    (if result-point
        (progn (goto-char result-point) t)
      nil)))

(defun cpo-outline-tree-forward-beginning (&optional count)
  "Move forward to the beginning of the next sibling outline tree node, COUNT times.
Only moves to siblings (same level under same parent), respecting
tree structure.  If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (if fwd
          (cpo-outline--tree-forward-beginning-single)
        (cpo-outline--tree-backward-beginning-single)))))

(defun cpo-outline-tree-backward-beginning (&optional count)
  "Move backward to the beginning of the current or previous sibling outline tree node, COUNT times.
If point is inside a heading's content, move to the beginning of that
heading.  If already at the beginning, move to the previous sibling.
Only moves to siblings (same level under same parent), respecting
tree structure.  If COUNT is negative, move forward."
  (interactive "p")
  (cpo-outline-tree-forward-beginning (- (or count 1))))

(defun cpo-outline-tree-forward-end (&optional count)
  "Move forward to the end of a sibling outline tree node, COUNT times.
Only moves to siblings (same level under same parent), respecting
tree structure.  If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (_i count)
      (if fwd
          (cpo-outline--tree-forward-end-single)
        (cpo-outline--tree-backward-end-single)))))

(defun cpo-outline-tree-backward-end (&optional count)
  "Move backward to the end of the previous sibling outline tree node, COUNT times.
Only moves to siblings (same level under same parent), respecting
tree structure.  If COUNT is negative, move forward."
  (interactive "p")
  (cpo-outline-tree-forward-end (- (or count 1))))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'outline-forward-same-level 'outline-backward-same-level)
  (repeatable-motion-define-pair 'cpo-outline-forward-full-sibling 'cpo-outline-backward-full-sibling)
  (repeatable-motion-define-pair 'cpo-outline-down-to-first-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-descendant nil)
  (repeatable-motion-define-pair 'cpo-outline-inorder-traversal-forward 'cpo-outline-inorder-traversal-backward)
  (repeatable-motion-define-pair 'cpo-outline-forward-half-or-full-sibling 'cpo-outline-backward-half-or-full-sibling)
  (repeatable-motion-define 'cpo-outline-expand-region nil)
  (repeatable-motion-define 'cpo-outline-expand-region/children-region nil)
  (repeatable-motion-define-pair 'cpo-outline-tree-forward-beginning 'cpo-outline-tree-backward-beginning)
  (repeatable-motion-define-pair 'cpo-outline-tree-forward-end 'cpo-outline-tree-backward-end)
  )



(provide 'cpo-outline)
