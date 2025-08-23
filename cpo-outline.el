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
        (parent-level (save-mark-and-excursion
                        (and (ignore-errors (outline-up-heading 1))
                             (org-current-level))))
        (cur-level nil)
        (end-point nil))
    (and
     parent-level
     (save-mark-and-excursion
       (outline-back-to-heading)
       (while (and (cpo-tree-walk--motion-moved
                    (lambda () (ignore-errors (outline-next-heading))))
                   (setq cur-level (org-current-level))
                   (setq end-point (point))
                   (and cur-level (< start-level cur-level)))
         nil)
       t)
     (cond
      ((equal cur-level start-level)
       (when (not half-sibling-only)
         (goto-char end-point)))
      ((< parent-level cur-level start-level)
       (goto-char end-point))
      (t nil)))))

(defun cpo-outline--backward-half-or-full-sibling-single (&optional half-sibling-only)
  (let ((start-point (point))
        (start-level (org-current-level))
        (parent-level (save-mark-and-excursion
                        (ignore-errors (outline-up-heading 1))
                        (org-current-level)))
        (keep-going t)
        (min-level nil)
        (min-level-point nil))
    (and
     parent-level
     (save-mark-and-excursion
       (outline-back-to-heading)
       (while (and keep-going
                   (cpo-tree-walk--motion-moved
                    (lambda () (ignore-errors (outline-previous-heading)))))
         (let ((cur-level (org-current-level)))
           (message "cur-level: %s, cur-point: %s, min-level: %s" cur-level (point) min-level)
           (when (not min-level)
             (setq min-level cur-level)
             (setq min-level-point (point)))
           (when (and (< parent-level cur-level)
                      (<= cur-level min-level))
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

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'outline-forward-same-level 'outline-backward-same-level)
  (repeatable-motion-define-pair 'cpo-outline-down-to-first-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-descendant nil)
  (repeatable-motion-define-pair 'cpo-outline-inorder-traversal-forward 'cpo-outline-inorder-traversal-backward)
  (repeatable-motion-define-pair 'cpo-outline-forward-half-or-full-sibling 'cpo-outline-backward-half-or-full-sibling)
  (repeatable-motion-define 'cpo-outline-expand-region nil)
  (repeatable-motion-define 'cpo-outline-expand-region/children-region nil)
  )



(provide 'cpo-outline)
