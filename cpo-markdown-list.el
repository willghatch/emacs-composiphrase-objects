;;; cpo-markdown-list.el --- Markdown list tree object -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cpo-tree-walk)

(cl-defstruct cpo-markdown-list--item
  parent
  children
  start
  anchor
  end
  indent
  content-column
  marker
  marker-type
  number
  delimiter)

(defconst cpo-markdown-list--item-regexp
  "^\\([ \t]*\\)\\(?:\\([-+*]\\)\\|\\([0-9]+\\)\\([.)]\\)\\)\\(?:[ \t]+\\|$\\)")

(defconst cpo-markdown-list--default-unordered-marker "-")
(defconst cpo-markdown-list--default-ordered-delimiter ".")

(defun cpo-markdown-list--blank-line-p ()
  (looking-at-p "^[ \t]*$"))

(defun cpo-markdown-list--line-item-data ()
  "Return item metadata for the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at cpo-markdown-list--item-regexp)
      (let* ((start (line-beginning-position))
             (anchor (progn
                       (back-to-indentation)
                       (point)))
             (indent (current-indentation))
             (marker-type (if (match-string 2) 'bullet 'number))
             (marker (or (match-string 2)
                         (concat (match-string 3)
                                 (match-string 4))))
             (number (and (match-string 3)
                          (string-to-number (match-string 3))))
             (delimiter (match-string 4))
             (content-column (progn
                               (goto-char (match-end 0))
                               (current-column))))
        (list :start start
              :anchor anchor
              :indent indent
              :marker marker
              :marker-type marker-type
              :number number
              :delimiter delimiter
              :content-column content-column)))))

(defun cpo-markdown-list--append-child (parent item)
  (setf (cpo-markdown-list--item-children parent)
        (nconc (cpo-markdown-list--item-children parent)
               (list item))))

(defun cpo-markdown-list--parse-buffer ()
  "Parse the current buffer into a Markdown list forest."
  (save-excursion
    (goto-char (point-min))
    (let ((roots nil)
          (stack nil)
          (line-start nil)
          (line-indent nil)
          (line-data nil))
      (while (< (point) (point-max))
        (setq line-start (line-beginning-position))
        (unless (cpo-markdown-list--blank-line-p)
          (setq line-indent (current-indentation))
          (while (and stack
                      (< line-indent
                         (cpo-markdown-list--item-content-column
                          (car stack))))
            (setf (cpo-markdown-list--item-end (car stack))
                  line-start)
            (pop stack))
          (setq line-data (cpo-markdown-list--line-item-data))
          (when line-data
            (let* ((parent (car stack))
                   (item (make-cpo-markdown-list--item
                          :parent parent
                          :children nil
                          :start (plist-get line-data :start)
                          :anchor (plist-get line-data :anchor)
                          :end nil
                          :indent (plist-get line-data :indent)
                          :content-column (plist-get line-data :content-column)
                          :marker (plist-get line-data :marker)
                          :marker-type (plist-get line-data :marker-type)
                          :number (plist-get line-data :number)
                          :delimiter (plist-get line-data :delimiter))))
              (if parent
                  (cpo-markdown-list--append-child parent item)
                (setq roots (nconc roots (list item))))
              (push item stack))))
        (forward-line 1))
      (while stack
        (setf (cpo-markdown-list--item-end (car stack))
              (point-max))
        (pop stack))
      roots)))

(defun cpo-markdown-list--search-position (&optional position)
  (let ((position (or position (point))))
    (if (and (= position (point-max))
             (< (point-min) position))
        (1- position)
      position)))

(defun cpo-markdown-list--find-containing-item (items position)
  (let ((position (cpo-markdown-list--search-position position))
        (found nil))
    (while (and items (not found))
      (let ((item (car items)))
        (cond
         ((< position (cpo-markdown-list--item-start item))
          (setq items nil))
         ((< position (cpo-markdown-list--item-end item))
          (setq found (or (cpo-markdown-list--find-containing-item
                           (cpo-markdown-list--item-children item)
                           position)
                          item)))
         (t
          (setq items (cdr items))))))
    found))

(defun cpo-markdown-list--current-item (&optional position)
  (let ((roots (cpo-markdown-list--parse-buffer)))
    (cons roots
          (cpo-markdown-list--find-containing-item roots position))))

(defun cpo-markdown-list--siblings (item roots)
  (let ((parent (cpo-markdown-list--item-parent item)))
    (if parent
        (cpo-markdown-list--item-children parent)
      roots)))

(defun cpo-markdown-list--previous-sibling (item roots)
  (let ((siblings (cpo-markdown-list--siblings item roots))
        (previous nil))
    (while (and siblings
                (not (eq (car siblings) item)))
      (setq previous (car siblings))
      (setq siblings (cdr siblings)))
    previous))

(defun cpo-markdown-list--next-sibling (item roots)
  (cadr (member item
                (cpo-markdown-list--siblings item roots))))

(defun cpo-markdown-list--last-child (item)
  (car (last (cpo-markdown-list--item-children item))))

(defun cpo-markdown-list--default-child-indent (item &optional marker-type)
  (let ((first-child (car (cpo-markdown-list--item-children item)))
        (marker-type (or marker-type
                         (cpo-markdown-list--item-marker-type item))))
    (if first-child
        (cpo-markdown-list--item-indent first-child)
      (if (eq marker-type 'number)
          (max (cpo-markdown-list--item-content-column item)
               (+ (cpo-markdown-list--item-indent item) 3))
        (cpo-markdown-list--item-content-column item)))))

(defun cpo-markdown-list--move-next-sibling ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (next (and item
                    (cpo-markdown-list--next-sibling item roots))))
    (when next
      (goto-char (cpo-markdown-list--item-start next)))))

(defun cpo-markdown-list--move-previous-sibling ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (previous (and item
                        (cpo-markdown-list--previous-sibling item roots))))
    (when previous
      (goto-char (cpo-markdown-list--item-start previous)))))

(defun cpo-markdown-list--forward-beginning-single ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (start (point)))
    (when item
      (let ((next (cpo-markdown-list--next-sibling item roots)))
        (when next
          (goto-char (cpo-markdown-list--item-start next))))
      (not (= start (point))))))

(defun cpo-markdown-list--backward-beginning-single ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (start (point)))
    (when item
      (if (/= (point)
              (cpo-markdown-list--item-start item))
          (goto-char (cpo-markdown-list--item-start item))
        (let ((previous (cpo-markdown-list--previous-sibling item roots)))
          (when previous
            (goto-char (cpo-markdown-list--item-start previous)))))
      (not (= start (point))))))

(defun cpo-markdown-list--forward-end-single ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (start (point)))
    (when item
      (if (< (point)
             (cpo-markdown-list--item-end item))
          (goto-char (cpo-markdown-list--item-end item))
        (let ((next (cpo-markdown-list--next-sibling item roots)))
          (when next
            (goto-char (cpo-markdown-list--item-end next)))))
      (not (= start (point))))))

(defun cpo-markdown-list--backward-end-single ()
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (start (point))
         (previous (and item
                        (cpo-markdown-list--previous-sibling item roots)))
         (result nil))
    (while (and previous
                (not result))
      (if (< (cpo-markdown-list--item-end previous) start)
          (setq result (cpo-markdown-list--item-end previous))
        (setq previous
              (cpo-markdown-list--previous-sibling previous roots))))
    (when result
      (goto-char result)
      t)))

(defun cpo-markdown-list-forward-beginning (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (forward-p (<= 0 count))
         (count (abs count)))
    (dotimes (_ count)
      (if forward-p
          (cpo-markdown-list--forward-beginning-single)
        (cpo-markdown-list--backward-beginning-single)))))

(defun cpo-markdown-list-backward-beginning (&optional count)
  (interactive "p")
  (cpo-markdown-list-forward-beginning (- (or count 1))))

(defun cpo-markdown-list-forward-end (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (forward-p (<= 0 count))
         (count (abs count)))
    (dotimes (_ count)
      (if forward-p
          (cpo-markdown-list--forward-end-single)
        (cpo-markdown-list--backward-end-single)))))

(defun cpo-markdown-list-backward-end (&optional count)
  (interactive "p")
  (cpo-markdown-list-forward-end (- (or count 1))))

(defun cpo-markdown-list-up-to-parent (&optional count)
  (interactive "p")
  (let ((count (abs (or count 1)))
        (moved nil))
    (dotimes (_ count)
      (let* ((context (cpo-markdown-list--current-item))
             (item (cdr context))
             (parent (and item
                          (cpo-markdown-list--item-parent item))))
        (if parent
            (progn
              (goto-char (cpo-markdown-list--item-start parent))
              (setq moved t))
          (setq count 0))))
    moved))

(defun cpo-markdown-list-down-to-first-child (&optional count)
  (interactive "p")
  (let ((count (abs (or count 1)))
        (moved nil))
    (dotimes (_ count)
      (let* ((context (cpo-markdown-list--current-item))
             (item (cdr context))
             (child (and item
                         (car (cpo-markdown-list--item-children item)))))
        (if child
            (progn
              (goto-char (cpo-markdown-list--item-start child))
              (setq moved t))
          (setq count 0))))
    moved))

(defun cpo-markdown-list-bounds (&optional anchor-point)
  (let* ((context (cpo-markdown-list--current-item anchor-point))
         (item (cdr context)))
    (when item
      (cons (cpo-markdown-list--item-start item)
            (cpo-markdown-list--item-end item)))))

(defun cpo-markdown-list-children-bounds (&optional anchor-point)
  (let* ((context (cpo-markdown-list--current-item anchor-point))
         (item (cdr context))
         (children (and item
                        (cpo-markdown-list--item-children item))))
    (when children
      (cons (cpo-markdown-list--item-start (car children))
            (cpo-markdown-list--item-end
             (cpo-markdown-list--last-child item))))))

(cpo-tree-walk--define-expand-region-funcs
 :def-expand-region cpo-markdown-list-expand-region
 :bounds-func 'cpo-markdown-list-bounds
 :children-bounds-func 'cpo-markdown-list-children-bounds
 :up-func (lambda () (cpo-markdown-list-up-to-parent 1)))

(cpo-tree-walk-define-operations
 :def-transpose-sibling-forward cpo-markdown-list-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-markdown-list-transpose-sibling-backward
 :use-object-name "markdown list item"
 :use-next-sibling #'cpo-markdown-list--move-next-sibling
 :use-previous-sibling #'cpo-markdown-list--move-previous-sibling
 :use-bounds 'cpo-markdown-list-bounds)

(defun cpo-markdown-list--marker-string (marker-type &optional marker number delimiter)
  (if (eq marker-type 'number)
      (format "%d%s"
              (or number 1)
              (or delimiter
                  cpo-markdown-list--default-ordered-delimiter))
    (or marker
        cpo-markdown-list--default-unordered-marker)))

(defun cpo-markdown-list--insertion-spec (prev next fallback explicit-type)
  (let* ((marker-type (or explicit-type
                          (and prev
                               (cpo-markdown-list--item-marker-type prev))
                          (and next
                               (cpo-markdown-list--item-marker-type next))
                          (and fallback
                               (cpo-markdown-list--item-marker-type fallback))
                          'bullet))
         (source (cond
                  ((and prev
                        (eq marker-type
                            (cpo-markdown-list--item-marker-type prev)))
                   prev)
                  ((and next
                        (eq marker-type
                            (cpo-markdown-list--item-marker-type next)))
                   next)
                  ((and fallback
                        (eq marker-type
                            (cpo-markdown-list--item-marker-type fallback)))
                   fallback)))
         (marker (and (eq marker-type 'bullet)
                      (if source
                          (cpo-markdown-list--item-marker source)
                        cpo-markdown-list--default-unordered-marker)))
         (delimiter (and (eq marker-type 'number)
                         (or (and source
                                  (cpo-markdown-list--item-delimiter source))
                             cpo-markdown-list--default-ordered-delimiter)))
         (number (and (eq marker-type 'number)
                      (cond
                       ((and prev
                             (eq 'number
                                 (cpo-markdown-list--item-marker-type prev)))
                        (1+ (cpo-markdown-list--item-number prev)))
                       ((and (not prev)
                             next
                             (eq 'number
                                 (cpo-markdown-list--item-marker-type next)))
                        (cpo-markdown-list--item-number next))
                       ((and fallback
                             (eq 'number
                                 (cpo-markdown-list--item-marker-type fallback)))
                        (cpo-markdown-list--item-number fallback))
                       (t 1)))))
    (list :marker-type marker-type
          :marker marker
          :delimiter delimiter
          :number number)))

(defun cpo-markdown-list--insert-item-line (position indent spec)
  (let ((marker-text (cpo-markdown-list--marker-string
                      (plist-get spec :marker-type)
                      (plist-get spec :marker)
                      (plist-get spec :number)
                      (plist-get spec :delimiter))))
    (goto-char position)
    (insert (make-string indent ?\s)
            marker-text
            " \n")
    (+ position indent (length marker-text) 1)))

(defun cpo-markdown-list--child-slot (item count)
  (let* ((children (cpo-markdown-list--item-children item))
         (count (max 0 (or count 0)))
         (length (length children)))
    (if (zerop length)
        (list :position (cpo-markdown-list--item-end item)
              :prev nil
              :next nil
              :fallback item)
      (let ((position (if (< count length)
                          (cpo-markdown-list--item-start
                           (nth count children))
                        (cpo-markdown-list--item-end
                         (cpo-markdown-list--last-child item)))))
        (list :position position
              :prev (if (zerop count)
                        nil
                      (nth (min (1- count) (1- length))
                           children))
              :next (nth count children)
              :fallback (car children))))))

(defun cpo-markdown-list--open-relative (position indent prev next fallback
                                                         &optional type)
  (let* ((spec (cpo-markdown-list--insertion-spec prev next fallback type))
         (new-point (cpo-markdown-list--insert-item-line
                     position indent spec)))
    (goto-char new-point)))

(cl-defun cpo-markdown-list-open (&key type &allow-other-keys)
  (interactive)
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context)))
    (when item
      (cpo-markdown-list--open-relative
       (cpo-markdown-list--item-end item)
       (cpo-markdown-list--item-indent item)
       item
       (cpo-markdown-list--next-sibling item roots)
       item
       type))))

(defun cpo-markdown-list-open-forward ()
  (interactive)
  (cpo-markdown-list-open))

(defun cpo-markdown-list-open-backward ()
  (interactive)
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context)))
    (when item
      (cpo-markdown-list--open-relative
       (cpo-markdown-list--item-start item)
       (cpo-markdown-list--item-indent item)
       (cpo-markdown-list--previous-sibling item roots)
       item
       item))))

(cl-defun cpo-markdown-list-open-forward-down (&key count type &allow-other-keys)
  (interactive)
  (let* ((context (cpo-markdown-list--current-item))
         (item (cdr context)))
    (when item
      (let* ((slot (cpo-markdown-list--child-slot item count))
             (position (plist-get slot :position))
             (prev (plist-get slot :prev))
             (next (plist-get slot :next))
             (fallback (plist-get slot :fallback))
             (indent (cpo-markdown-list--default-child-indent
                      item
                      (or type
                          (and fallback
                               (cpo-markdown-list--item-marker-type fallback))))))
        (cpo-markdown-list--open-relative
         position indent prev next fallback type)))))

(cl-defun cpo-markdown-list-open-forward-up (&key count type &allow-other-keys)
  (interactive)
  (let* ((count (max 1 (or count 1)))
         (context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (target item))
    (when item
      (dotimes (_ count)
        (when (cpo-markdown-list--item-parent target)
          (setq target (cpo-markdown-list--item-parent target))))
      (cpo-markdown-list--open-relative
       (cpo-markdown-list--item-end target)
       (cpo-markdown-list--item-indent target)
       target
       (cpo-markdown-list--next-sibling target roots)
       target
       type))))

(defun cpo-markdown-list-forward-slurp ()
  (interactive)
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context))
         (next (and item
                    (cpo-markdown-list--next-sibling item roots))))
    (when next
      (indent-rigidly
       (cpo-markdown-list--item-start next)
       (cpo-markdown-list--item-end next)
       (- (cpo-markdown-list--default-child-indent
           item
           (cpo-markdown-list--item-marker-type next))
          (cpo-markdown-list--item-indent next)))
      t)))

(defun cpo-markdown-list-forward-barf ()
  (interactive)
  (let* ((context (cpo-markdown-list--current-item))
         (item (cdr context))
         (child (and item
                     (cpo-markdown-list--last-child item))))
    (when child
      (indent-rigidly
       (cpo-markdown-list--item-start child)
       (cpo-markdown-list--item-end child)
       (- (cpo-markdown-list--item-indent item)
          (cpo-markdown-list--item-indent child)))
      t)))

(defun cpo-markdown-list--replace-marker (item new-marker)
  (save-excursion
    (goto-char (cpo-markdown-list--item-anchor item))
    (delete-region (point)
                   (+ (point)
                      (length (cpo-markdown-list--item-marker item))))
    (insert new-marker)))

(defun cpo-markdown-list--renumber-items (items)
  (let ((number 1)
        (pairs nil))
    (dolist (item items)
      (push (cons item number) pairs)
      (setq number (1+ number)))
    (dolist (pair pairs)
      (let* ((item (car pair))
             (marker (cpo-markdown-list--marker-string
                      'number
                      nil
                      (cdr pair)
                      (or (cpo-markdown-list--item-delimiter item)
                          cpo-markdown-list--default-ordered-delimiter))))
        (cpo-markdown-list--replace-marker item marker)))))

(defun cpo-markdown-list-fix-numbering (&optional count)
  (interactive "p")
  (ignore count)
  (let* ((context (cpo-markdown-list--current-item))
         (roots (car context))
         (item (cdr context)))
    (when (and item
               (eq 'number
                   (cpo-markdown-list--item-marker-type item)))
      (cpo-markdown-list--renumber-items
       (cl-remove-if-not
        (lambda (sibling)
          (eq 'number
              (cpo-markdown-list--item-marker-type sibling)))
        (cpo-markdown-list--siblings item roots))))))

(provide 'cpo-markdown-list)
;;; cpo-markdown-list.el ends here
