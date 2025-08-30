(require 'cpo-text-object-stuff)

;; this also contains email object...

(defun cpo-text-object-stuff--forward-url-beginning ()
  (let ((start-point (point))
        (end-point nil)
        (regexp (regexp-opt thing-at-point-uri-schemes)))
    (save-mark-and-excursion
      (let ((success (re-search-forward regexp nil t)))
        (when (and success (equal start-point (match-beginning 0)))
          (progn (forward-char 1)
                 (setq success (re-search-forward regexp nil t))))
        (when success
          (setq end-point (match-beginning 0)))))
    (when end-point (goto-char end-point))))

(defun cpo-text-object-stuff--backward-url-beginning ()
  (let* ((regexp (regexp-opt thing-at-point-uri-schemes))
         (success (re-search-backward regexp nil t)))
    (when success
      (goto-char (match-beginning 0)))))

(defun cpo-forward-url-beginning (&optional count)
  "Move forward to the beginning of a URL as defined by 'thing-at-point-uri-schemes', COUNT times.
If COUNT is negative, move backward."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (dotimes (i count)
      (funcall (if fwd
                   'cpo-text-object-stuff--forward-url-beginning
                 'cpo-text-object-stuff--backward-url-beginning)))))

(defun cpo-backward-url-beginning (&optional count)
  "Like `cpo-forward-url-beginning' but backward."
  (interactive "p")
  (cpo-forward-url-beginning (- (or count 1))))

;; TODO - abstract these movements to the end of a URL to be functions for
;; moving to the end of any text object that has motion to a beginning but not
;; an end.
(defun cpo-backward-url-end (&optional count)
  "Move backward to the end of a URL, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-forward-url-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (end-point nil))
        (save-mark-and-excursion
          (let* ((start-point (point))
                 (moved (cpo-tree-walk--motion-moved 'cpo-backward-url-beginning))
                 (bounds (and moved (bounds-of-thing-at-point 'url)))
                 (was-in-url (and bounds (<= start-point (cdr bounds)))))
            (cond
             (was-in-url (setq moved
                               (cpo-tree-walk--motion-moved
                                (lambda () (dotimes (i count)
                                             (cpo-backward-url-beginning))))))
             (moved (dotimes (i (- count 1)) (cpo-backward-url-beginning)))
             (t nil))
            (when moved
              (let ((bounds (bounds-of-thing-at-point 'url)))
                (and bounds (setq end-point (cdr bounds)))))))
        (when end-point (goto-char end-point)))))))

(defun cpo-forward-url-end (&optional count)
  "Move forward to the end of a URL, COUNT times."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count)))
    (cond
     ((not fwd) (cpo-backward-url-end count))
     ((equal 0 count) nil)
     (t
      (let ((start-point (point))
            (started-in-url nil)
            (started-at-end-of-url nil)
            (end-point nil))
        (save-mark-and-excursion

          ;; Check the current bounds of the URL
          (let ((bounds (bounds-of-thing-at-point 'url)))
            ;; If we are already in a URL, go to its end
            (when (and bounds
                       (<= (car bounds) start-point (cdr bounds)))
              (if (equal start-point (cdr bounds))
                  (setq started-at-end-of-url t)
                (progn
                  (setq started-in-url t)
                  (goto-char (cdr bounds))))))

          ;; Because bounds-of-thing-at-point for URL doesn't always work unless
          ;; you are at the start, go backward to find the start of a url to
          ;; check more carefully whether we were in a url.
          (unless (or started-in-url started-at-end-of-url)
            (cpo-backward-url-beginning)
            (let ((bounds (bounds-of-thing-at-point 'url)))
              (when bounds
                (if (<= (car bounds) start-point (cdr bounds))
                    (progn
                      (goto-char (cdr bounds))
                      (if (equal start-point (cdr bounds))
                          (setq started-at-end-of-url t)
                        (setq started-in-url t)))
                  (goto-char start-point)))))

          (setq end-point (and started-in-url (point)))

          ;; Finally move forward by URL then move to its end.
          (and (cpo-tree-walk--motion-moved
                (lambda ()
                  (dotimes (i (if started-in-url
                                  (- count 1)
                                count))
                    (cpo-forward-url-beginning))))
               (let ((final-bounds (bounds-of-thing-at-point 'url)))
                 (when final-bounds
                   (setq end-point (cdr final-bounds))))))
        (when end-point
          (goto-char end-point)))))))


(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-forward-url-beginning 'cpo-backward-url-beginning)
  (repeatable-motion-define-pair 'cpo-forward-url-end 'cpo-backward-url-end))

;;(cpo-text-object-stuff--def-move-thing-with-bounds-but-no-motion url)
(defun cpo-text-object-stuff--bounds-of-url-at-point ()
  (bounds-of-thing-at-point 'url))
(cpo-text-object-stuff--def-expand-region-to-thing url)
(cpo-text-object-stuff--define-transpose-funcs
 cpo-transpose-url-backward
 cpo-transpose-url-forward
 'cpo-text-object-stuff--bounds-of-url-at-point
 'cpo-backward-url-beginning
 'cpo-forward-url-beginning
 )
;; TODO - add keyword args for transpose functions to use different movement func.  But that said, these are so inefficient, they would be extremely frustrating if the things aren't quite close.  These movements really can't be generic and any good.
;;(cpo-text-object-stuff--def-transpose-thing url)

(cpo-text-object-stuff--def-move-thing-with-bounds-but-no-motion email)
(cpo-text-object-stuff--def-expand-region-to-thing email)
;;(cpo-text-object-stuff--def-transpose-thing email)

(provide 'cpo-url-object)
