;;; test-cpo-git-gutter.el --- Tests for cpo-git-gutter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'cpo-git-gutter)
(require 'git-gutter)
(require 'test-cpo-helpers)

;;; Test infrastructure

(defvar test-cpo-gg--test-files-dir
  (expand-file-name "git-gutter-test-files"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name)))))
  "Directory containing test files for git-gutter tests.")

(defvar test-cpo-gg--file1
  (expand-file-name "lorem-ipsum-1" test-cpo-gg--test-files-dir))
(defvar test-cpo-gg--file2
  (expand-file-name "lorem-ipsum-2" test-cpo-gg--test-files-dir))

(defun test-cpo-gg--git-root ()
  "Return the git repository root for the test files directory."
  (let ((default-directory test-cpo-gg--test-files-dir))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun test-cpo-gg--restore-test-files ()
  "Restore test files to their committed state."
  (let ((default-directory (test-cpo-gg--git-root)))
    (process-file "git" nil nil nil "checkout" "--"
                  "test/git-gutter-test-files/lorem-ipsum-1"
                  "test/git-gutter-test-files/lorem-ipsum-2")))

(defun test-cpo-gg--compute-diffinfos-sync (file)
  "Synchronously compute git-gutter diffinfos for FILE.
Runs `git diff -U0' via process-file and parses the output."
  (let* ((default-directory (file-name-directory file))
         (filename (file-name-nondirectory file)))
    (with-temp-buffer
      (process-file "git" nil t nil
                    "--no-pager" "-c" "diff.autorefreshindex=0"
                    "diff" "--no-color" "--no-ext-diff" "--relative" "-U0"
                    "--" filename)
      (git-gutter:process-diff-output (current-buffer)))))

(defun test-cpo-gg--setup-buffer (file)
  "Open FILE in a buffer, compute diffinfos synchronously, and enable git-gutter state.
Returns the buffer.  Caller is responsible for killing it."
  (let ((buf (find-file-noselect file t)))
    (with-current-buffer buf
      (revert-buffer t t t)
      (make-local-variable 'git-gutter:diffinfos)
      (setq git-gutter-mode t)
      (setq git-gutter:diffinfos (test-cpo-gg--compute-diffinfos-sync file)))
    buf))

(defun test-cpo-gg--modify-file1 ()
  "Modify lorem-ipsum-1 to create 3 hunks:
Hunk 1: Add 2 new lines after line 2 (in paragraph 1).
Hunk 2: Delete line 7 (in paragraph 2).
Hunk 3: Modify line 11 (in paragraph 3)."
  (with-temp-file test-cpo-gg--file1
    (insert-file-contents test-cpo-gg--file1)
    ;; Hunk 3: Modify line 11 (do this first so line numbers stay stable
    ;; for subsequent edits below it).
    (goto-char (point-min))
    (forward-line 10)  ;; now at line 11
    (let ((start (point)))
      (forward-line 1)
      (delete-region start (point)))
    (insert "MODIFIED: Pellentesque habitant morbi tristique senectus.\n")
    ;; Hunk 2: Delete line 7 ("Eu fugiat nulla pariatur.")
    (goto-char (point-min))
    (forward-line 6)  ;; now at line 7
    (let ((start (point)))
      (forward-line 1)
      (delete-region start (point)))
    ;; Hunk 1: Add 2 new lines after line 2
    (goto-char (point-min))
    (forward-line 2)  ;; now at line 3
    (insert "ADDED LINE ONE: This is new content for testing.\n")
    (insert "ADDED LINE TWO: More new content for testing.\n")))

(defun test-cpo-gg--modify-file2 ()
  "Modify lorem-ipsum-2 to create 2 hunks:
Hunk 1: Add a new line after line 5 (between paragraphs 1 and 2).
Hunk 2: Modify lines 15-16 (in paragraph 4)."
  (with-temp-file test-cpo-gg--file2
    (insert-file-contents test-cpo-gg--file2)
    ;; Hunk 2: Modify lines 15-16
    (goto-char (point-min))
    (forward-line 14)  ;; now at line 15
    (let ((start (point)))
      (forward-line 2)
      (delete-region start (point)))
    (insert "MODIFIED: Vestibulum ante ipsum primis in faucibus.\n")
    (insert "MODIFIED: Mauris sit amet massa vitae tortor.\n")
    ;; Hunk 1: Add new line after line 5
    (goto-char (point-min))
    (forward-line 5)  ;; after the blank line between paragraphs 1 and 2
    (insert "INSERTED: New line between paragraphs.\n")))

(defmacro test-cpo-gg--with-setup (&rest body)
  "Set up modified test files, open buffers, execute BODY, then clean up.
Binds `buf1' and `buf2' to the file buffers."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (test-cpo-gg--modify-file1)
         (test-cpo-gg--modify-file2)
         (let ((buf1 (test-cpo-gg--setup-buffer test-cpo-gg--file1))
               (buf2 (test-cpo-gg--setup-buffer test-cpo-gg--file2)))
           (unwind-protect
               (progn ,@body)
             (when (buffer-live-p buf1) (kill-buffer buf1))
             (when (buffer-live-p buf2) (kill-buffer buf2)))))
     (test-cpo-gg--restore-test-files)))

;;; Tests

;; Verify setup: diffinfos are populated
(ert-deftest test-cpo-gg-setup-produces-diffinfos ()
  "Sanity check: modifying test files produces diffinfos."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (should git-gutter-mode)
      (should git-gutter:diffinfos)
      (should (= (length git-gutter:diffinfos) 3)))
    (with-current-buffer buf2
      (should git-gutter-mode)
      (should git-gutter:diffinfos)
      (should (= (length git-gutter:diffinfos) 2)))))

;;; forward-beginning tests

(ert-deftest test-cpo-gg-forward-beginning-from-start ()
  "Forward beginning from line 1 should go to first hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-min))
      (cpo-git-gutter-hunk-forward-beginning)
      (let ((hunk1 (car git-gutter:diffinfos)))
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk1)))
        (should/looking-at "ADDED LINE ONE")))))

(ert-deftest test-cpo-gg-forward-beginning-to-second-hunk ()
  "Forward beginning from first hunk should go to second hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (cpo-git-gutter-hunk-forward-beginning)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk2)))
        (should/looking-at "Duis aute irure")))))

(ert-deftest test-cpo-gg-forward-beginning-with-count ()
  "Forward beginning with count 2 should skip to the second hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-min))
      (cpo-git-gutter-hunk-forward-beginning :count 2)
      (let ((hunk2 (nth 1 git-gutter:diffinfos)))
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk2)))
        (should/looking-at "Duis aute irure")))))

(ert-deftest test-cpo-gg-forward-beginning-at-last-hunk ()
  "Forward beginning from last hunk should not move."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let* ((last-hunk (car (last git-gutter:diffinfos)))
             (start-pos (cpo-git-gutter--hunk-start-pos last-hunk)))
        (goto-char start-pos)
        (cpo-git-gutter-hunk-forward-beginning)
        (should (= (point) start-pos))
        (should/looking-at "MODIFIED: Pellentesque")))))

(ert-deftest test-cpo-gg-forward-beginning-from-middle-of-hunk ()
  "Forward beginning from middle of a hunk should go to the next hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        ;; Hunk 0 spans lines 3-4, so go to line 4 (middle of hunk).
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (forward-line 1)
        (should/looking-at "ADDED LINE TWO")
        (cpo-git-gutter-hunk-forward-beginning)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk2)))
        (should/looking-at "Duis aute irure")))))

;;; backward-beginning tests

(ert-deftest test-cpo-gg-backward-beginning-from-second-hunk ()
  "Backward beginning from second hunk start should go to first hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk2))
        (cpo-git-gutter-hunk-backward-beginning)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk1)))
        (should/looking-at "ADDED LINE ONE")))))

(ert-deftest test-cpo-gg-backward-beginning-from-middle-of-hunk ()
  "Backward beginning from middle of a hunk should go to start of that hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos)))
        ;; Hunk 0 spans lines 3-4, so go to line 4.
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (forward-line 1)
        (should/looking-at "ADDED LINE TWO")
        (cpo-git-gutter-hunk-backward-beginning)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk1)))
        (should/looking-at "ADDED LINE ONE")))))

(ert-deftest test-cpo-gg-backward-beginning-at-first-hunk ()
  "Backward beginning from first hunk start should not move."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let* ((hunk1 (car git-gutter:diffinfos))
             (start-pos (cpo-git-gutter--hunk-start-pos hunk1)))
        (goto-char start-pos)
        (cpo-git-gutter-hunk-backward-beginning)
        (should (= (point) start-pos))
        (should/looking-at "ADDED LINE ONE")))))

;;; forward-end tests

(ert-deftest test-cpo-gg-forward-end-from-inside-hunk ()
  "Forward end from inside a hunk should go to end of that hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (should/looking-at "ADDED LINE ONE")
        (cpo-git-gutter-hunk-forward-end)
        (should (= (point)
                   (cpo-git-gutter--hunk-end-pos hunk1)))
        (should/looking-at "Ut enim ad minim")))))

(ert-deftest test-cpo-gg-forward-end-from-outside-hunk ()
  "Forward end from before all hunks should go to end of first hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-min))
      ;; point-min is line 1, first hunk starts at line 3; point is before it.
      (let ((hunk1 (car git-gutter:diffinfos)))
        (cpo-git-gutter-hunk-forward-end)
        (should (= (point)
                   (cpo-git-gutter--hunk-end-pos hunk1)))
        (should/looking-at "Ut enim ad minim")))))

(ert-deftest test-cpo-gg-forward-end-at-end-of-hunk ()
  "Forward end from end of first hunk should go to end of second hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-end-pos hunk1))
        (cpo-git-gutter-hunk-forward-end)
        (should (= (point)
                   (cpo-git-gutter--hunk-end-pos hunk2)))
        ;; Hunk 2 is a deletion hunk, so its end is at the start of
        ;; the line (same as start-pos, empty region).
        (should/looking-at "Duis aute irure")))))

(ert-deftest test-cpo-gg-forward-end-at-last-hunk ()
  "Forward end from end of last hunk should not move."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let* ((last-hunk (car (last git-gutter:diffinfos)))
             (end-pos (cpo-git-gutter--hunk-end-pos last-hunk)))
        (goto-char end-pos)
        (cpo-git-gutter-hunk-forward-end)
        (should (= (point) end-pos))
        (should/looking-at "Ac turpis egestas")))))

;;; backward-end tests

(ert-deftest test-cpo-gg-backward-end-from-after-second-hunk ()
  "Backward end from third hunk start should go to end of second hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk2 (nth 1 git-gutter:diffinfos))
            (hunk3 (nth 2 git-gutter:diffinfos)))
        ;; Go to start of third hunk (which is after end of second).
        (goto-char (cpo-git-gutter--hunk-start-pos hunk3))
        (should/looking-at "MODIFIED: Pellentesque")
        (cpo-git-gutter-hunk-backward-end)
        (should (= (point)
                   (cpo-git-gutter--hunk-end-pos hunk2)))
        ;; Hunk 2 is a deletion hunk, so its end is at the start of
        ;; the line (same as start-pos, empty region).
        (should/looking-at "Duis aute irure")))))

(ert-deftest test-cpo-gg-backward-end-from-between-hunks ()
  "Backward end from between first and second hunk should go to end of first."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let* ((hunk1 (nth 0 git-gutter:diffinfos))
             (hunk1-end-line (git-gutter-hunk-end-line hunk1)))
        ;; Hunk 0 ends at line 4; its end-pos is at the beginning of
        ;; line 5 (after the trailing newline).  Go to line 6, which
        ;; is strictly past the hunk end.
        (goto-char (point-min))
        (forward-line (1+ hunk1-end-line))  ;; two lines after hunk1 last line
        (should/looking-at "Nisi ut aliquip")
        (cpo-git-gutter-hunk-backward-end)
        (should (= (point)
                   (cpo-git-gutter--hunk-end-pos hunk1)))
        (should/looking-at "Ut enim ad minim")))))

;;; get-bounds tests

(ert-deftest test-cpo-gg-get-bounds-in-hunk ()
  "get-bounds should return (start . end) when point is in a hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (car git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (let ((bounds (cpo-git-gutter-hunk-get-bounds)))
          (should bounds)
          (should (= (car bounds) (cpo-git-gutter--hunk-start-pos hunk1)))
          (should (= (cdr bounds) (cpo-git-gutter--hunk-end-pos hunk1))))))))

(ert-deftest test-cpo-gg-get-bounds-outside-hunk ()
  "get-bounds should return nil when point is not in a hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-min))
      ;; Line 1 should not be in a hunk (our edits start at line 3).
      (should-not (cpo-git-gutter-hunk-get-bounds)))))

(ert-deftest test-cpo-gg-get-bounds-at-hunk-end ()
  "get-bounds should return bounds when point is at the end line of a hunk."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (car git-gutter:diffinfos)))
        ;; Go to the last line of the hunk.
        (goto-char (point-min))
        (forward-line (1- (git-gutter-hunk-end-line hunk1)))
        (should/looking-at "ADDED LINE TWO")
        (let ((bounds (cpo-git-gutter-hunk-get-bounds)))
          (should bounds)
          (should (= (car bounds) (cpo-git-gutter--hunk-start-pos hunk1)))
          (should (= (cdr bounds) (cpo-git-gutter--hunk-end-pos hunk1))))))))

;;; expand-region tests

(ert-deftest test-cpo-gg-expand-region-basic ()
  "expand-region with no active region should expand to hunk bounds."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let ((hunk1 (car git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should result)
          (should (region-active-p))
          (should (= (region-beginning) (cpo-git-gutter--hunk-start-pos hunk1)))
          (should (= (region-end) (cpo-git-gutter--hunk-end-pos hunk1)))
          ;; Default :position is nil, so point at beginning.
          (should/looking-at "ADDED LINE ONE")
          (should/mark-looking-at "Ut enim ad minim"))))))

(ert-deftest test-cpo-gg-expand-region-position-end ()
  "expand-region with :position 'end should put point at end."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let ((hunk1 (car git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (let ((result (cpo-git-gutter-hunk-expand-region :position 'end)))
          (should result)
          (should (= (point) (cpo-git-gutter--hunk-end-pos hunk1)))
          (should/looking-at "Ut enim ad minim")
          (should (= (mark) (cpo-git-gutter--hunk-start-pos hunk1)))
          (should/mark-looking-at "ADDED LINE ONE"))))))

(ert-deftest test-cpo-gg-expand-region-already-past-one-end ()
  "expand-region when region already extends past one hunk end should expand the other.
When :position is nil and only one end grows, point goes to the end that moved."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1))
             ;; Region starts before hunk (already past the beginning)
             ;; but ends inside the hunk (not yet at hunk end).
             (before-hunk (- hunk-beg 5))
             (inside-hunk (+ hunk-beg 10)))
        ;; Set up active region: mark at before-hunk, point at inside-hunk.
        (goto-char before-hunk)
        (set-mark before-hunk)
        (goto-char inside-hunk)
        (activate-mark)
        (should (region-active-p))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should result)
          ;; Beginning should keep original (before hunk bounds).
          (should (= (region-beginning) before-hunk))
          ;; End should have expanded to hunk end.
          (should (= (region-end) hunk-end))
          ;; Point should be at the end that moved (the end).
          (should (= (point) hunk-end))
          (should/looking-at "Ut enim ad minim"))))))

(ert-deftest test-cpo-gg-expand-region-past-end-expand-beginning ()
  "expand-region when region already extends past the end should expand the beginning.
When :position is nil and the end was already strictly beyond the hunk,
point goes to the beginning (the end that moved)."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1))
             ;; Region starts inside hunk but ends past hunk end.
             (inside-hunk (+ hunk-beg 10))
             (past-end (+ hunk-end 5)))
        ;; Set up active region: mark at past-end, point inside hunk.
        ;; Point must be inside the hunk for get-bounds to find it.
        (goto-char past-end)
        (set-mark past-end)
        (goto-char inside-hunk)
        (activate-mark)
        (should (region-active-p))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should result)
          ;; Beginning should have expanded to hunk start.
          (should (= (region-beginning) hunk-beg))
          ;; End should keep original (past hunk bounds).
          (should (= (region-end) past-end))
          ;; Point should be at the end that moved (the beginning).
          (should (= (point) hunk-beg))
          (should/looking-at "ADDED LINE ONE"))))))

(ert-deftest test-cpo-gg-expand-region-already-covers-hunk ()
  "expand-region when region already covers entire hunk and extends beyond both ends should return nil.
Point must be at a region endpoint, which is outside the hunk, so
get-bounds returns nil and no expansion happens."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1))
             ;; Region extends beyond both ends of the hunk.
             (before-hunk (- hunk-beg 5))
             (past-end (+ hunk-end 5)))
        (goto-char before-hunk)
        (set-mark before-hunk)
        (goto-char past-end)
        (activate-mark)
        (should (region-active-p))
        (let ((orig-point (point))
              (result (cpo-git-gutter-hunk-expand-region)))
          (should-not result)
          ;; Point should not have moved.
          (should (= (point) orig-point)))))))

(ert-deftest test-cpo-gg-expand-region-one-end-at-boundary-other-inside ()
  "expand-region when one end is exactly at hunk boundary and the other is inside.
Should expand the inside end.  Since the beginning is at the boundary
\(not strictly beyond it), the default :position applies and point
goes to the beginning."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1))
             ;; Region starts exactly at hunk start, ends inside.
             (inside-hunk (+ hunk-beg 10)))
        (goto-char hunk-beg)
        (set-mark hunk-beg)
        (goto-char inside-hunk)
        (activate-mark)
        (should (region-active-p))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should result)
          ;; Beginning stays at hunk start.
          (should (= (region-beginning) hunk-beg))
          ;; End should have expanded to hunk end.
          (should (= (region-end) hunk-end))
          ;; Point goes to beginning (default), since the beginning
          ;; was not strictly beyond the hunk bounds.
          (should (= (point) hunk-beg))
          (should/looking-at "ADDED LINE ONE")
          (should/mark-looking-at "Ut enim ad minim"))))))

(ert-deftest test-cpo-gg-expand-region-end-at-boundary-beg-inside ()
  "expand-region when end is at hunk boundary but beginning is inside.
Should expand the beginning."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1))
             ;; Region starts inside hunk, ends exactly at hunk end.
             (inside-hunk (+ hunk-beg 10)))
        (goto-char inside-hunk)
        (set-mark inside-hunk)
        (goto-char hunk-end)
        (activate-mark)
        (should (region-active-p))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should result)
          ;; Beginning should have expanded to hunk start.
          (should (= (region-beginning) hunk-beg))
          ;; End stays at hunk end.
          (should (= (region-end) hunk-end))
          ;; Point should be at the end that moved (the beginning).
          (should (= (point) hunk-beg))
          (should/looking-at "ADDED LINE ONE")
          (should/mark-looking-at "Ut enim ad minim"))))))

(ert-deftest test-cpo-gg-expand-region-already-exact-hunk ()
  "expand-region when region already exactly matches hunk bounds should return nil."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let* ((hunk1 (car git-gutter:diffinfos))
             (hunk-beg (cpo-git-gutter--hunk-start-pos hunk1))
             (hunk-end (cpo-git-gutter--hunk-end-pos hunk1)))
        (goto-char hunk-beg)
        (set-mark hunk-beg)
        (goto-char hunk-end)
        (activate-mark)
        (should (region-active-p))
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should-not result))))))

;;; Deletion hunk tests (pure deletion: lines removed, nothing added)

(ert-deftest test-cpo-gg-get-bounds-deletion-hunk ()
  "get-bounds on a deletion hunk should return an empty range (car = cdr)."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      ;; Hunk 1 (index 1) is a deletion hunk.
      (let ((del-hunk (nth 1 git-gutter:diffinfos)))
        (should (eq (git-gutter-hunk-type del-hunk) 'deleted))
        (goto-char (cpo-git-gutter--hunk-start-pos del-hunk))
        (should/looking-at "Duis aute irure")
        (let ((bounds (cpo-git-gutter-hunk-get-bounds)))
          (should bounds)
          (should (= (car bounds) (cdr bounds)))
          (should (= (car bounds) (cpo-git-gutter--hunk-start-pos del-hunk))))))))

(ert-deftest test-cpo-gg-expand-region-deletion-hunk ()
  "expand-region on a deletion hunk should produce an empty region (point = mark)."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (transient-mark-mode 1)
      (let ((del-hunk (nth 1 git-gutter:diffinfos)))
        (should (eq (git-gutter-hunk-type del-hunk) 'deleted))
        (goto-char (cpo-git-gutter--hunk-start-pos del-hunk))
        ;; expand-region with no active region, point at deletion hunk.
        ;; Since the hunk bounds are empty (start = end), the expansion
        ;; condition (bounds strictly larger than current region) is never
        ;; satisfied, so expand-region returns nil.
        (let ((result (cpo-git-gutter-hunk-expand-region)))
          (should-not result)
          ;; Point should not have moved.
          (should/looking-at "Duis aute irure"))))))

;;; Edge cases

(ert-deftest test-cpo-gg-forward-beginning-from-buffer-end ()
  "Forward beginning from end of buffer should not move."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-max))
      (let ((pos (point)))
        (cpo-git-gutter-hunk-forward-beginning)
        (should (= (point) pos))))))

(ert-deftest test-cpo-gg-backward-end-from-buffer-start ()
  "Backward end from start of buffer should not move."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (goto-char (point-min))
      (let ((pos (point)))
        (cpo-git-gutter-hunk-backward-end)
        (should (= (point) pos))
        (should/looking-at "Lorem ipsum")))))

(ert-deftest test-cpo-gg-negative-count-forward-is-backward ()
  "forward-beginning with negative count should move backward."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk2))
        (cpo-git-gutter-hunk-forward-beginning :count -1)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk1)))
        (should/looking-at "ADDED LINE ONE")))))

(ert-deftest test-cpo-gg-backward-beginning-negative-count-is-forward ()
  "backward-beginning with negative count should move forward."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((hunk1 (nth 0 git-gutter:diffinfos))
            (hunk2 (nth 1 git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos hunk1))
        (cpo-git-gutter-hunk-backward-beginning :count -1)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos hunk2)))
        (should/looking-at "Duis aute irure")))))

;;; Second file tests (verify setup works across multiple files)

(ert-deftest test-cpo-gg-file2-has-hunks ()
  "Second modified file should have hunks too."
  (test-cpo-gg--with-setup
    (with-current-buffer buf2
      (should (= (length git-gutter:diffinfos) 2))
      ;; Navigate forward through all hunks.
      (goto-char (point-min))
      (cpo-git-gutter-hunk-forward-beginning)
      (let ((first-hunk-pos (point)))
        (should (= first-hunk-pos
                   (cpo-git-gutter--hunk-start-pos
                    (car git-gutter:diffinfos))))
        (should/looking-at "INSERTED: New line")
        (cpo-git-gutter-hunk-forward-beginning)
        (should (= (point)
                   (cpo-git-gutter--hunk-start-pos
                    (nth 1 git-gutter:diffinfos))))
        (should/looking-at "MODIFIED: Vestibulum")))))

;;; Cross-file navigation tests

(ert-deftest test-cpo-gg-forward-file ()
  "forward-file should move to the first hunk in the next changed file."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      ;; Go to last hunk in file 1.
      (let ((last-hunk (car (last git-gutter:diffinfos))))
        (goto-char (cpo-git-gutter--hunk-start-pos last-hunk))
        (should/looking-at "MODIFIED: Pellentesque"))
      ;; Navigate forward to next file.
      (cpo-git-gutter-hunk-forward-file 1)
      ;; Should now be in file 2 at its first hunk.
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-2"))
      (should/looking-at "INSERTED: New line"))))

(ert-deftest test-cpo-gg-backward-file ()
  "backward-file should move to the first hunk in the previous changed file."
  (test-cpo-gg--with-setup
    (with-current-buffer buf2
      (goto-char (cpo-git-gutter--hunk-start-pos (car git-gutter:diffinfos)))
      (should/looking-at "INSERTED: New line")
      ;; Navigate backward to previous file.
      (cpo-git-gutter-hunk-backward-file 1)
      ;; Should now be in file 1 at its first hunk.
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-1"))
      (should/looking-at "ADDED LINE ONE"))))

(ert-deftest test-cpo-gg-forward-beginning-cross-files ()
  "forward-beginning with :cross-files t should cross to next file at boundary."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      ;; Go to last hunk in file 1.
      (let ((last-hunk (car (last git-gutter:diffinfos))))
        (goto-char (cpo-git-gutter--hunk-start-pos last-hunk))
        (should/looking-at "MODIFIED: Pellentesque"))
      ;; Forward-beginning with :cross-files should cross to file 2.
      (cpo-git-gutter-hunk-forward-beginning :count 1 :cross-files t)
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-2"))
      (should/looking-at "INSERTED: New line"))))

(ert-deftest test-cpo-gg-forward-beginning-no-cross-files-stays ()
  "forward-beginning without :cross-files should not cross file boundary."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let* ((last-hunk (car (last git-gutter:diffinfos)))
             (start-pos (cpo-git-gutter--hunk-start-pos last-hunk)))
        (goto-char start-pos)
        (cpo-git-gutter-hunk-forward-beginning)
        ;; Should stay in same file at same position.
        (should (string= (file-name-nondirectory (buffer-file-name))
                          "lorem-ipsum-1"))
        (should (= (point) start-pos))
        (should/looking-at "MODIFIED: Pellentesque")))))

(ert-deftest test-cpo-gg-backward-beginning-cross-files ()
  "backward-beginning with :cross-files t should cross to previous file at boundary."
  (test-cpo-gg--with-setup
    (with-current-buffer buf2
      ;; At first hunk of file 2.
      (let ((first-hunk (car git-gutter:diffinfos)))
        (goto-char (cpo-git-gutter--hunk-start-pos first-hunk))
        (should/looking-at "INSERTED: New line"))
      ;; Backward-beginning with :cross-files should cross to file 1.
      (cpo-git-gutter-hunk-backward-beginning :count 1 :cross-files t)
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-1"))
      (should/looking-at "ADDED LINE ONE"))))

(ert-deftest test-cpo-gg-forward-end-cross-files ()
  "forward-end with :cross-files t should cross to next file when at last hunk end."
  (test-cpo-gg--with-setup
    (with-current-buffer buf1
      (let ((last-hunk (car (last git-gutter:diffinfos))))
        (goto-char (cpo-git-gutter--hunk-end-pos last-hunk))
        (should/looking-at "Ac turpis egestas"))
      ;; Forward-end with :cross-files should cross to file 2.
      (cpo-git-gutter-hunk-forward-end :count 1 :cross-files t)
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-2"))
      (should/looking-at "INSERTED: New line"))))

(ert-deftest test-cpo-gg-backward-end-cross-files ()
  "backward-end with :cross-files t should cross to previous file when at buffer start."
  (test-cpo-gg--with-setup
    (with-current-buffer buf2
      (goto-char (point-min))
      (should/looking-at "Lorem ipsum")
      ;; Backward-end with :cross-files should cross to file 1.
      (cpo-git-gutter-hunk-backward-end :count 1 :cross-files t)
      (should (string= (file-name-nondirectory (buffer-file-name))
                        "lorem-ipsum-1"))
      (should/looking-at "ADDED LINE ONE"))))

(provide 'test-cpo-git-gutter)
;;; test-cpo-git-gutter.el ends here
