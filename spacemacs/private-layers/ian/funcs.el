(defun ic/org-capture-full-window ()
  "Run `(org-capture)' without splitting the frame"
  (interactive)
  (message "ic/org-capture-full-window")
  (flet ((org-switch-to-buffer-other-window (&rest args) (apply #'switch-to-buffer args)))
    (org-capture)))


(defadvice org-capture-finalize (after delete-capture-frame activate)
  "Close OrgCapture frame after completing a capture"
  (when (equal (frame-parameter nil 'name) "OrgCapture")
    (delete-frame)))



;;; functions taken from http://doc.norang.ca/org-mode.html

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


(defun ic/org-recipe-publish-to-html (plist org-filename target-dir)
  "Export ORG-FILENAME as html. Similar to `org-html-publish-to-html', but only exporting certain sections"
  (interactive)
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory org-filename)))
         (target-filename (concat target-dir base-name ".html")))
    (with-temp-file target-filename
      (insert-file-contents org-filename)
      (goto-char (point-min))
      )
    )
  )


(defun ic/org-export-filter-recipes (backend)
  "Filter out unwanted elements from org-mode recipes, but only when export as HTML."
  (when (equal backend 'html)
    (org-map-entries
     (lambda ()
       )
     ;; (lambda () (delete-region (point) (progn (forward-line) (point)))))
    )
  ))
