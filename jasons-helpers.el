;;; jasons-helpers --- Random stuff I find useful
;;; Commentary:
;;; Todo:
;;;  - Language specific comments
;;;  - Custom header information (git tags/commits, etc.)
;;;  - Copy as markup?
;;; Code:

(defun relative-to-root (root path)
  "Return the relative path from ROOT to PATH."
  (let ((abs-root (expand-file-name root))
        (abs-path (expand-file-name path)))
    (message abs-root)
    (message abs-path)
    (if (string-prefix-p abs-root abs-path)
        (substring abs-path (length abs-root))
      path)))

(defun copy-with-file-and-line-numbers (&optional begin end)
  "Copies the region BEGIN to END with source filename and line numbers as a comment."
  (interactive "r")

  (unless (and begin end)
    (error "Region not specified"))

  (let ((project-root (or
                       (and (not current-prefix-arg) (projectile-project-root))
                       "/")))
    (save-excursion
      (copy-region-as-kill begin end)
      (let ((line-start (line-number-at-pos begin))
            (line-end (line-number-at-pos end))
            (file-name (relative-to-root project-root (buffer-file-name))))
        (with-temp-buffer
          (insert (format "// %s, lines %d-%d\n" file-name line-start line-end))
          (yank)
          (copy-region-as-kill (point-min) (point-max)))))))

(provide 'jasons-helpers)

;;; jasons-helpers.el ends here
