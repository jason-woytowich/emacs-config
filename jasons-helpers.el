;;; jasons-helpers --- Random stuff I find useful
;;; Commentary:
;;; Todo:
;;;  - Language specific comments
;;;  - Custom header information (project root, git tags/commits, etc.)
;;;  - Be smarter about the size of the prepended line numbers, 5 digits does not look nice
;;;  - Copy as markup?
;;; Code:

(defun copy-with-line-numbers (&optional begin end)
  "Copies full lines covering the region BEGIN to END with line numbers prepended to each."
  (interactive "r")

  (unless (and begin end)
    (error "Region not specified"))

  (when (and begin end)
    (save-excursion
      (goto-char begin)
      (move-beginning-of-line nil)
      (setq begin (point))

      (goto-char end)
      (move-end-of-line nil)
      (setq end (point))

      (copy-region-as-kill begin end)

      (let ((line (line-number-at-pos begin))
            (end-line (line-number-at-pos end)))
        (with-temp-buffer
          (yank)
          (goto-char 0)
          (while (<= line end-line)
            (move-beginning-of-line nil)
            (insert (format "%5d: " line))
            (setq line (+ 1 line))
            (forward-line))
          (copy-region-as-kill (point-min) (point-max)))))))

(defun copy-with-file-and-line-numbers (&optional begin end)
  "Copies the region BEGIN to END with source filename and line numbers as a comment."
  (interactive "r")

  (unless (and begin end)
    (error "Region not specified"))

  (when (and begin end)
    (save-excursion
      (copy-region-as-kill begin end)
      (let ((line-start (line-number-at-pos begin))
            (line-end (line-number-at-pos end))
            (file-name (buffer-file-name)))
        (with-temp-buffer
          (insert (format "// %s, lines %d-%d\n" file-name line-start line-end))
          (yank)
          (copy-region-as-kill (point-min) (point-max)))))))

(provide 'jasons-helpers)

;;; jasons-helpers.el ends here
