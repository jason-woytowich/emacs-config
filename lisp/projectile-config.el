;;; projectile-config --- My projectile configuration
;;; Commentary:
;;; Code:

(require 'projectile)

;;;###autoload
(defun jfw-projectile-eshell ()
  "Start an eshell in the project root."
  (interactive)
  (when (projectile-project-p)
    (let* ((eshell-buffer-name (format "*eshell: %s*" (projectile-project-name)))
           (existing-buffer (get-buffer eshell-buffer-name)))
      (if existing-buffer
          (switch-to-buffer existing-buffer)
        (eshell)
        (cd (projectile-project-root))
        (eshell-clear)))))

(provide 'projectile-config)

;;; projectile-config.el ends here
