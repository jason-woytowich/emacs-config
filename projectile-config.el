;;; projectile-config --- My projectile configuration
;;; Commentary:
;;; Code:

(require 'projectile)
(require 'eshell-config)

(projectile-global-mode)

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

(define-key projectile-mode-map (kbd "C-c p E") 'jfw-projectile-eshell)

(defadvice projectile-regenerate-tags (around project-type-tags activate)
  "Custom tag generation for every project type."
  (interactive)
  (let* ((type (projectile-project-type))
         (cmd-fmt
          (cond
           ((eq 'haskell-cabal type) "hasktags -e %s")
           ((eq 'make type) "make etags")
           (t "etags %s")))
         (projectile-tags-command cmd-fmt))
    ad-do-it))

(provide 'projectile-config)

;;; projectile-config.el ends here
