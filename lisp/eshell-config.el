;;; eshell-config --- eshell customizations
;;; Commentary:
;;; Code:

(with-eval-after-load "eshell"
  (defun eshell-clear ()
    "Clear the eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(provide 'eshell-config)

;;; eshell-config.el ends here
