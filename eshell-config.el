;;; eshell-config --- eshell customizations
;;; Commentary:
;;; Code:

(require 'eshell)

(defun eshell-clear ()
  "Clear the eshell."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(provide 'eshell-config)

;;; eshell-config.el ends here
