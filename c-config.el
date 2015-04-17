;;; c-config -- Local C/C++ configuration
;;; Commentary:
;;; Code:

(require 'flycheck)

(dolist (hook `(c-mode-hook c++-mode-hook))
  (add-hook hook 'flycheck-mode))

(defun c++-mode-init ()
  "C++ Configuration Hook."
  (flycheck-mode)
  (setq flycheck-clang-language-standard "c++1y"))

(defun c-mode-init ()
  "C++ Configuration Hook."
  (flycheck-mode))

(provide 'c-config)
;;; c-config.el ends here
