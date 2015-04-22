;;; c-config -- Local C/C++ configuration
;;; Commentary:
;;; Code:

(require 'flycheck)

(dolist (hook `(c-mode-hook c++-mode-hook))
  (add-hook hook 'flycheck-mode)
  (add-hook hook 'c-mode-hook))

(defun c-mode-common-init ()
  (setq indent-tabs-mode nil))

(defun c++-mode-init ()
  "C++ Configuration Hook."
  (setq indent-tabs-mode nil)
  (setq flycheck-clang-language-standard "c++1y"))
(add-hook 'c++-mode-hook 'c++-mode-init)

(defun c-mode-init ()
  "C++ Configuration Hook."
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'c-mode-init)

(provide 'c-config)
;;; c-config.el ends here
