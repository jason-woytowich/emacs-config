;;; c-config -- Local C/C++ configuration
;;; Commentary:
;;; Code:

(require 'flycheck)

(defun c-mode-common-init ()
  (setq indent-tabs-mode nil))

(defun c++-mode-init ()
  "C++ Configuration Hook."
  (setq indent-tabs-mode nil)
  (setq flycheck-clang-language-standard "c++1y"))

(defun c-mode-init ()
  "C++ Configuration Hook."
  (setq indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'c-mode-init)
(add-hook 'c++-mode-hook 'c++-mode-init)
(add-hook 'c-mode-hook 'c-mode-init)

(provide 'c-config)
;;; c-config.el ends here
