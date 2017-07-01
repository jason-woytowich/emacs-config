;;; lisp-config --- Local lisp configuration
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'smartparens)

(dolist (hook `(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook 'flycheck-mode)
  (add-hook hook 'pretty-lambda-mode))

(provide 'lisp-config)
;;; lisp-config.el ends here
