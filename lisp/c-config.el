;;; c-config -- Local C/C++ configuration
;;; Commentary:
;;; Code:

(require 'flycheck)

(c-add-style "jfw-c-style"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (defun-open . 0)
                                   (inline-open . 0)
                                   (substatement-open . 0)
                                   (substatement-label . 0)
                                   (label . 0)
                                   (statement-cont . +)))))

(defun c++-mode-init ()
  "C++ Configuration Hook."
  (setq flycheck-clang-language-standard "c++1y"))

(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'c++-mode-init)

(provide 'c-config)
;;; c-config.el ends here
