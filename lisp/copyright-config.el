(require 'copyright)

(defun c-mode-copyright (year)
  (insert "/*\n")
  (insert "  Jason Woytowich - Copyright (c) " year "\n")
  (insert "  All rights reserved\n")
  (insert "*/\n"))

(defun c++-mode-copyright (year)
  (insert "// -*- mode: c++ -*-\n")
  (insert "//\n")
  (insert "// Jason Woytowich - Copyright (c) " year "\n")
  (insert "// All rights reserved\n")
  (insert "//\n"))

(defun asm-mode-copyright (year)
  (insert ";\n")
  (insert "; Jason Woytowich - Copyright (c) " year "\n")
  (insert "; All rights reserved\n")
  (insert ";\n"))

(defvar jfw-copyright-alist
  (list (cons 'c-mode 'c-mode-copyright)
        (cons 'c++-mode 'c++-mode-copyright)
        (cons 'asm-mode 'asm-mode-copyright)))

(defvar jfw-insert-copyright nil)

(defun jfw-current-year ()
  (substring (current-time-string) -4))

(defun jfw-do-insert-copyright (inserter)
  (let ((year (jfw-current-year)))
      (goto-char 0)
      (funcall inserter year)
      (insert "\n")))

(setq jfw-insert-copyright t)

;;;###autoload
(defun jfw-copyright-update ()
  (interactive)
  (let ((copyright-inserter (cdr (assoc major-mode jfw-copyright-alist))))
    (save-excursion
      (when
          (and copyright-inserter
               jfw-insert-copyright
               (not (copyright-find-copyright))
               (yes-or-no-p "Insert copyright?"))
        (jfw-do-insert-copyright copyright-inserter))
      (copyright-update nil t)
      )))

(provide 'copyright-config)
;;; copyright-config.el ends here
