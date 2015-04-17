;;; python-config --- Emacs Python settings
;;;
;;; Commentary:
;;;    --None

;;; Code:
(require 'flycheck)
(require 'nose)
(require 'auto-complete)
(require 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'flycheck-mode)

(setq jedi:complete-on-dot t)

(flycheck-define-checker python-pyflakes
  "A Python static checker"
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start
	  (file-name) ":" line ":" (optional column ":") " "
	  (message (zero-or-more not-newline))
	  line-end))
  :modes python-mode)
 
(add-to-list 'flycheck-checkers 'python-pyflakes)
 
(flycheck-define-checker python-pep8
  "A Python PEP8 style checker"
  :command ("pep8" source-inplace)
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ": "
          (message "E" (one-or-more digit) (zero-or-more not-newline))
          line-end)
   (warning line-start
            (file-name) ":" line ":" column ": "
            (message "W" (one-or-more digit) (zero-or-more not-newline))
            line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pep8)

(provide 'python-config)
;;; python-config ends here
