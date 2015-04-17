;;; key-config.el -- Prefered key bindings
;;; Commentary:
;;; Code:

(require 'org)
(require 'magit)
(require 'projectile)
(require 'dash-at-point)
(require 'nose)

;;; OSX keybindings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

; 'fn' shortcuts
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f10>") 'deft)
(global-set-key (kbd "<f11>") 'org-agenda)
(global-set-key (kbd "<f12>") 'org-capture)

; Search shortcuts
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c o") 'open-at-point)
(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)

; Python settings
(require 'python)

(dolist (key-pair `(("C-c a" nosetests-all)
		    ("C-c m" nosetests-module)
		    ("C-c ." nosetests-one)
		    ("C-c p a" nosetests-pdb-all)
		    ("C-c p m" nosetests-pdb-module)
		    ("C-c p ." nosetests-pdb-one)
		    ("C-c c" comment-region)
		    ("C-u C-c c" uncomment-region)))
  (let ((key (car key-pair))
	(action (cadr key-pair)))
    (define-key python-mode-map (kbd key) action)))


; Elisp settings
(define-key emacs-lisp-mode-map (kbd "C-c a") 'ert-all)

(provide 'key-config)
;;; key-config.el ends here