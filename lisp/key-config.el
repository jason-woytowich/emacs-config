;;; key-config.el -- Prefered key bindings
;;; Commentary:
;;; Code:


(require 'projectile)
(require 'nose)
(require 'avy)

;;; OSX keybindings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)

; Python settings
(require 'python)

(dolist (key-pair `(("C-c m" nosetests-module)
		    ("C-c ." nosetests-one)
		    ("C-c p a" nosetests-pdb-all)
		    ("C-c p m" nosetests-pdb-module)
		    ("C-c p ." nosetests-pdb-one)
		    ("C-u C-c c" uncomment-region)))
  (let ((key (car key-pair))
	(action (cadr key-pair)))
    (define-key python-mode-map (kbd key) action)))

; use iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'key-config)
;;; key-config.el ends here
