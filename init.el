;;; init -- Starting point of my emacs config
;;; Commentary:
;;; Code:

(require 'package)

(setq
 custom-file (expand-file-name "~/.emacs.d/custom.el"))

(load-file custom-file)

(defvar required-packages
  `(dash
    dash-at-point
    ag
    nose
    jedi
    flycheck-pyflakes
    haskell-mode
    smartparens
    ido-ubiquitous
    exec-path-from-shell
    deft
    org
    flycheck
    magit
    yaml-mode
    projectile
    zenburn-theme
    monokai-theme))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(dolist (package required-packages)
  (when (not (package-installed-p package))
    (package-install package)))

(let ((elpa-directory (expand-file-name "~/.emacs.d/elpa/")))
  (dolist (file (directory-files elpa-directory))
    (add-to-list 'load-path (concat elpa-directory file))))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defun open-at-point ()
  (interactive)
  (let ((url (thing-at-point 'url)))
    (message url)
    (start-process "*Open-At-Point*" nil "open" url)))

(server-start)

(add-hook 'prog-mode-hook 'whitespace-mode t)
(add-hook 'prog-mode-hook 'which-func-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace t)

(require 'theme-config)
(require 'org-config)
(require 'deft-config)
(require 'magit-config)
(require 'projectile)

(require 'ido-config)
(require 'c-config)
(require 'python-config)
(require 'lisp-config)
(require 'haskell-config)
(require 'key-config)
(require 'copyright-config)

(projectile-global-mode)

(defvar local-emacs-dir "~/EmacsLocal/")

(dolist (file (directory-files local-emacs-dir t "\.el$"))
  (message file)
  (load file))

(provide 'init)

;;; init.el ends here
