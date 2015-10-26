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
    flycheck-haskell
    haskell-mode
    dot-mode
    markdown-mode
    smartparens
    pretty-lambdada
    ido-ubiquitous
    smex
    exec-path-from-shell
    deft
    org
    flycheck
    magit
    avy
    yaml-mode
    git-gutter-fringe+
    projectile
    zenburn-theme
    monokai-theme))

(defun check-required-packages ()
  (dolist (package required-packages)
    (when (not (package-installed-p package))
      (package-install package))))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(check-required-packages)

(let ((elpa-directory (expand-file-name "~/.emacs.d/elpa/")))
  (dolist (file (directory-files elpa-directory))
    (add-to-list 'load-path (concat elpa-directory file))))

(setq Info-additional-directory-list '("/opt/local/share/info"))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defun open-at-point ()
  (interactive)
  (let ((url (or (dired-get-filename nil t)
                 (thing-at-point 'url))))
    (message url)
    (start-process "*Open-At-Point*" nil "open" url)))

(server-start)

(add-hook 'prog-mode-hook 'whitespace-mode t)
(add-hook 'prog-mode-hook 'which-func-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace t)

(require 'git-gutter-fringe+)

(require 'theme-config)
(require 'org-config)
(require 'deft-config)
(require 'magit-config)

(require 'ido-config)
(require 'c-config)
(require 'python-config)
(require 'lisp-config)
(require 'haskell-config)
(require 'key-config)
(require 'copyright-config)
(require 'window-config)
(require 'eshell-config)
(require 'projectile-config)
(require 'reddit)
(require 'plv)

(add-hook 'prog-mode-hook 'plv-hook t)

(defvar local-emacs-dir "~/EmacsLocal/")


(dolist (file (directory-files local-emacs-dir t "\.el$"))
  (message file)
  (load file))

(provide 'init)

;;; init.el ends here
