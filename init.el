;;; init -- Starting point of my emacs config
;;; Commentary:
;;; Code:
;;; Todo:

(require 'package)

(defvar config-dir (file-name-directory load-file-name))
(defvar cache-dir (concat config-dir "cache/"))

(load-file (concat config-dir "custom.el"))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(add-to-list 'load-path (concat config-dir "lisp/"))

(let ((elpa-directory (concat config-dir "elpa/")))
  (dolist (file (directory-files elpa-directory t "^[^.]"))
    (add-to-list 'load-path file)))

(setq Info-additional-directory-list '("/opt/local/share/info"
                                       "/opt/openocd/share/info"
                                       "/usr/local/Caskroom/gcc-arm-embedded/6_2-2016q4,20161216/gcc-arm-none-eabi-6_2-2016q4/share/doc/gcc-arm-none-eabi/info/"))

(fset 'yes-or-no-p 'y-or-n-p)

(defun handle-large-files ()
  "Disable features that may lag on large files."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'handle-large-files)

(use-package projectile
  :ensure
  :config  (setq projectile-cache-file (concat cache-dir "projectile.cache")
                 projectile-known-projects-file (concat cache-dir "projectile-bookmarks.eld")))
(use-package nose :ensure)
(use-package jedi :ensure)
(use-package ag :ensure)
(use-package dot-mode :ensure)
(use-package yaml-mode :ensure)
(use-package pretty-lambdada)
(use-package flycheck-pyflakes :ensure)

(use-package smex
  :ensure
  :bind ("M-x" . smex)
  :bind ("M-X" . smex-major-mode-commands))

(use-package avy
  :ensure
  :bind ("C-c j" . avy-goto-word-1))

(use-package deft
  :ensure
  :bind ("<f10>" . deft)
  :config (setq deft-directory (expand-file-name "~/org")
                deft-default-extension "org"
                deft-text-mode 'org-mode
                deft-auto-save-interval 0.0))

(use-package org-ref
  :config (setq org-ref-notes-directory "~/Reading"
                org-ref-bibliography-notes "~/Reading/index.org"
                org-ref-default-bibliography '("~/Reading/index.bib")
                org-ref-pdf-directory "~Reading/lib/"))

(use-package helm-bibtex
  :config (setq helm-bibtex-bibliography "$SOME/index.bib"
                helm-bibtex-library-path "$SOME/lib/"
                helm-bibtex-notes-path "$SOME/index.org"
                bibtex-completion-bibliography "$SOME/index.bib"
                bibtex-completion-notes-path "$SOME/index.org"))

(use-package exec-path-from-shell
  :ensure
  :demand
  :preface (exec-path-from-shell-initialize))

(use-package ido-ubiquitous
  :ensure
  :preface (progn (ido-mode)
                  (ido-ubiquitous-mode)))

(use-package flycheck :ensure)

(use-package flycheck-haskell :ensure)
(use-package git-gutter-fringe+ :ensure :demand)

(use-package haskell-mode
  :ensure
  :bind-keymap ("C-c v c" . haskell-cabal-visit-file)
  :config (progn (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
                 (add-hook 'haskell-mode-hook 'haskell-doc-mode)
                 (add-hook 'haskell-mode-hook 'flycheck-mode)
                 (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)))


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package magit
  :ensure
  :bind ("<f9>" . magit-status)
  :config (progn (setq magit-last-seen-setup-instructions "1.4.0"
                       magit-use-overlays nil
                       magit-completing-read-function #'magit-ido-completing-read
                       magit-diff-arguments '("--ignore-all-space")
                       magit-git-debug t
                       magit-push-current-set-remote-if-missing nil
                       magit-status-headers-hook `(magit-insert-diff-filter-header magit-insert-head-header magit-insert-upstream-header))
                 (defadvice magit-status (around magit-fullscreen activate)
                   (window-configuration-to-register :magit-fullscreen)
                   ad-do-it
                   (delete-other-windows))
                 (defadvice magit-mode-quit-window (after magit-restore-screen activate)
                   (jump-to-register :magit-fullscreen))))

(let ((local-org-dir "~/org/"))
    :ensure
  (use-package org
    :demand
    :bind ("C-c a" . org-agenda)
    :bind ("C-c c" . org-capture)
    :config (progn (setq org-agenda-files (list local-org-dir)
                         org-default-notes-file (expand-file-name "master.org" local-org-dir)
                         org-src-fontify-natively t
                         org-babel-load-languages (quote
                                                   ((dot . t)
                                                    (emacs-lisp . t)
                                                    (calc . t)
                                                    (python . t)
                                                    (awk . t)
                                                    (haskell . t)))
                         org-confirm-babel-evaluate nil
                         org-src-lang-modes (quote
                                             (("ocaml" . tuareg)
                                              ("elisp" . emacs-lisp)
                                              ("ditaa" . artist)
                                              ("asymptote" . asy)
                                              ("dot" . graphviz-dot)
                                              ("sqlite" . sql)
                                              ("calc" . fundamental)
                                              ("C" . c)
                                              ("cpp" . c++)
                                              ("C++" . c++)
                                              ("screen" . shell-script)
                                              ("python" . python)))
                         org-log-into-drawer "LOGBOOK"
                         org-capture-templates '(("n" "New task" entry (file+headline org-default-notes-file "Tasks") "** TODO %?\n   :LOGBOOK:\n   - Created %U\n   :END:")
                                                 ("N" "New task - Clock in" entry (file+headline org-default-notes-file "Tasks") "** TODO %?\n   :LOGBOOK:\n   - CREATED %U\n   :END:" :clock-in t)
                                                 ("x" "New task - With reference to current file" entry (file+headline org-default-notes-file "Tasks") "** TODO %?\n   :LOGBOOK:\n   - Created %U\n   :END:\n%a")
                                                 ("s" "New sub-task" entry (clock) "*** TODO %?\n   :LOGBOOK:\n   - Created %U\n   :END:")
                                                 ("r" "New review" entry (file+headline org-default-notes-file "Tasks") "** REVIEWING %?\n   :LOGBOOK:\n   - CREATED %U\n   :END:")
                                                 ("c" "Comment" plain (clock) " - %? %U")
                                                 ("C" "Comment - Include reference to current file" plain (clock) " - %? %U\n%a")))
                   (add-hook 'org-mode-hook 'visual-line-mode)
                   (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))))

(defun open-at-point ()
  (interactive)
  (let ((url (or (dired-get-filename nil t)
                 (thing-at-point 'url))))
    (message url)
    (start-process "*Open-At-Point*" nil "open" url)))
(global-set-key (kbd "C-c o") 'open-at-point)


(use-package spacemacs-light-theme)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(server-start)

(add-hook 'prog-mode-hook 'whitespace-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace t)

(require 'jasons-helpers)

(require 'c-config)
(require 'python-config)
(require 'lisp-config)
(require 'key-config)
(require 'copyright-config)
(require 'window-config)
(require 'eshell-config)
(require 'projectile-config)
(require 'reddit)

(defvar local-emacs-dir "~/EmacsLocal/")

(dolist (file (directory-files local-emacs-dir t "\.el$"))
  (message file)
  (load file))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
