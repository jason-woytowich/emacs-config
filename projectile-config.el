;;; projectile-config --- My projectile configuration
;;; Commentary:
;;; Code:

(require 'projectile)
(require 'eshell-config)

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (cond
   ((projectile-verify-files projectile-rails-rspec) 'rails-rspec)
   ((projectile-verify-files projectile-rails-test) 'rails-test)
   ((projectile-verify-files projectile-ruby-rspec) 'ruby-rspec)
   ((projectile-verify-files projectile-ruby-test) 'ruby-test)
   ((projectile-verify-files projectile-django) 'django)
   ((projectile-verify-files projectile-python-tox) 'python-tox)
   ((projectile-verify-files projectile-python-pip)'python)
   ((projectile-verify-files projectile-python-egg) 'python)
   ((projectile-verify-files projectile-symfony) 'symfony)
   ((projectile-verify-files projectile-lein) 'lein)
   ((projectile-verify-files projectile-scons) 'scons)
   ((projectile-verify-files projectile-maven) 'maven)
   ((projectile-verify-files projectile-gradle) 'gradle)
   ((projectile-verify-files projectile-grails) 'grails)
   ((projectile-verify-files projectile-rebar) 'rebar)
   ((projectile-verify-files projectile-sbt) 'sbt)
   ((projectile-verify-files projectile-haskell-cabal) 'haskell-cabal)
   ((projectile-verify-files projectile-make) 'make)
   ((projectile-verify-files projectile-gulp) 'gulp)
   ((projectile-verify-files projectile-grunt) 'grunt)
   ((projectile-verify-files projectile-rust-cargo) 'rust-cargo)
   ((projectile-verify-files projectile-r) 'r)
   ((funcall projectile-go-function) 'go)
   (t 'generic)))

(projectile-global-mode)

(defun jfw-projectile-eshell ()
  "Start an eshell in the project root."
  (interactive)
  (when (projectile-project-p)
    (let* ((eshell-buffer-name (format "*eshell: %s*" (projectile-project-name)))
           (existing-buffer (get-buffer eshell-buffer-name)))
      (if existing-buffer
          (switch-to-buffer existing-buffer)
        (eshell)
        (cd (projectile-project-root))
        (eshell-clear)))))

(define-key projectile-mode-map (kbd "C-c p E") 'jfw-projectile-eshell)

(defadvice projectile-regenerate-tags (around project-type-tags activate)
  "Custom tag generation for every project type."
  (interactive)
  (let* ((type (projectile-project-type))
         (cmd-fmt
          (cond
           ((eq 'haskell-cabal type) "hasktags -e %s")
           ((eq 'make type) "make etags")
           (t "etags %s")))
         (projectile-tags-command cmd-fmt))
    ad-do-it))

(provide 'projectile-config)

;;; projectile-config.el ends here
