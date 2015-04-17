(require 'org)

(setq org-capture-templates
 '(("t" "Default" entry (file+headline "~/org/todo.org" "Tasks") "** TODO %?\n  %T")
   ("c" "Code review item" entry (file+headline "~/org/code-review.org" "Comments") "** TODO %?\n  %T\n  %l")))

(setq org-agenda-files '("~/org/"))

(setq org-default-notes-file "~/org/notes.org")

(setq org-src-fontify-natively t)

(setq org-babel-load-languages
  (quote
   ((python . t)
    (dot . t)
    (emacs-lisp . t)
    (calc . t))))

(setq org-confirm-babel-evaluate nil)

(setq org-src-lang-modes
  (quote
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
    ("python" . python))))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(provide 'org-config)
