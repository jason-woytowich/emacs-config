;;; plv --- Project Local Variables

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'dash)

(defun -path-split (path)
  (split-string path "/"))

(defun -path-join (&rest args)
  (string-join args "/"))

(defconst plv-alist nil)

(defvar plv-save-file
  "~/.emacs.d/personal/plv-custom.el")

(defun plv-project-name ()
  (let ((project-root (projectile-project-root)))
    (when project-root
      (-last-item (-butlast (-path-split project-root))))))

(defun plv-get-var (symbol)
  (interactive "vVariable:\n")
  (message "%s = %s" (symbol-name symbol) (symbol-value symbol)))

(defvar -plv-project-files-alist nil)

(defun -alist-set (alist key value)
  (let ((slot (assoc key alist)))
    (if slot
        (progn
          (setcdr slot value)
          alist)
        (cons (cons key value) alist))))

(defun -alist-get-default (alist key default)
  (let ((slot (assoc key alist)))
    (if slot (cdr slot) default)))

(defun plv-set-var (project variable value)
  (interactive
   (let* ((prj (plv-project-name))
          (var (read-variable "Set variable: "))
          (val (read-from-minibuffer "Value: " nil nil t)))
     (list prj var val)))
  (and project variable
       (-plv-set-var project variable value)))

(defun -plv-get-project-vars (project)
  (let ((entry (assoc project plv-alist)))
    (when entry (cdr entry))))

(defun plv-hook ()
  (let ((project (plv-project-name))
        (file-name (buffer-file-name)))
    (message "Project: %s" project)
    (message "File: %s" file-name)
    (and project file-name
         (setq -plv-project-files-alist
               (-alist-set -plv-project-files-alist project (cons (buffer-file-name) (-alist-get-default -plv-project-files-alist project nil)))))
    (plv-apply project)))

(defun plv-apply (project)
  (--each (-plv-get-project-vars project)
    (let ((var (car it))
          (val (cdr it)))
      (set-variable var val))))

(defun -plv-project-alist-set (alist project key value)
  (let* ((project-vars (-alist-get-default (symbol-value alist) project nil))
         (project-vars-new (-alist-set project-vars key value)))
    (set alist (-alist-set (symbol-value alist) project project-vars-new))))

(defun -plv-project-alist-get (alist project key)
  (-alist-get-default (-alist-get-default alist project nil) key nil))

(defun -plv-set-var (project variable value)
    (message "Setting %s = %s" variable value)
    (-plv-project-alist-set 'plv-alist project variable value)
    (-plv-update-project-buffers project variable value)
    (plv-save))

(defun -plv-update-project-buffers (project variable value)
  (--each (-alist-get-default -plv-project-files-alist project nil)
    (let ((buffer (get-file-buffer it)))
      (when buffer
        (save-excursion
          (set-buffer buffer)
          (set-variable variable value))))))

(defun plv-save ()
  (let ((save-file (find-file-noselect plv-save-file)))
    (set-buffer save-file)
    (erase-buffer)
    (insert ";;; Auto-generated file\n\n")
    (insert (format "(setq plv-alist '%s)\n" plv-alist))
    (save-buffer)
    (kill-buffer)))

(require 'ert)

(ert-deftest -test-alist-methods ()
  "Make sure the alist methods work"
  (should (eq 'the-default (-alist-get-default nil 'akey 'the-default)))
  (let ((alist (-alist-set nil 'akey 'the-value)))
    (should (eq 'the-value (-alist-get-default alist 'akey 'the-default)))
    (let ((alist2 (-alist-set alist 'bkey 'the-other)))
      (should (eq 'the-value (-alist-get-default alist2 'akey 'the-default)))
      (should (eq 'the-other (-alist-get-default alist2 'bkey 'the-default))))))

(ert-deftest -test-plv-project-alist-set ()
  (setq -test-ply-alist nil)
  (should (eq nil (-plv-project-alist-get -test-ply-alist "Project1" 'var1)))
  (-plv-project-alist-set '-test-ply-alist "Project1" 'var1 1)
  (-plv-project-alist-set '-test-ply-alist "Project1" 'var2 2)
  (-plv-project-alist-set '-test-ply-alist "Project1" 'var2 3)
  (-plv-project-alist-set '-test-ply-alist "Project2" 'var1 5)
  (-plv-project-alist-set '-test-ply-alist "Project2" 'var2 6)
  (-plv-project-alist-set '-test-ply-alist "Project3" 'var1 7)
  (print (format "%s" -test-ply-alist))
  (should (eq 1 (-plv-project-alist-get -test-ply-alist "Project1" 'var1)))
  (should (eq 3 (-plv-project-alist-get -test-ply-alist "Project1" 'var2)))
  (should (eq 5 (-plv-project-alist-get -test-ply-alist "Project2" 'var1)))
  (should (eq 6 (-plv-project-alist-get -test-ply-alist "Project2" 'var2)))
  (should (eq 7 (-plv-project-alist-get -test-ply-alist "Project3" 'var1))))


(provide 'plv)

;;; plv.el ends here
