;;; disassemble --- Disassemble current buffer / function
;;; Commentary:

;;; Required packages:
(require 'projectile)
(require 'which-func)
(require 'json)

;;; Code:
(defun read-json-file (filename)
  "Parse the file at FILENAME as JSON and return it."
  (save-excursion
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (json-read))))

(defun comp-db-lookup-entry (db file-name)
  "Find the entry for the given FILE-NAME in the compilat"
  (seq-find (lambda (it) (string= file-name (alist-get 'file it))) db))

(defun comp-db-entry-output-file-name (entry)
  (let* ((args (split-string (comp-db-entry-command entry)))
         (output-flag-pos (seq-position args "-o")))
    (when output-flag-pos
      (seq-elt args (+ 1 output-flag-pos)))))

(defun comp-db-entry-command (entry)
  (alist-get 'command entry))

(defun comp-db-entry-file-name (entry)
  (alist-get 'file entry))

(defun comp-db-entry-directory (entry)
  (alist-get 'directory entry))

(defmacro with-directory (dir &rest forms)
  (declare (debug (form body))
           (indent 1))
  `(let ((old-pwd default-directory))
     (message (format "Changing directory to %s" ,dir))
     (cd-absolute ,dir)
     ,@forms
     (message (format "Changing directory back to %s" old-pwd))
     (cd-absolute old-pwd)
     (message "Done")))

(defun comp-db-recompile-filename (db file-name)
  (let ((entry (comp-db-lookup-entry db file-name)))
    (when entry (comp-db-entry-recompile entry))))

(defun comp-db-entry-disassemble-output (entry)
  (let* ((file-name (comp-db-entry-file-name entry))
         (buffer-name (format "*Disassembly of %s*" file-name))
         (output-file-name (comp-db-entry-output-file-name entry))
         (disassembler "otool")
         (disassembler-options (list "-tvV")))
    (switch-to-buffer-other-window (get-buffer-create buffer-name))
    (erase-buffer)
    (add-to-list 'disassembler-options output-file-name t)
    (apply #'call-process disassembler nil t t disassembler-options)
    (call-process-region (point-min) (point-max) "c++filt" t (current-buffer) t)
    (goto-char (point-min))
    (asm-mode)
    (read-only-mode)))

(defun comp-db-entry-recompile (entry)
  (let* ((command-string (comp-db-entry-command entry))
         (command-and-args (split-string command-string))
         (program (car command-and-args))
         (args (cdr command-and-args)))
    (with-current-buffer (get-buffer-create "*Compilation*")
      (erase-buffer)
      (with-directory (comp-db-entry-directory entry)
        (insert (format "Program: %s\n" program))
        (insert (format "Args: %s\n" (combine-and-quote-strings args)))
        (apply #'call-process program nil t t args)))))

(defun -find-function-range-in-disassembly (function-name)
  "Return range as a cons cell (start . end) covering the function called FUNCTION-NAME."
  (save-excursion
    (let ((start nil)
          (end nil))
    (re-search-forward (format "^%s(.*).*:" function-name))
    (setq start (line-beginning-position))
    (re-search-forward "^.*:$")
    (setq end (line-beginning-position))
    (if (and start end)
        (cons start end)
      nil))))

(defun -narrow-to-function (function-name)
  "Narrow disassembly buffer to just the FUNCTION-NAME."
  (let ((range (-find-function-range-in-disassembly function-name)))
    (if range
      (let ((start (car range))
            (end (cdr range)))
        (narrow-to-region start end))
      (error "Couldn't find function: %s" function-name))))

;;;###autoload
(defun disassemple-current-function ()
  "Show disassembly for the current function."
  (interactive)
  (let ((current-function (which-function)))
    (message (format "%s" current-function))
    (if current-function
        (progn
          (disassemble-current-buffer)
          (-narrow-to-function current-function))
      (error "Couldn't determine current function"))))

;;;###autoload
(defun disassemble-current-buffer ()
  "Show disassembly for the current buffer."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (project-root (projectile-project-root))
         (compilation-db (expand-file-name "compile_commands.json" project-root))
         (db (read-json-file compilation-db))
         (entry (comp-db-lookup-entry db file-name)))
    (unless entry (error "Could not %s in compilation database" file-name))
    (when entry
      (let ((output-file-name (comp-db-entry-output-file-name entry)))
        (when (not (file-exists-p output-file-name))
          (comp-db-entry-recompile entry))
        (comp-db-entry-disassemble-output entry)))))

(provide 'disassemble)
;;; disassemble ends here
