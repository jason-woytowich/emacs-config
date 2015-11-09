;;; hoogle --- Query 'hoogle'

;;; Commentary:

;;; Code:

(require 'json)
(require 'dash)

(defmacro with-text-properties (props &rest body)
  (declare (indent 2))
  `(let* ((start (point))
          (result (progn
                    ,@body))
          (end (point)))
     (add-text-properties start end ,props)
     result))

(defun build-search-url (query)
  "Build a url for a given QUERY."
  (format "http://www.haskell.org/hoogle/?mode=json&hoogle=%s" query))

(defun hoogle-search (query)
  "Search hoogle with the given QUERY."
  (with-current-buffer (url-retrieve-synchronously (build-search-url query) t t)
    (goto-char url-http-end-of-headers)
    (cdr (assoc 'results (json-read)))))

(defmacro vector-each (vector &rest body)
  `(--map (progn ,@body) ,vector))

(defun hoogle-open-location (data)
  (interactive)
  (let ((link (cdr (assoc 'location data))))
    (start-process "open" "*Hoogle Open*" "open" link)))

(defun hoogle-results-package (data)
  (let* ((location (cdr (assoc 'location data)))
         (docstart (string-match "\/\\([A-Za-z0-9-]+\\)\\.html" location))
         (name     (match-string 1 location))
         (path     (split-string name "-")))
       (string-join path ".")))

(defun hgl (query)
  "Run a search for the given QUERY."
  (interactive "sSearch: ")

  (switch-to-buffer (format "*Hoogle: %s*" query))
  (hoogle-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Hoogle search for: %s\n\n" query))

    (let ((pos (point)))
      (vector-each (hoogle-search query)
                   (let ((self (cdr (assoc 'self it)))
                         (docs (cdr (assoc 'docs it)))
                         (loc (cdr (assoc 'location it)))
                         (package (hoogle-results-package it)))
                     (with-text-properties (list 'data it 'action #'hoogle-open-location)
                         (insert (format "%s [%s]\n" self package)))))
      (goto-char pos))))


(defun hoogle-link-action ()
  (interactive)
  (let* ((pt     (point))
         (action (get-text-property pt 'action))
         (data   (get-text-property pt 'data)))
    (apply action data nil)))

(defun hoogle-goto-next ()
  (interactive))

(defvar hoogle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'hoogle-link-action)
    (define-key map (kbd "<return>") #'hoogle-link-action)
    (define-key map (kbd "<tab>") #'hoogle-goto-next)
    map))

(define-derived-mode hoogle-mode special-mode "Hoogle"
  "Major mode for crushing it!"
  (use-local-map hoogle-mode-map)
  (visual-line-mode t))

(provide 'hoogle)

;;; hoogle.el ends here
