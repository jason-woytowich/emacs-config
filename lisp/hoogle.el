;;; hoogle --- Query 'hoogle'

;;; Commentary:

;;; Code:

(require 'json)

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

;;;###autoload
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
                         (insert (format "- %s\n" (fontify-as-mode self 'haskell-mode)))
                       (with-text-properties (list 'face 'hoogle-package-face)
                           (insert (format "  %s\n" package)))
                       (with-text-properties (list 'face 'hoogle-description-face
                                                   'indent 4
                                                   'wrap-prefix "    "
                                                   'line-prefix "    ")

                           (insert (format "%s\n" docs))))))

      (goto-char pos))))


(defun hoogle-link-action ()
  (interactive)
  (let* ((pt     (point))
         (action (get-text-property pt 'action))
         (data   (get-text-property pt 'data)))
    (apply action data nil)))

(defun hoogle-goto-next ()
  (interactive)
  (let ((next (next-single-property-change (point) 'data nil (point-max))))
    (goto-char next)))

(defun hoogle-goto-prev ()
  (interactive)
  (let ((prev (previous-single-property-change (point) 'data nil (point-min))))
    (goto-char prev)))

(defun fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

(defvar hoogle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'hoogle-link-action)
    (define-key map (kbd "<return>") #'hoogle-link-action)
    (define-key map (kbd "<tab>") #'hoogle-goto-next)
    (define-key map (kbd "n") #'hoogle-goto-next)
    (define-key map (kbd "p") #'hoogle-goto-prev)
    map))

(defface hoogle-package-face
  '((t :foreground "grey30"))
  "Hoogle package name face"
  :group 'hoogle)

(defface hoogle-description-face
  '((t :height 90 :foreground "grey30"))
  "Hoogle function documentation face"
  :group 'hoogle)

(setq hoogle-font-lock-defaults
      '((("<[a-zA-Z0-9.]+>" 0 'hoogle-package-face t)
         ) t))

(define-derived-mode hoogle-mode special-mode "Hoogle"
  "Major mode for crushing it!"
  (use-local-map hoogle-mode-map)
;  (setq font-lock-defaults hoogle-font-lock-defaults)
  (visual-line-mode t))

(provide 'hoogle)

;;; hoogle.el ends here
