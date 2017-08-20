;;; reddit --- reddit on emacs

;;; Commentary:

;;; ToDo:
;;; - [ ] - Generalize the idea of sections
;;; - [ ] - Render targets
;;; - [ ] - Load more comments


;;; Code:

(require 'json)
(require 'url-http)

(defface reddit-new-face
  '((t :foreground "cornflower blue" :weight bold))
  "VISITED"
  :group 'reddit)

(defface reddit-comment-face
  '((t :foreground "cornflower blue"))
  "COMMENT"
  :group 'reddit)

(defface reddit-bold-face
  '((t :weight bold))
  "BOLD")

(defface reddit-extrabold-face
  '((t :weight ultrabold))
  "EXTRA BOLD")

(defface reddit-underline-face
  '((t :underline t))
  "BOLD")

(defface reddit-highlight-face
  '((t :background "grey20"))
  "Highlight article"
  :group 'reddit)

(defface reddit-visited-face
  '((t :foreground "slate blue"))
  "VISITED"
  :group 'reddit)

(defface reddit-interesting-face
  '((t :foreground "dark slate blue"))
  "INTERESTING"
  :group 'reddit)

(defface reddit-comment-author-face
  '((t :foreground "magenta"))
  "Reddit comment author"
  :group 'reddit)

(defface reddit-nsfw-face
  '((t :foreground "firebrick" :weight bold))
  "NSFW"
  :group 'reddit)

(defface reddit-deemphasized-face
  '((t :foreground "grey30"))
  "Reddit deemphasized face"
  :group 'reddit)

(defface reddit-host-face
  '((t :foreground "grey30"))
  "HOST"
  :group 'reddit)

(defface reddit-subreddit-face
  '((t :foreground "grey30"))
  "Subreddit"
  :group 'reddit)

(defvar reddit-hidden-selftext-text "...\n")

(defvar-local reddit-received-count 0)
(defvar-local reddit-last-received "")
(defvar-local reddit-subreddit nil)
(defvar-local reddit-order "hot")

(defvar reddit-visited-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reddit mode

(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'reddit-link-action)
    (define-key map (kbd "<return>") #'reddit-link-action)
    (define-key map (kbd "<tab>") #'reddit-goto-next-link-action)
    (define-key map (kbd "e") #'reddit-grab-more)
    (define-key map (kbd "c") #'reddit-open-comments)
    (define-key map (kbd "n") #'reddit-goto-next-article)
    (define-key map (kbd "o") #'reddit-change-order)
    (define-key map (kbd "p") #'reddit-goto-previous-article)
    (define-key map (kbd "g") #'reddit-refresh)
    map))

(setq reddit-font-lock-defaults
      '((("<[^>]+>" . 'reddit-deemphasized-face)
         ("^* .*" . 'reddit-new-face)
         ("^- .*" . 'reddit-visited-face)
         ("NSFW" 0 'reddit-nsfw-face t)
         ("\\[\\(\\w+\\)\\]" 1 'reddit-bold-face t)
         ("\\*\\*[^*
]+\\*\\*" 0 'reddit-extra-bold-face t)
         ("\\*[^*
]+\\*" 0 'reddit-bold-face t)
         ("/\\(r\\|u\\)/[[:alnum:]-_]+" 0 'reddit-interesting-face t)
         ) t))

(define-derived-mode reddit-mode special-mode "Reddit"
  "Major mode for crushing it!"
  (use-local-map reddit-mode-map)
  (setq font-lock-defaults reddit-font-lock-defaults)
  (visual-line-mode t))

;;; Commands

;;;###autoload
(defun reddit (&optional subreddit order)
  "Start reddit.  Optionally include SUBREDDIT or ORDER.  ORDER may be one of \"Hot\" \"New\"."
  (interactive)
  (unless order
    (setq order "hot"))
  (when current-prefix-arg
    (setq subreddit (read-minibuffer "Sub-reddit: "))
    (setq order (read-minibuffer "Order: " order)))
  (let* ((buffer-name (reddit-build-buffer-name subreddit order))
         (url (reddit-build-url "https" "www.reddit.com" subreddit order))
         (parsed (url-generic-parse-url url))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading...")
        (switch-to-buffer (current-buffer))))
    (url-https parsed #'reddit-handle-json (list #'reddit-render buffer subreddit order))))

(defun reddit-goto-next-link-action ()
  "Move the point forward to the next link action."
  (interactive)
  (message "Next link")
  (let* ((next (next-single-property-change (point) 'link-action))
         (nextnext (next-single-property-change next 'link-action)))
    (if (get-text-property next 'link-action)
        (goto-char next)
      (when (get-text-property nextnext 'link-action)
        (goto-char nextnext)))))


(defun reddit-goto-next-article ()
  "Move the point forward to the next article."
  (interactive)
  (let ((next (next-single-property-change (point) 'data)))
    (when next
      (goto-char next))))

(defun reddit-goto-previous-article ()
  "Move the point backward to the previous article."
  (interactive)
  (goto-char (previous-single-property-change (point) 'data nil (reddit-first-article-pos))))

(defun reddit-open-comments ()
  "Open the comments for the article at the point."
  (interactive)
  (reddit-comment-refresh (get-text-property (point) 'data)))

(defun reddit-refresh ()
  "Refresh this buffer."
  (interactive)
  (reddit reddit-subreddit reddit-order))

(defun reddit-change-order ()
  (interactive)
  (let ((order (read-minibuffer "Order: " reddit-order)))
    (reddit reddit-subreddit order)))

(defun reddit-link-action ()
  (interactive)
  (let ((action (get-text-property (point) 'link-action))
        (args (get-text-property (point) 'link-action-args)))
    (when action
      (apply action args))))

;; Link actions

(defun reddit-link-action-open (object)
  (reddit-visit object)
  (let ((region (reddit-find-article-title-region))
        (inhibit-read-only t))
    (add-text-properties (car region) (cdr region) (list 'face 'reddit-visited-face)))
  (if current-prefix-arg
      (start-process "open" "*Reddit Open*" "open" (reddit-get-data-item 'url object))
    (cond
     ((reddit-self-post-p object) (reddit-open-comments))
     (t (w3m (reddit-get-data-item 'url object))))))

(defun reddit-link-action-open-url (url)
  (if current-prefix-arg
      (start-process "open" "*Reddit Open*" "open" url)
    (w3m url)))

(defun reddit-link-action-open-subreddit (subreddit)
  (reddit subreddit))

;; Accessors

(defun reddit-get-data (object)
  "Extract the \"kind\" field from OBJECT."
  (when object
    (cdr (assoc 'data object))))

(defun reddit-get-kind (object)
  "Extract the \"kind\" field from OBJECT."
  (when object
    (cdr (assoc 'kind object))))

(defun reddit-listing-p (object)
  (when object
    (string-equal "Listing" (reddit-get-kind object))))

(defun reddit-listing-get-children (listing)
  (cdr (assoc 'children (reddit-get-data listing))))

(defun reddit-get-data-item (key object)
  (let ((data (reddit-get-data object)))
    (when data
      (cdr (assoc key data)))))

(defun reddit-get-subreddit (object)
  (reddit-get-data-item 'subreddit object))

(defun reddit-article-p (object)
  "Is OBJECT a reddit article?"
  (string-equal "t3" (reddit-get-kind object)))

(defun reddit-comment-p (object)
  "Is OBJECT a reddit comment"
  (string-equal "t1" (reddit-get-kind object)))

(defun reddit-more-p (object)
  (string-equal "more" (reddit-get-kind object)))

(defun reddit-self-post-p (object)
  "Is OBJECT a self post."
  (not (eq (reddit-get-data-item 'is_self object) :json-false)))

;;; Rendering

;; Tags
(defun reddit-render-subreddit (object)
  "Get the subreddit of OBJECT and propertize."
  (assert (or (reddit-article-p object) (reddit-comment-p object)))
  (let ((subreddit (reddit-get-subreddit object)))
    (propertize (format "/r/%s" subreddit)
                'link-action #'reddit-link-action-open-subreddit
                'link-action-args (list subreddit))))

(defun reddit-render-domain (article)
  "Get the link host of ARTICLE and propertize."
  (assert (reddit-article-p article))
  (reddit-get-data-item 'domain article))

(defun reddit-render-nsfw (article)
  (assert (reddit-article-p article))
  (let* ((over18 (reddit-get-data-item 'over_18 article))
         (nsfw-p (not (equal over18 :json-false))))
    (when nsfw-p "NSFW")))

(defun reddit-render-author (object)
  (assert (or (reddit-article-p object) (reddit-comment-p object)))
  (format "/u/%s" (reddit-get-data-item 'author object)))

(defun reddit-render-time (object)
  (assert (or (reddit-article-p object) (reddit-comment-p object)))
  (let* ((now (float-time))
         (then (reddit-get-data-item 'created_utc object))
         (time (round (- now then))))

     (let* ((seconds time)
            (minutes (/ seconds 60))
            (hours (/ minutes 60))
            (days (/ hours 24))
            (months (/ days 30))
            (years (/ days 365)))
       (cond
        ((> years 0) (format "%d years ago" years))
        ((> months 0) (format "%d months ago" months))
        ((> days 0) (format "%d days ago" days))
        ((> hours 0) (format "%d hours ago" hours))
        ((> minutes 0) (format "%d minutes ago" minutes))
        (t (format "%d seconds ago" seconds))))))

(defun reddit-render-comment-count (article)
  (assert (reddit-article-p article))
  (let ((comments (reddit-get-data-item 'num_comments article)))
    (format "Comments: %d" comments)))

(defun reddit-render-up-count (object)
  (assert (or (reddit-article-p object) (reddit-comment-p object)))
  (format "UPs: %s" (reddit-get-data-item 'ups object)))

(defun render-tags (elt &optional tags)
  (unless tags
    (setq tags '(reddit-render-subreddit
                 reddit-render-author
                 reddit-render-time
                 reddit-render-up-count
                 reddit-render-comment-count
                 reddit-render-domain
                 reddit-render-nsfw)))
  (--each tags
    (let ((tag (apply it elt nil)))
      (when tag
        (insert (format "<%s>" tag)))))
  (insert "\n"))

(defun render-title (article)
  (assert (reddit-article-p article))
  (let ((title (reddit-sanitize-text (reddit-get-data-item 'title article)))
        (visited (reddit-visited-p article)))
    (with-text-properties (list 'subsection 'title
                                'link-action #'reddit-link-action-open
                                'link-action-args (list article))
        (insert (format "%s %s\n" (if visited "-" "*") title)))))

(defun render-self (article &optional visible)
  (assert (reddit-article-p article))
  (let* ((selftext (reddit-get-data-item 'selftext article))
         (clean-selftext (reddit-sanitize-text selftext)))
    (when (and visible (reddit-self-post-p article))
      (with-text-properties (list 'subsection 'selftext)
          (insert (concat clean-selftext "\n"))))))

(defun reddit-visit (elt)
  (assert (reddit-article-p elt))
  (let ((id (reddit-get-data-item 'id elt)))
    (if (not (reddit-visited-p elt))
        (add-to-list 'reddit-visited-list id))))

(defun reddit-render-article (article &optional selfvisible)
  (assert (reddit-article-p article))
  (with-text-properties (list 'article (reddit-get-data-item 'name article)
                              'data article)
        (render-title article)
      (render-tags article)
      (render-self article selfvisible)))

(defun reddit-draw-sections (data sections)
  (--each sections
    (apply it (list data))))

(defun reddit-header (data)
  (with-text-properties '(section header)
      (if reddit-subreddit
          (insert (format "</r/%s/%s>" reddit-subreddit reddit-order))
        (insert (format "</%s>" reddit-order)))
    (insert (format "<Updated: %s>\n\n" (format-time-string "%a, %b %d %H:%M")))))

(defun reddit-render-articles (listing &optional selfvisible)
  (setq reddit-last-received (reddit-get-data-item 'after listing))
  (with-text-properties '(section articles)
    (let ((articles (reddit-listing-get-children listing)))
      (setq reddit-received-count (+ reddit-received-count (length articles)))
      (mapc (lambda (it)
              (reddit-render-article it selfvisible))
            articles))))

(defun reddit-footer (data)
  (with-text-properties '(section footer)
      (insert "\n<Press 'e' to load more article>\n")))

(defvar reddit-sections '(reddit-header
                          reddit-render-articles
                          reddit-footer))

(defun reddit-render (data buffer subreddit order)
  "Render the reddit data into BUFFER."
  (with-current-buffer buffer
    (reddit-mode)
    (setq reddit-subreddit subreddit)
    (setq reddit-order order)
    (setq reddit-received-count 0)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (reddit-draw-sections data reddit-sections)
      (reddit-goto-first-article))
    (switch-to-buffer (current-buffer))))

(defun reddit-replace-html-syms (sym)
  (cond
   ((string= "&lt;" sym) "<")
   ((string= "&gt;" sym) ">")
   ((string= "&amp;" sym) "&")
   (t sym)))

(defun reddit-make-link (text url)
  (propertize (format "_%s_" text)
              'link-action #'reddit-link-action-open-url
              'link-action-args (list url)))

(defun reddit-propertize-links (text)
  (while (string-match "\\[\\([^\]]*\\)\\](\\([^\)]*\\))" text)
    (setq text (replace-match (reddit-make-link (match-string 1 text) (match-string 2 text)) nil nil text)))
  text)

(defun reddit-sanitize-text (text)
  (let ((linked (reddit-propertize-links text)))
    (replace-regexp-in-string "&[[:alpha:]]+;" #'reddit-replace-html-syms linked)))

(defun reddit-visited-p (object)
  (member (reddit-get-data-item 'id object) reddit-visited-list))

(defmacro with-text-properties (props &rest body)
  (declare (indent 2))
  `(let* ((start (point))
          (result (progn
                    ,@body))
          (end (point)))
     (add-text-properties start end ,props)
     result))

(defun reddit-build-buffer-name (subreddit order)
  (if subreddit
      (format "*Reddit-%s*" subreddit)
    "*Reddit*"))

(defun reddit-build-query-string (query)
  (string-join
   (mapcar (lambda (it)
             (format "%s=%s"
                     (car it)
                     (cdr it)))
           query) "&"))

(defun reddit-build-url (type host subreddit order &optional query)
  (let* ((path (if subreddit (format "r/%s/%s.json" subreddit order)
                 (format "%s.json" order)))
         (query-string
         (when query
           (concat "?" (reddit-build-query-string query)))))
    (concat type "://" host "/" path query-string)))

(defun make-region (&optional start end)
  (cons (or start (point-min)) (or end (point-max))))

(defun reddit-find-property (prop val &optional region)
  (setq region (or region (make-region)))
  (let ((pt (car region))
        (limit (cdr region)))
    (while (and pt
                (< pt limit)
                (not (equal val (get-text-property pt prop))))
      (setq pt (next-single-property-change pt prop nil limit)))
    (when (and pt (< pt limit)) pt)))

(defun reddit-find-property-region (prop value &optional region)
  (setq region (or region (make-region)))
  (let ((region-start (reddit-find-property prop value region)))
    (when region-start
      (make-region region-start (next-single-property-change region-start prop nil (cdr region))))))

(defun reddit-find-article-title-region (&optional article)
  (unless article
    (setq article (get-text-property (point) 'article)))
  (let ((article-region (reddit-find-property-region 'article article)))
    (when article-region
      (reddit-find-property-region 'subsection 'title article-region))))

(defun reddit-find-article-selftext-region (&optional article)
  (unless article
    (setq article (get-text-property (point) 'article)))
  (let ((article-region (reddit-find-property-region 'article article)))
    (when article-region
      (reddit-find-property-region 'subsection 'selftext article-region))))

(defun reddit-next-action (&optional start)
  (interactive)
  (unless start
    (setq start (point)))
  (let ((next (next-single-property-change start 'link-action)))
    (when next
      (let ((link (get-text-property next 'link-action)))
        (if link (goto-char next)
          (reddit-next-action next))))))

;;; Movement:

(defun reddit-first-article-pos ()
  (or (next-single-property-change (point-min) 'section) (point-max)))

(defun reddit-goto-first-article ()
  (goto-char (reddit-first-article-pos)))

(defun reddit-end-of-articles-pos ()
  (let ((pt (reddit-find-property 'section 'articles)))
    (when pt (next-single-property-change pt 'section))))

(defun reddit-goto-end-of-articles ()
  (interactive)
  (let ((end (reddit-end-of-articles-pos)))
    (when end (goto-char end))))

(defun reddit-unescape (str)
  (replace-regexp-in-string "\\/" "/" str))



(defun reddit-render-more (data buffer-name)
  (with-current-buffer (get-buffer buffer-name)
    (save-excursion
      (reddit-goto-end-of-articles)
      (let ((inhibit-read-only t))
        (reddit-render-articles data)))))

(defun reddit-handle-json (handler &rest args)
  (goto-char url-http-end-of-headers)
  (let ((data (json-read)))
    (apply handler data args)))

(defun reddit-grab-more ()
  "Grab reddit summary."
  (interactive)
  (let* ((buffer-name (reddit-build-buffer-name reddit-subreddit reddit-order))
         (query `((count . ,reddit-received-count)
                  (after . ,reddit-last-received)))
         (url (reddit-build-url "https" "www.reddit.com" reddit-subreddit reddit-order query))
         (parsed (url-generic-parse-url url))
         (buffer (get-buffer-create buffer-name))
         (url-reqeust-method "GET"))
    (url-https parsed #'reddit-handle-json (list #'reddit-render-more buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reddit comments mode

(defvar reddit-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'reddit-link-action)
    (define-key map (kbd "SPC") #'reddit-link-action)
    (define-key map (kbd "<tab>") #'reddit-goto-next-link-action)
    (define-key map (kbd "g") #'reddit-comment-refresh)
    (define-key map (kbd "n") #'reddit-comment-goto-next)
    (define-key map (kbd "p") #'reddit-comment-goto-previous)
    (define-key map (kbd "f") #'reddit-comment-go-forward)
    (define-key map (kbd "b") #'reddit-comment-go-backward)
    map))

(define-derived-mode reddit-comment-mode special-mode "Reddit-Comments"
  "Major mode for crushing it!"
  (use-local-map reddit-comment-mode-map)
  (setq font-lock-defaults reddit-font-lock-defaults)
  (visual-line-mode t)
  (let ((author (reddit-get-data-item 'author (get-text-property (point-min) 'data))))
    (font-lock-add-keywords 'reddit-comment-mode
                            `((,(concat "/u/" author) 0 'reddit-comment-author-face t)) t)))


;;; Commands

(defun reddit-comment-goto-next ()
  "Move the point forward to the next comment."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'comment) (point-max))))

(defun reddit-comment-goto-previous ()
  "Move the point backward to the previous comment."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'comment) (point-min))))


(defun reddit-comment-go-backward ()
  (interactive)
  (reddit-comment-goto-previous))

(defun reddit-comment-refresh (&optional article)
  (interactive)
  (unless article
    (setq article (get-text-property (point-min) 'data)))
  (let* ((permalink (reddit-unescape (reddit-get-data-item 'permalink article)))
         (path (concat permalink ".json"))
         (buffer-name (format "*Reddit-Comments-%s*" (reddit-get-data-item 'name article)))
         (url (concat "https://www.reddit.com" path))
         (parsed (url-generic-parse-url url))
         (url-request-method "GET"))
    (url-https parsed #'reddit-handle-json (list #'reddit-handle-comments buffer-name))))

;;; Rendering

(defun reddit-build-indent (indent)
  (let ((result ""))
    (--dotimes indent
      (setq result (concat result "| ")))
  result))

(defun reddit-render-hrule ()
  (insert "<")
  (--dotimes 80
    (insert "-"))
  (insert ">\n"))

(defun reddit-render-comment (comment indent)
  "Render COMMENT.  INDENT as needed."
  (let ((indent-string (reddit-build-indent indent)))
    (cond
     ((reddit-comment-p comment)
      (progn
        (let ((comment_id (reddit-get-data-item 'name comment)))
          (with-text-properties (list 'comment comment_id
                                      'indent indent
                                      'wrap-prefix indent-string
                                      'line-prefix indent-string)
              (insert (format "%s\n" (reddit-sanitize-text (reddit-get-data-item 'body comment))))
            (render-tags comment '(reddit-render-author
                                   reddit-render-time
                                   reddit-render-up-count))))
        (let ((replies (reddit-get-data-item 'replies comment)))
          (when (not (stringp replies))
            (reddit-render-comments replies (+ indent 1))))))
     ((reddit-more-p comment)
      (with-text-properties (list 'wrap-prefix indent-string
                                  'line-prefix indent-string
                                  'indent indent)
          (insert "[more]\n"))))))

(defun reddit-render-comments (listing indent)
  "Render all comments in LISTING."
  (let ((comments (reddit-listing-get-children listing)))
    (mapc (lambda (comment) (reddit-render-comment comment indent)) comments)))

;;; Received data handlers

(defun reddit-handle-comments (data buffer-name)
  "Handle the received comment DATA.  Put the results in a buffer called BUFFER-NAME."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (let ((articles (elt data 0))
            (comments (elt data 1)))
        (erase-buffer)
        (reddit-render-articles articles t)
        (reddit-render-hrule)
        (reddit-render-comments comments 0)
        (reddit-comment-mode)))
    (goto-char 0)
    (switch-to-buffer (current-buffer))))

(provide 'reddit)
;;; reddit.el ends here
