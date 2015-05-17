;;; reddit --- reddit on emacs

;;; Commentary:

;;; ToDo:
;;; - [ ] - Fix inline self posts
;;; - [ ] - Generalize the idea of sections
;;; - [ ] - Create a self post page
;;; - [ ] - Render targets
;;; - [ ] - Create an imgur page
;;; - [ ] - Create a shortcut to opening in external browser

;;; Code:

(require 'json)
(require 'url-http)
(require 'dash)

(defface reddit-new-face
  '((t :foreground "cornflower blue" :weight bold))
  "VISITED"
  :group 'reddit)

(defface reddit-comment-face
  '((t :foreground "cornflower blue"))
  "COMMENT"
  :group 'reddit)

(defface reddit-highlight-face
  '((t :background "grey20"))
  "Highlight article"
  :group 'reddit)

(defface reddit-visited-face
  '((t :foreground "dark slate blue"))
  "VISITED"
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

(defvar-local reddit-received-count 0)
(defvar-local reddit-last-received "")
(defvar-local reddit-subreddit nil)
(defvar-local reddit-order "hot")

(defvar reddit-visited-list '())

(defun reddit-get-subreddit (elt)
  "Get the subreddit of ELT and propertize."
  (propertize (format "/r/%s" (cdr (assoc 'subreddit elt)))
              'link-action #'reddit-link-action-open-subreddit
              'face 'reddit-subreddit-face))

(defun reddit-get-domain (elt)
  "Get the link host of ELT and propertize."
  (let ((domain (cdr (assoc 'domain elt))))
    (propertize domain 'face 'reddit-host-face)))

(defun reddit-get-nsfw (elt)
  (let* ((over18 (cdr (assoc 'over_18 elt)))
         (nsfw-p (not (equal over18 :json-false))))
    (when nsfw-p (propertize "NSFW" 'face 'reddit-nsfw-face))))

(defun reddit-get-author (elt)
  (let ((author (cdr (assoc 'author elt))))
    (propertize (format "/u/%s" author) 'face 'reddit-deemphasized-face)))

(defun reddit-get-time (elt)
  (let* ((now (float-time))
         (then (cdr (assoc 'created_utc elt)))
         (time (round (- now then))))

    (propertize
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
        (t (format "%d seconds ago" seconds))))
     'face 'reddit-host-face)))

(defun reddit-get-comments (elt)
  (let ((comments (cdr (assoc 'num_comments elt))))
    (propertize (format "Comments: %d" comments) 'face 'reddit-host-face)))

(defun reddit-get-votes (elt)
  (let ((ups (cdr (assoc 'ups elt))))
    (propertize (format "Votes: %s" ups) 'face 'reddit-host-face)))

(defun render-tags (elt &optional tags)
  (unless tags
    (setq tags '(reddit-get-subreddit
                 reddit-get-author
                 reddit-get-time
                 reddit-get-votes
                 reddit-get-comments
                 reddit-get-domain
                 reddit-get-nsfw)))
  (--each tags
    (let ((tag (apply it elt nil)))
      (when tag
        (insert (format " %s " tag)))))
  (insert "\n"))

(defun render-title (elt)
  (let ((title (reddit-sanitize-text (cdr (assoc 'title elt)))))
    (with-text-properties (list 'face (if (reddit-visited-p elt)
                                          'reddit-visited-face
                                        'reddit-new-face)
                                'subsection 'title
                                'link-action #'reddit-link-action-open)
        (insert (format "%s\n" title)))))

(defun reddit-replace-html-syms (sym)
  (cond
   ((string= "&lt;" sym) "<")
   ((string= "&gt;" sym) ">")
   (t sym)))

(defvar reddit-hidden-selftext-text "...\n")

(defun reddit-sanitize-text (text)
  (replace-regexp-in-string "&[[:alpha:]]+;" #'reddit-replace-html-syms text))

(defun render-self (elt &optional visible)
  (let* ((selftext (cdr (assoc 'selftext elt)))
         (clean-selftext (reddit-sanitize-text selftext))
         (props (if visible (list 'subsection 'selftext
                                  'state 'visible)
                  (list 'display reddit-hidden-selftext-text
                                  'subsection 'selftext
                                  'state 'hidden))))
    (when (reddit-self-post-p elt)
      (with-text-properties props
          (insert (concat clean-selftext "\n"))))))

(defun reddit-visited-p (elt)
  (let ((id (cdr (assoc 'id elt))))
    (member id reddit-visited-list)))

(defun reddit-visit (elt)
  (let ((id (cdr (assoc 'id elt))))
    (if (not (reddit-visited-p elt))
        (add-to-list 'reddit-visited-list id))))

(defmacro with-text-properties (props &rest body)
  (declare (indent 2))
  `(let* ((start (point))
          (result (progn
                    ,@body))
          (end (point)))
     (add-text-properties start end ,props)
     result))

(defun reddit-render-article (data &optional selfvisible)
  (with-text-properties (list 'article (cdr (assoc 'name data))
                                'data data)
        (render-title data)
      (render-tags data)
      (render-self data selfvisible)))

(defun reddit-draw-sections (data sections)
  (--each sections
    (apply it (list data))))

(defun reddit-header (data)
  (with-text-properties '(face reddit-deemphasized-face section header)
      (if reddit-subreddit
          (insert (format "/r/%s/%s" reddit-subreddit reddit-order))
        (insert (format "Reddit/%s" reddit-order)))
    (insert (format " Updated: %s\n\n" (format-time-string "%a, %b %d %H:%M")))))


(defun reddit-render-articles (data &optional selfvisible)
  (setq reddit-last-received (cdr (assoc 'after (cdr (assoc 'data data)))))
  (with-text-properties '(section articles)
    (let ((articles (cdr (assoc 'children (cdr (assoc 'data data))))))
      (setq reddit-received-count (+ reddit-received-count (length articles)))
      (mapc (lambda (it)
              (let ((article-data (cdr (assoc 'data it))))
                (reddit-render-article article-data selfvisible)))
            articles))))


(defun reddit-footer (data)
  (with-text-properties '(face reddit-deemphasized-face
                          section footer)
      (insert "\nPress 'e' to load more article\n")))


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

(defun reddit (&optional subreddit order)
  "Grab reddit summary."
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

(defun reddit-self-post-p (elt)
  (not (eq (cdr (assoc 'is_self elt)) :json-false)))

(defun reddit-toggle-self-visibility ()
  (interactive)
  (let ((self-region (reddit-find-article-selftext-region)))
    (when self-region
      (let* ((start (car self-region))
             (end (cdr self-region))
             (hidden (equal 'hidden (get-text-property start 'state)))
             (inhibit-read-only t))
        (if hidden
            (progn
              (remove-text-properties start end '(display))
              (add-text-properties start end
                                 (list 'state 'visible)))
          (progn
            (add-text-properties start end
                                 (list 'display reddit-hidden-selftext-text
                                       'state 'hidden))))))))

;;; Link actions:

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

(defun reddit-link-action-open (elt)
  (reddit-visit elt)
  (let ((region (reddit-find-article-title-region))
        (inhibit-read-only t))
    (add-text-properties (car region) (cdr region) (list 'face 'reddit-visited-face)))
  (if current-prefix-arg
      (start-process "open" "*Reddit Open*" "open" (cdr (assoc 'url elt)))
  (cond
   ((reddit-self-post-p elt) (reddit-toggle-self-visibility))
   (t (w3m (cdr (assoc 'url elt)))))))

(defun reddit-link-action-open-subreddit (elt)
  (let ((subreddit (cdr (assoc 'subreddit elt))))
    (reddit subreddit)))

(defun reddit-link-action ()
  (interactive)
  (let ((action (get-text-property (point) 'link-action)))
    (when action
      (apply action (get-text-property (point) 'data) nil))))

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

(defun reddit-goto-next-article ()
  (interactive)
  (let ((next (next-single-property-change (point) 'data)))
    (when next
      (goto-char next))))

(defun reddit-goto-previous-article ()
  (interactive)
  (goto-char (previous-single-property-change (point) 'data nil (reddit-first-article-pos))))

(defun reddit-refresh ()
  (interactive)
  (reddit reddit-subreddit reddit-order))

(defun reddit-change-order ()
  (interactive)
  (let ((order (read-minibuffer "Order: " reddit-order)))
    (reddit reddit-subreddit order)))

(defun reddit-unescape (str)
  (replace-regexp-in-string "\\/" "/" str))

(defun reddit-render-comment (comment &optional indent)
  (unless indent
    (setq indent 0))
  (let* ((data (cdr (assoc 'data comment)))
         (comment_id (cdr (assoc 'name data))))
    (with-text-properties (list 'comment comment_id)
        (with-text-properties '(face reddit-comment-face)
            (insert (format "%s\n" (cdr (assoc 'body data)))))
        (with-text-properties '(face reddit-deemphasized-face)
            (render-tags data '(reddit-get-author
                                reddit-get-time
                                reddit-get-votes))))))

(defun reddit-comment-goto-next ()
  (interactive)
  (goto-char (or (next-single-property-change (point) 'comment) (point-max))))

(defun reddit-comment-goto-previous ()
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'comment) (point-min))))

(defun reddit-render-comments (comments)
  (let ((all (cdr (assoc 'children (cdr (assoc 'data comments))))))
    (mapc #'reddit-render-comment all)))

(defun reddit-handle-comments (data buffer-name)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (let ((articles (elt data 0))
            (comments (elt data 1)))
        (erase-buffer)
        (reddit-render-articles articles t)
        (reddit-render-comments comments)
        (reddit-comment-mode)))
    (goto-char 0)
    (switch-to-buffer (current-buffer))))


(defun reddit-open-comments ()
  (interactive)
  (let* ((data (get-text-property (point) 'data))
         (permalink (reddit-unescape (cdr (assoc 'permalink data))))
         (path (concat permalink ".json"))
         (buffer-name (format "*Reddit-Comments-%s*" (cdr (assoc 'name data))))
         (url (concat "https://www.reddit.com" path))
         (parsed (url-generic-parse-url url))
         (url-request-method "GET"))
    (url-https parsed #'reddit-handle-json (list #'reddit-handle-comments buffer-name))))

(defun reddit-comment-refresh ()
  (interactive)
  (let* ((data (get-text-property (point-min) 'data))
         (permalink (reddit-unescape (cdr (assoc 'permalink data))))
         (path (concat permalink ".json"))
         (buffer-name (format "*Reddit-Comments-%s*" (cdr (assoc 'name data))))
         (url (concat "https://www.reddit.com" path))
         (parsed (url-generic-parse-url url))
         (url-request-method "GET"))
    (url-https parsed #'reddit-handle-json (list #'reddit-handle-comments buffer-name))))


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


(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'reddit-link-action)
    (define-key map (kbd "<return>") #'reddit-link-action)
    (define-key map (kbd "<tab>") #'reddit-goto-next-article)
    (define-key map (kbd "e") #'reddit-grab-more)
    (define-key map (kbd "c") #'reddit-open-comments)
    (define-key map (kbd "n") #'reddit-goto-next-article)
    (define-key map (kbd "o") #'reddit-change-order)
    (define-key map (kbd "p") #'reddit-goto-previous-article)
    (define-key map (kbd "g") #'reddit-refresh)
    map))

(define-derived-mode reddit-mode special-mode "Reddit"
  "Major mode for crushing it!"
  (use-local-map reddit-mode-map)
  (visual-line-mode t))

(defvar reddit-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'reddit-comment-refresh)
    (define-key map (kbd "n") #'reddit-comment-goto-next)
    (define-key map (kbd "p") #'reddit-comment-goto-previous)
    map))

(define-derived-mode reddit-comment-mode special-mode "Comments"
  "Major mode for crushing it!"
  (use-local-map reddit-comment-mode-map)
  (visual-line-mode t))


(provide 'reddit)
;;; reddit.el ends here
