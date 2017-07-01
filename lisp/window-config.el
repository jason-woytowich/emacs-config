(defun jfw-split-window (window)
  (if (and (one-window-p t)
           (not (active-minibuffer-window))
           (> (window-width window) 200))
      (split-window-horizontally)))

(setq split-window-preferred-function 'jfw-split-window)

(defun jfw-display-buffer-give-up-and-use-same-window (buffer alist)
  (let ((copied-alist (copy-alist alist)))
    (setcdr (assq 'inhibit-same-window copied-alist) nil)
    (display-buffer-same-window buffer copied-alist)))

(customize-set-variable 'display-buffer-base-action
                        (cons '(display-buffer--maybe-same-window
                                display-buffer-reuse-window
                                display-buffer--maybe-pop-up-frame-or-window
                                display-buffer-in-previous-window display-buffer-use-some-window
                                jfw-display-buffer-give-up-and-use-same-window)
                              nil)
                        "Seriously, why isn't this an option")

(provide 'window-config)
;;; window-config.el ends here
