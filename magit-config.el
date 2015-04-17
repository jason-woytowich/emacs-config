;;; magit-config --- Local magit configuration

;;; Commentary:

;;; Code:
(require 'magit)

(setq
 magit-last-seen-setup-instructions "1.4.0"
 magit-use-overlays nil)


(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  (jump-to-register :magit-fullscreen))

(provide 'magit-config)
;;;  magit-config ends here
