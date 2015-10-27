;;; haskell-config --- My haskell configuration
;;; Commentary:
;;; Code:

(require 'haskell-mode)
(require 'flycheck-haskell)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)

(define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)

(provide 'haskell-config)

;;; haskell-config.el ends here
