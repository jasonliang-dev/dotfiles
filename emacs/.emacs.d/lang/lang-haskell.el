;;; lang-haskell.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for Haskell

;;; Code:

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init (setq haskell-process-type 'stack-ghci))

(use-package flycheck-haskell
  :ensure t
  :hook (haskell-mode . flycheck-haskell-setup))

(provide 'lang-haskell)

;;; lang-haskell.el ends here
