;;; lang-web.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for HTML

;;; Code:

(use-package web-mode
  :ensure t
  :init (setq web-mode-enable-auto-pairing nil)
  :mode ("\\.html?\\'" "\\.php?\\'" "\\.twig\\'" "\\.vue\\'")
  :hook (web-mode . lsp))

(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode))

(provide 'lang-web)

;;; lang-web.el ends here
