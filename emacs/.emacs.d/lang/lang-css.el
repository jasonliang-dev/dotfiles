;;; lang-css.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for CSS, Less.js

;;; Code:

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package emmet-mode
  :ensure t
  :hook (css-mode . emmet-mode))

(add-hook 'css-mode-hook 'lsp)

(provide 'lang-css)

;;; lang-css.el ends here
