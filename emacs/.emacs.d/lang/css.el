;;; css.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for CSS, Less.js

;;; Code:

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package emmet-mode
  :ensure t
  :hook (css-mode . emmet-mode))

(provide 'lang-css)

;;; css.el ends here
