;;; lang-markdown.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for Markdown

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" "\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "multimarkdown"))

(provide 'lang-markdown)

;;; lang-markdown.el ends here
