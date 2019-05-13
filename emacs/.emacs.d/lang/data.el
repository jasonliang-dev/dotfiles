;;; data.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for JSON, YAML

;;; Code:

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" ".eslintrc\\'" ".prettierrc\\'"))

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(provide 'lang-data)

;;; data.el ends here
