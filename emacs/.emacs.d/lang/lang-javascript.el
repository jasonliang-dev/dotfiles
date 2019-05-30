;;; lang-javascript.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for JavaScript and React

;;; Code:

(use-package emmet-mode
  :ensure t
  :hook (rjsx-mode . emmet-mode))

(use-package js2-mode
  :ensure t
  :general
  (:keymaps
   'js-mode-map
   [remap evil-jump-to-tag] 'js2-jump-to-definition)
  :mode "\\.js\\'"
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . lsp))
  :init
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :magic ("/\\*\\* @jsx React\\.DOM \\*/" "^import React"))

(provide 'lang-javascript)

;;; lang-javascript.el ends here
