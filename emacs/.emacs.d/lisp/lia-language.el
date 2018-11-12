;;; lia-language.el --- Emacs Config

;;; Commentary:

;; language specific configs

;;; Code:

(require 'use-package)

;; c/c++

(use-package clang-format
  :general
  (:states
   '(normal visual insert emacs)
   :keymaps '(c-mode-map c++-mode-map)
   "C-c C-f" #'clang-format-buffer))

;; .h files open in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; web (html, css, php, javascript)

(use-package web-mode
  :mode
  ("\\.php\\'" "\\.twig\\'" "\\.html?\\'"))

(use-package less-css-mode)

(use-package emmet-mode
  ;; C-j to expand
  :hook ((web-mode . emmet-mode)
         (sgnl-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package company-tern
  :hook ((js-mode . tern-mode)
         (js-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

;; other

(use-package elm-mode)

(use-package markdown-mode
  :mode
  ("README\\.md\\'" "\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode)

(provide 'lia-language)

;;; lia-language.el ends here
