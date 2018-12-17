;;; lia-language.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; language specific configs

;;; Code:

;; c/c++

(use-package clang-format
  :defer t
  :general
  (:keymaps
   '(c-mode-map c++-mode-map)
   "C-c C-f" #'clang-format-buffer))

;; .h files open in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; web

(use-package web-mode
  :init (setq web-mode-enable-auto-pairing nil)
  :mode ("\\.html?\\'" "\\.twig\\'" "\\.vue\\'"))

(use-package emmet-mode
  ;; C-j to expand
  :hook ((css-mode . emmet-mode)
         (php-mode . emmet-mode)
         (sgnl-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (web-mode . emmet-mode)))

;; css

(use-package less-css-mode
  :defer t)

;; php

(use-package php-mode
  :mode "\\.php\\'")

;; javascript

(use-package js2-mode
  :general
  (:keymaps
   'js-mode-map
   [remap evil-jump-to-tag] 'js2-jump-to-definition)
  :mode "\\.js\\'"
  :hook (js2-mode . js2-imenu-extras-mode)
  :init
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil))

(use-package company-tern
  :hook ((js-mode . tern-mode)
         (js-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :magic ("/\\*\\* @jsx React\\.DOM \\*/" "^import React"))

;; other

(use-package elm-mode
  :defer t
  :init (setq elm-format-on-save t))

(use-package markdown-mode
  :mode
  ("README\\.md\\'" "\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :defer t)

(provide 'lia-language)

;;; lia-language.el ends here
