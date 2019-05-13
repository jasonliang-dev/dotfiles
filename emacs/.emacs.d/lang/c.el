;;; c.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for C and C++

;;; Code:

(use-package clang-format
  :ensure t
  :defer t
  :general
  (:keymaps
   '(c-mode-map c++-mode-map)
   "C-c C-f" #'clang-format-buffer))

;; .h files open in c++ mode by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'lang-c)

;;; c.el ends here
