;;; lia-language.el --- Emacs Config

;;; Commentary:

;;
;; language specific configs
;;

;;; Code:

(require 'use-package)

;; c/c++

(use-package clang-format
  :config
  (defun lia/format-buffer-binding ()
    (local-set-key (kbd "C-c C-f") #'clang-format-buffer))

  (add-hook 'c-mode-hook 'lia/format-buffer-binding)
  (add-hook 'c++-mode-hook 'lia/format-buffer-binding))

;; html/css

(use-package emmet-mode
  ;; C-j to expand
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; javascript

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js-mode-hook (lambda ()
                            (tern-mode)
                            (company-mode))))

(use-package prettier-js
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode))

;; other

(use-package elm-mode)

(provide 'lia-language)

;;; lia-language.el ends here
