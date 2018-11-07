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

;; web (html, css, php, javascript)

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset lia/global-indent)
  (setq web-mode-css-indent-offset lia/global-indent)
  (setq web-mode-code-indent-offset lia/global-indent)
  :mode
  ("\\.php\\'" "\\.twig\\'" "\\.html?\\'")
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package emmet-mode
  ;; C-j to expand
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

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

(use-package markdown-mode
  :mode
  ("README\\.md\\'" "\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "multimarkdown"))

(provide 'lia-language)

;;; lia-language.el ends here
