;;; lia-completion.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Code completion and search engines

;;; Code:

(use-package company
  :ensure t
  :defer 1
  :general
  (company-active-map
   "C-n" #'company-select-next
   "C-p" #'company-select-previous)
  :init (setq company-idle-delay 0)
  :config (global-company-mode))

(use-package emmet-mode
  ;; C-j to expand
  :ensure t
  :hook (sgml-mode . emmet-mode))

(use-package helm
  :ensure t
  :defer t
  :general
  ("M-x"     'helm-M-x
   "C-x C-f" 'helm-find-files
   "C-x C-b" 'helm-mini))

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop
             helm-multi-swoop
             helm-multi-swoop-all))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

;; pair up delimiters
(electric-pair-mode t)

(provide 'lia-completion)

;;; lia-completion.el ends here
