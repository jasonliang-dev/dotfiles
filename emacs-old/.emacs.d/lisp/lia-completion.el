;;; lia-completion.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Code completion and search engines

;;; Code:

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind
  (:map
   company-active-map
   ("C-n" . #'company-select-next)
   ("C-p" . #'company-select-previous))
  :init
  ;; don't delay autocomplete suggesstions
  ;; (setq company-idle-delay 0)

  ;; popup completions after typing a single character
  (setq company-minimum-prefix-length 1))

(use-package emmet-mode
  ;; C-j to expand
  :ensure t
  :hook (sgml-mode . emmet-mode))

(use-package helm
  :ensure t
  :bind
  (("M-x" . 'helm-M-x)
   ("C-x C-f" . 'helm-find-files)
   ("C-x C-b" . 'helm-mini))
  :init
  (eval-after-load 'lia-keybind
    '(progn
       (lia-bind-leader "SPC" 'helm-M-x)
       (lia-bind-leader "b"   'helm-mini)
       (lia-bind-leader "f"   'helm-find-files))))

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop
             helm-multi-swoop
             helm-multi-swoop-all)
  :init
  (eval-after-load 'lia-keybind
    '(progn
       (lia-bind-leader "sS" 'helm-multi-swoop-all)
       (lia-bind-leader "ss" 'helm-swoop))))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init
  (eval-after-load 'lia-keybind
    '(progn
       (lia-bind-leader "pb"  'helm-projectile-switch-to-buffer)
       (lia-bind-leader "pd"  'helm-projectile-find-dir)
       (lia-bind-leader "pf"  'helm-projectile-find-file)
       (lia-bind-leader "pF"  'helm-projectile-find-file-dwim)
       (lia-bind-leader "ph"  'helm-projectile)
       (lia-bind-leader "pp"  'helm-projectile-switch-project)
       (lia-bind-leader "pr"  'helm-projectile-recentf)
       (lia-bind-leader "sgp" 'helm-projectile-grep))))

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
