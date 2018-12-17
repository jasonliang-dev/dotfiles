;; -*- lexical-binding: t; -*-

;;; lia-behaviour.el --- Emacs Config

;;; Commentary:

;; Change how Emacs behaves

;;; Code:

(use-package autopair
  :hook (after-init . autopair-global-mode))

(use-package avy
  :defer t)

(use-package company
  :hook (after-init . global-company-mode)
  :general
  (company-active-map
   "C-n" #'company-select-next
   "C-p" #'company-select-previous))

(use-package dumb-jump
  :defer t)

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defun lia/use-eslint-from-node-modules ()
    "If exists, use local eslint. https://emacs.stackexchange.com/q/21205"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'lia/use-eslint-from-node-modules))

(use-package magit
  :defer t
  :init
  (with-eval-after-load 'magit
    (require 'evil-magit)))

(use-package helm
  :defer t
  :general
  ("M-x"     'helm-M-x
   "C-x C-f" 'helm-find-files
   "C-x C-b" 'helm-mini)
  :config
  (require 'helm-config))

(use-package helm-swoop
  :defer t)

(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project))

(use-package linum-relative
  :defer t
  :init
  (setq linum-format " %4d " ;; add padding
        linum-relative-format " %4s "
        ;; display current line number
        linum-relative-current-symbol ""))

(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init (setq projectile-enable-caching t)
  :config (projectile-mode t))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :defer t)

;; yes/no prompt is now y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show welcome screen
(setq inhibit-startup-screen t)

;; disable bell
(setq ring-bell-function 'ignore)

;; show column number
(setq column-number-mode t)

;; tabs are the enemy
(setq-default indent-tabs-mode nil)

;; move backup~ files to its own directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

;; no #autosave# files
(setq auto-save-default nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ;; keyboard scroll one line at a time

;; indent `case' in switch/case
(c-set-offset 'case-label '+)

(defun lia/set-indent (n)
  "Set the indentation level to N spaces."
  (interactive)
  (setq-default c-basic-offset n)
  (setq-default javascript-indent-level n)
  (setq-default js-indent-level n)
  (setq-default js-switch-indent-offset n) ; switch-case indentation
  (setq-default css-indent-offset n)
  (setq-default web-mode-markup-indent-offset n)
  (setq-default web-mode-css-indent-offset n)
  (setq-default web-mode-code-indent-offset n))

(lia/set-indent 2)

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/

(defun lia/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun lia/remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'lia/remove-elc-on-save)

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
