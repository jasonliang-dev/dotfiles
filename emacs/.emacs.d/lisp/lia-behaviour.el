;;; lia-behaviour.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Change how Emacs behaves

;;; Code:

(use-package avy
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :defer t)

(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package flycheck
  :ensure t
  :hook (prog-mode . global-flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defun lia--use-eslint-from-node-modules ()
    "If exists, use local eslint. https://emacs.stackexchange.com/q/21205"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'lia--use-eslint-from-node-modules))

(use-package magit
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :general
  (:states
   'normal
   :keymaps 'neotree-mode-map
   "h"   '+neotree/collapse-or-up
   "l"   '+neotree/expand-or-open)
  :init
  (setq neo-window-fixed-size nil
        neo-smart-open t
        neo-show-hidden-files t
        neo-theme 'icons
        neo-window-width 30)
  :config
  (doom-themes-neotree-config)
  (set-face-attribute 'doom-neotree-dir-face nil :family "Roboto Condensed")
  (set-face-attribute 'doom-neotree-file-face nil :family "Roboto Condensed"))

(use-package linum-relative
  :ensure t
  :defer t
  :init
  (setq linum-format " %4d " ;; add padding
        linum-relative-format " %4s "
        ;; display current line number
        linum-relative-current-symbol ""))

(use-package projectile
  :ensure t
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
  :ensure t
  :hook (after-init . which-key-mode))

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

;; better mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

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

;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bspacemacs/spacemacs-ui-visual/funcs.el#L27

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+neotree/collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/

;;;###autoload
(defun lia/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun lia-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'lia-remove-elc-on-save)

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
