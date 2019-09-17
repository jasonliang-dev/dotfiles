;;; lia-behaviour.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Change how Emacs behaves

;;; Code:

;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
;;;###autoload
(defun lia/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (defvar neo-auto-indent-point)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (defvar neo-auto-indent-point)
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

(defun lia/open ()
  "Open current file (or selected file if in dired mode) in an external program."
  (interactive)
  (call-process "xdg-open" nil 0 nil
                (if (eq major-mode 'dired-mode)
                    (dired-get-file-for-visit)
                  buffer-file-name)))

(use-package avy
  :ensure t
  :defer t)

(use-package dream-eater
  :disabled t
  :commands (global-dream-eater-mode)
  :bind
  (:map
   global-dream-eater-mode-map
   ([remap helm-find-files] . 'find-file))
  :init
  (setq dream-eater/check-out-name "Jason Liang"
        dream-eater/email lia-secret-email
        dream-eater/exclude-list '("COMMIT_EDITMSG" ".*\\.org$")))

(use-package dired
  :bind
  (:map
   dired-mode-map
   ("C-c C-o" . 'lia/open)))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go))

(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :commands (er/mark-word
             er/mark-symbol
             er/mark-symbol-with-prefix
             er/mark-next-accessor
             er/mark-method-call
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-comment
             er/mark-url
             er/mark-email
             er/mark-defun
             er/mark-html-attribute
             er/mark-inner-tag
             er/mark-outer-tag))

(use-package eyebrowse
  :ensure t
  :defer 1
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-clang-include-path (list (expand-file-name "~/local/include/")))

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

(use-package restclient
  :ensure t
  :mode ("\\.http\\'")
  :bind
  (:map
   restclient-mode-map
   ([remap eval-last-sexp] . 'restclient-http-send-current-stay-in-window)))

(use-package magit
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :init
  (setq neo-window-fixed-size nil
        neo-smart-open t
        neo-show-hidden-files t
        neo-theme 'icons
        neo-window-width 30)
  :config
  (doom-themes-neotree-config)

  ;; neotree bindins
  (eval-after-load 'evil
    '(progn
       (evil-define-key 'normal neotree-mode-map
         "h" '+neotree/collapse-or-up
         "l" '+neotree/expand-or-open))))

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
  :config
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; don't execute base16-shell when running a terminal
;; see: dotfiles/bash/.shell-start
(setenv "DISABLE_WAL" "true")

;; yes/no prompt is now y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; files that change on disk automatically get reverted
(global-auto-revert-mode t)

;; ask before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; don't show welcome screen
(setq inhibit-startup-screen t)

;; disable bell
(setq ring-bell-function 'ignore)

;; show column number
(setq column-number-mode t)

;; move backup~ files to its own directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

;; no #autosave# files
(setq auto-save-default nil)

;; no .#lock files
(setq create-lockfiles nil)

;; better mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; guess target directory when copying/moving files in dired
;; i.e. get drag and drop functionality with two dired windows in a split
(setq dired-dwim-target t)

(defun lia-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'lia-remove-elc-on-save)

;; hide details from dired. mainly show file names
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
