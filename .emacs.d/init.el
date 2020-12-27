;;; -*- lexical-binding: t; -*-


;; NOTE: if emacs displays "Failed to download 'gnu' archive", here is
;; a quick and dirty solution:

;; (setq package-check-signature nil)
;; (package-refresh-contents)
;; (package-install 'undo-tree)

;; undo-tree is the only package I need from gnu elpa. so that's why
;; it's listed above.


;; -- EARLY INITIALIZATION -------------------------------------------


;; boost startup time, values are set again near the end of this file
(defvar lia-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; hide ugly gui
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; don't resize frame (window) when setting to a new font
(setq frame-inhibit-implied-resize t)

;; set font
(setq default-frame-alist '((font . "Roboto Mono Bold 9")))

;; yes/no prompt is now y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't load site-start
(setq site-run-file nil)

;; hide startup bloat
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil)

;; leader key prefix
(defvar lia-leader-map (make-sparse-keymap))


;; -- USE PACKAGE BOOTSTRAP ------------------------------------------


(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))


;; -- PACKAGES -------------------------------------------------------


;; super clean looking modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35
        doom-modeline-icon nil
        doom-modeline-buffer-file-name-style 'buffer-name)
  :config
  (doom-modeline-mode 1))

;; awesome theme collection
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

;; respect `.editorconfig' files
;; https://editorconfig.org/
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; undo system used for evil. also the visualizer is kinda cool (C-x u)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; I wouldn't be using emacs if it weren't for you, `evil-mode'. Vim
;; emuation in Emacs.
(use-package evil
  :ensure t
  :init
  ;; required for `evil-collection'
  (setq evil-want-keybinding nil)
  ;; half page up with C-u
  (setq evil-want-C-u-scroll t)
  ;; emacs movement in insert mode
  (setq evil-disable-insert-state-bindings t)
  ;; vim search behaviour
  (setq evil-search-module 'evil-search)
  ;; use undo tree
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode)
  ;; move by visual line
  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
  ;; leader key
  (define-key evil-normal-state-map " " lia-leader-map)
  (define-key evil-visual-state-map " " lia-leader-map)
  (define-key evil-motion-state-map (kbd "C-SPC") lia-leader-map)
  (define-key lia-leader-map "w" evil-window-map)
  (define-key lia-leader-map "f" 'find-file)
  (define-key lia-leader-map (kbd "TAB") 'mode-line-other-buffer)
  (define-key lia-leader-map (kbd "ESC") 'evil-ex-nohighlight)
  (define-key lia-leader-map (kbd "<f5>") 'revert-buffer))

;; vim keybindings for major modes that evil mode doesn't cover
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

;; multiple cursors! but specifically for evil mode? ok.
(use-package evil-mc
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "M-n") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-visual-state-map (kbd "M-n") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "M-p") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-visual-state-map (kbd "M-p") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map "grm" 'evil-mc-make-all-cursors)
  (define-key evil-visual-state-map "grm" 'evil-mc-make-all-cursors)
  (define-key evil-normal-state-map "grq" 'evil-mc-undo-all-cursors)
  (define-key lia-leader-map "rm" 'evil-mc-make-all-cursors)
  (define-key lia-leader-map "rq" 'evil-mc-undo-all-cursors)
  (global-evil-mc-mode 1))

;; quickly add, change, remove delimiters with evil mode bindings
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; select a region between two quotes, brackets, etc. convenient.
(use-package expand-region
  :ensure t
  :config
  (define-key lia-leader-map (kbd "v") 'er/expand-region))

;; syntax highlighting
(use-package flycheck)

;; format all of your ugly code with a press of a key
(use-package format-all
  :ensure t
  :config
  (define-key lia-leader-map (kbd "=") 'format-all-buffer))

;; completion for commands, file names, buffers, and so much more.
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (define-key lia-leader-map "b" 'ivy-switch-buffer))

;; an excellent git frontend
(use-package magit
  :ensure t
  :config
  (define-key lia-leader-map "g" 'magit-status))

;; cool features for working with projects. find files, swap between
;; .h/.c, grep in project, compile command, and so much more
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (define-key lia-leader-map "p" projectile-command-map))

;; use ivy completion for projectile commands
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; powerful Ctrl-f
(use-package swiper
  :ensure t
  :config
  (define-key lia-leader-map "s" 'swiper))

;; track the time spent programming
(use-package wakatime-mode
  :ensure t
  :init
  (setq wakatime-api-key (string-trim-right
                          (with-temp-buffer
                            (insert-file-contents
                             (concat user-emacs-directory "wakatime-api-key.txt"))
                            (buffer-string))))
  :config
  (global-wakatime-mode))


;; -- MISC -----------------------------------------------------------


;; make the compilation window small
(setq compilation-window-height 10)
(add-hook 'compilation-mode-hook
          (lambda ()
            (when (not (get-buffer-window "*compilation*"))
              (save-selected-window
                (save-excursion
                  (let* ((w (split-window-vertically))
                         (h (window-height w)))
                    (select-window w)
                    (switch-to-buffer "*compilation*")
                    (shrink-window (- h compilation-window-height))
                    (set-window-dedicated-p w t)))))))

;; files that change on disk automatically get reverted
(global-auto-revert-mode t)

;; automatic bracket pairing
(electric-pair-mode)

;; goto last location in buffer when reopening
(save-place-mode 1)

;; show whitespace characters
(global-whitespace-mode)

;; visualize tabs and trailing whitespace
(setq-default whitespace-style '(face tabs tab-mark trailing))

;; no tabs in my household >:(
(setq-default indent-tabs-mode nil)

;; backspace simply deletes a character. no more converting a tab
;; that's 8 characters long to 7 spaces.
(setq backward-delete-char-untabify-method nil)

;; ask before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

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


;; -- RESET ----------------------------------------------------------


;; we're at the end of init, change settings back to reasonable values
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist lia-file-name-handler-alist)


;; -- CUSTOM ---------------------------------------------------------


