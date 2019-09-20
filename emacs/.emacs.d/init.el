;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; an Emacs config

;;; Code:

(defconst lia-use-tabs nil
  "When non-nil, indentation uses tab characters instead of spaces.")

(defconst lia-indent-width 2
  "Default indentation width.")

(defconst lia-display-font "Iosevka 10"
  "Set the font.")

(defconst lia-theme 'doom-one
  "Emacs theme to use.")

(defconst lia-use-line-numbers nil
  "When non-nil, show line numbers when programming.")

(defconst lia-leader-key "SPC"
  "Leader key prefix.")

(defconst lia-leader-alt-key "M-SPC"
  "Alternative leader key when in `insert' and `emacs' state.")

;; -- EARLY INITIALIZATION -------------------------------------------

;; avoid garbage collection
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; unset file-name-handler-alist
(defvar lia--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; be quiet at startup; don't load or display anything unnecessary
;; shamelessly stolen from doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core.el#L112
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
        mode-line-format nil))

;; remove bars and blinking cursor
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; -- PACKAGE SETUP --------------------------------------------------

;; bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; if elpa.gnu.org is down. try the mirror on github:
                         ;; ("gnu" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
                         ))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; -- CUSTOMIZE FILE -------------------------------------------------

;; put emacs customize stuff in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; create the custom file if it doesn't exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file nil t)

;; -- PACKAGES -------------------------------------------------------

(use-package general
  :ensure t
  :config
  (general-create-definer lia-leader-def
    :states '(motion insert emacs)
    :keymaps 'override
    :prefix lia-leader-key
    :non-normal-prefix lia-leader-alt-key)

  (lia-leader-def "TAB" 'mode-line-other-buffer)
  (lia-leader-def ","   'rename-buffer)
  (lia-leader-def "e"   'eval-last-sexp)
  (lia-leader-def "k"   'kill-this-buffer)
  (lia-leader-def "r"   'revert-buffer)
  (lia-leader-def "u"   'undo-tree-visualize)

  ;; open config file
  (lia-leader-def "1"
    (lambda ()
      (interactive)
      (find-file (expand-file-name "init.el" user-emacs-directory))))

  ;; run tmux in scratchpad terminal
  (lia-leader-def "RET"
    (lambda ()
      (interactive)
      (when (zerop (shell-command
                    (concat "tmux new-window -c '"
                            (expand-file-name default-directory)
                            "'")))
        (call-process-shell-command "~/scripts/scratchpad.sh" nil 0))))

  ;; open current file externally
  (general-define-key
   :keymaps 'dired-mode-map
   "C-c C-o" 'lia/open)

  ;; global keybindings
  (general-define-key
   "C-s" 'save-buffer
   [remap delete-other-windows] 'lia/toggle-other-windows))

;; -- EVIL --

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :general
  ([remap evil-next-line]         'evil-next-visual-line
   [remap evil-previous-line]     'evil-previous-visual-line
   [remap evil-beginning-of-line] 'evil-beginning-of-visual-line
   [remap evil-end-of-line]       'evil-end-of-visual-line
   [remap evil-window-split]      'lia/evil-window-split-and-focus
   [remap evil-window-vsplit]     'lia/evil-window-vsplit-and-focus)
  :init
  (defun lia/evil-window-split-and-focus ()
    "Split window horizontally and focus other window."
    (interactive)
    (evil-window-split)
    (other-window 1))

  (defun lia/evil-window-vsplit-and-focus ()
    "Split window vertically and focus other window."
    (interactive)
    (evil-window-vsplit)
    (other-window 1))

  ;; leader bindings
  (lia-leader-def "ESC" 'evil-ex-nohighlight)
  (lia-leader-def "q"   'evil-quit)
  (lia-leader-def "w"   'evil-window-map)
  ;; scroll with C-u
  (setq evil-want-C-u-scroll t)
  ;; emacs movement in insert mode
  (defvar evil-disable-insert-state-bindings)
  (setq evil-disable-insert-state-bindings t)
  ;; vim search behaviour
  (setq evil-search-module 'evil-search))

(use-package evil-magit
  :ensure t
  :after (evil magit))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-mc
  :ensure t
  :general
  (:states
   'visual
   "C-n" 'evil-mc-make-and-goto-next-match
   "C-p" 'evil-mc-make-and-goto-prev-match
   "C-;" 'evil-mc-make-all-cursors)
  :config
  (global-evil-mc-mode 1))

(use-package evil-numbers
  :ensure t
  :general
  (:states
   'motion
   "C-a"   'evil-numbers/inc-at-pt
   "C-S-a" 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode))

;; -- THE BIG THREE --

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :general
  (ivy-minibuffer-map
   "TAB" 'ivy-alt-done)
  :init
  (lia-leader-def "SPC" 'counsel-M-x)
  (lia-leader-def "f"   'counsel-find-file)
  (lia-leader-def "b"   'ivy-switch-buffer)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :after ivy
  :init
  (lia-leader-def "s" 'counsel-grep-or-swiper)
  (lia-leader-def "y" 'counsel-yank-pop)
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :after ivy
  :init
  (lia-leader-def "S" 'swiper-all))

;; -- APPEARANCE --

(use-package doom-themes
  :ensure t
  :config
  (load-theme lia-theme))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 35
        doom-modeline-buffer-file-name-style 'buffer-name))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; display line numbers settings
(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)

;; visualize tabs and trailing whitespace
(setq-default whitespace-style '(face tabs tab-mark trailing))

;; highlight matching paren
(show-paren-mode t)

;; Enable whitespace mode everywhere
(global-whitespace-mode)

;; set font
(set-frame-font lia-display-font nil t)

;; -- EDITOR --

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :general
  (company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous)
  :init
  ;; don't delay autocomplete suggesstions
  (setq company-idle-delay 0)

  ;; popup completions after typing a single character
  (setq company-minimum-prefix-length 1))

(use-package dtrt-indent
  :ensure t
  :hook (prog-mode . dtrt-indent-mode))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go)
  :init
  (lia-leader-def "j" 'dumb-jump-go))

(use-package emmet-mode
  ;; C-j to expand
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (web-mode . emmet-mode)))

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (message "Path loaded"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :init
  (lia-leader-def "v" 'er/expand-region))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :init
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

(use-package format-all
  :ensure t
  :commands format-all-buffer
  :init
  (lia-leader-def "F" 'format-all-buffer))

(use-package magit
  :ensure t
  :defer t
  :init
  (lia-leader-def "g" 'magit-status))

(use-package projectile
  :ensure t
  :defer t
  :init
  (lia-leader-def "p" '(:keymap projectile-command-map :package projectile))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode))

;; -- LANGUAGES --

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :general
  (elm-mode-map
   [remap format-all-buffer] 'elm-format-buffer))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init (setq haskell-process-type 'stack-ghci))

(use-package flycheck-haskell
  :ensure t
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook ((js2-mode . js2-imenu-extras-mode))
  :init
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :magic ("/\\*\\* @jsx React\\.DOM \\*/" "^import React"))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" ".eslintrc\\'" ".prettierrc\\'"))

(use-package lua-mode
  :ensure nil
  :disabled t
  :mode "\\.lua\\'")

(use-package markdown-mode
  :ensure t
  :mode (".md\\'" "\\.md\\'" "\\.markdown\\'")
  :init (setq markdown-command "multimarkdown"))

(use-package php-mode
  :ensure nil
  :disabled t
  :mode "\\.php\\'")

(use-package restclient
  :ensure t
  :mode "\\.http\\'"
  :general
  (restclient-mode-map
   [remap eval-last-sexp] 'restclient-http-send-current-stay-in-window))

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(use-package web-mode
  :ensure t
  :init
  (setq-default web-mode-enable-auto-pairing nil)
  (add-to-list 'auto-mode-alist
               '("\\.vue\\'" . (lambda ()
                                 (web-mode)
                                 (setq web-mode-style-padding 0
                                       web-mode-script-padding 0))))
  :mode ("\\.php\\'" "\\.ejs\\'" "\\.twig\\'"))

;; -- FUNCTIONS ------------------------------------------------------

(defun lia/open ()
  "Open current file (or selected file if in dired mode) in an external program."
  (interactive)
  (call-process "xdg-open" nil 0 nil
                (if (eq major-mode 'dired-mode)
                    (dired-get-file-for-visit)
                  buffer-file-name)))

(defun lia/toggle-display-line-number-type ()
  "Toggle the line number type between absolute and relative."
  (interactive)
  (defvar display-line-numbers-type)
  (setq display-line-numbers-type
        (if (eq display-line-numbers-type 'relative)
            (progn (message "Line number type: absolute") t)
          (progn (message "Line number type: relative") 'relative)))
  ;; update line numbers if it's currently being displayed
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers--turn-on)))

(defun lia/toggle-other-windows ()
  "Make a window fill the frame, or restore previous windows."
  (interactive)
  (defvar lia--saved-buffer)
  (defvar lia--saved-window-configuration)
  (if (= 1 (length (window-list)))
      (if (bound-and-true-p lia--saved-window-configuration)
          (progn
            (setq lia--saved-buffer (current-buffer))
            (set-window-configuration lia--saved-window-configuration)
            (switch-to-buffer lia--saved-buffer))
        (message "Only one window"))
    (setq lia--saved-window-configuration (current-window-configuration))
    (delete-other-windows)))

;; -- SETTINGS -------------------------------------------------------

;; set tabs for indentation
(setq-default indent-tabs-mode lia-use-tabs)

;; change indent size
(setq-default tab-width lia-indent-width)
(setq-default evil-shift-width lia-indent-width)

;; show column number in the modebar
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

;; smooth scroll
(setq scroll-step 1
      scroll-conservatively 1000)

;; backspace simply deletes a character
(setq backward-delete-char-untabify-method nil)

;; guess target directory when copying/moving files in dired
;; i.e. get drag and drop functionality with two dired windows in a split
(setq dired-dwim-target t)

;; display line numbers when in `prog-mode'
(when lia-use-line-numbers
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; pair up delimiters: "", (), [], {}
(electric-pair-mode t)

;; yes/no prompt is now y/n
;; saves a bit of typing
(fset 'yes-or-no-p 'y-or-n-p)

;; files that change on disk automatically get reverted
(global-auto-revert-mode t)

;; -- LATE INITIALIZATION --------------------------------------------

;; reset garbage collector and file name handler
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist lia--file-name-handler-alist)

;; show startup time
(message
 (format "Started up in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done))

;;; init.el ends here
