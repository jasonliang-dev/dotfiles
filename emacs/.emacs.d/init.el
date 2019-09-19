;; -*- lexical-binding: t; -*-

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
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

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
        (call-process-shell-command "~/scripts/scratchpad.sh" nil 0)))))

;; -- EVIL --

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
  ;; leader bindings
  (lia-leader-def "ESC" 'evil-ex-nohighlight)
  (lia-leader-def "q"   'evil-quit)
  (lia-leader-def "w"   'evil-window-map)
  ;; scroll with C-u
  (setq evil-want-C-u-scroll t)
  ;; emacs movement in insert mode
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

;; -- THE BIG THREE --

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
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
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :after ivy
  :init
  (lia-leader-def "sS" 'swiper-all)
  (lia-leader-def "ss" 'swiper))

;; -- APPEARANCE --

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one))

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

;; Visualize tabs and trailing whitespace
(setq-default whitespace-style '(face tabs tab-mark trailing))

;; highlight matching paren
(show-paren-mode t)

;; Enable whitespace mode everywhere
(global-whitespace-mode)

;; set font
(set-frame-font "Iosevka 10" nil t)

;; --

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

(use-package emmet-mode
  ;; C-j to expand
  :ensure t
  :hook (sgml-mode . emmet-mode))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :init
  (lia-leader-def "v" 'er/expand-region))

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
  :commands projectile-command-map
  :init
  (lia-leader-def "p" 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

;; -- LANGUAGES --

(use-package restclient
  :ensure t
  :mode ("\\.http\\'")
  :general
  (restclient-mode-map
   [remap eval-last-sexp] 'restclient-http-send-current-stay-in-window))

;; --

(defun lia/toggle-line-number-type ()
  "Toggle the line number type between absolute and relative."
  (interactive)
  (setq display-line-numbers-type
        (if (eq display-line-numbers-type 'relative)
            (progn (message "Line number type: absolute") t)
          (progn (message "Line number type: relative") 'relative)))
  ;; update line numbers if it's currently being displayed
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers--turn-on)))

;; --

;; don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;; change indent size
(setq-default tab-width 2)
(setq-default evil-shift-width 2)

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

;; smooth scroll
(setq scroll-step 1
      scroll-conservatively 1000)

;; backspace simply deletes a character
(setq backward-delete-char-untabify-method nil)

;; reset garbage collector and file name handler
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist lia--file-name-handler-alist)

;; --

;; yes/no prompt is now y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; files that change on disk automatically get reverted
(global-auto-revert-mode t)

;; show startup time
(message
 (format "Started up in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done))
