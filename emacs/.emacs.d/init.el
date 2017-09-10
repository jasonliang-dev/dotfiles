;;; init.el --- My Emacs config

;;; Commentary:

;; This is my dodgy and messed up Emacs configuration!
;;
;; Most of the code here is copied and pasted from articles,
;; other people's dotfiles, Emacs Wiki, Reddit, Stack Exchange
;; and some other sources that I cannot remember.

;;; Code:

;; Move built in customization stuff to a different file
(setq custom-file
	  (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; sometimes I use emacs in Windows
;; the dropbox directory is different
(defconst lia/dropbox-directory "~/Dropbox/")

;; remove bars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; yes/no prompts are y/n
(fset 'yes-or-no-p 'y-or-n-p)



(require 'package)
(setq package-enable-at-startup nil)
;; add package repos
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
						 ("gnu" . "http://elpa.gnu.org/packages/")
						 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Install `use-package' if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; always install
(setq use-package-always-ensure t)


;; EVIL MODE
;; essentially, emacs is now vim
(use-package evil
  :init
  ;; Scroll up with C-u
  (setq evil-want-C-u-scroll t)

  ;; Start evil mode at the end
  (add-hook 'after-init-hook (evil-mode t))
  :config
  ;; fine undo history
  (setq evil-want-fine-undo t)

  ;; multiple cursors for evil
  (use-package evil-mc
	:diminish evil-mc-mode
	:config
	(global-evil-mc-mode 1))

  ;; use magit with evil keys
  (use-package evil-magit)

  ;; increment/decrement numbers
  (use-package evil-numbers)

  ;; evil keys in org mode
  (use-package evil-org
	:diminish evil-org-mode
	:config
	(add-hook 'org-mode-hook 'evil-org-mode)
	(add-hook 'evil-org-mode-hook
			  (lambda ()
				(evil-org-set-key-theme))))
  
  ;; vim surround
  (use-package evil-surround
	:config
	(global-evil-surround-mode t))

  ;; vim-like folding
  (use-package evil-vimish-fold
	:diminish evil-vimish-fold-mode
	:config
	(setq vimish-fold-header-width nil)
	(evil-vimish-fold-mode t)))

;; base16 colours
(use-package base16-theme
  :config
  ;; load light theme until it's past 6:00pm, then use dark theme
  ;; https://emacs.stackexchange.com/questions/6029/is-it-possible-to-execute-a-function-or-command-at-a-specific-time
  (load-theme 'base16-tomorrow t)
  (defvar lia/base16-colors base16-tomorrow-colors)
  (run-at-time "18:00" nil (lambda ()
							 (load-theme 'base16-material t)
							 (setq lia/base16-colors base16-material-colors)))

  ;; Set the cursor color based on the evil state
  ;; https://github.com/belak/base16-emacs#evil-mode
  (add-hook 'after-init-hook
			(lambda ()
			  (setq evil-emacs-state-cursor
					`(,(plist-get lia/base16-colors :base0D) box)
					evil-insert-state-cursor
					`(,(plist-get lia/base16-colors :base0D) bar)
					evil-motion-state-cursor
					`(,(plist-get lia/base16-colors :base0E) box)
					evil-normal-state-cursor
					`(,(plist-get lia/base16-colors :base0B) box)
					evil-replace-state-cursor
					`(,(plist-get lia/base16-colors :base08) bar)
					evil-visual-state-cursor
					`(,(plist-get lia/base16-colors :base09) box)))))

;; A completion framework
;; helm is heavy, but full of features
(use-package helm
  :diminish helm-mode
  :init
  (helm-mode 1)
  :config
  ;; helm integration with projectile
  (use-package helm-projectile
	:config
	(helm-projectile-on))

  ;;  search matches in another buffer
  (use-package helm-swoop
	:config
	(add-hook 'after-init-hook
			  (lambda ()
				(set-face-attribute
				 'helm-swoop-target-line-face nil
				 :background (plist-get lia/base16-colors :base01)
				 :foreground (plist-get lia/base16-colors :base05))

				(set-face-attribute
				 'helm-swoop-target-line-block-face nil
				 :background (plist-get lia/base16-colors :base01)
				 :foreground (plist-get lia/base16-colors :base05))

				(set-face-attribute
				 'helm-swoop-target-word-face nil
				 :background (plist-get lia/base16-colors :base0D)
				 :foreground (plist-get lia/base16-colors :base05))))))

;; Another completion framework
;; ivy is lightweight and simple
(use-package ivy
  :disabled
  :diminish ivy-mode
  :config
  (ivy-mode t)
  ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 16)
  ;; does not count candidates
  (setq ivy-count-format "")

  ;; ivy integeration with projectile
  (use-package counsel-projectile
	:config
	(counsel-projectile-on)))

;; manage keybindings
(use-package general
  :config
  ;; leader keybinds
  (general-define-key
   :states '(normal visual motion insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" 'ace-window
   "TAB" 'mode-line-other-buffer
   "br"	 'revert-buffer
   "c"	 'comment-region
   "d"	 '((lambda ()
			 (interactive)
			 (neotree-hide)
			 (deer))
		   :which-key "deer")
   "e"	 'emmet-expand-line
   "f"	 'flycheck-next-error
   "F"	 'flycheck-previous-error
   "gk"	 'general-describe-keybindings
   "gs"	 'magit-status
   "h"	 '((lambda ()
			 (interactive)
			 (find-file (concat lia/dropbox-directory "help")))
		   :which-key "my help")
   "j"	 'avy-goto-word-1
   "k"	 'kill-this-buffer
   "r"	 'er/expand-region
   "s"	 'eshell
   "t"	 'neotree-toggle
   "u"	 'undo-tree-visualize
   "U"	 'universal-argument
   "w"	 'lia/window-switch-split
   "ll"	 'nlinum-mode
   "lr"	 'nlinum-relative-toggle
   "oa"	 'org-agenda
   "oi"	 'org-toggle-inline-images
   "ol"	 'org-insert-link
   "oo"	 'ace-link-org

   ;; helm bindings
   "/" 'helm-swoop
   "m" 'helm-mini
   "p" 'helm-show-kill-ring
   "x" 'helm-M-x

   ;; ivy bindings
   ;;"/" 'swiper
   ;;"m" 'ivy-switch-buffer
   ;;"p" 'counsel-yank-pop
   ;;"x" 'counsel-M-x
   )

  ;; global bindings
  (general-define-key
   "C-S-h"	 'buf-move-left
   "C-S-j"	 'buf-move-down
   "C-S-k"	 'buf-move-up
   "C-S-l"	 'buf-move-right
   "C-c C-=" 'evil-numbers/inc-at-pt
   "C-c C--" 'evil-numbers/dec-at-pt
   "C-M-."	 'goto-last-change

   ;; helm bindings
   "C-x C-f" 'helm-find-files
   "C-x C-r" 'helm-recentf
   "M-x" 'helm-M-x

   ;; ivy bindings
   ;;"C-x C-f" 'counsel-find-file
   ;;"C-x C-r" 'counsel-recentf
   ;;"M-x" 'counsel-M-x
   )

  ;; evil mode
  (general-define-key
   :states '(normal visual motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)

  ;; vimish-fold
  (general-define-key
   :states 'normal
   :keymaps 'prog-mode-map
   "TAB" 'vimish-fold-toggle)

  ;; magit quit
  (general-define-key
   :keymaps 'magit-status-mode-map
   "q" 'magit-quit-session)

  ;; neotree
  (general-define-key
   [f8] 'neotree-toggle)
  (general-define-key
   :states 'normal
   :keymaps 'neotree-mode-map
   "q"	 'neotree-hide
   "h"	 'neotree-hidden-file-toggle
   "R"	 'neotree-refresh
   "M"	 'neotree-rename-node
   "C"	 'neotree-create-node
   "D"	 'neotree-delete-node
   "TAB" 'neotree-quick-look
   "RET" 'neotree-enter)

  ;; org mode
  (general-define-key
   :keymaps 'org-mode-map
   "C-c >" 'org-time-stamp-inactive)

  ;; org agenda
  (general-define-key
   :states 'emacs
   :keymaps 'org-agenda-mode-map
   "j" 'evil-next-line
   "k" 'evil-previous-line
   "J" 'org-agenda-later
   "K" 'org-agenda-earlier)

  ;; pdfs
  ;; https://github.com/noctuid/evil-guide#example-integration-with-pdf-tools
  (general-define-key
   :states 'emacs
   :keymaps 'doc-view-mode-map
   "h" (general-simulate-keys "p" t)
   "j" (general-simulate-keys "C-n" t)
   "k" (general-simulate-keys "C-p" t)
   "l" (general-simulate-keys "n" t)

   "J" (general-simulate-keys "SPC" t)
   "K" (general-simulate-keys "DEL" t)

   "g" 'pdf-view-first-page
   "G" 'pdf-view-last-page
   ;; alternatively
   "g" 'image-bob
   "G" 'image-eob
   (kbd "C-o") 'pdf-history-backward
   (kbd "C-i") 'pdf-history-forward
   "m" 'pdf-view-position-to-register
   "'" 'pdf-view-jump-to-register
   "/" 'pdf-occur
   "o" 'pdf-outline
   "f" 'pdf-links-action-perform
   "b" 'pdf-view-midnight-minor-mode))

;; quickly select different windows
(use-package ace-window
  :config
  (setq aw-keys '(?w ?e ?r ?s ?d ?f)))

;; quickly select links
(use-package ace-link
  :config
  (ace-link-setup-default))

;; always keep code indented nicely
(use-package aggressive-indent
  :diminish (aggressive-indent-mode . "🅘")
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

;; ICONS!
(use-package all-the-icons)

;; icons in dired
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package avy
  :config
  (avy-setup-default))

;; show the cursor when the window jumps
;; it's not that I have trouble finding the cursor
;; I think this just looks cool
(use-package beacon
  :diminish (beacon-mode . "🅑")
  :config
  (setq beacon-dont-blink-major-modes '(dired-mode
										neotree-mode
										magit-status-mode
										magit-popup-mode
										ranger-mode))
  (beacon-mode t))

;; swap windows (buffers)
(use-package buffer-move)

;; text completion
(use-package company
  :diminish (company-mode . "🅒")
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; a better startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner
		(concat user-emacs-directory "icon/emacs-sexy.png")
		dashboard-items '((recents . 10)
						  (bookmarks .10)
						  (projects . 10)
						  (agenda . 10))))

;; hide or shorten minor modes
(use-package diminish
  :config
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (diminish 'flyspell-mode)
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'undo-tree-mode))

;; emmet
(use-package emmet-mode
  :diminish (emmet-mode . "🅔")
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook	'emmet-mode))

;; expand region
(use-package expand-region)

(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80))

;; syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  ;; https://github.com/flycheck/flycheck/blob/master/flycheck.el#L3380
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
	(vector #b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00011001
			#b00110110
			#b01101100
			#b11011000
			#b01101100
			#b00110110
			#b00011001
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000))
  (use-package helm-flycheck))

;; dim surrounding text
(use-package focus
  :commands focus-mode)

;; show git changes in gutter
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode t)
  :config
  (setq fringes-outside-margins t)
  (add-hook 'after-init-hook
			(lambda ()
			  (set-face-foreground 'git-gutter-fr:added
								   (plist-get lia/base16-colors :base0B))
			  (set-face-foreground 'git-gutter-fr:modified
								   (plist-get lia/base16-colors :base0A))
			  (set-face-foreground 'git-gutter-fr:deleted
								   (plist-get lia/base16-colors :base08))))
  ;; https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom/config.el#L97
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
	"XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
	"XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
	"X......."
	"XX......"
	"XXX....."
	"XXXX...."))

;; git
(use-package magit
  :config

  (defadvice magit-status (around magit-fullscreen activate)
	(window-configuration-to-register :magit-fullscreen)
	ad-do-it
	(delete-other-windows))

  (defun magit-quit-session ()
	"Restores the previous window configuration and kills the magit buffer"
	(interactive)
	(kill-buffer)
	(jump-to-register :magit-fullscreen)))

;; major mode for markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Ever heard of NERDTREE? Basically that.
(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-smart-open t
		projectile-switch-project-action 'neotree-projectile-action
		neo-window-width 35
		neo-window-fixed-size nil
		neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; change neotree's text colours
  ;; oh boy, here we go.
  (set-face-foreground 'neo-banner-face
					   (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-header-face
					   (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-root-dir-face
					   (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-dir-link-face
					   (plist-get lia/base16-colors :base0D))
  (set-face-foreground 'neo-file-link-face
					   (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-expand-btn-face
					   (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-vc-default-face
					   (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-vc-user-face
					   (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-up-to-date-face
					   (plist-get lia/base16-colors :base03))
  (set-face-foreground 'neo-vc-edited-face
					   (plist-get lia/base16-colors :base0E))
  (set-face-foreground 'neo-vc-needs-merge-face
					   (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-unlocked-changes-face
					   (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-added-face
					   (plist-get lia/base16-colors :base0B))
  (set-face-foreground 'neo-vc-conflict-face
					   (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-missing-face
					   (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-ignored-face
					   (plist-get lia/base16-colors :base03))

  ;; http://nadeemkhedr.com/emacs-tips-and-best-plugins-to-use-with-evil-mode/#neotreelinkhttpsgithubcomjaypeiemacsneotree
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; linum is laggy. use nlinum instead
(use-package nlinum
  :config
  ;; Preset `nlinum-format' for minimum width.
  ;; https://www.emacswiki.org/emacs/LineNumbers#toc6
  (defun lia/nlinum-mode-hook ()
	(when nlinum-mode
	  (setq-local nlinum-format
				  (concat " %" (number-to-string
								;; Guesstimate number of buffer lines.
								(ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
						  "d "))))
  (add-hook 'nlinum-mode-hook #'lia/nlinum-mode-hook))

;; relative line numbers
(use-package nlinum-relative
  :config
  (setq nlinum-relative-current-symbol ""
		nlinum-relative-redisplay-delay 0)

  (nlinum-relative-setup-evil))

(use-package org
  :config
  (setq org-blank-before-new-entry '((heading) (plain-list-item)) ; blank lines between entries
		org-ellipsis " ⤵" ; custom ellipsis
		org-hide-emphasis-markers t ; hide formating characters
		org-log-done 'time ; add timestamps when task is done, or rescheduled
		org-log-redeadline 'time
		org-log-reschedule 'time)

  ;; better looking org headlines
  ;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html#orgheadline4
  (let* ((variable-tuple (cond
						  ((x-list-fonts "Roboto")
						   '(:font "Roboto"))
						  ((x-list-fonts "Source Sans Pro")
						   '(:font "Source Sans Pro"))
						  ((x-list-fonts "Lucida Grande")
						   '(:font "Lucida Grande"))
						  ((x-list-fonts "Verdana")
						   '(:font "Verdana"))
						  ((x-family-fonts "Sans Serif")
						   '(:family "Sans Serif"))
						  (nil (warn "Cannot find a Sans Serif Font."))))
		 (headline			 `(:inherit default :weight bold :height 140)))

	(custom-theme-set-faces 'user
							`(org-level-8 ((t (,@headline ,@variable-tuple))))
							`(org-level-7 ((t (,@headline ,@variable-tuple))))
							`(org-level-6 ((t (,@headline ,@variable-tuple))))
							`(org-level-5 ((t (,@headline ,@variable-tuple))))
							`(org-level-4 ((t (,@headline ,@variable-tuple))))
							`(org-level-3 ((t (,@headline ,@variable-tuple
														  :height 160))))
							`(org-level-2 ((t (,@headline ,@variable-tuple
														  :height 180))))
							`(org-level-1 ((t (,@headline ,@variable-tuple
														  :height 200))))
							`(org-document-title ((t (,@headline
													  ,@variable-tuple
													  :height 250
													  :underline nil))))))

  ;; set agenda files
  (setq org-agenda-files (list (concat lia/dropbox-directory "org/planner.org")))

  ;; org source code languages
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((css . t)
							   (emacs-lisp . t)
							   (java . t)
							   (js . t)
							   (latex . t)
							   (lisp . t)
							   (org . t)
							   (perl . t)
							   (python . t)
							   (ruby . t)
							   (sh . t)))

  ;; custom todo keywords
  (setq org-todo-keywords
		'((sequence "TODO(t)"
					"IN-PROGRESS(i)"
					"ON HOLD(h)"
					"WAITING(w)"
					"|"
					"DONE(d)"
					"CANCELED(c)")
		  (sequence "[ ](T)"
					"[-](I)"
					"[*](W)"
					"|"
					"[X](D)")))

  ;; I meant 3:00 in the afternoon! not 3:00am!
  ;; https://emacs.stackexchange.com/a/3320
  (defvar time-range-with-pm-suffix '("1:00" . "6:59"))
  (defun org-analyze-date-dwim (original-fun ans org-def org-defdecode)
	(let* ((time (funcall original-fun ans org-def org-defdecode))
		   (minute (nth 1 time))
		   (hour (nth 2 time))
		   (minutes (+ minute (* 60 hour)))
		   s)
	  (when (and (< hour 12)
				 (not (string-match "am" ans))
				 (>= minutes (org-hh:mm-string-to-minutes
							  (car time-range-with-pm-suffix)))
				 (<= minutes (org-hh:mm-string-to-minutes
							  (cdr time-range-with-pm-suffix))))
		(setf (nth 2 time) (+ hour 12))
		(when (boundp 'org-end-time-was-given)
		  (setq s org-end-time-was-given)
		  (if (and s (string-match "^\\([0-9]+\\)\\(:[0-9]+\\)$" s))
			  (setq org-end-time-was-given
					(concat (number-to-string
							 (+ 12 (string-to-number (match-string 1 s))))
							(match-string 2 s))))))
	  time))

  (advice-add 'org-read-date-analyze :around #'org-analyze-date-dwim)

  ;; cool looking bullets in org
  (use-package org-bullets
	:init
	(add-hook 'org-mode-hook
			  (lambda ()
				(org-bullets-mode 1)))
	(setq org-bullets-bullet-list '("•")))

  ;; sync with google calendar
  (use-package org-gcal
	:disabled
	:commands
	(org-gcal-sync
	 org-gcal-fetch
	 org-gcal-post-at-point
	 org-gcal-delete-at-point
	 org-gcal-refresh-token)
	:config
	(defconst lia/gcal-file (concat lia/dropbox-directory "org/org-gcal.el"))
	(when (file-exists-p lia/gcal-file) (load-file lia/gcal-file)))

  ;; pomodoro
  (use-package org-pomodoro
	:config
	(add-hook 'after-init-hook
			  (lambda ()
				(set-face-foreground 'org-pomodoro-mode-line
									 (plist-get lia/base16-colors :base0A))
				(set-face-foreground 'org-pomodoro-mode-line-break
									 (plist-get lia/base16-colors :base0C))))))

;; page break lines
(use-package page-break-lines
  :diminish (page-break-lines-mode . "🅛"))

;; powerline
(use-package powerline
  :config
  (setq powerline-default-separator 'slant
		powerline-height 35)

  (add-hook 'after-init-hook
			(lambda ()
			  (set-face-attribute
			   'powerline-active1 nil
			   :foreground (plist-get lia/base16-colors :base05)
			   :background (plist-get lia/base16-colors :base01))

			  (set-face-attribute
			   'powerline-active2 nil
			   :background (plist-get lia/base16-colors :base01))

			  (set-face-attribute
			   'powerline-inactive1 nil
			   :foreground (plist-get lia/base16-colors :base03)
			   :background (plist-get lia/base16-colors :base01))))

  ;; modeline from spacemacs
  (use-package spaceline
	:config
	(require 'spaceline-config)

	(setq spaceline-minor-modes-separator ""
		  spaceline-separator-dir-left '(right . right)
		  spaceline-separator-dir-right '(right . right)
		  spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

	(add-hook 'after-init-hook
			  (lambda ()
				(eval
				 `(defface lia/mode-line-face
					'((t :foreground ,(plist-get lia/base16-colors :base05)
						 :background ,(plist-get lia/base16-colors :base02)
						 :inherit 'mode-line))
					"Custom mode line face"
					:group 'lia/faces))))

	;; https://github.com/TheBB/spaceline/blob/e6ccec6c80ee2bbddbad5a88cb9d2cd2db8a1a33/spaceline.el#L122
	(setq spaceline-face-func
		  (lambda (face active)
			(cond
			 ((eq 'face1 face)
			  (if active 'powerline-active1 'powerline-inactive1))
			 ((eq 'face2 face)
			  (if active 'lia/mode-line-face 'powerline-inactive1))
			 ((eq 'line face)
			  (if active 'powerline-active2 'powerline-inactive1))
			 ((eq 'highlight face) (if active
									   (funcall spaceline-highlight-face-func)
									 'powerline-inactive1)))))

	;;(spaceline-toggle-minor-modes-off)
	(spaceline-toggle-buffer-size-off)
	(spaceline-toggle-buffer-encoding-abbrev-off)
	(spaceline-spacemacs-theme)))

;; features for projects
(use-package projectile
  :config
  (projectile-mode t)
  ;; https://github.com/sviridov/.emacs.d/blob/master/config/base/init-diminish.el#L25
  (setq-default projectile-mode-line
				'(:eval (format "🅟【%s】" (projectile-project-name)))))

;; rainbow brackets
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; emulate ranger in dired
(use-package ranger)

;; deal with pairs of parentheses better
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; use tabs for indentation, spaces for alignment
(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c
						'c++
						'java
						'javascript
						'cperl
						'python
						'ruby
						'nxml))

;; make the editor more sublime-y
(use-package sublimity
  :commands sublimity-mode
  :config
  (require 'sublimity-scroll))

;; better agenda
(use-package org-super-agenda
  :disabled
  :ensure nil
  :config
  (let ((org-super-agenda-groups
		 '((:auto-category t))))
	(org-agenda-list))
  (org-super-agenda-mode t))

;; neat features for web development
(use-package web-mode
  :config
  ;; use web mode when editing certain files
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun lia/web-mode-hook ()
	"Hooks for Web mode."
	(setq web-mode-markup-indent-offset 4)
	(setq web-mode-css-indent-offset 4)
	(setq web-mode-code-indent-offset 4)
	(emmet-mode))
  (add-hook 'web-mode-hook 'lia/web-mode-hook))

;; display available bindings
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode t))

;; distraction free editing
;; basically goyo vim
(use-package writeroom-mode
  :config
  )

;; templates
(use-package yasnippet
  :diminish (yas-minor-mode . "🅨")
  :config
  (yas-global-mode t)
  ;; yasnippet collection
  (use-package yasnippet-snippets))



;; start emacs as a server if one isn't running
;; https://stackoverflow.com/a/9999774
(when (and (fboundp 'server-running-p)
		   (not (server-running-p)))
  (server-start))

;; enable flyspell in text mode buffers
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; don't print flyspell messages because it's slow
(setq flyspell-issue-message-flag nil)

;; Change the default font
;; https://www.emacswiki.org/emacs/SetFonts#toc11
(defun font-candidate (&rest fonts)
  "Return existing font which first match in FONTS."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(set-frame-font
 (font-candidate '"Monego 10" "Source Code Pro 10" "Ubuntu Mono 12") nil t)

;; stop the cursor from blinking
(blink-cursor-mode 0)

;; highlight the current line
;;(global-hl-line-mode)

;; enable wordwrap
(global-visual-line-mode t)

;; change certain text into symbols (lambda, >=, etc)
(global-prettify-symbols-mode t)

;; Show matching parentheses
(defvar show-paren-delay 0.2)
(show-paren-mode t)

;; set the fringe color to the background color
(set-face-background 'fringe nil)

;; don't display the welcome screen
(setq inhibit-splash-screen t
	  inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;; set the indentation width
(setq-default tab-width 4)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; move backup~ files to its own directory
(setq backup-directory-alist
	  `((".*" . ,(concat user-emacs-directory "backups"))))

;; don't create those goddamn #autosave# files
(setq auto-save-default nil)

;; always follow symlinks under version control
(setq vc-follow-symlinks t)

;; smooth scrolling
(setq scroll-margin 0
	  scroll-conservatively 100000
	  scroll-preserve-screen-position 1)
;; mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(5)	; mouse scroll amount
	  mouse-wheel-progressive-speed nil ; don't accelerate scrolling
	  mouse-wheel-follow-mouse 't)		; scroll window under mouse

;; https://www.emacswiki.org/emacs/KillingBuffers#toc2
(defun lia/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; https://www.emacswiki.org/emacs/KillingBuffers#toc3
(defun lia/kill-dired-buffers ()
  "Kill dired buffers."
  (interactive)
  (mapc (lambda (buffer)
		  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
			(kill-buffer buffer)))
		(buffer-list)))

(defun lia/the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)

  (if (re-search-forward
	   "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
	  (message "Found duplicated word.")
	(message "End of buffer")))

(defun lia/window-switch-split ()
  "Switch between horizontal/vertical layout."
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

;;; init.el ends here
