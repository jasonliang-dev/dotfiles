;; Custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; remove bars
(defun lia/remove-bars ()
  "Remove menubar, toolbar, and the scrollbar."
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
(lia/remove-bars)

(toggle-frame-maximized) ; open emacs maximized

(fset 'yes-or-no-p 'y-or-n-p) ; yes/no prompts are y/n

;; package repos
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (setq use-package-always-ensure t)


;; ############
;; Package list
;; ############

;; :init - before
;; :config - after

;; base16 colours
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-ocean t)
  (defvar lia/base16-colors base16-ocean-colors
    "It's nice to have some colour"))

;; text completion
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; hide or shorten minor modes
(use-package diminish
  :disabled
  :ensure t
  :init
  (diminish 'jiggle-mode))

;; vim, in emacs
(use-package evil
  :ensure t
  :init
  ;; Scroll up with C-u
  (setq evil-want-C-u-scroll t)
  :config
  ;; move by visual line
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (evil-mode t))

(use-package evil-leader
  :ensure evil
  :init
  (global-evil-leader-mode))

(use-package evil-numbers
  :ensure evil
  :init
  (global-set-key (kbd "C-c C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt))

(use-package evil-org
  :ensure evil
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))

;; syntax checking
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; fold marked regions
(use-package folding
  :disabled
  :ensure t
  :init
  (load "folding" 'nomessage 'noerror)
  (folding-mode-add-find-file-hook)
  :config
  (folding-add-to-marks-list 'web-mode "<!-- {{{ " "<!-- }}} -->" " -->" nil t))

;; show git changes in gutter
(use-package git-gutter-fringe
  :ensure t
  :init
  (global-git-gutter-mode t)
  (setq fringes-outside-margins t)
  :config
  (set-face-foreground 'git-gutter-fr:modified (plist-get lia/base16-colors :base0A))
  (set-face-foreground 'git-gutter-fr:added    (plist-get lia/base16-colors :base0B))
  (set-face-foreground 'git-gutter-fr:deleted  (plist-get lia/base16-colors :base08))
  ;; https://github.com/hlissner/.emacs.d/blob/master/core/core-vcs.el#L24
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center))

;; narrow lists
(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; java add-on
(use-package jdee
  :disabled
  :ensure t)

;; relative line numbers
(use-package linum-relative
  :ensure t)

;; git
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status) ; run magit-status with a keybinding

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; line numbers
(use-package nlinum
  :ensure t)

;; cool looking bullets in org
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

;; powerline. it looks cool.
(use-package powerline
  :disabled
  :config
  (use-package airline-themes))

;; powerline with evil support
(use-package powerline-evil
  :disabled
  :ensure powerline
  :init
  (powerline-evil-vim-color-theme))

;; rainbow brackets
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; modeline from spacemacs
(use-package spaceline
  :ensure t
  :config
  (setq powerline-default-separator 'slant)
  (setq powerline-height 30)

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (defface lia/face-active1
    ;; '((t :background (plist-get lia/base16-colors :base02)
    ;; 	 :foreground (plist-get lia/base16-colors :base06)
    '((t :background "#343d46" ; arguments don't get evaluated
	 :foreground "#c0c5ce"
	 :inherit 'mode-line))
    "Custom face for spaceline."
    :group 'spaceline)

  (defmacro lia/init-faces ()
    `(defface lia/face-active1
       '((t :background (plist-get lia/base16-colors :base02)
	    :foreground (plist-get lia/base16-colors :base06)
	    :inherit 'mode-line))
       "Custom face for spaceline."
       :group 'spaceline))
  (lia/init-faces)

  ;; https://github.com/TheBB/spaceline/blob/e6ccec6c80ee2bbddbad5a88cb9d2cd2db8a1a33/spaceline.el#L122
  (setq spaceline-face-func
	(lambda (face active)
	  (cond
	   ((eq 'face1 face) (if active 'lia/face-active1 'powerline-inactive1))
	   ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
	   ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
	   ((eq 'highlight face) (if active
				     (funcall spaceline-highlight-face-func)
				   'powerline-inactive1))))))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-spacemacs-theme))

;; neat features for web development
(use-package web-mode
  :disabled
  :ensure t)

;; templates
(use-package yasnippet
  :disabled
  :ensure t)

;; ##########
;; Appearance
;; ##########

;; https://github.com/belak/base16-emacs#evil-mode
;; Set the cursor color based on the evil state
(setq evil-emacs-state-cursor   `(,(plist-get lia/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get lia/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get lia/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get lia/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get lia/base16-colors :base08) hbar)
      evil-visual-state-cursor  `(,(plist-get lia/base16-colors :base09) box))

;; set font
(set-face-attribute 'default nil :font "Source Code Pro 10")

;; highlight the current line
(global-hl-line-mode 1)

;; custom mode line
;; https://emacs-fu.blogspot.ca/2011/08/customizing-mode-line.html
(defun lia/mode-line ()
  "Custom mode line."
  (interactive)
  (setq-default mode-line-format
		(list
		 ;; Any changes since last save?
		 " %* "

		 ;; the buffer name; the file name as a tool tip
		 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				     'help-echo (buffer-file-name)))

		 ;; line and column
		 "(" ;; '%02' to set to 2 chars at least; prevents flickering
		 (propertize "%02l" 'face 'font-lock-type-face) ","
		 (propertize "%02c" 'face 'font-lock-type-face)
		 ") "

		 ;; the current major mode for the buffer.
		 "["
		 '(:eval (propertize "%m" 'face 'font-lock-string-face
				     'help-echo buffer-file-coding-system))
		 "] ")))
;;(lia/mode-line)

;; set the fringe color to the background color
;; http://emacs.stackexchange.com/a/5343
(defun lia/tone-down-fringes ()
  "Set the fringe colour to the background colour."
  (set-face-attribute 'fringe nil
		      :foreground (face-foreground 'default)
		      :background (face-background 'default)))
(lia/tone-down-fringes)

;; don't display the welcome screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; ########
;; Org mode
;; ########

;; hide formating characters
(setq org-hide-emphasis-markers t)

;; add timestamps when DONE
(setq org-log-done 'time)

;; set agenda directory
(setq org-agenda-files '("~/Dropbox/org/"))

;; custom ellipsis
(setq org-ellipsis " ⤵")

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
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "ON HOLD(h)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	(sequence "[ ](T)" "[-](I)" "[*](W)" "|" "[X](D)")))

;; #####
;; Other
;; #####

(server-start) ; start emacs as a server

;; goto the last change
(global-set-key (kbd "C-M-.") 'goto-last-change)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; scrolll a certain amount at a time
(setq mouse-wheel-scroll-amount '(amount ((shift) . amount))) ; mouse scroll amount
(setq mouse-wheel-progressive-speed nil)                      ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                            ; scroll window under mouse
(setq scroll-step 1)                                          ; keyboard scroll one line at a time

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

(defun lia/the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)

  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

(defun lia/window-swap ()
  "Swap your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't swap a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* (
		  (w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2))
		  )
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

(global-visual-line-mode t) ; enable wordwrap

;; open todo list
;; (find-file "~/Dropbox/org/todo.org")
