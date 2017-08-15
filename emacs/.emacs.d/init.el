;;; init.el --- My emacs config

;;; Commentary:

;; 

;;; Code:

;; Custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; remove bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;(toggle-frame-maximized) ; open emacs maximized

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

(setq use-package-always-ensure t)


;; ############
;; Package list
;; ############

;; :init - before
;; :config - after

;; always keep code indented nicely
(use-package aggressive-indent
  ;;:diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

;; ICONS!
(use-package all-the-icons)

;; base16 colours
(use-package base16-theme
  :config
  (load-theme 'base16-ocean t)
  (defvar lia/base16-colors base16-ocean-colors
    "It's nice to have some colour"))

;; text completion
(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; a better startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Now with 40% more flavor!"
	dashboard-startup-banner 'logo
	dashboard-items '((recents . 10)
			  (bookmarks .10)
			  (projects . 10)
			  (agenda . 10))))

;; hide or shorten minor modes
(use-package diminish
  :config
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'undo-tree-mode))

;; vim, in emacs
(use-package evil
  :init
  ;; Scroll up with C-u
  (setq evil-want-C-u-scroll t)
  :config
  ;; move by visual line
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; leader key
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key "t" 'org-agenda))

  ;; multiple cursors for evil
  (use-package evil-mc
    :diminish evil-mc-mode
    :config
    (global-evil-mc-mode 1)
    (evil-leader/set-key "c" 'evil-mc-undo-all-cursors))

  ;; use magit with evil keys
  (use-package evil-magit)

  ;; increment/decrement numbers
  (use-package evil-numbers
    :init
    (evil-leader/set-key
      "a" 'evil-numbers/inc-at-pt
      "x" 'evil-numbers/dec-at-pt)
    (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

  ;; evil keys in org mode
  (use-package evil-org
    :diminish evil-org-mode
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
	      (lambda ()
		(evil-org-set-key-theme))))
  (use-package evil-surround
    :config
    (global-evil-surround-mode t)))

;; syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; fold marked regions
(use-package folding
  :disabled
  :init
  (load "folding" 'nomessage 'noerror)
  (folding-mode-add-find-file-hook)
  :config
  (folding-add-to-marks-list 'web-mode "<!-- {{{ " "<!-- }}} -->" " -->" nil t))

;; show git changes in gutter
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode t)
  (setq fringes-outside-margins t)
  :config
  (set-face-foreground 'git-gutter-fr:added    (plist-get lia/base16-colors :base0B))
  (set-face-foreground 'git-gutter-fr:modified (plist-get lia/base16-colors :base0A))
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
  :diminish helm-mode
  :init
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; helm integration with projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; java add-on
(use-package jdee
  :disabled)

;; git
(use-package magit
  :config
  (evil-leader/set-key "g" 'magit-status)

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

;; Ever heard of NERDTREE? Basically that.
(use-package neotree
  :config
  (setq neo-smart-open t
	projectile-switch-project-action 'neotree-projectile-action
	neo-window-fixed-size nil
	neo-theme (if (display-graphic-p) 'icons 'arrow))

  (evil-leader/set-key "n" 'neotree-toggle)
  (global-set-key [f8] 'neotree-toggle)

  ;; http://nadeemkhedr.com/emacs-tips-and-best-plugins-to-use-with-evil-mode/#neotreelinkhttpsgithubcomjaypeiemacsneotree
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "z") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "m") 'neotree-rename-node)
  (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
  (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

;; relative line numbers
;; https://www.emacswiki.org/emacs/LineNumbers#toc6
(use-package nlinum-relative
  :config
  ;; Preset `nlinum-format' for minimum width.
  (defun lia/nlinum-mode-hook ()
    (when nlinum-mode
      (setq-local nlinum-format
		  (concat "%" (number-to-string
			       ;; Guesstimate number of buffer lines.
			       (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
			  "d "))))
  (add-hook 'nlinum-mode-hook #'lia/nlinum-mode-hook)

  (setq nlinum-relative-current-symbol ""
	nlinum-relative-redisplay-delay 0)

  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; cool looking bullets in org
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

;; page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode)

;; features for projects
(use-package projectile
  :config
  (projectile-mode t)
  ;; https://github.com/sviridov/.emacs.d/blob/master/config/base/init-diminish.el#L25
  (setq-default projectile-mode-line
		'(:eval (format "Pro[%s]" (projectile-project-name))))
  (evil-leader/set-key
    "p f" 'projectile-find-file
    "p p" 'projectile-switch-project))

;; rainbow brackets
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t))

;; modeline from spacemacs
(use-package spaceline
  :config
  (require 'spaceline-config)

  (setq powerline-default-separator 'slant
	powerline-height 30
	spaceline-minor-modes-separator " "
	spaceline-separator-dir-left '(right . right)
	spaceline-separator-dir-right '(right . right)
	spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (eval
   `(defface lia/face-active1
      '((t :background ,(plist-get lia/base16-colors :base01)
	   :foreground ,(plist-get lia/base16-colors :base05)
	   :inherit 'mode-line))
      "Custom active face for spaceline."
      :group 'spaceline))

  (eval
   `(defface lia/face-inactive1
      '((t :background ,(plist-get lia/base16-colors :base01)
	   :foreground ,(plist-get lia/base16-colors :base03)
	   :inherit 'mode-line))
      "Custom inactive face for spaceline."
      :group 'spaceline))
  
  ;; https://github.com/TheBB/spaceline/blob/e6ccec6c80ee2bbddbad5a88cb9d2cd2db8a1a33/spaceline.el#L122
  (setq spaceline-face-func
	(lambda (face active)
	  (cond
	   ((eq 'face1 face) (if active 'lia/face-active1 'lia/face-inactive1))
	   ((eq 'face2 face) (if active 'mode-line 'lia/face-inactive1))
	   ((eq 'line face) (if active 'powerline-active2 'lia/face-inactive1))
	   ((eq 'highlight face) (if active
				     (funcall spaceline-highlight-face-func)
				   'lia/face-inactive1)))))

  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-spacemacs-theme))

;; make the editor more sublime-y
(use-package sublimity
  :disabled
  :config
  ;;(require 'sublimity-map)
  (require 'sublimity-scroll)
  (sublimity-mode t))

;; neat features for web development
(use-package web-mode
  :disabled)

;; templates
(use-package yasnippet
  ;;:diminish yas-minor-mode
  :config
  (yas-global-mode t))

;; yasnippet collection
(use-package yasnippet-snippets)

;; ##########
;; Appearance
;; ##########

(set-face-attribute 'default nil :font "Source Code Pro 10") ; set font

(global-hl-line-mode 1) ; highlight the current line

(global-visual-line-mode t) ; enable wordwrap

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;; https://github.com/belak/base16-emacs#evil-mode
;; Set the cursor color based on the evil state
(setq evil-emacs-state-cursor   `(,(plist-get lia/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get lia/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get lia/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get lia/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get lia/base16-colors :base08) hbar)
      evil-visual-state-cursor  `(,(plist-get lia/base16-colors :base09) box))

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
(setq inhibit-splash-screen t
      inhibit-startup-message t)

;; ########
;; Org mode
;; ########

(setq org-agenda-files '("~/Dropbox/org/") ; set agenda directory
      org-ellipsis " ⤵" ; custom ellipsis
      org-hide-emphasis-markers t ; hide formating characters
      org-log-done 'time) ; add timestamps when DONE

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

(setq vc-make-backup-files t) ; Make backups of files, even when they're in version control

(global-set-key (kbd "C-M-.") 'goto-last-change) ; goto the last change

;; start emacs as a server if one isn't running
;; https://stackoverflow.com/a/9999774
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; scroll a line at a time
;; (setq mouse-wheel-scroll-amount '(amount (1 . 1))) ; mouse scroll amount
;; (setq mouse-wheel-progressive-speed nil)           ; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't)                 ; scroll window under mouse
;; (setq scroll-step 1)                               ; keyboard scroll one line at a time

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
		  (s2 (window-start w2)))
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

(evil-mode t) ; Start evil mode at the end

;;; init.el ends here
