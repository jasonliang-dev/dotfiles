;;; init-packages.el --- Part of my Emacs config

;;; Commentary:

;; This file installs and configures the packages that I personally use

;;; Code:

;; add package repos
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)

;; Install use-package if not already installed
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
  :config
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

;; A completion framework
;; helm is heavy, but full of features
(use-package helm
  :disabled
  :diminish helm-mode
  :init
  (helm-mode 1)
  :config
  ;; helm integration with projectile
  (use-package helm-projectile
    :disabled
    :config
    (helm-projectile-on)))

;; Another completion framework
;; ivy is lightweight and simple
(use-package ivy
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
   "/"   'swiper
   "c"   'comment-region
   "d"   (lambda ()
	   (interactive)
	   (neotree-hide)
	   (deer))
   "e"   'flycheck-previous-error
   "e"   'flycheck-next-error
   "f"   (lambda ()
	   (interactive)
	   (neotree-hide)
	   (ranger))
   "gk"  'general-describe-keybindings
   "gs"  'magit-status
   "h"   'help
   "h"   (lambda ()
	   (interactive)
	   (find-file "~/Dropbox/help"))
   "k"   'kill-this-buffer
   "m"   'ivy-switch-buffer
   "p"   'counsel-yank-pop
   "r"   'er/expand-region
   "s"   'eshell
   "t"   'neotree-toggle
   "w"   'lia/window-swap
   "w"   'lia/window-switch-split
   "x"   'counsel-M-x
   "ll"  'nlinum-mode
   "lr"  'nlinum-relative-toggle
   "oa"  'org-agenda
   "ol"  'org-insert-link
   "oo"  'ace-link-org
   "op"  'org-pomodoro
   "ot"  'org-todo)

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

  ;; evil-numbers
  (general-define-key
   :prefix "C-c"
   "+" 'evil-numbers/inc-at-pt
   "-" 'evil-numbers/dec-at-pt)

  ;; buffer-move
  (general-define-key
   "C-S-h" 'buf-move-left
   "C-S-j" 'buf-move-down
   "C-S-k" 'buf-move-up
   "C-S-l" 'buf-move-right)

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
   "q"   'neotree-hide
   "h"	 'neotree-hidden-file-toggle
   "z"	 'neotree-stretch-toggle
   "R"	 'neotree-refresh
   "m"	 'neotree-rename-node
   "c"	 'neotree-create-node
   "d"	 'neotree-delete-node
   "TAB" 'neotree-quick-look
   "RET" 'neotree-enter)

  ;; org mode
  (general-define-key
   :keymaps 'org-mode-map
   :prefix "C-c"
   ">" 'org-time-stamp-inactive)
  ;; org agenda
  (general-define-key
   :states 'emacs
   :keymaps 'org-agenda-mode-map
   "j" 'evil-next-line
   "k" 'evil-previous-line
   "J" 'org-agenda-later
   "K" 'org-agenda-earlier)

  ;; other bindings
  (general-define-key
   "C-M-." 'goto-last-change))

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
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

;; ICONS!
(use-package all-the-icons)

;; icons in dired
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; base16 colours
(use-package base16-theme
  :config
  (load-theme 'base16-material t)
  (defvar lia/base16-colors base16-material-colors
    "It's nice to have some colour")

  ;; https://github.com/belak/base16-emacs#evil-mode
  ;; Set the cursor color based on the evil state
  (setq evil-emacs-state-cursor   `(,(plist-get lia/base16-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get lia/base16-colors :base0D) bar)
        evil-motion-state-cursor  `(,(plist-get lia/base16-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get lia/base16-colors :base0B) box)
        evil-replace-state-cursor `(,(plist-get lia/base16-colors :base08) hbar)
        evil-visual-state-cursor  `(,(plist-get lia/base16-colors :base09) box)))

;; show the cursor when the window jumps
;; it's not that I have trouble finding the cursor
;; I think this just looks cool
(use-package beacon
  :diminish beacon-mode
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
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; a better startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (concat user-emacs-directory "icon/emacs-sexy.png")
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

;; emmet
(use-package emmet-mode
  :diminish (emmet-mode . "em")
  :config
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; expand region
(use-package expand-region)

;; syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
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
            #b00000000)))

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
  (set-face-foreground 'git-gutter-fr:added    (plist-get lia/base16-colors :base0B))
  (set-face-foreground 'git-gutter-fr:modified (plist-get lia/base16-colors :base0A))
  (set-face-foreground 'git-gutter-fr:deleted  (plist-get lia/base16-colors :base08))
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
  (set-face-foreground 'neo-banner-face              (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-header-face              (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-root-dir-face            (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-dir-link-face            (plist-get lia/base16-colors :base0D))
  (set-face-foreground 'neo-file-link-face           (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-expand-btn-face          (plist-get lia/base16-colors :base0C))
  (set-face-foreground 'neo-vc-default-face          (plist-get lia/base16-colors :base05))
  (set-face-foreground 'neo-vc-user-face             (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-up-to-date-face       (plist-get lia/base16-colors :base03))
  (set-face-foreground 'neo-vc-edited-face           (plist-get lia/base16-colors :base0E))
  (set-face-foreground 'neo-vc-needs-merge-face      (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-unlocked-changes-face (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-added-face            (plist-get lia/base16-colors :base0B))
  (set-face-foreground 'neo-vc-conflict-face         (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-missing-face          (plist-get lia/base16-colors :base08))
  (set-face-foreground 'neo-vc-ignored-face          (plist-get lia/base16-colors :base03))

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

;; cool looking bullets in org
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

;; pomodoro
(use-package org-pomodoro
  :config
  (set-face-foreground 'org-pomodoro-mode-line (plist-get lia/base16-colors :base0A))
  (set-face-foreground 'org-pomodoro-mode-line-break (plist-get lia/base16-colors :base0C)))

;; page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode)

;; powerline
(use-package powerline
  :config
  (setq powerline-default-separator 'slant
        powerline-height 35)

  (set-face-attribute 'powerline-active1 nil
                      :foreground (plist-get lia/base16-colors :base05)
                      :background (plist-get lia/base16-colors :base01))

  (set-face-attribute 'powerline-active2 nil
                      :background (plist-get lia/base16-colors :base01))

  (set-face-attribute 'powerline-inactive1 nil
                      :foreground (plist-get lia/base16-colors :base03)
                      :background (plist-get lia/base16-colors :base01)))

;; features for projects
(use-package projectile
  :config
  (projectile-mode t)
  ;; https://github.com/sviridov/.emacs.d/blob/master/config/base/init-diminish.el#L25
  (setq-default projectile-mode-line
                '(:eval (format "Pro[%s]" (projectile-project-name)))))

;; rainbow brackets
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; emulate ranger in dired
(use-package ranger
  :config
  (setq ranger-cleanup-eagerly t))

;; deal with pairs of parentheses better
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; use tabs for indentation, spaces for alignment
;; relevent image: https://camo.githubusercontent.com/38467eb65de742741e216b6df7f986f2d7a621c6/687474703a2f2f7777772e656d61637377696b692e6f72672f706963732f7374617469632f54616273537061636573426f74682e706e67
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

;; stop jumping all over the place, it hurts my eyes
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t))

;; modeline from spacemacs
(use-package spaceline
  :config
  (require 'spaceline-config)

  (setq spaceline-minor-modes-separator " "
        spaceline-separator-dir-left '(right . right)
        spaceline-separator-dir-right '(right . right)
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (eval
   `(defface lia/mode-line-face
      '((t :foreground ,(plist-get lia/base16-colors :base05)
           :background ,(plist-get lia/base16-colors :base02)
           :inherit 'mode-line))
      "Custom mode line face"
      :group 'lia/faces))

  ;; https://github.com/TheBB/spaceline/blob/e6ccec6c80ee2bbddbad5a88cb9d2cd2db8a1a33/spaceline.el#L122
  (setq spaceline-face-func
        (lambda (face active)
          (cond
           ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
           ((eq 'face2 face) (if active 'lia/mode-line-face 'powerline-inactive1))
           ((eq 'line face) (if active 'powerline-active2 'powerline-inactive1))
           ((eq 'highlight face) (if active
                                     (funcall spaceline-highlight-face-func)
                                   'powerline-inactive1)))))

  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-spacemacs-theme)

  (use-package spaceline-all-the-icons
    :disabled
    :ensure nil
    :config
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-neotree)))

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
    (setq web-mode-code-indent-offset 4))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; display available bindings
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode t))

;; templates
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

;; yasnippet collection
(use-package yasnippet-snippets)

(provide 'init-packages)

;;; init-packages.el ends here
