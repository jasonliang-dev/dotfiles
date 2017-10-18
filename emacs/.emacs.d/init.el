;;; init.el --- My Emacs config

;;; Commentary:

;; This is my dodgy and messed up Emacs configuration!
;;
;; Most of the code here is copied and pasted from articles,
;; other people's dotfiles, Emacs Wiki, Reddit, Stack Exchange
;; and some other sources that I cannot remember.

;;; Code:

;; Move built in customization stuff to a different file
(defconst custom-file
  (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; dropbox directory
(defconst lia/dropbox-directory "~/Dropbox/")

;; planner file as org document
(defconst lia/planner-file (concat lia/dropbox-directory "org/planner.org"))

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

;; completion framework
(use-package helm
  :diminish helm-mode
  :init
  (helm-mode 1)
  :config
  ;; get Dash docsets
  (use-package helm-dash)

  ;; helm integration with projectile
  (use-package helm-projectile
    :config
    (helm-projectile-on))

  ;; search matches in another buffer
  (use-package helm-swoop))

;; manage keybindings
(use-package general
  :config
  ;; leader keybinds
  (general-define-key
   :states '(normal visual motion insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "RET" 'eshell
   "SPC" 'avy-goto-word-1
   "TAB" 'mode-line-other-buffer
   "br"  'revert-buffer
   "c"   'comment-region
   "d"   '((lambda () (interactive) (deer)) :which-key "deer")
   "e"   'org-export-dispatch
   "f"   'flycheck-next-error
   "F"   'flycheck-previous-error
   "gk"  'general-describe-keybindings
   "gs"  'magit-status
   "k"   'kill-this-buffer
   "r"   'er/expand-region
   "s"   'google-this-search
   "t"   'neotree-toggle
   "u"   'undo-tree-visualize
   "U"   'universal-argument
   "w"   'ace-window
   "W"   'lia/window-switch-split
   "ll"  'nlinum-mode
   "lr"  'nlinum-relative-toggle
   "o"   '(:ignore t :which-key "org")
   "oa"  'org-agenda
   "oc"  'org-capture
   "oi"  'org-toggle-inline-images
   "ok"  'org-archive-subtree-default
   "ol"  'org-insert-link
   "oo"  'ace-link-org
   "ot"  'org-todo
   "ow"  'writeroom-mode
   "1"   '((lambda () (interactive)
			 (find-file lia/dropbox-directory))
		   :which-key "Dropbox")
   "2"   '((lambda () (interactive)
			 (find-file (concat user-emacs-directory "init.el")))
		   :which-key "Emacs Config")
   "3"   '((lambda () (interactive)
			 (find-file lia/planner-file))
		   :which-key "Planner")
   "8"   'fci-mode

   ;; helm bindings
   "/" 'helm-swoop
   "m" 'helm-mini
   "p" 'helm-show-kill-ring
   "x" 'helm-M-x)

  ;; global bindings
  (general-define-key
   "C-S-h"   'buf-move-left
   "C-S-j"   'buf-move-down
   "C-S-k"   'buf-move-up
   "C-S-l"   'buf-move-right
   "C-c C-=" 'evil-numbers/inc-at-pt
   "C-c C--" 'evil-numbers/dec-at-pt
   "C-M-."   'goto-last-change

   ;; helm bindings
   "C-x C-f" 'helm-find-files
   "C-x C-r" 'helm-recentf
   "M-x" 'helm-M-x)

  ;; evil mode
  (general-define-key
   :states '(normal visual motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right

   "C-e" 'end-of-line
   "C-y" 'yank)

  ;; vimish-fold
  (general-define-key
   :states 'normal
   :keymaps 'prog-mode-map
   "TAB" 'vimish-fold-toggle)

  ;; company bindings
  (eval-after-load 'company
	(general-define-key
	 :states 'insert
	 :keymaps 'company-mode-map
	 "TAB" 'company-complete
	 "C-n" 'company-select-next
	 "C-p" 'company-select-previous))

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
   "C-r" 'neotree-refresh
   "q"   'neotree-hide
   "h"   'neotree-hidden-file-toggle
   "M"   'neotree-rename-node
   "C"   'neotree-create-node
   "D"   'neotree-delete-node
   "TAB" 'neotree-quick-look
   "RET" 'neotree-enter)

  ;; org mode bindings
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
   :states '(normal emacs)
   :keymaps 'doc-view-mode-map
   "h" (general-simulate-keys "p" t)
   "j" (general-simulate-keys "C-n" t)
   "k" (general-simulate-keys "C-p" t)
   "l" (general-simulate-keys "n" t)

   "J" (general-simulate-keys "SPC" t)
   "K" (general-simulate-keys "DEL" t)

   "g" 'pdf-view-first-page
   "G" 'pdf-view-last-page
   (kbd "C-o") 'pdf-history-backward
   (kbd "C-i") 'pdf-history-forward
   "m" 'pdf-view-position-to-register
   "'" 'pdf-view-jump-to-register
   "/" 'pdf-occur
   "o" 'pdf-outline
   "f" 'pdf-links-action-perform
   "b" 'pdf-view-midnight-minor-mode)

  ;; tetris
  ;; this is emacs after all.
  (general-define-key
   :keymaps 'tetris-mode-map
   "x" 'tetris-rotate-next
   "z" 'tetris-rotate-prev)
  )

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
  :diminish (aggressive-indent-mode . "🄸")
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

;; ICONS!
(use-package all-the-icons)

;; icons in dired
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; use auctex for tex/latex documents
(use-package auctex
  :defer t
  :config
  ;; https://www.emacswiki.org/emacs/AUCTeX#toc2
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; jump around quickly
(use-package avy
  :config
  (avy-setup-default))

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
  :diminish (company-mode . "🄲")
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; idle completion
  (setq company-idle-delay 0.2)
  ;; wrap around top and bottom of completion list
  (setq company-selection-wrap-around t)

  ;; quick documentation popup
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 1))

  ;; javascript
  (use-package company-tern
    :config
    (add-to-list 'company-backend 'company-tern)
    (add-hook 'js2-mode-hook (lambda () (tern-mode))))

  ;; completion for html, web mode
  (use-package company-web))

;; a better startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner
        (concat user-emacs-directory "icon/emacs-sexy.png")
        dashboard-items '((recents . 10)
						  (bookmarks . 10)
						  ;;(projects . 10)
                          (agenda . 10))))

;; hide or shorten minor modes
(use-package diminish
  :config
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (diminish 'all-the-icons-dired-mode)
  (diminish 'auto-revert-mode)
  (diminish 'flyspell-mode)
  (diminish 'tern-mode)
  (diminish 'visual-line-mode)
  (diminish 'undo-tree-mode))

;; doom themes
(use-package doom-themes
  :config
  ;; Load the theme
  (load-theme 'doom-vibrant t)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  (set-face-attribute 'doom-neotree-dir-face nil :family "Fira Sans Condensed")
  (set-face-attribute 'doom-neotree-file-face nil :family "Fira Sans Condensed")
  (set-face-attribute 'doom-neotree-hidden-file-face nil :family "Fira Sans Condensed")
  (set-face-attribute 'doom-neotree-text-file-face nil :family "Fira Sans Condensed")
  (set-face-attribute 'doom-neotree-media-file-face nil :family "Fira Sans Condensed")
  (set-face-attribute 'doom-neotree-data-file-face nil :family "Fira Sans Condensed")

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; emmet
(use-package emmet-mode
  :diminish (emmet-mode . "🄴")
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; erc highlight nicknames
(use-package erc-hl-nicks)

;; erc show images
(use-package erc-image
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; expand region
(use-package expand-region)

;; indicator at column 80
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80)
  ;;(add-hook 'prog-mode-hook 'fci-mode)
  )

;; syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  ;; use tidy in web mode
  (flycheck-add-mode 'html-tidy 'web-mode)
  ;; use csslint in css
  (flycheck-add-mode 'css-csslint 'css-mode)

  ;; use helm with flycheck
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

;; google
(use-package google-this
  :diminish (google-this-mode . "🄶")
  :config
  (google-this-mode t))

;; live html development
(use-package impatient-mode)

;; improved javascript mode
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

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
  :config
  (setq neo-smart-open t
        projectile-switch-project-action 'neotree-projectile-action
        neo-window-width 40
        neo-window-fixed-size nil
        neo-theme (if (display-graphic-p) 'icons 'arrow))

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
  (add-hook 'nlinum-mode-hook #'lia/nlinum-mode-hook)

  ;; relative line numbers
  (use-package nlinum-relative
    :config
    (setq nlinum-relative-current-symbol ""
          nlinum-relative-redisplay-delay 0)

    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)))

;; org mode
(use-package org
  :diminish (org-beamer-mode . "🄱")
  :config
  ;; blank lines between entries
  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))
  
  ;; custom ellipsis
  (setq org-ellipsis " ▼")

  ;; some images are too big. scale them
  (setq org-image-actual-width (/ (display-pixel-width) 3))

  ;; add timestamps when task is done, or rescheduled
  (setq org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time)

  ;; set agenda files
  (setq org-agenda-files
        (list lia/planner-file
			  (concat lia/dropbox-directory "org/timetable.org")))

  ;; set up org capture file
  (setq org-default-notes-file lia/planner-file)

  ;; custom todo keywords
  (setq org-todo-keywords
        '((sequence "[ ](t)" "[-](i)" "[*](w)" "|" "[X](d)" "[x](c)")))

  ;; org capture capture templates
  ;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
  ;; http://orgmode.org/manual/Template-expansion.html#Template-expansion
  (setq org-capture-templates
        '(("c" "Task" entry (file+headline lia/planner-file "Tasks")
           "* [ ] %?\n")
          ("f" "Task with file" entry (file+headline lia/planner-file "Tasks")
           "* [ ] %?\n  %a\n")))

  ;; pretty fonts in source code
  (setq org-src-fontify-natively t)

  ;; org source code languages
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t) (latex . t) (js . t)))

  ;; use xelatex as well as shell escape when exporting org document
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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
  (use-package org-pomodoro)

  ;; better agenda
  (use-package org-super-agenda
    :disabled
    :ensure nil
    :config
    (let ((org-super-agenda-groups
           '((:auto-category t))))
      (org-agenda-list))
    (org-super-agenda-mode t))

  ;; org export to bootstrap
  (use-package ox-twbs))


;; page break lines
(use-package page-break-lines
  :diminish (page-break-lines-mode . "🄻"))

;; powerline
(use-package powerline
  :config
  (setq powerline-default-separator 'slant
        powerline-height 35)
  
  ;; modeline from spacemacs
  (use-package spaceline
    :config
    (require 'spaceline-config)

    (setq spaceline-minor-modes-separator ""
          spaceline-separator-dir-left '(right . right)
          spaceline-separator-dir-right '(right . right)
          spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-spacemacs-theme)))

;; features for projects
(use-package projectile
  :config
  (projectile-mode t)
  ;; https://github.com/sviridov/.emacs.d/blob/master/config/base/init-diminish.el#L25
  (setq-default projectile-mode-line
                '(:eval (format "🄿【%s】" (projectile-project-name)))))

;; rainbow brackets
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; emulate ranger in dired
(use-package ranger)

;; `compile' alternative
(use-package smart-compile
  :config
  (add-to-list 'smart-compile-alist '("\\.java$" . "javac %f")))

;; deal with pairs of parentheses better
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; live javascript interaction
(use-package skewer-mode
  :diminish (skewer-mode . "🅂")
  :config
  ;; manually set the mode hooks
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; make the editor more sublime-y
(use-package sublimity
  :commands sublimity-mode
  :config
  (require 'sublimity-scroll))

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
    (emmet-mode)
    (skewer-html-mode))
  (add-hook 'web-mode-hook 'lia/web-mode-hook))

;; display available bindings
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode t))

;; distraction free editing
;; basically goyo vim
(use-package writeroom-mode)

;; templates
(use-package yasnippet
  :diminish (yas-minor-mode . "🅈")
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
 (font-candidate '"Iosevka 10" "Fira Mono 9" "Source Code Pro 9"
				 "Monego 9" "Ubuntu Mono 12") nil t)

;; stop the cursor from blinking
(blink-cursor-mode 0)

;; highlight the current line
(global-hl-line-mode)

;; enable wordwrap
(global-visual-line-mode t)

;; change certain text into symbols (lambda, >=, etc)
(global-prettify-symbols-mode t)

;; Show matching parentheses
;;(defvar show-paren-delay 0.2)
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
(setq mouse-wheel-scroll-amount '(5)    ; mouse scroll amount
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't)      ; scroll window under mouse

;; erc hide joins, quits
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

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
