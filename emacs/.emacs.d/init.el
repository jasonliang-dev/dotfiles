(server-start)
;; move up with C-u
(setq evil-want-C-u-scroll t)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; have use-package automatically install packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(use-package auto-complete
  :init
  (ac-config-default))
(use-package base16-theme)
(use-package evil-leader
  :init
  (global-evil-leader-mode))
(use-package evil-numbers
  :init
  (global-set-key (kbd "C-c C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt))
(use-package evil-org)
(use-package evil-visual-mark-mode)
(use-package helm)
(use-package jdee)
(use-package magit)
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package yasnippet)

;; UI
;; no welcome screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; highlight current line
(global-hl-line-mode 1)
;; remove those ugly bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; font
(set-face-attribute 'default nil :font "Source Code Pro 10")
;; cursor colour
(set-cursor-color "#c0c5ce")
;; word wrap
(global-visual-line-mode t)

;; Org
;; timestamps when todo item is done
(setq org-log-done 'time)
;; agenda files
(setq org-agenda-files '("~/Dropbox/"))
;; todo states
(setq org-todo-keywords
            '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
;; org source block
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages '((C)
                             (R)
                             (asymptote)
                             (awk)
                             (calc)
                             (clojure)
                             (comint)
                             (css .t)
                             (ditaa)
                             (dot)
                             (emacs-lisp .t)
                             (fortran)
                             (gnuplot)
                             (haskell)
                             (io)
                             (java .t)
                             (js .t)
                             (latex .t)
                             (ledger)
                             (lilypond)
                             (lisp .t)
                             (matlab)
                             (maxima)
                             (mscgen)
                             (ocaml)
                             (octave)
                             (org .t)
                             (perl)
                             (picolisp)
                             (plantuml)
                             (python .t)
                             (ref)
                             (ruby .t)
                             (sass)
                             (scala)
                             (scheme)
                             (screen)
                             (sh .t)
                             (shen)
                             (sql)
                             (sqlite)))

;; scroll a line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; all yes/no prompts are y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; goto last change
(global-set-key [(control meta .)] 'goto-last-change)

;; these need to be at the bottom
(require 'evil-org 'evil)
(evil-mode t)
