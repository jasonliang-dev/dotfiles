(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-ocean)))
 '(custom-safe-themes
   (quote
    ("78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" default)))
 '(package-selected-packages
   (quote
    (yasnippet rainbow-delimiters magit evil-org evil-numbers evil-leader base16-theme use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; have use-package automatically install packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package base16-theme :ensure t)
(use-package evil-leader :ensure t)
(use-package evil-numbers :ensure t)
(use-package evil-org :ensure t)
(use-package evil-visual-mark-mode :ensure t)
(use-package helm :ensure t)
(use-package magit :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package yasnippet :ensure t)

;; UI
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
                             (latex)
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

;; Emacs is Vim
;; numbers
(global-set-key (kbd "C-c C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)
;; scroll a line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; move up with C-u
(setq evil-want-C-u-scroll t)
;; leader
(global-evil-leader-mode)

;; Keyboard
;; all prompts are y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; goto last change
(global-set-key [(control meta .)] 'goto-last-change)

;; these need to be at the bottom
(require 'evil-org 'evil)
(evil-mode t)
