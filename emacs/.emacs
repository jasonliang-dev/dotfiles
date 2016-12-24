;; A guy on the internet told me to put this in
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
 '(ansi-color-names-vector
   ["#2b303b" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#c0c5ce"])
 '(ansi-term-color-vector
   [unspecified "#2b303b" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#c0c5ce"])
 '(custom-enabled-themes (quote (base16-ocean)))
 '(custom-safe-themes
   (quote
    ("78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" default)))
 '(package-selected-packages
   (quote
    (neotree evil-org evil-leader linum-relative base16-theme magit evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 'evil
			  'base16-theme
			  'evil-leader
			  'evil-org
			  'helm
			  'linum-relative
			  'magit
			  'neotree)

;; THE GUI IS UGLY
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(set-default-font "Fantasque Sans Mono 10")
(set-cursor-color "#c0c5ce")
;; Make line numbers more readable
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; Remove annoying startup stuffs
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; TODO Hide leading stars in org mode

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Highlight current line
(global-hl-line-mode -1)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Run source code in org mode
(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
			     (python .t)
			     (java .t)))

;; Create timestamps when a TODO item is done
(setq org-log-done 'time)

;; Fix tab key for org mode
(setq evil-want-C-i-jump nil)

;; Scroll up with C-u
(setq evil-want-C-u-scroll t)

;; Enable leader key
;;(global-evil-leader-mode)

;; line numbers
(global-linum-mode 1)
(setq linum-format "%d ")
(linum-relative-global-mode)

;; toggle neotree
(global-set-key (kbd "M-n") 'neotree-toggle)

(require 'evil-org)
(require 'evil)
(evil-mode t)
