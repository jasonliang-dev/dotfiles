(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Better Defaults for Emacs
(add-to-list 'load-path "~/.emacs.d/better-defaults/")
(require 'better-defaults)

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;; Move to top/bottom of the window
(global-set-key (kbd "M-p") (function
                            (lambda ()
                            "Go to top of page."
                            (interactive)
                            (move-to-window-line 0))))

(global-set-key (kbd "M-n") (function
                            (lambda ()
                            "Go to bottom of page."
                            (interactive)
                            (move-to-window-line -1))))

;; Turn off splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Tabs are spaces
(setq tab-width 2
      indent-tabs-mode nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Reduce echo keystroke time
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Highlight parenthesis
(show-paren-mode t)

;; Show column number
(setq column-number-mode t)

;; Convert to lowercase
(put 'downcase-region 'disabled nil)
