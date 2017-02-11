;; Some changes before loading the main config file
;; remove bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-fringe-mode 3)
;; yes/no prompts are y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; load config.org
(package-initialize)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
