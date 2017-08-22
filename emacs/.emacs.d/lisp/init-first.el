;; init-first.el --- Part of my Emacs config

;;; Commentary:

;; The following code runs before anything else

;;; Code:

;; remove bars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p) ; yes/no prompts are y/n

(provide 'init-first)

;;; init-first.el ends here
