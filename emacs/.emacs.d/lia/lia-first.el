;; lia-first.el --- Part of my Emacs config

;;; Commentary:

;; The following code runs before anything else

;;; Code:

;; remove bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;(toggle-frame-maximized) ; open emacs maximized

(fset 'yes-or-no-p 'y-or-n-p) ; yes/no prompts are y/n

(provide 'lia-first)

;;; lia-first.el ends here
