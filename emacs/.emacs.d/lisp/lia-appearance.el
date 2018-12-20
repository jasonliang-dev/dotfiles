;;; lia-appearance.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Change the appearance of Emacs

;;; Code:

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 35
        doom-modeline-buffer-file-name-style 'relative-to-project)
  :hook (after-init . doom-modeline-init))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; remove gui bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; highlight current line
(global-hl-line-mode)

;; highlight matching paren
(show-paren-mode t)

;; set font size

;; +0.140s to startup time
;; (set-frame-font "Iosevka 11" nil t)

;; can't benchmark since esup crashes prematurely.
;; but I think it shaves about 0.1s compared to `set-frame-font' above
(add-to-list 'default-frame-alist '(font . "Iosevka 11"))

(provide 'lia-appearance)

;;; lia-appearance.el ends here
