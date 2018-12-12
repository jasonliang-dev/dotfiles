;;; lia-appearance.el --- Emacs Config

;;; Commentary:

;; Change the appearance of Emacs

;;; Code:

(require 'use-package)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  ;; (setq doom-modeline-height 35)
  :hook
  (after-init . doom-modeline-init))

;; remove gui bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; highlight current line
(global-hl-line-mode)

;; highlight matching paren
(show-paren-mode t)

;; set font size
(set-frame-font "Iosevka 11" nil t)

(provide 'lia-appearance)

;;; lia-appearance.el ends here
