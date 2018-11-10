;;; lia-appearance.el --- Emacs Config

;;; Commentary:

;;
;; Change the appearance of Emacs
;;

;;; Code:

(require 'use-package)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  ;; (setq doom-modeline-height 35)
  :hook
  (after-init . doom-modeline-init))

(use-package spaceline
  :disabled t
  :init
  (setq powerline-default-separator 'slant
        powerline-height 25
        spaceline-separator-dir-left '(right . right)
        spaceline-separator-dir-right '(left . left)
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (require 'spaceline-config)

  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-helm-mode)
  (spaceline-spacemacs-theme))

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
