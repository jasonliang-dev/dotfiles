;;; lia-appearance.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Change the appearance of Emacs

;;; Code:

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35
        doom-modeline-buffer-file-name-style 'relative-to-project)
  :hook (after-init . doom-modeline-init))

(use-package hide-mode-line
  :ensure t
  :hook (neotree-mode . hide-mode-line-mode))

(use-package linum-relative
  :ensure t
  :defer t
  :init
  (setq linum-format " %4d " ;; add padding
        linum-relative-format " %4s "
        ;; display current line number
        linum-relative-current-symbol ""))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; remove gui bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; highlight current line
(global-hl-line-mode)

;; highlight matching paren
(show-paren-mode t)

;; hide cursor except for selected window
(setq-default cursor-in-non-selected-windows nil)

;; set font size

;; +0.140s to startup time
;; (set-frame-font "Iosevka 11" nil t)

;; can't benchmark since esup crashes prematurely.
;; but I think it shaves about 0.1s compared to `set-frame-font' above
;; depends on the machine you're running on ofc
(add-to-list 'default-frame-alist '(font . "Iosevka 11"))

(provide 'lia-appearance)

;;; lia-appearance.el ends here
