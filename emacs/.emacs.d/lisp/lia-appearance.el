;;; lia-appearance.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Change the appearance of Emacs

;;; Code:

(defun lia/toggle-line-number-type ()
  "Toggle the line number type between absolute and relative."
  (interactive)
  (defvar display-line-numbers-type)
  (setq display-line-numbers-type
        (if (eq display-line-numbers-type 'relative)
            (progn (message "Line number type: absolute") t)
          (progn (message "Line number type: relative") 'relative)))
  ;; update line numbers if it's currently being displayed
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers--turn-on)))

;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bspacemacs/spacemacs-ui-visual/funcs.el#L27

(use-package doom-themes
  :ensure t
  :config
  (defvar lia-theme)
  (load-theme lia-theme t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35
        doom-modeline-buffer-file-name-style 'buffer-name)
  :config (doom-modeline-mode))

;; Enable font ligatures
;; Fira Code Symbol is required
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
(use-package fira-code-mode
  :disabled t
  :hook ((prog-mode . fira-code-mode)
         (org-mode . fira-code-mode)))

(use-package hide-mode-line
  :ensure t
  :hook (neotree-mode . hide-mode-line-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; remove gui bars
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; set font
(defvar lia-font)
(add-to-list 'default-frame-alist `(font . ,lia-font))

;; highlight current line when programming
;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; highlight matching paren
(show-paren-mode t)

;; Enable whitespace mode everywhere
(global-whitespace-mode)

;; hide cursor except for selected window
(setq-default cursor-in-non-selected-windows nil)

;; display line numbers
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t)

;; use line numbers when coding
(defvar lia-use-line-numbers)
(when lia-use-line-numbers
    (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Visualize tabs and trailing whitespace
(setq-default whitespace-style '(face tabs tab-mark trailing))

(provide 'lia-appearance)

;;; lia-appearance.el ends here
