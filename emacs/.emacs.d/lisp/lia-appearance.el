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

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (defvar neo-auto-indent-point)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (defvar neo-auto-indent-point)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+neotree/collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35
        doom-modeline-buffer-file-name-style 'relative-to-project)
  :hook (emacs-startup . doom-modeline-mode))

;; Enable font ligatures
;; Fira Code Symbol is required
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
(use-package fira-code-mode
  :hook ((prog-mode . fira-code-mode)
         (org-mode . fira-code-mode)))

(use-package hide-mode-line
  :ensure t
  :hook (neotree-mode . hide-mode-line-mode))

(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :general
  (:states
   'normal
   :keymaps 'neotree-mode-map
   "SPC" nil
   "h"   '+neotree/collapse-or-up
   "l"   '+neotree/expand-or-open)
  :init
  (setq neo-window-fixed-size nil
        neo-smart-open t
        neo-show-hidden-files t
        neo-theme 'icons
        neo-window-width 30)
  :config
  (doom-themes-neotree-config)
  (set-face-attribute 'doom-neotree-dir-face nil :family "Roboto Condensed")
  (set-face-attribute 'doom-neotree-file-face nil :family "Roboto Condensed"))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; remove gui bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; highlight current line when programming
(add-hook 'prog-mode-hook 'hl-line-mode)

;; highlight matching paren
(show-paren-mode t)

;; hide cursor except for selected window
(setq-default cursor-in-non-selected-windows nil)

;; display line numbers
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Visualize tabs and trailing whitespace
(setq-default whitespace-style '(face tabs tab-mark trailing))

;; Enable whitespace mode everywhere
(global-whitespace-mode)

;; set font
(add-to-list 'default-frame-alist '(font . "Fira Code 9"))

(provide 'lia-appearance)

;;; lia-appearance.el ends here
