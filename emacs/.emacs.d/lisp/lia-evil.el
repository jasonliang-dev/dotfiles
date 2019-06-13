;;; lia-evil.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; anything evil-mode related goes here

;;; Code:

(defun lia-evil-scroll-line-down ()
  "Scroll the view down by multiple lines."
  (interactive)
  (evil-scroll-line-down 3))

(defun lia-evil-scroll-line-up ()
  "Scroll the view up by multiple lines."
  (interactive)
  (evil-scroll-line-up 3))

(use-package evil
  :ensure t
  :hook (emacs-startup . evil-mode)
  :general
  ([remap evil-next-line]         'evil-next-visual-line
   [remap evil-previous-line]     'evil-previous-visual-line
   [remap evil-beginning-of-line] 'evil-beginning-of-visual-line
   [remap evil-end-of-line]       'evil-end-of-visual-line
   [remap evil-scroll-line-down]  'lia-evil-scroll-line-down
   [remap evil-scroll-line-up]    'lia-evil-scroll-line-up)
  :init
  ;; scroll with C-u
  (setq evil-want-C-u-scroll t)
  ;; let evil-collection handle keys
  (setq evil-want-keybinding nil)
  ;; emacs movement in insert mode
  (setq evil-disable-insert-state-bindings t))

(use-package evil-collection
  :ensure t
  :after evil
  :init (defvar evil-collection-outline-bind-tab-p nil)
  :config (evil-collection-init))

(use-package evil-iedit-state
  :ensure t
  :after evil iedit
  :commands (evil-iedit-state
             evil-iedit-insert-state))

(use-package evil-matchit
  :ensure t
  :after evil
  :hook (prog-mode . evil-matchit-mode))

(use-package evil-numbers
  :ensure t
  :after evil
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt))

(use-package evil-snipe
  :ensure t
  :after evil
  :init (setq evil-snipe-repeat-keys nil)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after evil magit)

(provide 'lia-evil)

;;; lia-evil.el ends here
