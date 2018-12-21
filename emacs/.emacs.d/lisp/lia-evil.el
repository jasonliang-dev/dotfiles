;;; lia-evil.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; anything evil-mode related goes here

;;; Code:

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-magit
  :after (evil magit))

(provide 'lia-evil)

;;; lia-evil.el ends here
