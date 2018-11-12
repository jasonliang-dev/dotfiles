;;; lia-evil.el --- Emacs Config

;;; Commentary:

;; anything evil-mode related goes here

;;; Code:

(require 'use-package)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit)

(provide 'lia-evil)

;;; lia-evil.el ends here
