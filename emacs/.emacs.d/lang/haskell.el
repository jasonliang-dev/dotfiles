;;; haskell.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for Haskell

;;; Code:

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init (setq haskell-process-type 'stack-ghci)
  :general
  (:keymaps
   'haskell-mode-map
   "C-c C-f" '(lambda()
                (interactive)
                (save-buffer)
                (shell-command
                 (concat "brittany --write-mode=inplace "
                         (shell-quote-argument buffer-file-name)))
                (revert-buffer t t))))

(use-package intero
  :ensure t
  :commands (intero-mode))

(provide 'lang-haskell)

;;; haskell.el ends here
