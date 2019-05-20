;;; lang-haskell.el --- Language Support -*- lexical-binding: t; -*-

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
                (let ((tmp-buf (generate-new-buffer "tmp-haskell-format"))
                      (formatted (shell-command-to-string
                                  (concat "brittany "
                                          (shell-quote-argument buffer-file-name)))))
                  (with-current-buffer tmp-buf (insert formatted))
                  (replace-buffer-contents tmp-buf)
                  (kill-buffer tmp-buf)))))

(use-package intero
  :ensure t
  :commands (intero-mode))

(provide 'lang-haskell)

;;; lang-haskell.el ends here
