;;; lia-behaviour.el --- Emacs Config

;;; Commentary:

;;
;; Change how Emacs behaves
;;

;;; Code:

(require 'use-package)

(use-package avy)

(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package dumb-jump)

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)

  (defun lia/use-eslint-from-node-modules ()
    "If exists, use local eslint.
https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'lia/use-eslint-from-node-modules))

(use-package magit)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package projectile
  :config
  (projectile-mode t))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package which-key
  :config
  (which-key-mode))

;; yes/no prompt is now y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show welcome screen
(setq inhibit-startup-screen t)

;; show column number
(setq column-number-mode t)

;; tabs are the enemy
(setq-default indent-tabs-mode nil)

;; move backup~ files to its own directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

;; no #autosave# files
(setq auto-save-default nil)

;; .h files open in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; indent `case' in switch/case
(c-set-offset 'case-label '+)

(defun lia/set-indent (n)
  "Set the indentation level to a value N."
  (interactive)
  (setq-default c-basic-offset n)
  (setq-default javascript-indent-level n) ;javascript-mode
  (setq-default js-indent-level n) ; js-mode, what's the difference?
                                   ; who knows?
  (setq-default js-switch-indent-offset n) ; switch-case indentation
  (setq-default css-indent-ofset n))

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
