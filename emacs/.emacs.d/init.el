;;; init.el --- Emacs Config

;;; Commentary:

;;
;; Just a simple Emacs config
;;

;;; Code:

(require 'package)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'lia-appearance)
(require 'lia-behaviour)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  :config
  (load-theme 'doom-spacegrey t)
  (doom-themes-visual-bell-config))

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package magit)

(use-package helm
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit company flycheck use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
