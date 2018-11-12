;;; init.el --- Emacs Config

;;; Commentary:

;; Just a Emacs config

;;; Code:

;; avoid garbage collection until the end
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; disable file handler
(defvar lia/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; disable automatic package loading
(setq package-enable-at-startup nil)

;; don't add customize at the end of `init.el'
(setq package--init-file-ensured t)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; load files in lisp directory

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'lia-evil)
(require 'lia-keybind)
(require 'lia-appearance)
(require 'lia-behaviour)
(require 'lia-language)
(require 'lia-org)

;; set gc and file handler back to default
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist lia/file-name-handler-alist)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets json-mode markdown-mode elm-mode prettier-js company-tern emmet-mode less-css-mode web-mode clang-format yasnippet-snippets yasnippet which-key smooth-scrolling smartparens linum-relative helm-projectile helm flycheck dumb-jump company avy doom-modeline doom-themes general evil-magit evil-surround evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald" :height 1.3))))
 '(org-level-2 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald" :height 1.2))))
 '(org-level-3 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald" :height 1.1))))
 '(org-level-4 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald" :height 1.1))))
 '(org-level-5 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald"))))
 '(org-level-6 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald"))))
 '(org-level-7 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald"))))
 '(org-level-8 ((t (:inherit default :foreground "#bbc2cf" :font "Oswald")))))

;;; init.el ends here
