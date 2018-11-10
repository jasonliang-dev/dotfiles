;;; init.el --- Emacs Config

;;; Commentary:

;;; Just a Emacs config

;;; Code:

(require 'package)

;; avoid garbage collection until the end
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; disable file handler
(defvar lia/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

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

(defvar lia/global-indent 2)

;; load files in lisp directory

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'lia-evil)
(require 'lia-keybind)
(require 'lia-appearance)
(require 'lia-behaviour)
(require 'lia-language)
(require 'lia-org)

(lia/set-indent lia/global-indent)

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
    (spaceline which-key magit company flycheck use-package helm evil-visual-mark-mode))))
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
