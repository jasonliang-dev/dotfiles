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

;; put emacs customize stuff in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;;; init.el ends here
