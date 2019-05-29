;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Just a Emacs config

;;; Code:

;; -- STARTUP OPTIMIZATIONS ------------------------------------------

;; avoid garbage collection until the end
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; disable file handler
(defvar lia--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; set gc and file handler back to default after all the dust has settled
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist lia--file-name-handler-alist)))

;; remove gui bars. done as early as possible
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; don't load site-start
(setq site-run-file nil)

;; be quiet at startup; don't load or display anything unnecessary
;; shamelessly stolen from doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core.el#L112
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
        mode-line-format nil))

;; -- PACKAGE SETUP --------------------------------------------------

(require 'package)

(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; install use-package if not installed already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; install packages declared by `use-package'
;; this doesn't work for me. is it because of lexical binding?
;; just add :ensure t to everything instead
;;(setq use-package-always-ensure t)

;; -- CUSTOMIZE FILE -------------------------------------------------

;; put emacs customize stuff in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; create the custom file if it doesn't exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file nil t)

;; -- LOAD LOCAL ELISP -----------------------------------------------

;; load files in lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))

(require 'lia-keybind)    ;; leader and other global hotkeys
(require 'lia-evil)       ;; evil mode specific configs
(require 'lia-appearance) ;; emacs appearance
(require 'lia-completion) ;; helm, code completion, snippets
(require 'lia-behaviour)  ;; additional behaviours that don't fit in previous sections
(require 'lia-org)        ;; org mode configuration

;; load languages
(require 'lang-c)
(require 'lang-clojure)
(require 'lang-css)
(require 'lang-data)
(require 'lang-elm)
(require 'lang-haskell)
(require 'lang-web)
(require 'lang-javascript)
(require 'lang-lua)
(require 'lang-markdown)
(require 'lang-php)

(lia/set-indent 2) ;; set default indentation level

;;; init.el ends here
