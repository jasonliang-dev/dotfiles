;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Just a Emacs config

;;; Code:

;; -- EARLY INITIALIZATION -------------------------------------------

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
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; if elpa.gnu.org is down. try the mirror on github:
                         ;; ("gnu" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
                         ))

(package-initialize)

;; install use-package if not installed already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; -- CUSTOMIZE FILE -------------------------------------------------

;; put emacs customize stuff in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; create the custom file if it doesn't exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file nil t)

;; -- LOAD REST OF CONFIG --------------------------------------------

;; load secret file
(defvar lia-secret-file (if (eq system-type 'windows-nt)
                            (concat (getenv "HOMEPATH") "\\Dropbox\\lia-secret.el")
                          "~/Dropbox/lia-secret.el"))

(when (file-exists-p lia-secret-file)
  (load lia-secret-file nil t))

;; load files in emacs config directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))

;; load settings, a collection of things that I change frequently
(require 'lia-settings)

(require 'lia-keybind)    ;; leader and other global hotkeys
(require 'lia-evil)       ;; evil mode specific configs
(require 'lia-appearance) ;; emacs appearance
(require 'lia-completion) ;; helm, code completion, snippets
(require 'lia-editor)     ;; indentation, text manipulation
(require 'lia-behaviour)  ;; additional behaviours that don't fit in previous sections
(require 'lia-org)        ;; org mode configuration

;; load languages
(require 'lang-c)
(require 'lang-clojure)
(require 'lang-css)
(require 'lang-data)
(require 'lang-elm)
(require 'lang-haskell)
(require 'lang-javascript)
(require 'lang-lua)
(require 'lang-markdown)
;; (require 'lang-php) ;; <-- screw you
(require 'lang-prolog)
(require 'lang-scala)
(require 'lang-web)

;; -- EXTRAS ---------------------------------------------------------

;; set default indentation level
(lia/set-indent lia-indent-width)

;; set indentation with tabs or spaces
(if lia-use-tabs
    (lia/global-enable-tabs)
  (lia/global-disable-tabs))

;; stop doing M-x emacs-init-time everytime I start emacs
(message (emacs-init-time))

;;; init.el ends here