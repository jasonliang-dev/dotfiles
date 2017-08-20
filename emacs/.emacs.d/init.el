;;; init.el --- My Emacs config

;;; Commentary:

;; 

;;; Code:

;; required by package
(package-initialize)

;; Add my config directory to load path
(defvar lia/config-directory
  (concat user-emacs-directory "lia"))
(add-to-list 'load-path lia/config-directory)

;; Move built in customization stuff to a different file
(setq custom-file
      (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; load my configuration files
(require 'lia-first)
(require 'lia-packages)
(require 'lia-appearance)
(require 'lia-org)
(require 'lia-settings)

;; Start evil mode at the end
(evil-mode t)

;;; init.el ends here
