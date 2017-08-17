;;; init.el --- My Emacs config

;;; Commentary:

;; 

;;; Code:

;; required by packages
(package-initialize)

;; Move built in customization stuff to a different file
(setq custom-file
      (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Add my config directory to load path
(defvar lia/config-directory
  (concat user-emacs-directory "lia"))
(add-to-list 'load-path lia/config-directory)

(require 'lia-first)
(require 'lia-packages)
(require 'lia-appearance)
(require 'lia-org)
(require 'lia-settings)

(evil-mode t) ; Start evil mode at the end

;;; init.el ends here
