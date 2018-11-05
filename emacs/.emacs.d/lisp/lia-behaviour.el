;;; lia-behaviour.el --- Emacs Config

;;; Commentary:

;;
;; Change how Emacs behaves
;;

;;; Code:

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

(lia/set-indent 2)

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
