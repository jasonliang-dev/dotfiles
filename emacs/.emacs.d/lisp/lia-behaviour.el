;;; lia-behaviour.el --- Emacs Config

;;; Commentary:

;;
;; Change how Emacs behaves (+ Keybindings).
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

;; https://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs
(defun lia/run-external (command)
  "Run a shell COMMAND that use the current directory."
  (interactive "s")
  (shell-command (concat command " . > /dev/null 2>&1 & disown") nil nil))

(global-set-key (kbd "C-c RET")
		#'(lambda () (interactive)
		    (lia/run-external "~/scripts/term.sh")))

(global-set-key (kbd "C-c SPC")
		#'(lambda () (interactive)
		    (lia/run-external "~/scripts/files.sh")))

(provide 'lia-behaviour)

;;; lia-behaviour.el ends here
