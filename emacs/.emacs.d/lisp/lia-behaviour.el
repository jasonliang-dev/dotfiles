;;; lia-behaviour.el --- Emacs Config

;;; Commentary:

;;
;; Change how Emacs behaves (+ Keybindings).
;;

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq-default indent-tabs-mode nil)

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
