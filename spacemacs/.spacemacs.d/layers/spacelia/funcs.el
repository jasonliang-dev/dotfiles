;; https://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs
(defun lia/run-external (command)
  "Run a shell COMMAND that use the current directory."
  (interactive "s")
  (shell-command (concat command " . > /dev/null 2>&1 & disown") nil nil))
