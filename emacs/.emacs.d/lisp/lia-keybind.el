;;; lia-keybind.el --- Emacs Config

;;; Commentary:

;;
;; set keybindings
;;

;;; Code:

(require 'use-package)

(use-package general
  :config
  ;; https://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs
  (defun lia/run-external (command)
    "Run a shell COMMAND that use the current directory."
    (interactive "s")
    (shell-command
     (concat command " . > /dev/null 2>&1 & disown") nil nil))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "TAB"   'mode-line-other-buffer
   "ESC"   'evil-ex-nohighlight
   "`"     'eshell
   ";"     'avy-goto-char-timer
   "f"     'helm-find-files
   "b"     'helm-mini
   "p"     'projectile-command-map
   "k"     'kill-this-buffer
   "g"     'magit-status
   "j"     'dumb-jump-go

   "wh"    'windmove-left
   "wj"    'windmove-down
   "wk"    'windmove-up
   "wl"    'windmove-right
   "ws"    'evil-window-split
   "wv"    'evil-window-vsplit

   "oa"    'org-agenda
   "oc"    '(lambda() (interactive) (org-capture nil "a"))

   "1"     '(lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "2"     '(lambda() (interactive) (find-file "~/Dropbox/org/outline.org"))
   "RET"   '(lambda () (interactive) (lia/run-external "~/scripts/term.sh"))
   "C-SPC" '(lambda () (interactive) (lia/run-external "~/scripts/files.sh")))

  (general-define-key
   "M-x"     'helm-M-x
   "C-x C-f" 'helm-find-files
   "C-x C-b" 'helm-mini)
  )

(provide 'lia-keybind)

;;; lia-keybind.el ends here
