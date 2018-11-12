;;; lia-keybind.el --- Emacs Config

;;; Commentary:

;; leader key bindings

;;; Code:

(require 'use-package)

(use-package general
  :config
  ;; https://emacs.stackexchange.com/q/7650
  (defun run-external (command)
    "Run a shell COMMAND that use the current directory."
    (interactive "s")
    (shell-command
     (concat command " . > /dev/null 2>&1 & disown") nil nil))

  ;; https://emacs.stackexchange.com/q/7742
  (defun browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)
    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))

  ;; leader key
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ""    '(nil :wk "leader key")
   "ESC" '(evil-ex-nohighlight :wk "clear highlight")
   "TAB" '(mode-line-other-buffer :wk "other buffer")
   ";"   '(avy-goto-char-timer :wk "avy jump to")
   "`"   '(eshell :wk "terminal")
   "b"   '(helm-mini :wk "buffers")
   "f"   '(helm-find-files :wk "find files")
   "g"   '(magit-status :wk "git status")
   "i"   '("C-c C-f" :wk "format buffer")
   "j"   '(dumb-jump-go :wk "jump")
   "k"   '(kill-this-buffer :wk "kill buffer")
   "p"   '(projectile-command-map :wk "projectile prefix")
   "u"   '(undo-tree-visualize :wk "show undo tree")

   ;; toggles
   "t"  '(:ignore t :wk "toggles")
   "tl" '(linum-mode :wk "line numbers")
   "tr" '(linum-relative-toggle :wk "relative line numbers")

   ;; window navigation
   "H"  '(evil-window-move-far-left :wk "move window left")
   "J"  '(evil-window-move-very-bottom :wk "move window right")
   "K"  '(evil-window-move-very-top :wk "move window up")
   "L"  '(evil-window-move-far-right :wk "move window down")
   "w"  '(:ignore t :wk "window")
   "wh" '(windmove-left :wk "go left")
   "wj" '(windmove-down :wk "go down")
   "wk" '(windmove-up :wk "go up")
   "wl" '(windmove-right :wk "go right")
   "ws" '(evil-window-split :wk "horizontal split")
   "wv" '(evil-window-vsplit :wk "vertical split")
   "wo" '(delete-other-windows :wk "maximize")
   "wq" '(delete-window :wk "close")

   ;; org
   "a"  '((lambda() (interactive) (org-agenda nil "c")) :wk "my agenda")
   "oa" '(org-agenda :wk "agenda commands")
   "oc" '((lambda() (interactive) (org-capture nil "c")) :wk "add to todo list")

   ;; visit files
   "1" '((lambda() (interactive) (find-file "~/.emacs.d/init.el")) :wk "init.el")
   "2" '((lambda() (interactive) (find-file "~/Dropbox/org/todo.org")) :wk "todo list")
   "3" '((lambda() (interactive) (find-file "~/Dropbox/org/outline.org")) :wk "notes")

   ;; run external
   "RET"   '((lambda () (interactive) (run-external "~/scripts/term.sh")) :wk "external terminal")
   "C-SPC" '(browse-file-directory :wk "open file explorer")))

(provide 'lia-keybind)

;;; lia-keybind.el ends here
