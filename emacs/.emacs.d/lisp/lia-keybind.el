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

  ;; leader key
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "ESC"   'evil-ex-nohighlight
   "TAB"   'mode-line-other-buffer
   "?"     'which-key-show-major-mode
   ";"     'avy-goto-char-timer
   "`"     'eshell
   "b"     'helm-mini
   "f"     'helm-find-files
   "g"     'magit-status
   "j"     'dumb-jump-go
   "k"     'kill-this-buffer
   "p"     'projectile-command-map

   ;; window navigation
   "H"     'evil-window-move-far-left
   "J"     'evil-window-move-very-bottom
   "K"     'evil-window-move-very-top
   "L"     'evil-window-move-far-right
   "wh"    'windmove-left
   "wj"    'windmove-down
   "wk"    'windmove-up
   "wl"    'windmove-right
   "ws"    'evil-window-split
   "wv"    'evil-window-vsplit
   "wo"    'delete-other-windows
   "wq"    'delete-window

   ;; org
   "a"     'org-agenda-list
   "oa"    'org-agenda
   "oc"    '(lambda() (interactive) (org-capture nil "a"))

   ;; visit files
   "1"     '(lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "2"     '(lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
   "3"     '(lambda() (interactive) (find-file "~/Dropbox/org/outline.org"))

   ;; run external
   "RET"   '(lambda () (interactive) (lia/run-external "~/scripts/term.sh"))
   "C-SPC" '(lambda () (interactive) (lia/run-external "~/scripts/files.sh")))

  ;; helm
  (general-define-key
   "M-x"     'helm-M-x
   "C-x C-f" 'helm-find-files
   "C-x C-b" 'helm-mini)

  ;; c/c++
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps '(c-mode-map c++-mode-map)
   "C-c C-f" #'clang-format-buffer)

  ;; org-agenda
  (general-define-key
   :states 'normal
   :keymaps 'org-agenda-mode-map
   "RET" 'org-agenda-switch-to
   "c"   'org-agenda-goto-calendar
   "q"   'org-agenda-quit
   "r"   'org-agenda-redo
   "t"   'org-agenda-todo
   "H"   'org-agenda-do-date-earlier
   "L"   'org-agenda-do-date-later

   "1"  'org-agenda-day-view
   "2"  'org-agenda-week-view
   "3"  'org-agenda-fortnight-view
   "4"  'org-agenda-month-view
   "5"  'org-agenda-year-view

   "j"   'org-agenda-next-line
   "k"   'org-agenda-previous-line
   "J"   'org-agenda-next-date-line
   "K"   'org-agenda-previous-date-line
   "h"   'org-agenda-earlier
   "l"   'org-agenda-later)
  )

(provide 'lia-keybind)

;;; lia-keybind.el ends here
