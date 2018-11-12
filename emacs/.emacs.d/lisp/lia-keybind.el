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
   "u"     'undo-tree-visualize

   ;; toggles
   "tl"    'linum-mode
   "tr"    'linum-relative-toggle

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
   "a"     '(lambda() (interactive) (org-agenda nil "c"))
   "oa"    'org-agenda
   "oc"    '(lambda() (interactive) (org-capture nil "c"))

   ;; visit files
   "1"     '(lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "2"     '(lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
   "3"     '(lambda() (interactive) (find-file "~/Dropbox/org/outline.org"))

   ;; run external
   "RET"   '(lambda () (interactive) (run-external "~/scripts/term.sh"))
   "C-SPC" 'browse-file-directory))

(provide 'lia-keybind)

;;; lia-keybind.el ends here
