;;; lia-keybind.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; leader key bindings

;;; Code:

(use-package general
  :ensure t
  :config
  ;; leader key
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ""    '(nil :wk "leader key")
   "SPC" '(helm-M-x :wk "run command")
   "ESC" '(evil-ex-nohighlight :wk "clear highlight")
   "TAB" '(mode-line-other-buffer :wk "other buffer")
   ";"   '(avy-goto-char-timer :wk "avy jump to")
   "`"   '(eshell :wk "terminal")
   "b"   '(helm-mini :wk "buffers")
   "f"   '(helm-find-files :wk "find files")
   "g"   '(magit-status :wk "git status")
   "j"   '(dumb-jump-go :wk "jump")
   "k"   '(kill-this-buffer :wk "kill buffer")
   "l"   '(org-open-at-point :wk "open link")
   "q"   '(evil-quit :wk "close buffer")
   "r"   '(revert-buffer :wk "reload file")
   "u"   '(undo-tree-visualize :wk "show undo tree")

   ;; toggles
   "t"  '(:ignore t :wk "toggles")
   "tl" '(display-line-numbers-mode :wk "line numbers")
   "tt" '(neotree-toggle :wk "neotree")

   ;; helm
   "ss" 'helm-swoop
   "sS" 'helm-multi-swoop-all

   ;; projectile
   "p"   '(:ignore t :wk "projectile")
   "p!"  'projectile-run-shell-command-in-root
   "p%"  'projectile-replace-regexp
   "p&"  'projectile-run-async-shell-command-in-root
   "pD"  'projectile-dired
   "pF"  'helm-projectile-find-file-dwim
   "pG"  'projectile-regenerate-tags
   "pI"  'projectile-invalidate-cache
   "pR"  'projectile-replace
   "pT"  'projectile-test-project
   "pa"  'projectile-toggle-between-implementation-and-test
   "pb"  'helm-projectile-switch-to-buffer
   "pc"  'projectile-compile-project
   "pd"  'helm-projectile-find-dir
   "pd"  'projectile-find-dir
   "pf"  'helm-projectile-find-file
   "pg"  'projectile-find-tag
   "ph"  'helm-projectile
   "pk"  'projectile-kill-buffers
   "pp"  'helm-projectile-switch-project
   "pr"  'helm-projectile-recentf
   "pv"  'projectile-vc
   "sgp" 'helm-projectile-grep

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
   "w=" '(balance-windows :wk "equal splits")

   ;; org
   "a"  '((lambda() (interactive) (org-agenda nil "c")) :wk "my agenda")
   "o"  '(:ignore t :wk "org")
   "oa" '(org-agenda :wk "agenda commands")
   "oc" '((lambda() (interactive) (org-capture nil "c")) :wk "add to todo list")

   ;; visit files
   "1" '((lambda() (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :wk "init.el")
   "2" '((lambda() (interactive) (find-file "~/Dropbox/org/todo.org")) :wk "todo list")
   "3" '((lambda() (interactive) (find-file "~/Dropbox/org/outline.org")) :wk "notes")

   ;; run external
   "RET"   '((lambda () (interactive) (run-external "~/scripts/term.sh")) :wk "external terminal")
   "C-SPC" '(browse-file-directory :wk "open file explorer"))

  ;; global bindings
  (general-define-key
   "C-s" 'save-buffer
   "<f5>" 'revert-buffer))

;; https://emacs.stackexchange.com/q/7650
;;;###autoload
(defun run-external (command)
  "Run a shell COMMAND that use the current directory."
  (interactive "s")
  (shell-command
   (concat command " . > /dev/null 2>&1 & disown") nil nil))

;; https://emacs.stackexchange.com/q/7742
;;;###autoload
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(provide 'lia-keybind)

;;; lia-keybind.el ends here
