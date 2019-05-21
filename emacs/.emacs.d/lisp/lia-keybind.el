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
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ""    '(nil :wk "leader key")
   "SPC" '(helm-M-x :wk "run command")
   "ESC" '(evil-ex-nohighlight :wk "clear highlight")
   "TAB" '(mode-line-other-buffer :wk "other buffer")
   ";"   '(avy-goto-char-timer :wk "avy jump to")
   "`"   '(lia/terminal :wk "terminal")
   "b"   '(helm-mini :wk "buffers")
   "e"   "C-x C-e"
   "f"   '(helm-find-files :wk "find files")
   "g"   '(magit-status :wk "git status")
   "j"   '(dumb-jump-go :wk "jump")
   "k"   '(kill-this-buffer :wk "kill buffer")
   "l"   '(org-open-at-point :wk "open link")
   "q"   '(evil-quit :wk "close buffer")
   "r"   '(revert-buffer :wk "reload file")
   "u"   '(undo-tree-visualize :wk "show undo tree")
   "v"   '(er/expand-region :wk "expand region")

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
   "a"  '(lia/agenda :wk "my agenda")
   "o"  '(:ignore t :wk "org")
   "oa" '(org-agenda :wk "agenda commands")
   "oc" '(lia/org-capture :wk "add to todo list")

   ;; edit `init.el'
   "1" '(lia/config-file :wk "init.el")

   ;; run external
   "RET"   '(lia/external-terminal :wk "external terminal")
   "C-SPC" '(browse-file-directory :wk "open file explorer"))

  ;; global bindings
  (general-define-key
   "C-s"     'save-buffer
   "<f5>"    'revert-buffer
   "C-c C-u" 'universal-argument))

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

(defun lia/terminal ()
  "Launch a terminal.
Run `eshell' if Emacs is running on Windows,
otherwise, run `ansi-term' with user shell."
  (interactive)
  (if (eq system-type 'windows-nt)
      (eshell)
    (ansi-term (getenv "SHELL"))))

(defun lia/external-terminal ()
  "Open a new terminal window."
  (interactive)
  (run-external "EMACS_TERM=\"\" ~/scripts/term.sh"))

(defun lia/config-file ()
  "Edit Emacs config."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun lia/agenda ()
  "Show the agenda."
  (interactive)
  (org-agenda nil "c"))

(defun lia/org-capture ()
  "Add a todo item to the todo list."
  (interactive)
  (org-capture nil "c"))

(provide 'lia-keybind)

;;; lia-keybind.el ends here
