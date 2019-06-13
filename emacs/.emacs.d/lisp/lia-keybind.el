;;; lia-keybind.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; leader and global key bindings

;;; Code:

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

(defun lia/goto-org-directory ()
  "Navigate to `org-directory'."
  (interactive)
  (defvar org-directory)
  (find-file org-directory))

(defun lia/iedit ()
  "Execute `iedit-mode' and `evil-iedit-state'."
  (interactive)
  (iedit-mode)
  (evil-iedit-state))

(defun lia/iedit-insert ()
  "Execute `iedit-mode' and `evil-iedit-insert-state'."
  (interactive)
  (iedit-mode)
  (evil-iedit-insert-state))

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
   "ESC" '(evil-ex-nohighlight :wk "clear highlight")
   "SPC" '(helm-M-x :wk "run command")
   "TAB" '(mode-line-other-buffer :wk "other buffer")
   ";"   '(avy-goto-char-2 :wk "avy jump to")
   "`"   '(lia/terminal :wk "terminal")
   "b"   '(helm-mini :wk "buffers")
   "e"   "C-x C-e"
   "F"   '(format-all-buffer :wk "format buffer")
   "f"   '(helm-find-files :wk "find files")
   "g"   '(magit-status :wk "git status")
   "j"   '(dumb-jump-go :wk "jump")
   "k"   '(kill-this-buffer :wk "kill buffer")
   "q"   '(evil-quit :wk "close buffer")
   "r"   '(revert-buffer :wk "reload file")
   "u"   '(undo-tree-visualize :wk "show undo tree")
   "v"   '(er/expand-region :wk "expand region")

   ;; toggles
   "t"  '(:ignore t :wk "toggles")
   "tl" '(display-line-numbers-mode :wk "line numbers")
   "tt" '(neotree-toggle :wk "neotree")

   ;; search
   "s"  '(:ignore t :wk "search")
   "sS" 'helm-multi-swoop-all
   "ss" 'helm-swoop

   ;; projectile
   "p"   '(:ignore t :wk "projectile")
   "p!"  'projectile-run-shell-command-in-root
   "p%"  'projectile-replace-regexp
   "p&"  'projectile-run-async-shell-command-in-root
   "pa"  'projectile-toggle-between-implementation-and-test
   "pb"  'helm-projectile-switch-to-buffer
   "pc"  'projectile-compile-project
   "pd"  'helm-projectile-find-dir
   "pD"  'projectile-dired
   "pd"  'projectile-find-dir
   "pf"  'helm-projectile-find-file
   "pF"  'helm-projectile-find-file-dwim
   "pg"  'projectile-find-tag
   "pG"  'projectile-regenerate-tags
   "ph"  'helm-projectile
   "pI"  'projectile-invalidate-cache
   "pk"  'projectile-kill-buffers
   "pp"  'helm-projectile-switch-project
   "pr"  'helm-projectile-recentf
   "pR"  'projectile-replace
   "pT"  'projectile-test-project
   "pv"  'projectile-vc
   "sgp" 'helm-projectile-grep

   ;; window navigation
   "w"  '(:ignore t :wk "window")
   "wh" '(windmove-left :wk "go left")
   "wj" '(windmove-down :wk "go down")
   "wk" '(windmove-up :wk "go up")
   "wl" '(windmove-right :wk "go right")
   "wH" '(evil-window-move-far-left :wk "move window left")
   "wJ" '(evil-window-move-very-bottom :wk "move window right")
   "wK" '(evil-window-move-very-top :wk "move window up")
   "wL" '(evil-window-move-far-right :wk "move window down")
   "ws" '(evil-window-split :wk "horizontal split")
   "wv" '(evil-window-vsplit :wk "vertical split")
   "wo" '(delete-other-windows :wk "maximize")
   "wq" '(delete-window :wk "close")
   "w=" '(balance-windows :wk "equal splits")

   ;; org
   "a"  '(lia/agenda :wk "my agenda")
   "o"  '(:ignore t :wk "org")
   "oa" '(org-agenda :wk "agenda commands")
   "oc" '(org-capture :wk "capture")
   "od" '(lia/goto-org-directory :wk "org directory")
   "oe" '(org-export-dispatch :wk "org export")
   "ol" '(org-open-at-point :wk "open link")
   "ot" '(org-todo :wk "change todo state")
   "ox" '(org-archive-subtree :wk "archive")

   ;; lsp-mode
   "l"   '(:ignore t :wk "lsp")
   "lf"  '(:ignore t :wk "find")
   "lfd" '(lsp-ui-peek-find-definitions :wk "definitions under point")
   "lfr" '(lsp-ui-peek-find-references :wk "references under point")
   "lh"  '(lsp-ui-doc-show :wk "show documentation")
   "lH"  '(lsp-ui-doc-hide :wk "hide documentation")
   "ls"  '(helm-lsp-workspace-symbol :wk "search symbol")

   ;; edit `init.el'
   "1" '(lia/config-file :wk "init.el")

   ;; run external
   "RET"   '(lia/external-terminal :wk "external terminal")
   "C-SPC" '(browse-file-directory :wk "open file explorer"))

  ;; motion state bindings
  (general-define-key
   :states '(normal visual motion)
   "C-a"     'evil-numbers/inc-at-pt
   "C-S-a"   'evil-numbers/dec-at-pt
   "C-c C-u" 'universal-argument
   "C-;"     'lia/iedit)

  ;; normal mode bindings
  (general-define-key
   :states 'normal
   "gcc" 'comment-line)

  ;; insert mode bindings
  (general-define-key
   :states 'insert
   "C-;" 'lia/iedit-insert)

  ;; visual mode bindings
  (general-define-key
   :states 'visual
   "gc" 'comment-dwim)

  ;; global bindings
  (general-define-key
   "C-s"  'save-buffer
   "<f5>" 'revert-buffer))

(provide 'lia-keybind)

;;; lia-keybind.el ends here
