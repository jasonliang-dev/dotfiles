;;; lia-keybind.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; leader and global key bindings

;;; Code:

(require 'seq)

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
otherwise, run `ansi-term'."
  (interactive)
  (cond ((eq system-type 'windows-nt)
         (eshell))
        ((get-buffer "*ansi-term*")
         (switch-to-buffer "*ansi-term*"))
        (t (ansi-term "/bin/bash"))))

(defun lia/external-terminal ()
  "Open a terminal window.
If there's a tmux session, create a new tmux window and focus the
scratchpad terminal.
Otherwise, open a regular terminal window."
  (interactive)
  (if (zerop (shell-command
              (concat "tmux new-window -c '" (expand-file-name default-directory) "'")))
      (call-process-shell-command "DISABLE_WAL=\"\" ~/scripts/scratchpad.sh" nil 0)
    ;; TODO: terminal crashes here.
    (shell-command "DISABLE_WAL=\"\" ~/scripts/term.sh . 2>&1 > /dev/null & disown")))

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

(defun lia-bind-leader (KEY DEF)
  "Add a keybinding using the leader key.

binds <leader>KEY to run DEF."
  (defvar lia-leader-map)
  (define-key lia-leader-map (kbd KEY) DEF))

(define-minor-mode lia-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t
  :keymap (make-sparse-keymap))

(lia-intercept-mode)

;; create the leader keymap
(define-prefix-command 'lia-leader-map)

;; bind leader key to keymap
(eval-after-load 'evil
  '(progn
     (defvar lia-leader-key)
     (defvar lia-leader-alt-key)

     (dolist (state '(normal visual insert))
       (evil-make-intercept-map
        ;; NOTE: This requires an evil version from 2018-03-20 or later
        (evil-get-auxiliary-keymap lia-intercept-mode-map state t t)
        state))

     (evil-define-key 'motion lia-intercept-mode-map
       (kbd lia-leader-key) 'lia-leader-map)
     (evil-define-key 'insert lia-intercept-mode-map
       (kbd lia-leader-alt-key) 'lia-leader-map)
     (evil-define-key 'emacs lia-intercept-mode-map
       (kbd lia-leader-alt-key) 'lia-leader-map)

     (lia-bind-leader "ESC" 'evil-ex-nohighlight)
     (lia-bind-leader "SPC" 'helm-M-x)
     (lia-bind-leader "TAB" 'mode-line-other-buffer)
     (lia-bind-leader ";"   'avy-goto-char-2)
     (lia-bind-leader "`"   'lia/terminal)
     (lia-bind-leader ","   'rename-buffer)
     (lia-bind-leader "b"   'helm-mini)
     (lia-bind-leader "e"   'eval-last-sexp)
     (lia-bind-leader "F"   'format-all-buffer)
     (lia-bind-leader "f"   'helm-find-files)
     (lia-bind-leader "g"   'magit-status)
     (lia-bind-leader "j"   'dumb-jump-go)
     (lia-bind-leader "k"   'kill-this-buffer)
     (lia-bind-leader "q"   'evil-quit)
     (lia-bind-leader "r"   'revert-buffer)
     (lia-bind-leader "u"   'undo-tree-visualize)
     (lia-bind-leader "v"   'er/expand-region)
     (lia-bind-leader "w"   'evil-window-map)

     ;; toggles
     (lia-bind-leader "tl" 'display-line-numbers-mode)
     (lia-bind-leader "tt" 'neotree-toggle)

     ;; search
     (lia-bind-leader "sS" 'helm-multi-swoop-all)
     (lia-bind-leader "ss" 'helm-swoop)

     ;; projectile
     (lia-bind-leader "p!"  'projectile-run-shell-command-in-root)
     (lia-bind-leader "p%"  'projectile-replace-regexp)
     (lia-bind-leader "p&"  'projectile-run-async-shell-command-in-root)
     (lia-bind-leader "pa"  'projectile-toggle-between-implementation-and-test)
     (lia-bind-leader "pb"  'helm-projectile-switch-to-buffer)
     (lia-bind-leader "pc"  'projectile-compile-project)
     (lia-bind-leader "pd"  'helm-projectile-find-dir)
     (lia-bind-leader "pD"  'projectile-dired)
     (lia-bind-leader "pd"  'projectile-find-dir)
     (lia-bind-leader "pf"  'helm-projectile-find-file)
     (lia-bind-leader "pF"  'helm-projectile-find-file-dwim)
     (lia-bind-leader "pg"  'projectile-find-tag)
     (lia-bind-leader "pG"  'projectile-regenerate-tags)
     (lia-bind-leader "ph"  'helm-projectile)
     (lia-bind-leader "pI"  'projectile-invalidate-cache)
     (lia-bind-leader "pk"  'projectile-kill-buffers)
     (lia-bind-leader "pp"  'helm-projectile-switch-project)
     (lia-bind-leader "pr"  'helm-projectile-recentf)
     (lia-bind-leader "pR"  'projectile-replace)
     (lia-bind-leader "pT"  'projectile-test-project)
     (lia-bind-leader "pv"  'projectile-vc)
     (lia-bind-leader "sgp" 'helm-projectile-grep)

     ;; org
     (lia-bind-leader "a"  'lia/agenda)
     (lia-bind-leader "oa" 'org-agenda)
     (lia-bind-leader "oc" 'org-capture)
     (lia-bind-leader "od" 'lia/goto-org-directory)
     (lia-bind-leader "oe" 'org-export-dispatch)
     (lia-bind-leader "ol" 'org-open-at-point)
     (lia-bind-leader "ot" 'org-todo)
     (lia-bind-leader "ox" 'org-archive-subtree)

     ;; edit `init.el'
     (lia-bind-leader "1" 'lia/config-file)

     ;; run external
     (lia-bind-leader "RET"   'lia/external-terminal)
     (lia-bind-leader "C-SPC" 'browse-file-directory)))

(global-set-key (kbd "C-s")   'save-buffer)
(global-set-key (kbd "C-a")   'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-S-a") 'evil-numbers/dec-at-pt)

(provide 'lia-keybind)

;;; lia-keybind.el ends here
