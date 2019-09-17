;;; lia-evil.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; anything evil-mode related goes here

;;; Code:

(defun lia/evil-window-split-and-focus ()
  "Split window horizontally and focus other window."
  (interactive)
  (evil-window-split)
  (other-window 1))

(defun lia/evil-window-vsplit-and-focus ()
  "Split window vertically and focus other window."
  (interactive)
  (evil-window-vsplit)
  (other-window 1))

(use-package evil
  :ensure t
  :hook (emacs-startup . evil-mode)
  :bind
  (([remap evil-next-line] . 'evil-next-visual-line)
   ([remap evil-previous-line] . 'evil-previous-visual-line)
   ([remap evil-beginning-of-line] . 'evil-beginning-of-visual-line)
   ([remap evil-end-of-line] . 'evil-end-of-line)
   ([remap evil-window-split] . 'lia/evil-window-split-and-focus)
   ([remap evil-window-vsplit] . 'lia/evil-window-vsplit-and-focus))
  :init
  ;; scroll with C-u
  (setq evil-want-C-u-scroll t)
  ;; let evil-collection handle keys
  (setq evil-want-keybinding nil)
  ;; emacs movement in insert mode
  (setq evil-disable-insert-state-bindings t)
  ;; vim search behaviour
  (setq evil-search-module 'evil-search)
  ;; leader key bindings
  (eval-after-load 'lia-keybind
    '(progn
       (lia-bind-leader "ESC" 'evil-ex-nohighlight)
       (lia-bind-leader "q"   'evil-quit)
       (lia-bind-leader "w"   'evil-window-map))))

(use-package evil-collection
  :ensure t
  :after evil
  :init (defvar evil-collection-outline-bind-tab-p nil) ;; TODO: inspect this
  :config (evil-collection-init))

(use-package evil-matchit
  :ensure t
  :after evil
  :hook (prog-mode . evil-matchit-mode))

(use-package evil-numbers
  :ensure t
  :after evil
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)
  :init
  (global-set-key (kbd "C-a")   'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-S-a") 'evil-numbers/dec-at-pt))

(use-package evil-snipe
  :ensure t
  :after evil
  :init (setq evil-snipe-repeat-keys nil)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after evil magit)

(provide 'lia-evil)

;;; lia-evil.el ends here
