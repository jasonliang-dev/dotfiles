;;; lia-evil.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; anything evil-mode related goes here

;;; Code:

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :general
  (:states
   '(normal visual motion)
   [remap evil-next-line] 'evil-next-visual-line
   [remap evil-previous-line] 'evil-previous-visual-line
   [remap evil-beginning-of-line] 'evil-beginning-of-visual-line
   [remap evil-end-of-line] 'evil-end-of-visual-line
   [remap evil-scroll-line-down] 'lia/evil-scroll-line-down
   [remap evil-scroll-line-up] 'lia/evil-scroll-line-up)
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-search-module 'evil-search))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config (global-evil-surround-mode))

(use-package evil-magit
  :ensure t
  :after (evil magit))

(defun lia/evil-scroll-line-down ()
  "Scroll the view down by multiple lines."
  (interactive)
  (evil-scroll-line-down 3))

(defun lia/evil-scroll-line-up ()
  "Scroll the view up by multiple lines."
  (interactive)
  (evil-scroll-line-up 3))

(provide 'lia-evil)

;;; lia-evil.el ends here
