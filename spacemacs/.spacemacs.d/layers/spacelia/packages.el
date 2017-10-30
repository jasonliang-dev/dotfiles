;;; packages.el --- spacelia layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jason Liang
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `spacelia-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacelia/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacelia/pre-init-PACKAGE' and/or
;;   `spacelia/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacelia-packages
  '(beacon
    buffer-move
    doom-themes
    general
    yasnippet-snippets
    writegood-mode
    writeroom-mode)


  "The list of Lisp packages required by the spacelia layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun spacelia/init-beacon ()
  (use-package beacon
    :config
    (setq beacon-blink-when-buffer-changes t
          beacon-blink-when-point-moves-vertically 10
          beacon-dont-blink-major-modes '(dired-mode
                                          neotree-mode
                                          magit-status-mode
                                          magit-popup-mode
                                          ranger-mode))
    (beacon-mode t)))

(defun spacelia/init-buffer-move ()
  (use-package buffer-move))

(defun spacelia/init-doom-themes ()
  (use-package doom-themes
    :config
    ;; use custom theme
    (load-theme 'doom-vibrant)
    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
    (set-face-attribute 'doom-neotree-dir-face nil
                        :family "Fira Sans Condensed")
    (set-face-attribute 'doom-neotree-file-face nil
                        :family "Fira Sans Condensed")
    (set-face-attribute 'doom-neotree-hidden-file-face nil
                        :family "Fira Sans Condensed")
    (set-face-attribute 'doom-neotree-text-file-face nil
                        :family "Fira Sans Condensed")
    (set-face-attribute 'doom-neotree-media-file-face nil
                        :family "Fira Sans Condensed")
    (set-face-attribute 'doom-neotree-data-file-face nil
                        :family "Fira Sans Condensed")

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(defun spacelia/init-general ()
  (use-package general
    :config
    ;; leader key
    (general-define-key
     :states '(normal visual motion insert emacs)
     :prefix "SPC"
     :non-normal-prefix "C-SPC"
     "RET" '((lambda () (interactive)
               (lia/run-external "~/scripts/term.sh"))
             :which-key "terminal")
     "C-SPC" '((lambda () (interactive)
                 (lia/run-external "~/scripts/files.sh"))
               :which-key "file manager"))

    ;; evil bindings
    (general-define-key
     :states '(normal visual motion)
     ;; move by visual line
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line

     ;; quickly navigate windows
     "C-h" 'evil-window-left
     "C-j" 'evil-window-down
     "C-k" 'evil-window-up
     "C-l" 'evil-window-right

     ;; yes. Don't judge me.
     "C-e" 'end-of-line
     "C-y" 'yank)

    ;; move buffer
    (general-define-key
     "C-S-h"   'buf-move-left
     "C-S-j"   'buf-move-down
     "C-S-k"   'buf-move-up
     "C-S-l"   'buf-move-right)

    ;; auto complete bindings
    (eval-after-load 'company
      (general-define-key
       :states 'insert
       :keymaps 'company-mode-map
       "C-n" 'company-select-next
       "C-p" 'company-select-previous))))

(defun spacelia/init-yasnippet-snippets ()
  (use-package yasnippet-snippets))

(defun spacelia/init-writegood-mode ()
  (use-package writegood-mode
    :config
    (add-hook 'org-mode-hook 'writegood-mode)))

(defun spacelia/init-writeroom-mode ()
  (use-package writeroom-mode))

;;; packages.el ends here
