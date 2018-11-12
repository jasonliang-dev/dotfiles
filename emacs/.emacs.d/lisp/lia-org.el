;;; lia-org.el --- Emacs Config

;;; Commentary:

;;
;; org-mode things
;;

;;; Code:

(require 'use-package)

(use-package org-bullets
  :init (setq org-bullets-bullet-list '(" "))
  :hook (org-mode . org-bullets-mode))

(setq org-agenda-files '("~/Dropbox/org/"))

;; target file for notes. capture notes here.
(setq org-default-notes-file "~/Dropbox/org/todo.org")

;; capture format
(setq org-capture-templates
      '(("c" "My TODO task format." entry
         (file 'org-default-notes-file)
         "* TODO %?"
         :prepend nil
         :empty-lines-before 0
         :empty-lines-after 0)))

;; start agenda in normal mode
(eval-after-load 'org-agenda
 '(progn (evil-set-initial-state 'org-agenda-mode 'normal)))

;; custom agenda view
(setq org-agenda-custom-commands
      '(("c" "My agenda"
         ((agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if
                       'todo 'done 'scheduled 'deadline))
                    (org-agenda-overriding-header "Unscheduled tasks")))))))
;; log time when done
(setq org-log-done (quote time))

;; log reschedules
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;; hide bold italic markers
(setq org-hide-emphasis-markers t)

;; wrap lines in org mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; change heading appearance
(let* ((variable-tuple
        (cond
         ((x-list-fonts "Oswald")          '(:font "Oswald"))
         ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
         ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
         (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.3))))))

(provide 'lia-org)

;;; lia-org.el ends here
