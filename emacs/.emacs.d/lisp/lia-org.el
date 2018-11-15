;;; lia-org.el --- Emacs Config

;;; Commentary:

;; org-mode things

;;; Code:

(require 'use-package)

(use-package org
  :general
  (:states
   'normal
   :keymaps 'org-agenda-mode-map
   "RET" 'org-agenda-switch-to
   "."   'org-agenda-goto-today
   "a"   'org-agenda-archive-default-with-confirmation
   "c"   'org-agenda-goto-calendar
   "q"   'org-agenda-quit
   "r"   'org-agenda-redo
   "s"   'org-save-all-org-buffers
   "t"   'org-agenda-todo
   "u"   'org-agenda-undo

   "1"   'org-agenda-day-view
   "2"   'org-agenda-week-view
   "3"   'org-agenda-fortnight-view
   "4"   'org-agenda-month-view
   "5"   'org-agenda-year-view

   "H"   'org-agenda-do-date-earlier
   "L"   'org-agenda-do-date-later

   "j"   'org-agenda-next-line
   "k"   'org-agenda-previous-line
   "J"   'org-agenda-next-date-line
   "K"   'org-agenda-previous-date-line
   "h"   'org-agenda-earlier
   "l"   'org-agenda-later)
  :init
  (setq org-agenda-files '("~/Dropbox/org/")
        ;; agenda starts on current day
        org-agenda-start-on-weekday nil
        ;; target file for notes. capture notes here.
        org-default-notes-file "~/Dropbox/org/todo.org"
        ;; log time when done
        org-log-done 'time
        ;; log reschedules
        org-log-redeadline 'time
        org-log-reschedule 'time
        ;; hide bold italic markers
        org-hide-emphasis-markers t
        ;; syntax highlighting in source blocks
        org-src-fontify-natively t
        ;; edit source blocks in current window
        org-src-window-setup 'current-window)

  ;; capture format
  (setq org-capture-templates
        '(("c" "My TODO task format." entry
           (file 'org-default-notes-file)
           "* TODO %?"
           :prepend nil
           :empty-lines-before 0
           :empty-lines-after 0)))

  ;; custom agenda view
  (setq org-agenda-custom-commands
        '(("c" "My agenda"
           ((agenda "")
            (alltodo ""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if
                         'todo 'done 'scheduled 'deadline))
                      (org-agenda-overriding-header "Unscheduled tasks")))))))
  :config
  ;; start agenda in normal mode
  (eval-after-load 'org-agenda
    '(progn (evil-set-initial-state 'org-agenda-mode 'normal)))

  ;; wrap lines in org mode
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; run source code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t))))

(use-package org-bullets
  :init (setq org-bullets-bullet-list '(" "))
  :hook (org-mode . org-bullets-mode))

(provide 'lia-org)

;;; lia-org.el ends here
