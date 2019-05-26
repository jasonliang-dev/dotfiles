;;; lia-org.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; org-mode things

;;; Code:

(use-package org
  :ensure t
  :defer t
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
  ;; set the directory storing org files
  (setq org-directory (if (eq system-type 'windows-nt)
                          (concat (getenv "HOMEPATH") "\\Dropbox\\org\\")
                        "~/Dropbox/org/"))

  (setq
   ;; agenda file locations
   org-agenda-files (list org-directory)
   ;; hide repeating tasks (habits)
   ;; org-agenda-repeating-timestamp-show-all nil
   ;; use 12 hour clock
   org-agenda-timegrid-use-ampm t
   ;; agenda starts on current day
   org-agenda-start-on-weekday nil
   ;; show edits in invisible regions
   org-catch-invisible-edits 'show
   ;; target file for notes. capture notes here.
   org-default-notes-file (concat org-directory "index.org")
   ;; move the consistency graph to the right
   org-habit-graph-column 56
   ;; number of days after today in consistency graph
   org-habit-following-days 1
   ;; number of days before today in consistency graph
   org-habit-preceding-days 9
   ;; log time when done
   org-log-done 'time
   ;; log reschedules
   org-log-redeadline 'time
   org-log-reschedule 'time
   ;; hide bold italic markers
   ;; org-hide-emphasis-markers t
   ;; syntax highlighting in source blocks
   org-src-fontify-natively t
   ;; edit source blocks in current window
   org-src-window-setup 'current-window)

  ;; capture templates
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (setq org-capture-templates
        '(("c" "New task" entry (file+headline org-default-notes-file "Tasks")
           "** %?")
          ("l" "New task with link" entry (file+headline org-default-notes-file "Tasks")
           "** %?\n   %a")
          ("n" "New note" entry (file+headline org-default-notes-file "Notes")
           "** %?")
          ("e" "New event" entry (file+headline org-default-notes-file "Events")
           "** %?\n   SCHEDULED: %t")))

  ;; custom agenda view
  (setq org-agenda-custom-commands
        '(("c" "My agenda"
           ((alltodo ""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if
                         'todo 'done 'scheduled 'deadline))
                      (org-agenda-overriding-header "Unscheduled tasks")))
            (agenda "")))))
  :config
  ;; enable habits
  (require 'org-habit)

  ;; start agenda in normal mode
  (eval-after-load 'org-agenda
    '(progn (evil-set-initial-state 'org-agenda-mode 'normal)))

  ;; wrap lines in org mode
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; run source code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(provide 'lia-org)

;;; lia-org.el ends here
