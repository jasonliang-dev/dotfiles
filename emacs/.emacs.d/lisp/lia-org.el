;;; lia-org.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; org-mode things

;;; Code:

;; enable habits
(require 'org-habit)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list '("∙")))

(eval-after-load 'evil
  '(progn
     ;; start agenda in normal mode
     (evil-set-initial-state 'org-agenda-mode 'normal)

     ;; agenda bindings
     (evil-define-key 'normal org-agenda-mode-map
       (kbd "RET") 'org-agenda-switch-to
       "." 'org-agenda-goto-today
       "c" 'org-agenda-goto-calendar
       "q" 'org-agenda-quit
       "r" 'org-agenda-redo
       "s" 'org-save-all-org-buffers
       "t" 'org-agenda-todo
       "u" 'org-agenda-undo
       "x" 'org-agenda-archive

       "1" 'org-agenda-day-view
       "2" 'org-agenda-week-view
       "3" 'org-agenda-fortnight-view
       "4" 'org-agenda-month-view
       "5" 'org-agenda-year-view

       "H" 'org-agenda-do-date-earlier
       "L" 'org-agenda-do-date-later

       "j" 'org-agenda-next-line
       "k" 'org-agenda-previous-line
       "J" 'org-agenda-next-date-line
       "K" 'org-agenda-previous-date-line
       "h" 'org-agenda-earlier
       "l" 'org-agenda-later)))

;; go into insert mode when using org capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; wrap lines in org mode
(add-hook 'org-mode-hook 'visual-line-mode)
;; disable tabs in org mode.
(add-hook 'org-mode-hook 'lia/disable-tabs)

;; run source code blocks
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

;; set the directory storing org files
(setq org-directory (if (eq system-type 'windows-nt)
                        (concat (getenv "HOMEPATH") "\\Dropbox\\org\\")
                      "~/Dropbox/org/"))

;; agenda file locations
(setq org-agenda-files (list org-directory))

;; hide repeating tasks (habits)
;; (setq org-agenda-repeating-timestamp-show-all nil)

;; use 12 hour clock
(setq org-agenda-timegrid-use-ampm t)

;; agenda starts on current day
(setq org-agenda-start-on-weekday nil)

;; show edits in invisible regions
(setq org-catch-invisible-edits 'show)

;; target file for notes. capture notes here.
(setq org-default-notes-file (concat org-directory "index.org"))

;; move the consistency graph to the right
(setq org-habit-graph-column 56)
;; number of days after today in consistency graph
(setq org-habit-following-days 1)
;; number of days before today in consistency graph
(setq org-habit-preceding-days 9)

;; log time when done
(setq org-log-done 'time)

;; log deadlines
(setq org-log-redeadline 'time)
;; log reschedules
(setq org-log-reschedule 'time)

;; hide bold italic markers
;; (setq org-hide-emphasis-markers t)

;; syntax highlighting in source blocks
(setq org-src-fontify-natively t)

;; edit source blocks in current window
(setq org-src-window-setup 'current-window)

;; capture templates
;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-capture-templates
      '(("c" "New task" entry (file+headline org-default-notes-file "Tasks")
         "** %?")
        ("l" "New task with link" entry (file+headline org-default-notes-file "Tasks")
         "** %?\n   %a")))

;; custom agenda view
(setq org-agenda-custom-commands
      '(("c" "My agenda"
         ((alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if
                       'todo 'done 'scheduled 'deadline))
                    (org-agenda-overriding-header "Unscheduled tasks")))
          (agenda ""))
         ((org-agenda-compact-blocks t)))))

(provide 'lia-org)

;;; lia-org.el ends here
