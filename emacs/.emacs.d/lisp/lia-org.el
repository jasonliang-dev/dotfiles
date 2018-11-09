;;; lia-org.el --- Emacs Config

;;; Commentary:

;;
;; org-mode things
;;

;;; Code:

;; default todo cycle
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED")))

(setq org-agenda-files '("~/Dropbox/org/"))

;; target file for notes. capture notes here.
(setq org-default-notes-file "~/Dropbox/org/todo.org")

;; capture format
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file 'org-default-notes-file)
         "* TODO %?"
         :prepend nil
         :empty-lines-before 0
         :empty-lines-after 0)))

;; log time when done
(setq org-log-done (quote time))

;; log reschedules
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;; start agenda in normal mode
(eval-after-load 'org-agenda
 '(progn
    (evil-set-initial-state 'org-agenda-mode 'normal)))

(provide 'lia-org)

;;; lia-org.el ends here
