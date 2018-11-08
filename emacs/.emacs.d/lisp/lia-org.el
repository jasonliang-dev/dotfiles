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

(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file 'org-default-notes-file)
         "* TODO %?")))

;; add blank line for each heading
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

;; log time when done
(setq org-log-done (quote time))

;; log reschedules
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

(provide 'lia-org)

;;; lia-org.el ends here
