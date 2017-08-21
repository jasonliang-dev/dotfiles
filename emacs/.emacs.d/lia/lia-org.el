;;; lia-org.el --- Part of my Emacs config

;;; Commentary:

;;

;;; Code:

(require 'org)

(define-key org-mode-map (kbd "C-c >") 'org-time-stamp-inactive)

(setq org-blank-before-new-entry '((heading) (plain-list-item)) ; blank lines between entries
      org-ellipsis " ⤵" ; custom ellipsis
      org-hide-emphasis-markers t ; hide formating characters
      org-log-done 'time ; add timestamps when task is done, or rescheduled
      org-log-redeadline 'time
      org-log-reschedule 'time)

;; set agenda files
;; https://www.reddit.com/r/emacs/comments/4z1pfn/allow_orgagendafiles_to_fail_gracefully/d6s62ue/
(setq org-agenda-files
      (delq nil
	    (mapcar (lambda (file) (and (file-exists-p file) file))
		    '("~/Dropbox/org/"))))

;; org source code languages
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages '((css . t)
			     (emacs-lisp . t)
			     (java . t)
			     (js . t)
			     (latex . t)
			     (lisp . t)
			     (org . t)
			     (perl . t)
			     (python . t)
			     (ruby . t)
			     (sh . t)))

;; custom todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)"
		  "IN-PROGRESS(i)"
		  "ON HOLD(h)"
		  "WAITING(w)"
		  "|"
		  "DONE(d)"
		  "CANCELED(c)")
	(sequence "[ ](T)"
		  "[-](I)"
		  "[*](W)"
		  "|"
		  "[X](D)")))

;; I meant 3:00 in the afternoon! not 3:00am!
(defvar time-range-with-pm-suffix '("1:00" . "6:59"))
(defun org-analyze-date-dwim (original-fun ans org-def org-defdecode)
  (let* ((time (funcall original-fun ans org-def org-defdecode))
         (minute (nth 1 time))
         (hour (nth 2 time))
         (minutes (+ minute (* 60 hour)))
         s)
    (when (and (< hour 12)
               (not (string-match "am" ans))
               (>= minutes (org-hh:mm-string-to-minutes (car time-range-with-pm-suffix)))
               (<= minutes (org-hh:mm-string-to-minutes (cdr time-range-with-pm-suffix))))
      (setf (nth 2 time) (+ hour 12))
      (when (boundp 'org-end-time-was-given)
        (setq s org-end-time-was-given)
        (if (and s (string-match "^\\([0-9]+\\)\\(:[0-9]+\\)$" s))
            (setq org-end-time-was-given
                  (concat (number-to-string (+ 12 (string-to-number (match-string 1 s))))
                          (match-string 2 s))))))
    time))

(advice-add 'org-read-date-analyze :around #'org-analyze-date-dwim)


(provide 'lia-org)

;;; lia-org.el ends here
