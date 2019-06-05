;;; dream-eater.el  --- Dreamwaver check in/out -*- lexical-binding: t; -*-

;;; Commentary:

;; Acknowledge Dreamweaver's check in/out lock files

;; When this mode is enabled, all new buffers will be opened in read
;; only mode.  To make the buffer writable and create a lock file for
;; Dreamweaver users to see, run `dream-eater/check-out'.  To remove
;; the lock file and remove write access, run `dream-eater/check-in'.

;;; Code:

(defvar dream-eater/checkout-name "emacs")
(defvar dream-eater/email "example@domain.com")

(defun dream-eater--make-buffer-read-only ()
  "Make the current buffer read only.

All this function does is set `buffer-read-only' to t. The only
reason why this function exists is because of adding and removing
hooks."
  (setq buffer-read-only t))

(defun dream-eater--make-buffer-writable ()
  "Make the current buffer writable."
  (setq buffer-read-only nil))

(defun dream-eater/check-out ()
  "For the current buffer, grant write access and create a Dreamweaver lock file."
  (interactive)
  (let ((dream-eater--lock-file (concat buffer-file-name ".LCK"))
        (dream-eater--user-lock-contents
         (concat dream-eater/checkout-name "||" dream-eater/email)))
    (if (file-exists-p dream-eater--lock-file)
        (let ((dream-eater--lock-file-contents
               (with-temp-buffer
                 (insert-file-contents dream-eater--lock-file)
                 (buffer-string))))
          (if (string= dream-eater--lock-file-contents dream-eater--user-lock-contents)
              (message "You already checked out this file.")
            (message (concat
                      (car (split-string dream-eater--lock-file-contents "||"))
                      " has already checked out this file."))))
      (progn
        (dream-eater--make-buffer-writable)
        (write-region dream-eater--user-lock-contents nil dream-eater--lock-file)))))

(defun dream-eater/check-in ()
  "For the current buffer, save modifications, make read only, and remove the lock file."
  (interactive)
  nil)

(defun dream-eater--enable ()
  "Enable Dream Eater."
  (add-hook 'find-file-hook 'dream-eater--make-buffer-read-only))

(defun dream-eater--disable ()
  "Disable Dream Eater."
  (remove-hook 'find-file-hook 'dream-eater--make-buffer-read-only))

(define-minor-mode global-dream-eater-mode
  "Dream Eater minor mode"
  :lighter " DreamEater"
  :global t
  (if global-dream-eater-mode
      (dream-eater--enable)
    (dream-eater--disable)))

(provide 'dream-eater-mode)

;;; dream-eater.el ends here
