;;; dream-eater.el  --- Dreamwaver check in/out -*- lexical-binding: t; -*-

;;; Commentary:

;; Acknowledge Dreamweaver's check in/out lock files

;; When this mode is enabled, all new buffers will be opened in read
;; only mode.  To make the buffer writable and create a lock file for
;; Dreamweaver users to see, run `dream-eater/check-out'.  To remove
;; the lock file and remove write access, run `dream-eater/check-in'.

;;; Code:

(defvar dream-eater/check-out-name "emacs")
(defvar dream-eater/email "example@domain.com")

(defvar dream-eater--lock-file-list '()
  "Store the list of all lock files.")

(defun dream-eater--make-buffer-read-only ()
  "Make the current buffer read only.

All this function does is set `buffer-read-only' to t. The only
reason why this function exists is because of adding and removing
hooks.

My elisp-fu is trash."
  (setq buffer-read-only t))

(defun dream-eater--make-buffer-writable ()
  "Make the current buffer writable."
  (setq buffer-read-only nil))

(defun dream-eater--remove-lock-file (file)
  "Remove FILE from the lock file list and delete it from disk."
  (when (file-exists-p file)
    (delete-file file))
  (setq dream-eater--lock-file-list
        (delete file dream-eater--lock-file-list)))

(defun dream-eater--remove-current-buffer-lock-file ()
  "Remove the current buffer's lock file if it exists."
  (dream-eater--remove-lock-file (concat buffer-file-name ".LCK")))

(defun dream-eater--remove-all-lock-files ()
  "Remove all stored lock files."
  (mapc 'dream-eater--remove-lock-file dream-eater--lock-file-list))

(defun dream-eater/put ()
  "Save the current buffer.  Used over `save-buffer'.

Ensure that the lock file belongs to the user before saving to
avoid overriding other people's changes."
  (interactive)
  (let ((dream-eater--lock-file (concat buffer-file-name ".LCK"))
        (dream-eater--user-lock-contents
         (concat dream-eater/check-out-name "||" dream-eater/email)))
    (if (file-exists-p dream-eater--lock-file)
        (let ((dream-eater--lock-file-contents
               (with-temp-buffer
                 (insert-file-contents dream-eater--lock-file)
                 (buffer-string))))
          (if (string= dream-eater--lock-file-contents
                       dream-eater--user-lock-contents)
              (save-buffer)
            (message (concat
                      (car (split-string dream-eater--lock-file-contents "||"))
                      " has replaced your lock file."))))
      (message "Refusing to put changes without a lock file."))))

(defun dream-eater/check-out ()
  "For the current buffer, grant write access and create a Dreamweaver lock file."
  (interactive)
  (let ((dream-eater--lock-file (concat buffer-file-name ".LCK"))
        (dream-eater--user-lock-contents
         (concat dream-eater/check-out-name "||" dream-eater/email)))
    (if (file-exists-p dream-eater--lock-file)
        (let ((dream-eater--lock-file-contents
               (with-temp-buffer
                 (insert-file-contents dream-eater--lock-file)
                 (buffer-string))))
          (if (string= dream-eater--lock-file-contents
                       dream-eater--user-lock-contents)
              (message "You already checked out this file.")
            (message (concat ;; maybe add option to edit anyways?
                      (car (split-string dream-eater--lock-file-contents "||"))
                      " has already checked out this file."))))
      (progn
        (dream-eater--make-buffer-writable)
        (write-region dream-eater--user-lock-contents nil dream-eater--lock-file)
        (setq dream-eater--lock-file-list
              (cl-adjoin dream-eater--lock-file dream-eater--lock-file-list
                         :test 'string=))))))

(defun dream-eater/check-in ()
  "For the current buffer, save modifications, make read only, and remove the lock file."
  (interactive)
  (let ((dream-eater--lock-file (concat buffer-file-name ".LCK"))
        (dream-eater--user-lock-contents
         (concat dream-eater/check-out-name "||" dream-eater/email)))
    (if (file-exists-p dream-eater--lock-file)
        (let ((dream-eater--lock-file-contents
               (with-temp-buffer
                 (insert-file-contents dream-eater--lock-file)
                 (buffer-string))))
          (if (string= dream-eater--lock-file-contents
                       dream-eater--user-lock-contents)
              (progn
                (when (and (buffer-modified-p)
                           (y-or-n-p "There are unsaved changes.  Do you want to save? "))
                  (dream-eater/put))
                (dream-eater--make-buffer-read-only)
                (dream-eater--remove-current-buffer-lock-file))
            (message (concat
                      (car (split-string dream-eater--lock-file-contents "||"))
                      " has replaced your lock file."))))
      (message "File is not checked out."))))

(defun dream-eater--enable ()
  "Enable Dream Eater."
  (add-hook 'find-file-hook 'dream-eater--make-buffer-read-only)
  (add-hook 'kill-buffer-hook 'dream-eater--remove-current-buffer-lock-file)
  (add-hook 'kill-emacs-query-functions 'dream-eater--remove-all-lock-files))

(defun dream-eater--disable ()
  "Disable Dream Eater."
  (remove-hook 'find-file-hook 'dream-eater--make-buffer-read-only)
  (remove-hook 'kill-buffer-hook 'dream-eater--remove-current-buffer-lock-file)
  (remove-hook 'kill-emacs-query-functions 'dream-eater--remove-all-lock-files))

(define-minor-mode global-dream-eater-mode
  "Dream Eater minor mode"
  :lighter " DreamEater"
  :global t
  (if global-dream-eater-mode
      (dream-eater--enable)
    (dream-eater--disable)))

(provide 'dream-eater-mode)

;;; dream-eater.el ends here
