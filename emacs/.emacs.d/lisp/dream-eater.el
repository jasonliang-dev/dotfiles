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

(defvar dream-eater--default-cursor-color nil
  "Save the user's cursor color.")

(defvar dream-eater--checked-out-list '()
  "Store the list of all files checked out.")

;; store functions as dream eater mode fsets them
;; so they don't get lost after dream eater mode is disabled
(setf (symbol-function 'dream-eater--save-buffer)
      (symbol-function 'save-buffer))

(setf (symbol-function 'dream-eater--read-only-mode)
      (symbol-function 'read-only-mode))

(declare-function dream-eater--save-buffer "dream-eater.el")

(defun dream-eater--lock-file-status (file)
  "Given FILE, return `owned', `disowned', or `no-lock'.

return `owned' if lock file exists and lock file belongs to this user.
return `disowned' if lock file exists and lock file doesn't belong to this user.
return `no-lock' if lock file doesn't exist."
  (let ((dream-eater--lock-file (concat file ".LCK"))
        (dream-eater--user-lock-contents
         (concat dream-eater/check-out-name "||" dream-eater/email)))
    (if (file-exists-p dream-eater--lock-file)
        (let ((dream-eater--lock-file-contents
               (with-temp-buffer
                 (insert-file-contents dream-eater--lock-file)
                 (buffer-string))))
          (if (string= dream-eater--lock-file-contents
                       dream-eater--user-lock-contents)
              (list 'owned dream-eater--lock-file)
            (cons 'disowned (split-string dream-eater--user-lock-contents "||"))))
      (list 'no-lock dream-eater--lock-file dream-eater--user-lock-contents))))

(defun dream-eater--make-buffer-read-only ()
  "Make the current buffer read only.

All this function does is set `buffer-read-only' to t. The only
reason why this function exists is because of adding and removing
hooks."
  (setq buffer-read-only t))

(defun dream-eater--make-buffer-writable ()
  "Make the current buffer writable."
  (setq buffer-read-only nil))

(defun dream-eater--on-open ()
  "Perform buffer behaviour depending on lock file status."
  (let ((lock-file-state (dream-eater--lock-file-status (buffer-file-name))))
    (pcase (car lock-file-state)
      ('owned (message "Warning: Your lock file still exists for this file."))
      (_ (dream-eater--make-buffer-read-only)))))

(defun dream-eater--remove-lock-file (file)
  "Remove the lock file associated with FILE from disk.

Return nil if lock file was not removed (maybe the lock file
belongs to another user).  Return t otherwise."
  (let ((lock-file-state (dream-eater--lock-file-status file)))
    (pcase (car lock-file-state)
      ('owned
       (progn
         (delete-file (car (cdr lock-file-state)))
         (setq dream-eater--checked-out-list
               (delete file dream-eater--checked-out-list))
         (message (concat "Removed " (car (cdr lock-file-state))))
         t))
      ('disowned
       (progn
         (message (concat "Refusing to remove "
                          (car (cdr lock-file-state))
                          "'s lock file."))
         nil))
      ('no-lock t))))

(defun dream-eater--remove-current-buffer-lock-file ()
  "Remove the current buffer's lock file if it exists."
  (dream-eater--remove-lock-file buffer-file-name))

(defun dream-eater--remove-all-lock-files ()
  "Remove all stored lock files.

Return nil if any lock files were not removed (maybe the lock
file belongs to another user).  Return t if all lock files were
removed."
  (seq-every-p 'identity
               (mapcar 'dream-eater--remove-lock-file
                       dream-eater--checked-out-list)))

(defun dream-eater/put ()
  "Save the current buffer.  Used over `save-buffer'.

Ensure that the lock file belongs to the user before saving to
avoid overriding other people's changes."
  (interactive)
  (let ((lock-file-state (dream-eater--lock-file-status (buffer-file-name))))
    (pcase (car lock-file-state)
      ('owned (dream-eater--save-buffer))
      ('disowned (message (concat (car (cdr lock-file-state))
                                  " has replaced your lock file.")))
      ('no-lock (message "Refusing to put changes without a lock file.")))))

(defun dream-eater/check-out ()
  "For the current buffer, grant write access and create a Dreamweaver lock file."
  (interactive)
  (let ((lock-file-state (dream-eater--lock-file-status (buffer-file-name))))
    (pcase (car lock-file-state)
      ('owned (message "You already checked out this file."))
      ('disowned (message (concat
                           (car (cdr lock-file-state))
                           " has already checked out this file.")))
      ('no-lock (progn
                  (dream-eater--make-buffer-writable)
                  (write-region (car (cdr (cdr lock-file-state)))
                                nil
                                (car (cdr lock-file-state)))
                  (setq dream-eater--checked-out-list
                        (cl-adjoin buffer-file-name dream-eater--checked-out-list
                                   :test 'string=)))))))

(defun dream-eater/check-in ()
  "For the current buffer, save modifications, make read only, and remove the lock file."
  (interactive)
  (let ((lock-file-state (dream-eater--lock-file-status (buffer-file-name))))
    (pcase (car lock-file-state)
      ('owned (progn
                (when (and (buffer-modified-p)
                           (y-or-n-p
                            "There are unsaved changes.  Do you want to save? "))
                  (dream-eater/put))
                (dream-eater--make-buffer-read-only)
                (dream-eater--remove-current-buffer-lock-file)))
      ('disowned (message (concat
                           (car (cdr lock-file-state))
                           " has replaced your lock file.")))
      ('no-lock (message "File is not checked out.")))))

(defun dream-eater--enable ()
  "Enable Dream Eater."
  (fset 'save-buffer 'dream-eater/put)
  (fset 'read-only-mode
        #'(lambda () (interactive) (message "Refusing to toggle read only mode \
when dream eater mode is enabled")))

  (add-hook 'prog-mode-hook 'dream-eater--on-open)
  (add-hook 'kill-buffer-hook 'dream-eater--remove-current-buffer-lock-file)
  (add-hook 'kill-emacs-query-functions 'dream-eater--remove-all-lock-files)

  (setq dream-eater--default-cursor-color (face-attribute 'cursor :background))
  (set-cursor-color "#E9C771"))

(defun dream-eater--disable ()
  "Disable Dream Eater."
  (fset 'save-buffer 'dream-eater--save-buffer)
  (fset 'read-only-mode 'dream-eater--read-only-mode)

  (remove-hook 'prog-mode-hook 'dream-eater--on-open)
  (remove-hook 'kill-buffer-hook 'dream-eater--remove-current-buffer-lock-file)
  (remove-hook 'kill-emacs-query-functions 'dream-eater--remove-all-lock-files)

  (set-cursor-color dream-eater--default-cursor-color))

(define-minor-mode global-dream-eater-mode
  "Dream Eater minor mode"
  :lighter " DreamEater"
  :global t
  (if global-dream-eater-mode
      (dream-eater--enable)
    (dream-eater--disable)))

(provide 'dream-eater-mode)

;;; dream-eater.el ends here
