;;; init-settings --- Part of my Emacs config

;;; Commentary:

;; Some other settings

;;; Code:

;; start emacs as a server if one isn't running
;; https://stackoverflow.com/a/9999774
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; set the indentation width
(setq tab-width 4)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(5)    ; mouse scroll amount
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't)      ; scroll window under mouse

;; goto the last change
(global-set-key (kbd "C-M-.") 'goto-last-change)

(defun lia/the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)

  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

(defun lia/window-switch-split ()
  "Switch between horizontal/vertical layout."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun lia/window-swap ()
  "Swap your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't swap a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(provide 'init-settings)

;;; init-settings.el ends here
