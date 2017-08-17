;; lia-appearance.el --- Part of my Emacs config

;;; Commentary:

;; Make Emacs look a bit more pretty

;;; Code:

;; Change the default face
(set-face-attribute 'default nil
		    :font "Source Code Pro 10")

(global-hl-line-mode 1) ; highlight the current line

(global-visual-line-mode t) ; enable wordwrap

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode t)

;; custom mode line
;; https://emacs-fu.blogspot.ca/2011/08/customizing-mode-line.html
(defun lia/mode-line ()
  "Custom mode line."
  (interactive)
  (setq-default mode-line-format
		(list
		 ;; Any changes since last save?
		 " %* "

		 ;; the buffer name; the file name as a tool tip
		 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				     'help-echo (buffer-file-name)))

		 ;; line and column
		 "(" ;; '%02' to set to 2 chars at least; prevents flickering
		 (propertize "%02l" 'face 'font-lock-type-face) ","
		 (propertize "%02c" 'face 'font-lock-type-face)
		 ") "

		 ;; the current major mode for the buffer.
		 "["
		 '(:eval (propertize "%m" 'face 'font-lock-string-face
				     'help-echo buffer-file-coding-system))
		 "] ")))
;;(lia/mode-line)

;; set the fringe color to the background color
;; http://emacs.stackexchange.com/a/5343
(set-face-attribute 'fringe nil
		    ;;:foreground (face-foreground 'default)
		    :background (face-background 'default))

;; don't display the welcome screen
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(provide 'lia-appearance)

;;; lia-appearance.el ends here
