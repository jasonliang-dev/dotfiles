;; init-appearance.el --- Part of my Emacs config

;;; Commentary:

;; Make Emacs look a bit more pretty

;;; Code:

;; Change the default font
(set-frame-font "Source Code Pro 10")

;; stop the cursor from blinking
(blink-cursor-mode 0)

;; highlight the current line
(global-hl-line-mode)

;; enable wordwrap
(global-visual-line-mode t)

;; change certain text into symbols (lambda, >=, etc)
(global-prettify-symbols-mode t)

;; Show matching parentheses
(defvar show-paren-delay 0.2)
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

;; set the fringe size
(fringe-mode 16)

;; set the fringe color to the background color
(set-face-background 'fringe nil)

;; don't display the welcome screen
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(provide 'init-appearance)

;;; init-appearance.el ends here
