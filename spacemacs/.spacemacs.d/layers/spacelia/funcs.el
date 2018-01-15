;; https://emacs.stackexchange.com/questions/7650/how-to-open-a-external-terminal-from-emacs
(defun lia/run-external (command)
  "Run a shell COMMAND that use the current directory."
  (interactive "s")
  (shell-command (concat command " > /dev/null 2>&1 & disown") nil nil))

;; http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html
(defun lia/setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  ;;(setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )

(defun lia/tab-indent-style ()
  (interactive)
  ;; use tab instead of space
  (setq-local indent-tabs-mode t)
  ;; indent 4 spaces width
  (lia/setup-indent 4))

(defun lia/my-indent-style ()
  (interactive)
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (lia/setup-indent 2))

(defun lia/1045-indent-style ()
  (interactive)
  (setq indent-tabs-mode nil)
  (lia/setup-indent 4))

(defun lia/1181-indent-style ()
  (interactive)
  (setq indent-tabs-mode nil)
  (lia/setup-indent 3))
