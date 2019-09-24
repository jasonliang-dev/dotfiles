;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Bootstrap `config.org' file.
;;
;; Typically, a literate Emacs config would be loaded like so:
;;
;; (org-babel-load-file "~/.emacs.d/config.org")
;;
;; But since I care about startup time, I'm not using
;; `org-babel-load-file'.
;;
;; Instead, I'm loading my config using something I found on a blog
;; post by Malabarba:
;; http://endlessparentheses.com/init-org-Without-org-mode.html
;;
;; I've edited their solution to suit my needs (no messaging, support
;; for multiple level 1 headers).

;;; Code:

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "config.org" user-emacs-directory))
  (search-forward "#+BEGIN_SRC")
  (move-beginning-of-line 1)
  (while (not (eobp))
    (if (looking-at "^#\\+BEGIN_SRC +emacs-lisp.*$")
        ;; Evaluate Code Blocks
        (let ((l (match-end 0)))
          (search-forward "\n#+END_SRC")
          (eval-region l (match-beginning 0))))
    (forward-line 1)))

;;; init.el ends here
