;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Bootstrap `config.org' file.
;;
;; Since I care about startup time, I'm not using
;; `org-babel-load-file'.  Instead, I'm using some wonderful code
;; found on a blog post by Malabarba:
;; http://endlessparentheses.com/init-org-Without-org-mode.html

;;; Code:

(defvar lia/config.org-message-depth 3
  "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file-contents "~/.emacs.d/config.org")
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Evaluate Code Blocks
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp.*$")
      (let ((l (match-end 0)))
        (search-forward "\n#+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* ")
      (goto-char (point-max))))))

;;; init.el ends here
