;; load config.org
(package-initialize)

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
