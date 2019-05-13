;;; clojure.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for Clojure

;;; Code:

(use-package cider
  :ensure t
  :commands (clojure-mode
             clojurec-mode
             clojurescript-mode))

(provide 'lang-clojure)

;;; clojure.el ends here
