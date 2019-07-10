;;; lang-prolog.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for Prolog

;;; Code:

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.plt\\'" . prolog-mode))

(provide 'lang-prolog)

;;; lang-prolog.el ends here
