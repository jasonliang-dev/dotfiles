;;; lia-settings.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Things that I change frequently.

;;; Code:

(setq-default
 ;; indentation width for most buffers
 lia-indent-width 2
 ;; when non-nil, indentation uses tab characters instead of spaces
 lia-use-tabs nil
 ;; Emacs theme to use
 lia-theme 'doom-one
 ;; display font
 lia-font "Iosevka 10"
 ;; when non-nil, buffers will show line numbers when in prog-mode
 lia-use-line-numbers nil)

(provide 'lia-settings)

;;; lia-settings.el ends here
