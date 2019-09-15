;;; lang-web.el --- Language Support -*- lexical-binding: t; -*-

;;; Commentary:

;; Language support for HTML

;;; Code:

(use-package web-mode
  :ensure t
  :init
  (setq-default web-mode-enable-auto-pairing nil)
  (add-to-list 'auto-mode-alist
               '("\\.vue\\'" . (lambda ()
                                 (web-mode)
                                 (setq web-mode-style-padding 0
                                       web-mode-script-padding 0))))
  :mode ("\\.php\\'" "\\.ejs\\'" "\\.twig\\'"))

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)))

(provide 'lang-web)

;;; lang-web.el ends here
