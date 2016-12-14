;;; editor-javascript.el --- Custom JavaScript editor configuration/settings -*- lexical-binding: t; -*-

;; web-mode customizations
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-hook 'css-mode-hook (lambda ()
                          (emmet-mode)
                          (ac-emmet-css-setup)))
(add-hook 'sgml-mode-hook (lambda ()
                          (emmet-mode)
                          (ac-emmet-html-setup)))
(add-hook 'web-mode-hook (lambda ()
                          (emmet-mode)
                          (ac-emmet-html-setup)))
