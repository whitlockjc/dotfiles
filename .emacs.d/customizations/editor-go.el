;;; editor-go.el --- Custom Go editor configuration/settings -*- lexical-binding: t; -*-

(when (boundp 'env-config-path)
  (setenv "GOPATH" env-config-gopath)
  (setenv "GOROOT" env-config-goroot))

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook '(lambda ()
                           (setq-local tab-width 2)
                           (local-set-key (kbd "M-.") 'godef-jump)
                           (local-set-key (kbd "C-c C-r") 'go-rename)))

(add-hook 'go-mode-hook 'go-eldoc-setup)
