;;; smex.el --- Custom smex configuration/settings -*- lexical-binding: t; -*-

(setq smex-save-file (expand-file-name "smex" user-emacs-directory))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
