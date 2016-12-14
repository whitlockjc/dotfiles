;;; magit.el --- Custom configuration for magit -*- lexical-binding: t; -*-

(require 'magit)

(if (file-exists-p  "/usr/local/bin/emacsclient")
    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
  (setq magit-emacsclient-executable (executable-find "emacsclient")))

(global-set-key (kbd "C-x g") 'magit-status)
