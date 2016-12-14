;;; auto-complete.el --- Custom configuration for auto-configuration -*- lexical-binding: t; -*-

(require 'go-autocomplete)
(require 'auto-complete-config)

;; Turn on auto-complete but don't start by default
(setq ac-auto-start 2)

;; Bind the key to do auto-complete
(global-set-key "\M-/" 'auto-complete)

;; Add yasnippet to auto-complete
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

(eval-after-load 'tern
  '(progn
     (setq tern-command (append tern-command '("--no-port-file")))
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; Enable auto-complete globally
(ac-config-default)
