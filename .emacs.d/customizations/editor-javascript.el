;;; editor-javascript.el --- Custom JavaScript editor configuration/settings -*- lexical-binding: t; -*-

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

;; Let autopair handle this
(setq js2-mirror-mode nil)

;; Add JSHint/JSLint globals to externs
(setq js2-include-jslint-globals t)

;; Turn on Node.js support
(setq js2-include-node-externs t)

;; Turn off Rhino support
(setq js2-include-rhino-externs nil)

;; Turn on extra highlighting (Ecma built-ins, etc.)
(setq js2-highlight-level 3)

;; Indentation settings
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

;; Refactor keyboard bindings
(js2r-add-keybindings-with-prefix "C-c C-r")

;; Make shell scripts with node associated with js2-mode
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Use Tern auto-complete for JavaScript/JS2 modes
(add-hook 'js-mode-hook (lambda ()
                          (setq-local ac-auto-start nil)
                          (tern-mode t)
                          (local-set-key (kbd "M-/") 'tern-ac-complete)))
(add-hook 'js2-mode-hook (lambda ()
                           (setq-local ac-auto-start nil)
                           (tern-mode t)
                           (local-set-key (kbd "M-/") 'tern-ac-complete)))
