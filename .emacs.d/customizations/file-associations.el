;;; file-associations.el --- File associations for Emacs -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("BUILD$" .  bazel-mode))
(add-to-list 'auto-mode-alist '("\\.json$" .  json-mode))
(add-to-list 'auto-mode-alist '(".jshintrc$" .  json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
