;;; ido.el --- Custom ido configuration/settings -*- lexical-binding: t; -*-

;; Use ido everywhere
(setq ido-everywhere t)

;; Enable ido
(ido-mode t)

;; Enable ido to display vertically
(ido-vertical-mode 1)

;; Allow matches within words if there is no direct match
(setq ido-enable-flex-matching t)

;; Required for fuzzy matching
(setq ido-enable-prefix nil)

;; When finding buffers/files, disregard case
(setq ido-case-fold t)

;; Disable work directory merging
(setq ido-auto-merge-work-directories-length -1)

;; Create new buffers when match not found
(setq ido-create-new-buffer 'always)

;; Maximum number of prospects
(setq ido-max-prospects 10)

;; Try out flx-ido for better flex matching between words
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "bower_components")
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Ido at point
(ido-at-point-mode)

;; Ido ubiquitous mode
(ido-ubiquitous-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
