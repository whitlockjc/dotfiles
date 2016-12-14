;;; .emacs --- My Emacs Customizations -*- lexical-binding: t; -*-

;; Always turn off the menu bar, scroll bar and tool bar
;; Done here to avoid seeing these things even during initialization
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off the startup screen
(setq inhibit-startup-message t)

;; Custom themes directory
;; Done here so we can load the theme prior to customizations to support packages that are impacted by theme settings
(setq custom-theme-directory (expand-file-name
			      (concat user-emacs-directory "themes")))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; Are we on a Mac?
(setq is-mac (equal system-type 'darwin))

;; Load environment-specific configuration
(load-file (concat user-emacs-directory "env-config.el"))

;; Set PATH to include items installed by Homebrew/NPM/...
(setenv "PATH" (concat env-config-path (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

;; Add MELPA to the package repositories and refresh
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; Refresh the package contents when necessary
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Required packages
(setq package-list
      '(ace-jump-mode
        ac-emmet
        ac-js2
	atom-dark-theme
	atom-one-dark-theme
        auto-complete
	coffee-mode
	dakrone-theme
	diff-hl
	dockerfile-mode
        editorconfig
        emmet-mode
        flx-ido
	flycheck
        go-autocomplete
        go-eldoc
        go-guru
	go-mode
        guide-key
        ibuffer-projectile
        ido-at-point
        ido-ubiquitous
	ido-vertical-mode
	js2-mode
	js2-refactor
	json-mode
	magit
	markdown-mode
        minimap
        neotree
	powerline
        projectile
        rainbow-delimiters
        smex
        sr-speedbar
        smooth-scrolling
        tern
        tern-auto-complete
        typescript-mode
        web-mode
        yaml-mode
        yasnippet))

;; Mac specific packages
(when is-mac
  (add-to-list 'package-list 'exec-path-from-shell))

;; Install required packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load the desired theme
(if (window-system)
    (load-theme 'atom-one-dark t)
  (load-theme 'atom-dark t))

;; Emacs configurations (Order matters)
(setq configs-list
      '(general
        ido
        editor
        editor-go
	editor-javascript
        editor-web
        file-associations
	flycheck
        guide-key
        keyboard
        magit
	shell
        smex
        yasnippet
        auto-complete
	frame-geometry))

;; Mac specific configuration
(when is-mac
  (add-to-list 'configs-list 'mac))

(dolist (config-name configs-list)
  (let ((file (format "%s/%s.el"
                      (expand-file-name
                       (concat user-emacs-directory "customizations"))
                      config-name)))
    (when (file-regular-p file)
      (load file))))

;; Load in full screen mode
;; (when (window-system)
;;   (toggle-frame-fullscreen))

;; The customizations in the directory above are a mix of the following:
;;
;;  * https://github.com/technomancy/better-defaults
;;  * https://github.com/magnars/.emacs.d
