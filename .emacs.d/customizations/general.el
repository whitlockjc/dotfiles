;;; general.el --- General Emacs configuration -*- lexical-binding: t; -*-

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Write backup files to ~/.emacs.d/backups
(setq backup-directory-alist
      `((".*" . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
    
;; Write auto-save files to ~/.emacs.d/auto-saves
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "auto-saves")) t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Uniquify buffers with the same filename
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Neotree
;; (require 'neotree)
;; (setq neo-smart-open t)
;; (global-set-key [f9] 'neotree-toggle)

;; sr-speedbar
(require 'sr-speedbar)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
;; (setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-x 40)
(global-set-key [f9] 'sr-speedbar-toggle)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Keyboard and Mouse interaction configuration
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; Move files to trash when deleting
;; (setq delete-by-moving-to-trash t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep track of recent files
(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; Disable recentf for certain files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Make all commands behave as if arguments provided
(setq apropos-do-all t)

;; Make Emacs file name completion case sensitive
(setq read-file-name-completion-ignore-case nil);

;; GnuPG support (Not worth a separate configuration file for a one-liner)
(setq epg-gpg-program "/usr/local/bin/gpg")

;; Powerline support (Not worth a separate configuration file for a one-liner)
(powerline-center-theme)

;; Fix a Powerline display issue but this is a general configuration
(setq ns-use-srgb-colorspace nil)

;; Function to get the current face at cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Make dired stop creating so many buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Put the minimap on the right
(setq minimap-window-location 'right)

;; projectile
(projectile-global-mode)

;; Turn off frame suspend
(when (window-system)
  (put 'suspend-frame 'disabled t))
