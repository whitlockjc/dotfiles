;;; frame-geometry.el --- Emacs package for storing/restoring window geometry -*- lexical-binding: t; -*-

(setq frameg-file (expand-file-name "frame-geometry" user-emacs-directory))

(defun save-frameg ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/frame-geometry."
  (let ((frameg-font (frame-parameter (selected-frame) 'font))
        (frameg-left (frame-parameter (selected-frame) 'left))
        (frameg-top (frame-parameter (selected-frame) 'top))
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height)))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";;; This file stores the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '((font . \"" frameg-font "\")\n"
       (format "        (top . %d)\n" (max frameg-top 0))
       (format "        (left . %d)\n" (max frameg-left 0))
       (format "        (width . %d)\n" (max frameg-width 0))
       (format "        (height . %d)))\n" (max frameg-height 0)))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

;; Loads a window's frame geometry
(defun load-frameg ()
  "Loads ~/.emacs.d/frame-geometry which should load the previous frame's geometry."
  (when (file-readable-p frameg-file)
    (load-file frameg-file)))

;; Register frame geometry hooks only for window systems
(when (window-system)
  ;; Frame geometry hooks
  (add-hook 'after-init-hook 'load-frameg)
  (add-hook 'kill-emacs-hook 'save-frameg))
