;;; frame-geometry.el --- Emacs package for storing/restoring window geometry -*- lexical-binding: t; -*-

(setq frameg-file (expand-file-name "frame-geometry" user-emacs-directory))

(defun theme-frameg (frame)
  "Sets the appropriate theme for the frame"
  (message "%s" "theme-frameg")
  (select-frame frame)
  (if (window-system frame)
      (progn
        (disable-theme 'atom-dark)
        (enable-theme 'atom-one-dark))
    (progn
      (disable-theme 'atom-one-dark)
      (enable-theme 'atom-dark))))

(defun save-frameg (frame)
  "Gets the current frame's geometry and saves to ~/.emacs.d/frame-geometry."
  (message "%s" "save-frameg")
  (select-frame frame)
  (if (window-system frame)
      (let ((frameg-font (frame-parameter frame 'font))
            (frameg-left (frame-parameter frame 'left))
            (frameg-top (frame-parameter frame 'top))
            (frameg-width (frame-parameter frame 'width))
            (frameg-height (frame-parameter frame 'height)))
        (with-temp-buffer
          ;; Turn off backup for this file
          (make-local-variable 'make-backup-files)
          (setq make-backup-files nil)
          (insert
           ";;; This file stores the previous emacs frame's geometry.\n"
           ";;; Last generated " (current-time-string) ".\n"
           "(setq default-frame-alist\n"
           "      '((font . \"" frameg-font "\")\n"
           (format "        (top . %d)\n" (max frameg-top 0))
           (format "        (left . %d)\n" (max frameg-left 0))
           (format "        (width . %d)\n" (max frameg-width 0))
           (format "        (height . %d)\n" (max frameg-height 0))
           (format "        (horizontal-scroll-bars . nil)\n")
           (format "        (vertical-scroll-bars . nil)))\n"))
          (when (file-writable-p frameg-file)
            (write-file frameg-file))))))

;; Loads a window's frame geometry
(defun load-frameg ()
  "Loads ~/.emacs.d/frame-geometry which should load the previous frame's geometry."
  (message "%s" "load-frameg")
  (when (file-readable-p frameg-file)
    (load-file frameg-file)))

;; Register frame geometry hooks
;; (add-hook 'after-init-hook 'load-frameg)
(add-hook 'before-make-frame-hook 'load-frameg t)
(add-hook 'after-make-frame-functions 'theme-frameg t)
;; (add-hook 'kill-emacs-hook 'save-frameg))
(add-hook 'delete-frame-functions 'save-frameg t)
