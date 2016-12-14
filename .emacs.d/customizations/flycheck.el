;;; flycheck.el --- Custom flycheck customization -*- lexical-binding: t; -*-

(add-hook 'after-init-hook 'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Display errors below the current buffer and shrink to fit
(defun shrink-to-fit ()
  (let ((window (get-buffer-window "*Flycheck errors*")))
    (fit-window-to-buffer window 10 5)
    (shrink-window-if-larger-than-buffer window)))

(defun display-buffer-window-below-and-shrink (buffer alist)
  (let ((window (or (get-buffer-window buffer)
		    (display-buffer-below-selected buffer alist))))
    (when window
      (shrink-to-fit)
      window)))

(add-to-list 'display-buffer-alist
	     `(,(rx string-start "*Flycheck errors*" string-end)
	       (display-buffer-window-below-and-shrink . ((reusable-frames . t)))))
(add-hook 'flycheck-error-list-after-refresh-hook 'shrink-to-fit)

;; Keyboard shortcuts
(global-set-key (kbd "M-s-~") 'flycheck-next-error)
(global-set-key (kbd "M-s-π") 'flycheck-previous-error)

;; Dynamic mode-line based on http://www.lunaryorn.com/2014/07/30/new-mode-line-support-in-flycheck.html
(setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked nil)
          (`no-checker (propertize " -" 'face 'warning))
          (`running (propertize " ✷" 'face 'success))
          (`errored (propertize " !" 'face 'error))
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'error)
                              (no-warnings 'warning)
                              (t 'success))))
             (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                         'face face)))
          (`interrupted " -")
          (`suspicious '(propertize " ?" 'face 'warning)))))

