;;; shell.el --- Custom configuration for shells -*- lexical-binding: t; -*-

;; When exiting a shell, close its buffer
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; Don't ask me for my shell
(when (boundp 'env-config-path)
  (defvar my-term-shell env-config-shell)

  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term))

;; Make it where copy/paste work in ansi-term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))
