;;; editor.el --- Custom configuration for all editors -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist
             '(font . "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"))

;; Disable tabs by default
(setq-default indent-tabs-mode nil)

;; A more sane ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use UTF-8 everywhere
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
;; (setq jump-char-lazy-highlight-face nil)

;; Lines should be 120 characters wide
(setq fill-column 120)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 120)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; No electric indent
(setq electric-indent-mode 1)

;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

;; Visual feedback on selections
(transient-mark-mode t)

;; Turn on visual line mode
;; (visual-line-mode t)

;; Turn on visual line mode only for text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Turn on column number mode
(column-number-mode t)

;; Turn on line number mode
(line-number-mode t)

;; Turn on current line highlighting
(global-hl-line-mode 1)

;; Keep syntax highlighting behind current line
(set-face-foreground 'highlight nil)

;; Show matching parens
(show-paren-mode t)

;; Blink the cursor
(blink-cursor-mode t)

;; CamelCase support
(global-subword-mode 1)

;; Linum mode
(global-linum-mode t)

;; Custom face/function to pad the line number in a way that does not conflict with whitespace-mode
;; (defface linum-padding
;;   `((t :inherit 'linum
;;        :foreground ,(face-attribute 'linum :background nil t)))
;;   "Face for displaying leading zeroes for line numbers in display margin."
;;   :group 'linum)

;; (defun linum-format-func (line)
;;   (let ((w (length
;;             (number-to-string (count-lines (point-min) (point-max))))))
;;     (concat
;;      (propertize " " 'face 'linum-padding)
;;      (propertize (make-string (- w (length (number-to-string line))) ?0)
;;                  'face 'linum-padding)
;;      (propertize (number-to-string line) 'face 'linum)
;;      (propertize " " 'face 'linum-padding)
;;      )))

;; (setq linum-format 'linum-format-func)

;; Highlight changes mode
;; (global-highlight-changes-mode t)

;; Remove changes highlights after save
;; (add-hook 'after-save-hook
;;           '(lambda()
;;              (if (boundp 'highlight-changes-mode)
;;                  (highlight-changes-remove-highlight (point-min) (point-max)))))

;; Enable electric pairs
(electric-pair-mode t)

;; Add VC differences to the margin
(when (window-system)
  (global-diff-hl-mode)
  (setq diff-hl-side 'right))

;; Setup rainbow delimiters
(add-hook 'markdown-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Whitespace mode-line
;; (require 'whitespace)
;; (setq whitespace-line-column 120)
;; (global-whitespace-mode t)

;; Fix keyboard-based scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
