;;; guide-key.el --- Custom guide-key configuration/settings -*- lexical-binding: t; -*-

(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))

(guide-key-mode 1)

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
