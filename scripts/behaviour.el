;; Automatically indent code after RET
(electric-indent-mode +1)

;; Have those awesome matching pairs
(electric-pair-mode t)

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Set such that emacs does not use the ugly word-wrapping
(global-visual-line-mode 1)

(display-time-mode t)

;; Use vertical splitting more often than horizontal
(setq split-height-threshold 200)

;;(setq linum-format " %d ")
;;(global-linum-mode t)

