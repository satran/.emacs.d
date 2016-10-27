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

;; Do not pollute the working directory. Add it to emacs folder.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; split frame into 3 windows evenly
(defun split-3-even ()
  (interactive)
  (dotimes (number 2)
    (split-window-right))
  (balance-windows))


;; save history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; When you start typing and text is selected, replace it with what
;; you are typing, or pasting, or whatever.
(delete-selection-mode 1)

;; Change typing yes to y and no to n in minibuffer
(fset 'yes-or-no-p 'y-or-n-p)

;; powerline to setup mode line
;;(setq powerline-display-buffer-size nil)
;;(setq powerline-display-hud nil)
;;(powerline-default-theme)
