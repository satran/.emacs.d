(ido-mode t)
(load-or-install-package 'ido-vertical-mode)
(ido-vertical-mode 1)

;; Do the fuzzy matching bit
(setq ido-enable-flex-matching t)

;; Preventing auto-searches unless called explicitly
(setq ido-auto-merge-work-directories-length -1)

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)

;; Keybindings to move up and down similar to emacs buffers
(setq ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))

;; Smex
(load-or-install-package 'smex)
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
