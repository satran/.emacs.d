(setq eshell-directory-name "/home/satran/.emacs.d/eshell")
(setq eshell-prompt-function (lambda () (concat "% ")))
(setq eshell-prompt-regexp "% ")
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-S-l") 'eshell-clear-buffer)))

;; Disable hl-line for eshell and ansi-terms
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq-local global-hl-line-mode nil)
	    (font-lock-mode t)))
(add-hook 'term-mode-hook
	  (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'term-mode-hook
	  (lambda () (font-lock-mode t)))


(add-hook 'shell-mode-hook
	  (lambda ()
	    (font-lock-mode t)
	    (local-set-key (kbd "M-n") 'comint-next-matching-input-from-input)))
(add-hook 'shell-mode-hook
	  (lambda () (local-set-key (kbd "M-p") 'comint-previous-matching-input-from-input)))

;; Have shell open in the same window
;; (add-to-list 'display-buffer-alist
     ;; '("^\\*shell\\*$" . (display-buffer-same-window)))


(defvar name-counter 0)

(defun new-shell-name ()
  (setq name-counter (+ 1 name-counter))
  (concat "shell-" (number-to-string name-counter)))

(defun new-shell ()
  (interactive)
  (shell (new-shell-name)))

(global-set-key (kbd "C-S-z") 'new-shell)
