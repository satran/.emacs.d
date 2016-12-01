;; Full screen in Linux
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth))
  (progn
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
    (menu-bar-mode -1) ;;no menubar
    (scroll-bar-mode -1) ;; no scroll bar
    ))

;; Move past a given character, like vims f
(defun move-past-next-char (x)
  "Move the next occurrence of the character x"
  (interactive "k")
  (search-forward x))

;; Global Keybindings
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-M-z") 'ansi-term)

(global-set-key (kbd "C-x C-j") 'next-buffer)
(global-set-key (kbd "C-x C-k") 'previous-buffer)

(global-set-key (kbd "C-x g") 'magit-status)

;; Settings keybindings for Scroll line by line.
(global-set-key (kbd "C-M-g") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)

(global-set-key (kbd "C-S-f") 'speedbar-get-focus)

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; More reasonable next windows
(global-set-key "\C-x\C-n" 'next-multiframe-window)
(global-set-key "\C-x\C-p" 'previous-multiframe-window)
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(global-set-key "\C-\M-f" 'move-past-next-char)
(global-set-key (kbd "<C-return>") 'plumb)
(global-set-key [f11] 'fullscreen)

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;; Move across split windows using the shit+arrow keys
(windmove-default-keybindings)
