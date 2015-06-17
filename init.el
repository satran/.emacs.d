;; Setting the default load-directory
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(setenv "GOROOT" "/usr/local/go")
(setenv "EDITOR" "emacs")
(setenv "VISUAL" "emacs")
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/scripts")))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/go/bin"))
(setenv "GOPATH"(getenv "HOME"))

(setq exec-path
      (append exec-path
	      '("/usr/local/go/bin" "/home/satran/bin" "/home/satran/scripts")))

;; Adding custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/solarized-theme-20150424.53")

;; Setting up Marmalade and gny and melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Setting line numbers to all files
;;(global-linum-mode 1)

;; Offset the number by two spaces to work around some weird fringe 
;;(setq linum-format "%3d ")

;; Setting column number
(column-number-mode 1)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Set C style to k&r
(setq c-default-style "linux")

;; Set the cursor to a bar
(setq default-cursor-type 'bar)

;; Disable the menubar for terminal
(menu-bar-mode -1)

;; Globally disable syntax highlight
(global-font-lock-mode 0)

;; Disable the fringes
;;(set-fringe-mode '(0 . 0))

;; GUI specific settings
;; Load the customizations after an emacsclient startsup.

(setq current-font-size 14)
(setq current-font-name "DejaVu Sans Mono")
    
;; Highlighting current line
(global-hl-line-mode 1)

(set-face-italic-p 'italic nil)

(load-theme 'solarized-light t)
(add-to-list 'default-frame-alist `(font . ,(concat current-font-name "-" (number-to-string current-font-size))))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars nil))

(defun inc-font-size ()
  "increases font size"
  (setq current-font-size (+ 1 current-font-size))
  (let ((font-name (concat current-font-name " "
			   (number-to-string current-font-size))))
    (set-face-attribute 'default nil :font font-name)))

(defun dec-font-size ()
  "decreases font size"
  (setq current-font-size (- 1 current-font-size))
  (let ((font-name (concat current-font-name " "
			   (number-to-string current-font-size))))
    (set-face-attribute 'default nil :font font-name)))

(defun office-mode ()
  (progn
    (disable-theme 'solarized-light)
    (load-theme 'solarized-dark t)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono 17")))

;; Disabling bold fonts
(set-face-bold-p 'bold nil)

;; Automatically indent code after RET
(electric-indent-mode +1)

;; Have those awesome matching pairs
(electric-pair-mode t)

;; Highlight matching parenthesis
(show-paren-mode 1)

;; On enter new line and indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)

;; Disable the gaudy colors in shell
(setq ansi-color-names-vector		; better contrast colors
      ["black" "red4" "chartreuse4" "goldenrod3"
       "DodgerBlue4" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tramp settings
(setq tramp-default-method "ssh")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq recentf-auto-cleanup 'never) 

;; IDO mode.
(require 'ido)
(ido-mode t)
(ido-vertical-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Set such that emacs does not use the ugly word-wrapping
(global-visual-line-mode 1)

;; Keybinding to start the shell
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "C-S-z") 'ansi-term)
(setq Eshell-directory-name "/home/satran/.emacs.d/eshell")
(setq eshell-prompt-function (lambda () (concat "% ")))
(setq eshell-prompt-regexp "% ")

;; Settings keybindings for Scroll line by line.
(global-set-key (kbd "C-M-g") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)

;; CTags settings
(setq path-to-ctags "/usr/bin/etags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((full-dir-name (directory-file-name dir-name)))
    (shell-command
     (format "find %s -iname \"*.[c,h]\" | xargs %s -f %s/TAGS -R"
	     full-dir-name path-to-ctags full-dir-name))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"] t)
 '(erc-hide-list (quote ("JOIN" "KICK" "MODE")))
 '(erc-notice-highlight-type (quote prefix))
 '(erc-prompt ">")
 '(gnus-visible-headers (quote ("^From:" "^Subject:" "^Date:" "^To:" "^[BGF]?Cc:")))
 '(w3m-default-display-inline-images t))

(setq gdb-many-windows t)

;; My Shortcuts
(global-set-key (kbd "C-S-f") 'speedbar-get-focus)

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; Eldoc mode for C
(setq c-eldoc-includes "`pkg-config glib-2.0 tokyocabinet --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


(global-set-key "\C-x\C-n" 'next-multiframe-window)
(global-set-key "\C-x\C-p" 'previous-multiframe-window)

;; Move across split windows using the shit+arrow keys
(windmove-default-keybindings)

;; Move past a given character, like vims f
(defun move-past-next-char (x)
  "Move the next occurrence of the character x"
  (interactive "k")
  (search-forward x))
(global-set-key "\C-\M-f" 'move-past-next-char)

(defun plumb ()
  "sort of the plumber found in plan9"
  (interactive)
  (let ((file "") (line-no 1) (line (thing-at-point 'line)))
    (string-match "\\([_~0-9a-zA-Z\-\\.\\/]+\\):\?\\([0-9]*\\)" line)
    (setq file (match-string 1 line))
    (setq line-no (match-string 2 line))
    (if (and (not (string-equal file "")) (file-exists-p file))
	(progn
	  (message "%s:%s" file line-no)
	  (switch-to-buffer (find-file-noselect file))
	  (beginning-of-buffer)
	  (if line-no (forward-line (- (string-to-number line-no) 1))))
      (message "%s not found" file))))
;;(global-set-key "\C-c\C-g" 'plumb)
(global-set-key (kbd "<C-return>") 'plumb)

;; A few key bindings that I would want to remember
;; C-x r <SPC> <char-for-register> - Mark a register
;; C-x r j <char-for-register> - Jump to a register
;; C-u C-<SPC> jump to the previous mark in the buffer

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

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

(global-set-key [f11] 'fullscreen)

;; Go lang setup
;; Install go-mode, go-eldoc
;; Go modules required
;;     go get code.google.com/p/rog-go/exp/cmd/godef
;;     go get -u github.com/nsf/gocode
;;     go get golang.org/x/tools/cmd/goimports
(add-hook 'before-save-hook 'gofmt-before-save)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'flycheck-mode)

(setq linum-disabled-modes-list
      '(eshell-mode term-mode wl-summary-mode compilation-mode eww-mode erc-mode
		    mu4e-about-mode mu4e-compose-mode mu4e-headers-mode
		    mu4e-main-mode mu4e-view-mode mu4e~main-toggle-mail-sending-mode))
(defun linum-on ()
  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

(setq gnus-summary-line-format "%I- %s [%a|%d]\n")

(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(display-time-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-fringe-error ((t (:background "#FF6E64" :foreground "#990A1B" :weight normal))))
 '(flycheck-fringe-info ((t (:background "#69B7F0" :foreground "#00629D" :weight normal))))
 '(flycheck-fringe-warning ((t (:background "#DEB542" :foreground "#7B6000" :weight normal))))
 '(magit-item-highlight ((t nil)))
 '(vertical-border ((t (:inherit default :underline nil :weight normal))))
 '(w3m-anchor ((t (:foreground "DeepSkyBlue4"))))
 '(w3m-arrived-anchor ((t (:foreground "DodgerBlue4")))))

;; slime settings to load sbcl
(add-to-list 'load-path "/bin/sbcl")
;;(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "/bin/sbcl") 

(setq w3m-command "/bin/w3m")

;; disable bold fonts
(mapc
 (lambda (face)
        (when (eq (face-attribute face :weight) 'bold)
          (set-face-attribute face nil :weight 'normal)))
 (face-list))

;; This is specifically for stumpwm and ratpoison. The annoying space between frames.
(setq frame-resize-pixelwise t)

;; Temporary setting to make things quicker in terminology
(remove-hook 'find-file-hooks 'vc-find-file-hook)
