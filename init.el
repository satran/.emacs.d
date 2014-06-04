;; Setting the default load-directory
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;; Adding custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Setting line numbers to all files
(global-linum-mode 1)

;; Setting column number
(column-number-mode 1)

;; Offset the number by two spaces to work around some weird fringe 
(setq linum-format "%3d ")

;; Disable splash screen
(setq inhibit-startup-message t)

;; Set C style to k&r
(setq c-default-style "k&r"
          c-basic-offset 4)

;; Set the cursor to a bar
(setq default-cursor-type 'bar)

;; Disable the menubar for terminal
(menu-bar-mode -1)

(load-theme 'plain-light t)

;; GUI specific settings
;; Load the customizations after an emacsclient startsup.
(defun disable-crappy-frames (&optional frame)
  "Disables scrollbars, toolbars and fringe while in graphical mode."
  (when (or window-system frame)
    ;; Setting the color scheme.
    ;; (load-theme 'oceanic t)

    ;; Highlighting current line
    (global-hl-line-mode 1)

    (set-face-italic-p 'italic nil)

    ;; Disabling the fringe
    (set-fringe-mode '(0 . 0))

    ;; Disable the scrollbar
    (scroll-bar-mode -1)
                                        ; check if we're on OSX
    (when (featurep 'ns-win)
      (custom-set-faces
       '(default ((t (:height 120 :width normal :family "Menlo"))))))
    ;; Setting the default font
    ;; (set-default-font "Meslo-12")
    ;; (set-face-attribute 'default nil :font "Liberation Mono 9")

    ;; Disable the toolbar
    (tool-bar-mode -1)))
(disable-crappy-frames)

;; Disabling bold fonts
(set-face-bold-p 'bold nil)

;; Setting up Marmalade and gny and melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Width and Height
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 85)) 


;; On enter new line and indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)


;; Python Settings
(setq
 python-check-command "epylint"
 python-shell-interpreter "/usr/bin/ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "[0-9]+ > "
 python-shell-prompt-output-regexp "[0-9]+ < "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; Automatically indent code after RET
(electric-indent-mode +1)
;; Have those awesome matching pairs
(electric-pair-mode t)

;; Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

(defun pycheck-on-save ()
  "Use a linter to check file on save for python buffers."
  (interactive)
  (if (eq major-mode 'python-mode)
      (python-check (concat "epylint " buffer-file-name))
    "Not a python buffer"))

;;(add-hook 'after-save-hook 'pycheck-on-save)

;; Python Rope
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-shortcuts nil)
;; (setq ropemacs-local-prefix "C-c C-p")


;; Cython mode
(autoload 'cython-mode "cython-mode" "Loads mode for Cython files." t)
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

;; CMake mode
(autoload 'cmake-mode "cmake-mode" "Loads mode for CMake files." t)
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))


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

(require 'git)

;; IDO mode.
(require 'ido)
(ido-mode t)
(ido-vertical-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Set such that emacs does not use the ugly word-wrapping
(global-visual-line-mode 1)

;; Keybinding to start the shell
(global-set-key (kbd "C-z") 'shell)

;; Settings keybindings for Scroll line by line.
(global-set-key (kbd "C-M-g") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)


;; Open terminal in the current directory
(global-set-key (kbd "C-M-;")
                '(lambda ()
                   (interactive)
                   (shell-command
                    (format "open -a /Applications/iTerm.app --args %s"
                            default-directory))))


;; CTags settings
(setq path-to-ctags "/usr/bin/etags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (let ((full-dir-name (directory-file-name dir-name)))
      (shell-command
       (format "find %s -iname \"*.[c,h]\" | xargs %s -f %s/TAGS -R"
               full-dir-name path-to-ctags full-dir-name))))

;; cscope for emacs.
;(require 'ascope)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"] t)
 '(custom-safe-themes (quote ("d6e98500f46f207c1e14a6facc7d55c1ed463a221415768c086310514ddbeed7" "6229b49d2311e403f24383a69bd4d249b3d92eb64e38a62b735824d57444232b" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "9c18f9e7e62aa3572044943a89fc477c1094e699502d9bb8b8c7231c556e8d63" "3b9470f0a19817fd7a6f737a745a52faf66bc648af90bd6ef1a55e62ee2e0e33" "2fc5680862f16d65dce33536d89ef96dc820c20cfc929d1cdcc2d2eabfff8abf" "40310b1ea4b1d8d6b29624dab09a814dc5ffe61da805e54f839403ee8426748a" "ed3944f5b5174942ed528e28bec8022ec3e1f4b99ede73ceec6a75e69e87a89c" default)))
 '(ecb-options-version "2.40")
 '(ruler-mode-current-column-char 42)
 '(ruler-mode-fill-column-char 124)
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(speedbar-show-unknown-files t))

(setq gdb-many-windows t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ruler-mode-column-number ((t (:inherit ruler-mode-default :foreground "dark gray"))) t)
 '(ruler-mode-comment-column ((t (:inherit ruler-mode-default :foreground "dark gray"))) t)
 '(ruler-mode-current-column ((t (:inherit ruler-mode-default :foreground "Red" :weight bold))) t)
 '(ruler-mode-default ((t (:inherit default :foreground "grey64"))) t)
 '(speedbar-tag-face ((t (:foreground "gray80"))) t))

;; My Shortcuts
(global-set-key (kbd "C-S-f") 'speedbar-get-focus)

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; Javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "#555555")

;; Eldoc mode for C
(setq c-eldoc-includes "`pkg-config glib-2.0 tokyocabinet --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;; A few of my own customizations
;; Disable all extras of GUI.
(add-hook 'server-visit-hook 'disable-crappy-frames)
(add-hook 'after-make-frame-functions 'disable-crappy-frames)

;; Jumping windows
(global-set-key "\C-x\C-n" 'other-window)
(defun other-window-backward(&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (prefix-numeric-value n)))
(global-set-key "\C-x\C-p" 'other-window-backward)


;; Move past a given character, like vims f
(defun move-past-next-char (x)
  "Move the next occurrence of the character x"
  (interactive "k")
  (progn
    (search-forward x)))
(global-set-key "\C-\M-f" 'move-past-next-char)



;; A few key bindings that I would want to remember
;; C-x r <SPC> <char-for-register> - Mark a register
;; C-x r j <char-for-register> - Jump to a register
;; C-u C-<SPC> jump to the previous mark in the buffer

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Full screen in Linux
;; (defun fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen) nil 'fullboth))
;;   (progn
;;     (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
;;     (menu-bar-mode -1) ;;no menubar
;;     (scroll-bar-mode -1) ;; no scroll bar
;;     ))
;; 
;; (global-set-key [f11] 'fullscreen)

;; Go Language specifics
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
;; Extending the evn PATH with go's root path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin/"))
(setq exec-path (append exec-path '("/usr/local/go/bin")))
;; speedbar
(speedbar 1)
(speedbar-add-supported-extension ".go")
(add-hook
 'go-mode-hook
 '(lambda ()
    ;; gocode
    (auto-complete-mode 1)
    (setq ac-sources '(ac-source-go)) ;; Imenu & Speedbar
    (setq imenu-generic-expression
	  '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
	    ("func" "^func *\\(.*\\) {" 1)))
    (imenu-add-to-menubar "Index")
    ;; Outline mode
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "//\\.\\|//[^\r\n\f][^\r\n\f]\\|pack\\|func\\|impo\\|cons\\|var.\\|type\\|\t\t*....") (outline-minor-mode 1)
    (local-set-key "\M-a" 'outline-previous-visible-heading)
    (local-set-key "\M-e" 'outline-next-visible-heading)
    ;; Menu bar
    (require 'easymenu)
    (defconst go-hooked-menu
      '("Go tools"
	["Go run buffer" go t]
	￼["Go reformat buffer" go-fmt-buffer t]
	["Go check buffer" go-fix-buffer t])) (easy-menu-define
						go-added-menu (current-local-map) "Go tools" go-hooked-menu)
	;; Other
	(setq show-trailing-whitespace t) ))
;; helper function
(defun go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))
;; helper function
(defun go-fmt-buffer ()
  "run gofmt on current buffer"
  (interactive)
  (if buffer-read-only
      (progn
	(ding)
	(message "Buffer is read only"))
    (let ((p (line-number-at-pos))
	  (filename (buffer-file-name)) (old-max-mini-window-height max-mini-window-height))
      (show-all)
      (if (get-buffer "*Go Reformat Errors*") (progn
						(delete-windows-on "*Go Reformat Errors*")
						(kill-buffer "*Go Reformat Errors*")))
      (setq max-mini-window-height 1)
      (if (= 0 (shell-command-on-region (point-min) (point-max) "gofmt" "*Go Reformat Output*" nil "*Go Reformat Errors*"
					t))
	  (progn
	    (erase-buffer)
	    (insert-buffer-substring "*Go Reformat Output*") (goto-char (point-min))
	    (forward-line (1- p)))
	(with-current-buffer "*Go Reformat Errors*" (progn
						      (goto-char (point-min))
						      (while (re-search-forward "<standard input>" nil t) (replace-match filename))
						      (goto-char (point-min))
						      (compilation-mode))))
      (setq max-mini-window-height old-max-mini-window-height) (delete-windows-on "*Go Reformat Output*")
      (kill-buffer "*Go Reformat Output*"))))
;; helper function
(defun go-fix-buffer ()
  "run gofix on current buffer"
  (interactive)
  (show-all)
  (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))
