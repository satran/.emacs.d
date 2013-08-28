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

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

;; Set C style to k&r
(setq c-default-style "k&r"
          c-basic-offset 4)

;; Highlighting current line
(global-hl-line-mode 1)

;; Disable the menubar for terminal
(menu-bar-mode -1)

(load-theme 'tangofringe t)

;; GUI specific settings
(when (window-system)
  ;; Setting the color scheme.
  ;; (load-theme 'oceanic t)

  (set-face-italic-p 'italic nil)

  ;; Disabling the fringe
  ;;(set-fringe-mode '(0 . 0))

  ;; Disable the scrollbar
  (scroll-bar-mode -1)
  
  ;; Setting the default font
  (set-face-attribute 'default nil :font "Meslo LG M 9")
  ;;(set-face-attribute 'default nil :font "Liberation Mono 9")
  ;; Disable the toolbar
  (tool-bar-mode -1))

;; Disabling bold fonts
(set-face-bold-p 'bold nil)

;; Setting up Marmalade and gny and melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Width and Height
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 96)) 


;; On enter new line and indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)


;; SLIME settings
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Python settings 
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; Automatically indent code after RET
(electric-indent-mode +1)

;; Python Rope
;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-shortcuts nil)
;;(setq ropemacs-local-prefix "C-c C-p")

;; Multi-Term settings
;(require 'multi-term)
;(setq multi-term-program "/bin/zsh")
;; Without this the prompt would have weird characters like 4m
;(setenv "TERMINFO" "~/.terminfo")

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
(global-set-key (kbd "C-M-y") 'scroll-up-line)
(global-set-key (kbd "C-M-g") 'scroll-down-line)


;; Open terminal in the current directory
(defun open-terminal ()
  (shell-command
   (open (concat "-n -a /Applications/iTerm.app --args " default-directory))))

(global-set-key (kbd "C-M-d") 'open-terminal)

;; CTags settings
(setq path-to-ctags "/usr/local/bin/etags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -R %s" path-to-ctags dir-name (directory-file-name dir-name)))
    )

;; cscope for emacs.
;(require 'ascope)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes (quote ("3b9470f0a19817fd7a6f737a745a52faf66bc648af90bd6ef1a55e62ee2e0e33" "2fc5680862f16d65dce33536d89ef96dc820c20cfc929d1cdcc2d2eabfff8abf" "40310b1ea4b1d8d6b29624dab09a814dc5ffe61da805e54f839403ee8426748a" "ed3944f5b5174942ed528e28bec8022ec3e1f4b99ede73ceec6a75e69e87a89c" default)))
 '(ecb-options-version "2.40")
 '(ruler-mode-current-column-char 42)
 '(ruler-mode-fill-column-char 124)
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil))

(setq gdb-many-windows t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ruler-mode-column-number ((t (:inherit ruler-mode-default :foreground "dark gray"))))
 '(ruler-mode-comment-column ((t (:inherit ruler-mode-default :foreground "dark gray"))))
 '(ruler-mode-current-column ((t (:inherit ruler-mode-default :foreground "Red" :weight bold))))
 '(ruler-mode-default ((t (:inherit default :foreground "grey64"))))
 '(speedbar-tag-face ((t (:foreground "gray80")))))

;; My Shortcuts
(global-set-key (kbd "C-M-f") 'speedbar-get-focus)

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; Javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'fill-column-indicator)
(setq-default fci-rule-column 80)

;; AutoComplete settings
;; (require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (global-auto-complete-mode t)
