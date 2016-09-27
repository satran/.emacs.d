;; Setting; Column' number
(column-number-mode 1)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Set the cursor to a bar
(setq default-cursor-type 'bar)

;; Globally disable syntax highlight
;;(global-font-lock-mode 0)

;; Highlighting current line
(global-hl-line-mode 1)

(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Disable the gaudy colors in shell
(setq ansi-color-names-vector         ; better contrast colors
      ["black" "red4" "chartreuse4" "goldenrod3"
       "DodgerBlue4" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; This is specifically for stumpwm and ratpoison. The annoying space between frames.
(setq frame-resize-pixelwise t)

;; Adding custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
;;  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/solarized-theme-20160106.15")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/gotham-theme-20160102.1454")
(setq solarized-scale-org-headlines nil)
(setq solarized-use-less-bold t)
(setq solarized-use-more-italic nil)
(setq solarized-emphasize-indicators nil)
(load-theme 'knot-dark t)
