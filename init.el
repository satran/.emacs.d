;; Set the number of bytes of consing before garbage collection
;; Small hack to make the init time a bit faster
(setq gc-cons-threshold 100000000)

;; Setting the default load-directory
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Setting up Marmalade and gny and melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize) 

(defun load-or-install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(setq script-files '("lib"
		     "behaviour"
		     "environments"
		     "looks"
		     "c"
		     "clojure"
		     "erc"
		     "go"
		     "ido"
		     "keybindings"
		     "lisp"
;;		     "mu4e"
		     "org"
		     "plumb"
		     "eww"
		     "vi"
		     "shell"))

(dolist (file script-files)
  (load-file (concat "~/.emacs.d/scripts/" file ".el")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "white" :foreground "gray30" :height 180 :family "Ubuntu Mono"))))
 '(Info-quoted ((t (:height 0.8 :family "Monospace"))))
 '(fixed-pitch ((t (:family "DejaVu Sans Mono"))))
 '(fringe ((t nil)))
 '(hl-line ((t (:background "white smoke"))))
 '(org-level-1 ((t nil)))
 '(org-level-2 ((t nil)))
 '(org-level-3 ((t nil)))
 '(org-level-4 ((t nil)))
 '(org-level-5 ((t nil)))
 '(org-level-6 ((t nil)))
 '(org-todo ((t (:weight bold))))
 '(variable-pitch ((t (:height 0.8 :family "Merriweather Light")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gotham-theme yaml-mode w3m tao-theme solarized-theme smex paredit org-bullets markdown-mode magit ido-vertical-mode go-guru go-eldoc go-autocomplete flycheck exwm color-theme-sanityinc-solarized cider c-eldoc))))
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
