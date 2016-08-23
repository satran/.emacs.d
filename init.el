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
		     "shell"))

(dolist (file script-files)
  (load-file (concat "~/.emacs.d/scripts/" file ".el")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:background "white" :foreground "gray30" :height 180 :family "Ubuntu Mono"))))
 '(Info-quoted ((t (:height 0.8 :family "Monospace"))))
 '(fixed-pitch ((t (:family "DejaVu Sans Mono"))))
;; '(hl-line ((t (:background "white smoke"))))
 '(variable-pitch ((t (:height 0.8 :family "Merriweather Light")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
