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
		     "autocomplete"
		     "c"
		     "clojure"
		     "erc"
		     "go"
		     "ido"
		     "keybindings"
		     "lisp"
;;		     "mail"
		     "org"
		     "plumb"
		     "eww"
		     "shell"))

(dolist (file script-files)
  (load-file (concat "~/.emacs.d/scripts/" file ".el")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:height 0.8 :family "Monospace"))))
 '(fixed-pitch ((t (:family "DejaVu Sans Mono"))))
 '(fringe ((t nil)))
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
 '(ansi-color-names-vector
   ["#110F13" "#B13120" "#719F34" "#CEAE3E" "#7C9FC9" "#7868B5" "#009090" "#F4EAD5"])
 '(custom-safe-themes
   (quote
    ("0371ff0cdce7c7ea0beee7716c0c251da292903f4db29758c44be8d25de003da" default)))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#202020" t)
 '(fringe-mode 4 nil (fringe))
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(package-selected-packages
   (quote
    (org-bullets smex ido-vertical-mode go-eldoc go-mode cider c-eldoc go-autocomplete))))
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
