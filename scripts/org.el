(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-log-done t)
(setq org-catch-invisible-edits t)

;;(setq org-babel-default-header-args
;;      (cons '(:padline . "no")
;;	    (assq-delete-all :padline org-babel-default-header-args)))

(setq org-agenda-files '("~/org/src/notes.org"))
(setq org-default-notes-file "~/org/src/notes.org")

(setq org-capture-templates
      '(("w" "todo work" entry (file "~/org/work.org")
	 "* TODO %?\n%U\n%F\n")
	("p" "todo personal" entry (file "~/org/personal.org")
	 "* TODO %?\n%U\n%F\n")
	("n" "note" entry (file "~/org/notes.org")
	 "* %?\n")))
(load-or-install-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))


