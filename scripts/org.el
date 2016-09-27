(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)
;;(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-log-done t)
(setq org-catch-invisible-edits t)

;;(setq org-babel-default-header-args (cons '(:padline . "no") (assq-delete-all :padline org-babel-default-header-args)))

(setq org-agenda-files '("~/Dropbox/org/"))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-default-notes-file "~/Dropbox/org/notes.org")

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Dropbox/org/gtg.org")
	 "* TODO %?\n")
	("n" "note" entry (file "~/Dropbox/org/notes.org")
	 "* %?\n")))

(load-or-install-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))

(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/org/gtg.org"))

(setq org-agenda-custom-commands 
      '(("w" todo "WAITING" nil) 
	("n" todo "NEXT" nil)
	("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))
