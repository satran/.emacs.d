;; Set C style to k&r
(setq c-default-style "linux")

;; CTags settings
(setq path-to-ctags "/usr/bin/etags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((full-dir-name (directory-file-name dir-name)))
    (shell-command
     (format "find %s -iname \"*.[c,h]\" | xargs %s -f %s/TAGS -R"
	     full-dir-name path-to-ctags full-dir-name))))

(setq gdb-many-windows t)

;; Eldoc mode for C
(load-or-install-package 'c-eldoc)
(setq c-eldoc-includes "`pkg-config glib-2.0 tokyocabinet --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
