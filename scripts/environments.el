(setenv "GOROOT" "/usr/local/go")
(setenv "EDITOR" "emacs")
(setenv "VISUAL" "emacs")
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/go/bin"))
(setenv "GOPATH" (if (getenv "GOPATH") (getenv "GOPATH") (getenv "HOME")))
(setenv "DOTFPATH" (expand-file-name "~/.dotf"))

(setq exec-path
      (append exec-path
	      '("/usr/local/go/bin" "/home/satran/bin" "/home/satran/scripts")))
