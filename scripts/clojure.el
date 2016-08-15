(load-or-install-package 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojurescript-mode #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
