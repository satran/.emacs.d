(deftheme oceanic "my custom theme")
(custom-theme-set-faces
  'oceanic
  '(default ((t (:foreground "#ffffff" :background "#1B2630"))))
  '(cursor ((t (:background "#1b8ac0"))))
  '(fringe ((t (:background "#1a1a1a"))))
  '(mode-line ((t (:foreground "#404040" :background "#c4c4c4"))))
  '(region ((t (:background "#505f77"))))
  '(font-lock-builtin-face ((t (:foreground "#ecaa4b"))))
  '(font-lock-comment-face ((t (:foreground "#6D6D6D"))))
  '(font-lock-function-name-face ((t (:foreground "#F2AAEC"))))
  '(font-lock-keyword-face ((t (:foreground "#E47D80"))))
  '(font-lock-string-face ((t (:foreground "#8AD6F2"))))
  '(font-lock-type-face ((t (:foreground"#ecbdf4"))))
  '(font-lock-constant-face ((t (:foreground "#78BDD6"))))
  '(font-lock-variable-name-face ((t (:foreground "#FFFFFF"))))
  '(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
  '(font-lock-warning-face ((t (:foreground "red" :bold t))))
  '(font-lock-type-face ((t (:foreground "#FCB666"))))
  '(fringe ((t (:foreground "#ffffff" :background "#1B2630"))))
)
(provide-theme 'oceanic)
