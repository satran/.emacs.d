;;; black-theme.el
(deftheme black
  "black")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'black
   `(cursor ((,class (:background "red" :foreground "#ffffff"))))
   `(border-color ((,class (:background "#87893B"))))
   `(default ((,class (:background "#000000" :foreground "#efefef"))))
   `(highlight ((,class (:background "#333333"))))
   `(fringe ((,class (:background "#000000"))))
   `(mode-line ((,class :background "#770005" :foreground "#ffffff")))
   `(mode-line-inactive ((,class :foreground "#ffffff" :background ,"#444444")))
   `(header-line ((,class (:foreground "#E5FFFE" :background "black"))))
   `(minibuffer-prompt ((,class (:foregrond "#0084C8" :bold nil))))
   `(region ((,class (:foreground unspecified :background "#222222"))))
   `(dired-header ((,class (:bold t :foreground "#0084C8"))))
   `(widget-button ((,class (:bold t :foreground "#0084C8"))))

   `(success ((,class (:bold t :foreground "#4E9A06"))))
   `(warning ((,class (:foreground "#CE5C00"))))
   `(error ((,class (:foreground "#B50000"))))
   '(erc-current-nick-face ((t (:foreground "#efefef"  :underline nil :weight normal))))
   '(erc-default-face ((t (:foreground "#efefef"))))
   '(erc-input-face ((t (:foreground "#efefef"))))
   '(erc-nick-default-face ((t (:foreground "#efefef" :weight normal))))
   '(erc-notice-face ((t (:foreground "#efefef"))))
   '(erc-timestamp-face ((t (:foreground "#efefef"))))
   ))
