;; A lot of times I have tried quitting emacs and then realized that I
;; forgot to do something to a buffer. This is to fix it. Source
;; [[http://trey-jackson.blogspot.de/2010/04/emacs-tip-36-abort-minibuffer-when.html][here]]

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
