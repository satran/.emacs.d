;; Tried it a while back but reverted, way too dependent on emacs
;; keybindings. I'm keeping it here in case I want to give it another
;; shot.
(require 'evil)
(evil-mode t)
(define-key evil-normal-state-map " xs" 'save-buffer)
(define-key evil-normal-state-map " xk" 'kill-buffer)
(define-key evil-normal-state-map " xf" 'find-file)
(define-key evil-normal-state-map " xb" 'ido-switch-buffer)
(define-key evil-normal-state-map " xn" 'next-multiframe-window)
(define-key evil-normal-state-map " xp" 'previous-multiframe-window)
(define-key evil-normal-state-map " z" 'shell)
(define-key evil-normal-state-map " x0" 'delete-window)
(define-key evil-normal-state-map " x1" 'delete-other-windows)
(define-key evil-normal-state-map " x2" 'split-window-below)
(define-key evil-normal-state-map " x3" 'split-window-right)
(define-key evil-normal-state-map " x " 'pop-global-mark)


;; Use emacs state rather than insert mode of evil
;;(evil-define-state emacs
;; "Emacs state that can be exited with the escape key."
;; :tag " <EE> "
;; :message "-- EMACS WITH ESCAPE --"
;; :input-method t
;; ;; :intercept-esc nil
;; )
(defadvice evil-insert-state
    (around emacs-state-instead-of-insert-state activate)
  (evil-emacs-state))
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(setq evil-emacs-state-cursor '("default" bar))
