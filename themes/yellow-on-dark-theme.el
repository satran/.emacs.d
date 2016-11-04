;;; yellow-on-dark-theme.el --- based on minimal theme by Anler Hp

;; Copyright (C) 2014 Anler Hp
;; Author: Anler Hp <anler86 [at] gmail.com>
;; Keywords: color, theme, minimal
;; X-URL: http://github.com/ikame/minimal-theme
;; URL: http://github.com/ikame/minimal-theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(deftheme yellow-on-dark "minimal dark theme.")

(let* ((class '((class color) (min-colors 89)))
       (foreground "wheat3")
       (background "grey20")
       (keyword "wheat1")
       (cursor keyword)
       (border "grey10")
       (minibuffer cursor)
       (region "dark slate gray")
       (comment "wheat4")
       (comment-delimiter comment)
       (constant keyword)
       (string "tan")
       (modeline-foreground foreground)
       (modeline-background "grey18")
       (modeline-foreground-inactive comment)
       (modeline-background-inactive background)
       (hl-background "grey14")
       (hl-face-background nil)
       (linum "grey30")
       (failure "red")
       (org-background "grey8")
       )
  (setq fci-rule-color comment)
  (custom-theme-set-faces
   'yellow-on-dark

   ;; basic stuff
   `(default ((,class (:background ,background :foreground ,foreground))))
   `(cursor ((,class (:background ,cursor :inverse-video t))))
   `(vertical-border ((,class (:foreground ,border))))

   ;; minibuffer
   `(minibuffer-prompt ((,class (:foreground ,minibuffer))))

   ;; region
   `(region ((,class (:background ,region))))
   `(secondary-selection ((,class (:background ,region))))

   ;; fringe
   `(fringe ((,class (:background ,background :foreground ,foreground))))
   
   ;; faces
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-constant-face ((,class (:foreground ,keyword))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-type-face ((,class (:foreground ,keyword))))
   `(font-lock-function-name-face ((,class (:foreground ,keyword))))
   `(font-lock-variable-name-face ((,class (:foreground ,keyword))))

   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-delimiter))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-doc-face ((,class (:inherit (font-lock-comment-face)))))
   `(font-lock-string-face ((,class (:foreground ,foreground :foreground ,string))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,foreground :background ,region :weight normal))))
   `(isearch-fail ((,class (:foreground ,failure :bold t))))
   `(lazy-highlight
     ((,class (:foreground ,foreground :background ,region))))

   ;; ido-mode
   `(ido-subdir ((,class (:foreground ,foreground ))))
   `(ido-only-match ((,class (:foreground ,foreground))))

   ;; show-paren
   `(show-paren-match
     ((,class (:background ,region))))
   `(show-paren-mismatch
     ((,class (:foreground ,failure))))

   ;; modeline
   `(mode-line
       ((,class (:inverse-video unspecified
                                :overline ,border
                                :underline nil
                                :foreground ,modeline-foreground
                                :background ,modeline-background
                                :box (:line-width 1 :color ,border :style unspecified)
                                ))))
     ;;`(mode-line-buffer-id ((,class (:weight bold))))
     `(mode-line-inactive
       ((,class (:inverse-video unspecified
                                :overline ,border
                                :underline nil
                                :foreground ,modeline-foreground-inactive
                                :background ,modeline-background-inactive
                                :box (:line-width 1 :color ,border)
                                ))))

      ;; hl-line-mode
     `(hl-line ((,class (:background ,hl-background))))
     `(hl-line-face ((,class (:background ,hl-face-background))))

     ;; linum mode
     `(linum ((t (:inherit (shadow default) :foreground ,linum :height 0.8))))

     ;; auto-complete
     `(ac-candidate-face ((,class (:background ,modeline-background :foreground ,foreground))))
     `(ac-selection-face ((,class (:background ,region :foreground ,foreground))))
     `(ac-candidate-mouse-face ((,class (:background ,region :foreground ,foreground))))
     `(ac-completion-face ((,class (:foreground ,comment :underline t))))
     `(ac-gtags-candidate-face ((,class (:background ,modeline-background :foreground ,string))))
     `(ac-gtags-selection-face ((,class (:background ,region :foreground ,comment))))
     `(popup-summary-face ((,class (:background ,modeline-background :foreground ,comment))))

     ;; org mode
     `(org-document-title ((,class (:foreground ,foreground))))

     `(org-link ((,class (:background ,org-background :foreground ,foreground :underline t))))
     `(org-tag ((,class (:background ,org-background :foreground ,foreground))))
     `(org-warning ((,class (:background ,region :foreground ,foreground :weight bold))))
     `(org-todo ((,class (:background ,region :foreground ,foreground :weight bold))))
     `(org-done ((,class (:background ,region :foreground ,foreground :weight bold))))
     `(org-table ((,class (:background ,org-background))))
     `(org-code ((,class (:background ,org-background))))
     `(org-date ((,class (:background ,org-background :underline t))))
     `(org-block ((,class (:background ,org-background))))
     `(org-block-background ((,class (:background ,org-background :foreground ,foreground))))
     `(org-block-begin-line
       ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))
     `(org-block-end-line
       ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))

     ;; ERC settings
     `(erc-current-nick-face ((t (:foreground ,foreground  :underline nil :weight normal))))
     `(erc-default-face ((t (:foreground ,foreground))))
     `(erc-input-face ((t (:foreground ,foreground))))
     `(erc-nick-default-face ((t (:foreground ,keyword :weight normal))))
     `(erc-notice-face ((t (:foreground ,foreground))))
     `(erc-timestamp-face ((t (:foreground ,foreground))))

     ;; Misc
     `(sh-heredoc ((t (:foreground ,string))))
     `(sh-quoted-exec ((t (:foreground ,keyword))))
     `(font-lock-warning-face ((t (:foreground ,foreground))))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yellow-on-dark)
