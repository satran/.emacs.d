;;; plumb.el implements plumbing similar to plan9

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
	  matches)
      (while (string-match regexp string pos)
	(push (match-string 0 string) matches)
	(setq pos (match-end 0)))
      matches)))

(defun plumb ()
  "sort of the plumber found in plan9"
  (interactive)
  (let ((file "")
	(line-no 1)
	(input (thing-at-point 'line))
	(regexp "\\([_~0-9a-zA-Z\-\\.\\/]+\\):\?\\([0-9]*\\)"))
    (dolist (line (re-seq regexp input))
      (message "%s" line)
      (string-match regexp line)
      (setq file (match-string 1 line))
      (setq line-no (match-string 2 line))
      (if (and (not (string-equal file "")) (file-exists-p file))
	  (progn
	    (message "%s:%s" file line-no)
	    (find-file-other-window file)
	    (beginning-of-buffer)
	    (if line-no (forward-line (- (string-to-number line-no) 1))))))))

(defun git-diff (&optional name)
  (interactive)
  (shell-command (concat "git diff " name)
		 (switch-to-buffer (make-temp-name "diff")))
  (diff-mode))
