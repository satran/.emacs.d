(defun jmn-erc-format-nick (&optional user channel-data)
  "Like `erc-format-nick' but trim/pad nick to a fixed length.
     - Based on xwl-erc-format-nick"
  (let ((nick (erc-format-nick user channel-data)))
    (setq nick (substring nick 0 5))
    (setq nick (format "%5s" nick))
    nick))

;; Use with below (or customize):
(setq erc-format-nick-function 'jmn-erc-format-nick) 
