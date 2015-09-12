(require 'mu4e)

;; default
(setq mu4e-maildir "~/mail")
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
;; (setq mu4e-html2text-command "html2text --reference-links")
(setq mu4e-get-mail-command "true"
      mu4e-update-interval 300)
(setq mu4e-headers-fields (quote ((:subject) (:from-or-to . 22) )))
(setq mu4e-headers-visible-columns 80)
 

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-maildir-shortcuts
    '(("/ranjeev/INBOX"               . ?i)
      ("/optiopay/INBOX"              . ?p)
      ("/ranjeev/[Gmail].All Mail"    . ?a)
      ("/optiopay/[Gmail].All Mail"   . ?r)))

;; something about ourselves
(setq
   user-full-name  "Satyajit Ranjeev"
   mu4e-compose-signature
    (concat
      "satran\n"
      "http://satran.in\n"))

(setq mu4e-drafts-folder "/ranjeev/[Gmail].Drafts"
      mu4e-sent-folder   "/ranjeev/[Gmail].Sent Mail"
      mu4e-trash-folder  "/ranjeev/[Gmail].Trash"
      user-mail-address "s@ranjeev.in")

(defvar my-mu4e-account-alist
  '(("ranjeev"
     (mu4e-drafts-folder "/ranjeev/[Gmail].Drafts")
     (mu4e-sent-folder   "/ranjeev/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/ranjeev/[Gmail].Trash")
     (user-mail-address "s@ranjeev.in")
     (smtpmail-smtp-user "s@ranjeev.in"))
    ("optiopay"
     (mu4e-drafts-folder "/optiopay/[Gmail].Drafts")
     (mu4e-sent-folder   "/optiopay/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/optiopay/[Gmail].Trash")
     (user-mail-address "sr@optiopay.com")
     (smtpmail-smtp-user "sr@optiopay.com"))))

(require 'smtpmail)
;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
