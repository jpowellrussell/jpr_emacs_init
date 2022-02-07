;;; init_mu.el --- Email indexer and searcher

;; ==============================================================================
;; Mu Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-28 1801
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Email indexer and searcher, with a built in email client (mu4e) for Emacs so
;; that I can start to live entirely within Emacs

;;; Code:

;; Make sure emacs can find the mu4e package files.
(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e")
(use-package mu4e
  :ensure t
  :defer t
  :bind ("C-c m" . mu4e)
  :config
  ;; Require smtpmail
  (require 'smtpmail)

  ;; Tell mu4e where to find Maildir
  (setq mu4e-maildir "/Users/Jeff/Maildir")

  ;; Tell mu4e how to sync email
  (setq mu4e-get-mail-command "/usr/local/bin/mbsync -a")

  ;; Tell mu4e to use w3m for html rendering
  ;; (setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")

  ;; Tell mu4e to change filenames when moving, because otherwise it will break
  ;; mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Taken from mu4e page to define bookmarks, I'll likely want to define some
  ;; more useful ones once I get an idea for how my workflow goes in mu4e.
  (add-to-list 'mu4e-bookmarks
             '("size:5M..500M" "Big Messages" ?b))

  ;; Kill message composition buffer once sent
  (setq message-kill-buffer-on-exit t)

  ;; Define function for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Define program to use for send mail function
  (setq sendmail-program "/usr/local/bin/msmtp")

  ;; Define where to put attachments
  (setq mu4e-attachment-dir "~/Downloads")

  ;; Use Ivy for auto-complete when switching maildir
  (setq mu4e-completing-read-function 'ivy-completing-read)

  ;; Auto-complete addresses when composing
  (setq mu4e-compose-complete-addresses t)

  ;; Don't include own email address when replying to all
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Don't include self on cc list when composing a message
  (setq mu4e-compose-keep-self-cc nil)

  ;; "Flow" lines and don't insert hard line breaks so that my emails don't look
  ;; weird to non-plain-text email users (meaning just about everyone else)
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

  ;; Don't auto-include a signature, as it's behavior with gmail and outlook
  ;; quoting conventions is not what I want when replying (it puts it after all
  ;; quoted text, and Emacs appears to have a hard enough preference for
  ;; old-school in-line replies that it won't let you put a signature before
  ;; quoted text)

  (setq mu4e-compose-signature-auto-include nil)

  ;; Set the default policy for which context to use
  (setq mu4e-context-policy 'pick-first)

  ;; Don't show duplicate messages in queries
  (setq mu4e-headers-skip-duplicates t)

  ;; Define header date format when composing messages
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

  ;; Define the fields for message headers
  (setq mu4e-headers-fields '((:human-date   . 20)
                              (:flags        . 6)
                              (:mailing-list . 10)
                              (:from         . 22)
                              (:subject)))

  ;; Show related messages when searching, like replies
  (setq mu4e-headers-include-related t)

  ;; Try out the new Gnus Article view-based message view
  (setq mu4e-view-use-gnus t)

  ;; Show addresses rather than only names
  (setq mu4e-view-show-addresses t)

  ;; Show images included in messages (though I haven't seen it work yet)
  (setq mu4e-view-show-images t)

  ;; Use ImageMagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Set up action to view in browser
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; Show smtp debug info
  (setq smtpmail-debug-info t)

  ;; Make the above debug info more verbose
  (setq smtpmail-debug-verb t)

  ;; Don't ask to confirm when quitting
  (setq mu4e-confirm-quit nil)

  ;; Match messages on headers
  (defun my-msg-match (msg arg address)
    "Match messages (MSG) with arguments (ARG) or address (ADDRESS) on headers."
    (mu4e-message-contact-field-matches msg arg address))

  (defun revert-if-mu4e-main ()
    "Revert/update buffer if it's mu4e-main."
    (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
      (revert-buffer)))

  ;; Define "contexts", which are how mu4e defines different behaviors and
  ;; defaults for different accounts.
  (setq mu4e-contexts
        `(
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda ()
                          (mu4e-message "Entering gmail context")
                          (revert-if-mu4e-main))
            :leave-func (lambda ()
                          (mu4e-message "Leaving gmail context")
                          (revert-if-mu4e-main))
            :match-func (lambda (msg)
                          (when msg
                            (or (my-msg-match msg :to "jeff.powell.russell@gmail.com")
                                (my-msg-match msg :from "jeff.powell.russell@gmail.com")
                                (my-msg-match msg :cc "jeff.powell.russell@gmail.com")
                                (my-msg-match msg :bcc "jeff.powell.russell@gmail.com")
                                (string-match-p "^/gmail/Inbox"
                                                (mu4e-message-field msg :maildir)))))
            :vars '( ( user-mail-address           . "jeff.powell.russell@gmail.com" )
                     ( mu4e-compose-signature      . "Cheers,\nJeff" )
                     ( message-cite-style          . message-cite-style-gmail )
                     ( message-cite-reply-position . above )
                     ( smtpmail-smtp-user          . "jeff.powell.russell@gmail.com" )
                     ( smtpmail-smtp-server        . "smtp.gmail.com" )
                     ( smtpmail-smtp-service       . 587 )
                     ( smtpmail-stream-type        . starttls)
                     ( mu4e-maildir-shortcuts      . ((:maildir "/gmail/Inbox"    :key ?i )
                                                      (:maildir "/gmail/All_Mail" :key ?a )
                                                      (:maildir "/gmail/Trash"    :key ?t )))
                     ( mu4e-sent-messages-behavior . delete )
                     ( mu4e-sent-folder            . "/Gmail/Sent")
                     ( mu4e-drafts-folder          . "/Gmail/Drafts")
                     ( mu4e-trash-folder           . "/Gmail/Trash")
                     ))
          ,(make-mu4e-context
            :name "rice"
            :enter-func (lambda ()
                          (mu4e-message "Entering Rice context")
                          (revert-if-mu4e-main))
            :leave-func (lambda ()
                          (mu4e-message "Leaving Rice context")
                          (revert-if-mu4e-main))
            :match-func (lambda (msg)
                          (when msg
                            (or (my-msg-match msg :to "jr75@rice.edu")
                                (my-msg-match msg :from "jr75@rice.edu")
                                (my-msg-match msg :cc "jr75@rice.edu")
                                (my-msg-match msg :bcc "jr75@rice.edu")
                                (my-msg-match msg :to "jeff.russell@rice.edu")
                                (my-msg-match msg :from "jeff.russell@rice.edu")
                                (my-msg-match msg :cc "jeff.russell@rice.edu")
                                (my-msg-match msg :bcc "jeff.russell@rice.edu")
                                (string-match-p "^/rice/Inbox"
                                                (mu4e-message-field msg :maildir)))))
            :vars '( ( user-mail-address           . "jr75@rice.edu" )
                     ( smtpmail-smtp-user          . "jr75" )
                     ( smtpmail-smtp-server        . "smtp.mail.rice.edu" )
                     ( smtpmail-smtp-service       . 465 )
                     ( smtpmail-stream-type        . ssl)
                     ( mu4e-compose-signature      . "Jeff Russell, MBA\nLecturer, Communication Program\nJones Graduate School of Business | Rice University\nC: 832.264.0583" )
                     ( message-cite-style          . message-cite-style-outlook )
                     ( message-cite-reply-position . above )
                     ( mu4e-maildir-shortcuts      . ((:maildir "/Rice/Inbox"         :key ?i )
                                                      (:maildir "/Rice/Archive"       :key ?a )
                                                      (:maildir "/Rice/Local_Archive" :key ?l )))
                     ( mu4e-sent-messages-behavior . sent )
                     ( mu4e-sent-folder            . "/Rice/Sent Items")
                     ( mu4e-drafts-folder          . "/Rice/Drafts")
                     ( mu4e-trash-folder           . "/Rice/Deleted Items")
                     ))
          ))
)

(provide 'init_mu)
;;; init_mu.el ends here
