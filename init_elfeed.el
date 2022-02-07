;;; init_elfeed.el --- Elfeed RSS reader

;; ==============================================================================
;; Elfeed RSS and Atom Reader Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-06-08 2131
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Elfeed manages reading RSS and Atom feeds in Emacs with a nifty database
;; approach.

;;; Code:
(defun my-elfeed-goodies-setup ()
  "Setup some, but not all, elfeed extras"
  (interactive)
  (add-hook 'elfeed-new-entry-hook #'elfeed-goodies/html-decode-title)
  (when (boundp 'elfeed-new-entry-parse-hook)
    (add-hook 'elfeed-new-entry-parse-hook #'elfeed-goodies/parse-author))
  (setq elfeed-search-header-function #'elfeed-goodies/search-header-draw
        elfeed-show-entry-switch #'elfeed-goodies/switch-pane
        elfeed-show-entry-delete #'elfeed-goodies/delete-pane
        ;elfeed-show-refresh-function #'elfeed-goodies/show-refresh--plain
        )
  (define-key elfeed-show-mode-map "n" #'elfeed-goodies/split-show-next)
  (define-key elfeed-show-mode-map "p" #'elfeed-goodies/split-show-prev))

(use-package elfeed
  :straight t
  :ensure t
  :bind ("C-c n r" . elfeed) ;mnemonic: "news read"
  :config
  (setq elfeed-feeds "~/.emacs.d/elfeed.org")
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "marginalrevolution"
                                :before "6 months ago"
                                :remove 'unread
                                :add 'old))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "twitter"
                                :before "3 months ago"
                                :remove 'unread
                                :add 'old))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "reddit\\.com"
                                :before "4 months ago"
                                :remove 'unread
                                :add 'old))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title "Ansi Common Lisp"
                                :remove 'unread
                                :add 'old))
  (setq-default elfeed-search-filter "+unread")
  (setq elfeed-log-level 'debug)
  )

;; Allows for managing elfeed feeds in an org file, making organization and
;; tagging simpler

(use-package elfeed-org
  :straight t
  :config (elfeed-org)
  )

;; Note that it's important to load elfeed goodies first, apparently. When I had
;; the order reversed, the search buffer loaded weirdly. Now it seems to be
;; working right. One change I might want to see about making is that
;; elfeed-goodies seems not to show the header information when you click on an
;; entry, and I think I would like for it to be there, especially if I'm going
;; to save any entries.

(use-package elfeed-goodies
  :straight t
  :ensure t
  :after (elfeed)
  :config
  (my-elfeed-goodies-setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  )

(use-package elfeed-score
  :straight t
  :ensure t
  :after (elfeed elfeed-goodies)
  :config
   (setq elfeed-score-score-file "~/git/jpr_emacs_init/elfeed.score")
   (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
   (setq elfeed-score-log-level 'debug)
   (setq elfeed-score-log-debug t)
   (progn
     (elfeed-score-enable)
     (define-key elfeed-search-mode-map "=" elfeed-score-map))
  )

(provide 'init_elfeed)

;;; init_elfeed.el ends here
