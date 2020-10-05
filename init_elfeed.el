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
  :defer t
  :bind ("C-c n r" . elfeed) ;mnemonic: "news read"r
  :config (setq elfeed-feeds "~/.emacs.d/elfeed.org")
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
  :defer t
  :config
  (my-elfeed-goodies-setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  )

(use-package elfeed-score
  :straight t
  :ensure t
  :defer t
  :config
   (setq elfeed-score-score-file "~/git/jpr_emacs_init/elfeed.score")
   (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map))
  )

(provide 'init_elfeed)

;;; init_elfeed.el ends here
