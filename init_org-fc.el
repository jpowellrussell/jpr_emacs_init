;;; init_org-fc.el --- Set up for Org-Flashcards, another SRS plugin for Org Mode

;; ==============================================================================
;; org-fc Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-03 1518
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Org-fc is another option for using mode to maintain SRS flashcards. It looks
;; like it might have better performance and easier customization than Org-Drill.

;;; Code:
(use-package org-fc
  :straight
  (org-fc
   :type git :host github :repo "l3kn/org-fc"
   :files (:defaults "awk" "demo.org"))
  :bind ("C-c f" . org-fc-hydra/body)
  :custom
  (org-fc-directories '("~/dropbox/khs/increading/"))
  :config
  (require 'org-fc-hydra)
  (require 'org-fc-keymap-hint)

  (defun org-fc-type-increading-init ()
    "Mark headline as card of increading type."
    (interactive)
    (org-fc--init-card "increading")
    (org-fc-review-data-update '("front")))

  (defvar org-fc-type-increading--hidden '())

  (defun org-fc-type-increading-setup (_position)
    "Prepare an increading card for review."
    (interactive)
    (if (org-fc-has-back-heading-p)
        (progn
          (org-show-subtree)
          (setq org-fc-type-increading--hidden (org-fc-hide-subheading "Back")))
      (setq org-fc-type-increading--hidden nil)
      (org-flag subtree t)))

  (defun org-fc-type-increading-flip ()
    "'Flip' an Increading Card. There is not actually a flip."
    (interactive)
    (org-show-subtree)
    (save-excursion
      (dolist (pos org-fc-type-increading--hidden)
        (goto-char pos)
        (org-show-subtree)))
    (org-fc-with-point-at-back-heading
     (org-fc-show-latex)))

  (org-fc-register-type
   'increading
   'org-fc-type-increading-setup
   'org-fc-type-increading-flip
   'org-fc-noop)
  )

(provide 'init_org-fc)

;;; init_org-fc.el ends here
