;;; init_org-journal.el --- Daily Journal Extension for Org Mode

;; ==============================================================================
;; Org-Journal Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-16 2017
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Daily journaling plugin for Org-Mode, apparently pairs well with Org-Roam

;;; Code:
(use-package org-journal
  :straight t
  :ensure t
  :defer t
  :bind
  ("C-c n j" . org-journal-new-entry)
  :init
  ; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/dropbox/KHS/daily_leaves/")
  (org-journal-date-format "%A, %Y-%m-%d"))

(provide 'init_org-journal)

;;; init_org-journal.el ends here
