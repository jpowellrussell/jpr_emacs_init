;;; init_org-roam-bibtex.el --- Reference management integration for Org-Roam

;; ==============================================================================
;; Org-Roam-Bibtex (ORB) Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-17 1820
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; A package that better integrates reference management with bibtex in Org-Roam

;;; Code:
(use-package org-roam-bibtex
  :straight
  (org-roam-bibtex
   :type git :host github :repo "org-roam/org-roam-bibtex")
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions)))
  )

(provide 'init_org-roam-bibtex)

;;; init_org-roam-bibtex.el ends here
