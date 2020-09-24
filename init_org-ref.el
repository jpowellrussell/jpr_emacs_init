;;; init_org-ref.el --- Manage references and citations in Org

;; ==============================================================================
;; Org-Ref Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-15 1434
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Package for managing citations and cross-references in org mode

;;; Code:
(use-package org-ref
  :straight t
  :after (org ivy-bibtex)
  :config
  (setq reftex-default-bibliography
        '("~/dropbox/khs/works_list/wellsprings.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/dropbox/khs/works_list/notes.org"
        org-ref-default-bibliography "~/dropbox/khs/works_list/wellsprings.bib"
        org-ref-pdf-directory "~/dropbox/khs/increading")
  )

(provide 'init_org-ref)

;;; init_org-ref.el ends here
