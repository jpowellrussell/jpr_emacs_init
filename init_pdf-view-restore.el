;; =============================================================================
;; PDF View Restore Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-17 1927
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Package that takes you back to the last place viewed in a PDF

(use-package pdf-view-restore
  :straight t
  :defer t
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  )

;; init_pdf-view-restore.el ends here
