;;; init_org-drill.el --- Set up SRS extension for Org

;; =============================================================================
;; Org-Drill Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-21 1336
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Org-Drill is a Spaced Repetition Study extension for Org-Mode to allow for
;; memorization and incremental reading in the style of Supermemo or Anki

;;; Code:
(use-package org-drill
  :straight t
  :config
  ;;(setq org-drill-scope directory)
  (setq org-drill-use-visible-cloze-face-p t)
  (setq org-drill-maximum-items-per-session 250)
  (setq org-drill-maximum-duration 60)
  (setq org-drill-add-random-noise-to-intervals-p t)
  )

(provide 'init_org-drill)

;;; init_org-drill.el ends here
