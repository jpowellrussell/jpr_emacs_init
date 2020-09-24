;;; init_company-org-roam.el --- Allows for in-buffer autocomplete for links

;; ==============================================================================
;; Company-Org-Roam Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-20 1815
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Company-Org-Roam is a backend extension for Company that allows for in-buffer completion suggestions for org-roam links.

;;; Code:
(use-package company-org-roam
  :straight (:host github :type git :repo "org-roam/company-org-roam")
  :config
  (push 'company-org-roam company-backends)
  )

(provide 'init_company-org-roam)

;;; init_company-org-roam.el ends here
