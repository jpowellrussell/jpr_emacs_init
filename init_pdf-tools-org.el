;;; init_pdf-tools-org.el --- Exports annotations to Org file

;; ==============================================================================
;; PDF-Tools-Org Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-08 1652
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Expands pdf-tools functionality to export annotations (like highlights) to an
;; org file.

;;; Code:
(straight-use-package
 '(pdf-tools-org :type git :host github :repo "machc/pdf-tools-org")
  )

(provide 'init_pdf-tools-org)

;;; init_pdf-tools-org.el ends here
