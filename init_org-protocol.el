;;; init_org_protocol.el --- Set up org extension for custom org actions

;; =============================================================================
;; Org Protocol Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-21 1319
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; An extension for Org mode that handles calls to the Emacs client in order to
;; build custom Org actions.  A dependency for some of the other packages I want
;; to use with Org mode to build an incremental reading tool.

;;; Code:
(use-package org-protocol
  :straight t
  )

(provide 'init_org_protocol)

;;; init_org_protocol.el ends here
