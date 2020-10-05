;;; init_org-sidebar.el --- Sidebar with handy info for org files

;; ==============================================================================
;; Org Sidebar Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-06-09 2001
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Creates a sidebar in a new window with a customizable "sidebar". I'm
;; interested in the "tree mode" sidebar, though Treemacs seems to offer that if
;; the Org file is open, so it might not be needed

;;; Code:
(use-package org-sidebar
  :straight t
  :defer t
  )

(provide 'init_org-sidebar)

;;; init_org-sidebar.el ends here
