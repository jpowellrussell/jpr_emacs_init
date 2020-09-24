;;; init_org-protocol-capture-html.el --- Sets up better HTML to Org conversion

;; ==============================================================================
;; Org Protocol Capture HTML Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-25 1355
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Uses pandoc for better website capturing into Org format.  See init_org.el for
;; capture template(s).

;;; Code:
(use-package org-protocol-capture-html
  :straight (org-protocol-capture-html :type git :host github
                                       :repo "alphapapa/org-protocol-capture-html"))

(provide 'init_org-protocol-capture-html)

;;; init_org-protocol-capture-html.el ends here
