;;; init_ox-rss.el --- set up RSS backend for org-publish

;; ==============================================================================
;; ox-rss Setup File
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2023-05-24 1548
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Allows for org-publish to generate an RSS page.

;;; Code:
(use-package ox-rss
  :defer t
  :straight (ox-rss :type git :host github :repo "Munksgaard/org-mode/blob/master/contrib/lisp")
  )

(provide 'init_ox-rss)

;;; init_ox-rss.el ends here
