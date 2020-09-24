;;; init_ov-highlight.el --- Persistent overlay highlighting with local variables

;; ==============================================================================
;; OV-Highlight Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 1639
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Uses file local variables to set persistent overlay highlights, so it ought
;; to play nicely with Org mode, or at least that's the hope. Otherwise I'll
;; have to do some trickery to make font lock ignore text properties

;;; Code:
(use-package ov-highlight
  :straight (ov-highlight :type git :host github
                          :repo "jkitchin/ov-highlight")
  :bind
  ("C-c C-H" . ov-highlight-read)
  :config
  (ov-highlight-make "read" '(:foreground "OliveDrab1"))
  )

(provide 'init_ov-highlight)

;;; init_ov-highlight.el ends here
