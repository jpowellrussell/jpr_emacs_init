;;; init_sublimity.el --- Set up Sublime Text Inspired Document Map

;; =============================================================================
;; Sublimity Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-25 1348
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Inspired by Sublime Text, provides smooth scrolling, a mini preview map of
;; the whole file that scrolls with current position, and a distraction-free
;; mode.  I'm mostly interested in the mini-map, as I got used to that in Sublime
;; Text and VS Code

;;; Code:
(use-package sublimity
  :straight t
  :hook (prog-mode . sublimity-mode 1)
  :config
  (sublimity-scroll)
  (sublimity-map)
  )

(provide 'init_sublimity)

;;; init_sublimity.el ends here
