;;; init_zenburn.el --- Set up Zenburn Theme

;; ==============================================================================
;; Zenburn Theme Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-28 1536
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; A lovely low-contrast theme. I usually use the high contrast modification,
;; but I wanted to test out whether the selection face is clearer in this.

;;; Code:
(use-package zenburn-theme
  :straight t
  :config (load-theme 'zenburn t)
  )

(provide 'init_zenburn)

;;; init_zenburn.el ends here
