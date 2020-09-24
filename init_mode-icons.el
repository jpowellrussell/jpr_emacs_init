;;; init_mode-icons.el --- Set up package for icons in the mode line

;; ==============================================================================
;; Mode-Icons Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 2006
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Instead of major mode name, shows icons, we'll see how I like it

;;; Code:
(use-package mode-icons
  :straight t
  :config
  (mode-icons-mode)
  )

(provide 'init_mode-icons)

;;; init_mode-icons.el ends here
