;;; init_dired.el --- Packages to extend built-in Dired

;; ==============================================================================
;; Dired Extension Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2021-03-17 2059
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; There are a number of packages that extend what dired can do, and it makes sense to me to keep all of their setup code here in one file.

;;; Code:
;; Dired+ adds a whole mess of extra functions
(use-package dired+
  :defer t
  :ensure t
  :straight t
  :hook (dired-mode . dired-hide-details-mode)
  :config
  )

;; Dired customizations
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)

(provide 'init_dired)

;;; init_dired.el ends here
