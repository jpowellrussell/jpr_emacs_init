;;; init_kaolin-themes.el --- Set up the Kaolin Theme

;; =============================================================================
;; Kaolin Treemacs Theme Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-23 0842
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; This theme includes a treemacs theme that uses all-the-icons, which I was
;; unable to make work in making my own theme, so I got this to learn from

;;; Code:
(use-package kaolin-themes
  :straight t
  :config
  ;(load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme)
  ;;(setq kaolin-themes-treemacs-hl-line t)
  )

(provide 'init_kaolin-themes)

;;; init_kaolin-themes.el ends here
