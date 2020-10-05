;; =============================================================================
;; Flycheck Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-17 2005
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Package for syntax checking in Emacs, backed up by language-specific
;; configuration


(use-package flycheck
  :straight t
  :defer t
  :init (global-flycheck-mode)
  )

;; init_flycheck.el ends here
