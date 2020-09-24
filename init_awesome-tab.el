;;; init_awesome-tab.el --- Set up package for tabs for buffers

;; ==============================================================================
;; Awesome Tab Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 1843
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Awesome tab provides for tabs for buffers, which I think will help me keep
;; track of which buffers are open, even if I continue to use other methods to
;; switch between them

;;; Code:
(use-package awesome-tab
  :straight (awesome-tab :type git :host github
			 :repo "manateelazycat/awesome-tab")
  :config
  (setq awesome-tab-height 110)
  (setq awesome-tab-active-bar-height 15)

  (awesome-tab-mode t)
  )

(provide 'init_awesome-tab)

;;; init_awesome-tab.el ends here
