;;; init_winum.el --- Set up mode to allow switching to windows by number

;; ==============================================================================
;; Winum Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1319
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Gives each window a number and allows for switching between with C-x 1-9

;;; Code:
(use-package winum
  :straight t
  :ensure t
  :config (winum-mode))

(provide 'init_winum)

;;; init_winum.el ends here
