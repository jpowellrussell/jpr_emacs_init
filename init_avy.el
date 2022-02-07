;;; init_avy.el --- Jump to typed characters

;; ==============================================================================
;; Avy Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2021-03-04 1130
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; I installed this as a dependency for clunky method to highlight text in a pdf-tools using the keyboard

;;; Code:
(use-package avy
  :straight t
  :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  )

(provide 'init_avy)

;;; init_avy.el ends here
