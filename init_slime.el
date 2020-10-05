;;; init_slime.el --- Configure Slime, an interactive common lisp minor mode

;; ==============================================================================
;; Slime Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-04 1739
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This is recommended by the SBCL website, so let's see how it goes.

;;; Code:
(use-package slime
  :ensure t
  :straight t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  )

(provide 'init_slime)

;;; init_slime.el ends here
