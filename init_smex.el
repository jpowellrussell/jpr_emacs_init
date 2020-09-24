;;; init_smex.el --- Use Ido for M-x

;; =============================================================================
;; Smex Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-18 1638
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Tool built on top of Ido to allow smart completion for M-x

;;; Code:
(use-package smex
  :straight t
  :init (smex-initialize)
  )

;;; (provide (buffer-name))
;;; init_smex.el ends here
