;;; init_pyvenv.el --- python virtual environment for emacs

;; =============================================================================
;; Pyvenv Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-18 1622
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Global minor mode that replicates the changes done by virtualenv within Emacs

;;; Code:
(use-package pyvenv
  :straight t
  :defer t
  )

(provide 'init_pyenv)
;;; init_pyvenv.el ends here
