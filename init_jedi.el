;;; init_jedi.el --- Autocomplete for Python

;; =============================================================================
;; Jedi Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-18 1637
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Autocompletes and looks up documentation for Python

;;; Code:
(use-package jedi
  :straight t
  :defer t
  :hook (python-mode . jedi-mode)
  )

(provide 'init_jedi)
;;; init_jedi.el ends here
