;;; init_numerologizer.el --- Initializers numerologizer package

;; ==============================================================================
;; Numerologizer
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2022-02-01 2339
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; I wrote this, and there's more detail in the actual package.

;;; Code:
(use-package numerologizer
  :straight (numerologizer :type git :host github :repo "jpowellrussell/numerologizer"
            :fork (:host github
                         :repo "jpowellrussell/numerologizer"))
  :demand t)

(provide 'init_numerologizer)

;;; init_numerologizer.el ends here
