;;; init_elpy.el --- Set up a Python IDE package

;; ==============================================================================
;; Elpy Python IDE Set up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1322
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Elpy is supposed to be one-stop shop for Python configuration, so I'm trying
;; it out. *Learn Python the Hard Way* recommends against using an IDE as a
;; beginner, but from the substance of his arguments against it, I don't think a
;; mode in Emacs really qualifies.

;;; Code:
(use-package elpy
  :straight t
  :defer t
  :init
  (elpy-enable)
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  )

(provide 'init_elpy)

;;; init_elpy.el ends here
