;; =============================================================================
;; Elpy Python IDE  Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-17
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Elpy is supposed to be one-stop shop for Python configuration, so I'm trying
;; it out. *Learn Python the Hard Way* recommends against using an IDE as a
;; beginner, but from the substance of his arguments against it, I don't think a
;; mode in Emacs really qualifies.

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-rpc-virtualenv-path 'current)

  )

;; init_elpy.el ends here
