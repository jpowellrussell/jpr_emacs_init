;; =============================================================================
;; Buffer Flip Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-17 1941
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Flip between buffers in the stack with chosen keybindings

(use-package buffer-flip
  :straight t
  :bind (("M-<tab>" . buffer-flip)
	 :map buffer-flip-map
	 ( "M-<tab>"   . buffer-flip-forward)
	 ( "M-S-<tab>" . buffer-flip-backward)
	 ( "M-ESC"     . buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
	'("^\\*helm\\b"
	  "^\\*swiper\\*$"))
  )

;; init_buffer-flip.el ends here
