;;; init_buffer-flip.el --- Set up package for easy switching between buffers

;; ==============================================================================
;; Buffer Flip Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1245
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Flip between buffers in the stack with chosen keybindings

;;; Code:
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

(provide 'init_buffer-flip)

;;; init_buffer-flip.el ends here
