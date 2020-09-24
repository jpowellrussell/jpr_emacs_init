;; =============================================================================
;; Fringe Current Line Configuration
;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-14
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Package to put an indicator in the fringe next the line holding the point

(use-package fringe-current-line
  :straight t
  :config
  (global-fringe-current-line-mode 1)
  (defcustom fringe-current-line-indicator-bitmap
    (if (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'fringe-current-line-indicator-bitmap
          (vector #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111
                  #b00000111))
      'vertical-bar)
    "A more prominent fringe bitmap, modeled on the Treemacs one."
    :type 'fringe-bitmap
    :options (if (fboundp 'fringe-bitmaps)
                 (cons 'fringe-current-line-indicator-bitmap fringe-bitmaps)
               nil)
    :group 'fringe-current-line)

  (setq fcl-fringe-bitmap fringe-current-line-indicator-bitmap)
  )

;; init_fringe-current-line.el ends here
