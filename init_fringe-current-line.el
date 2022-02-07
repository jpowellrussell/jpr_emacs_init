;;; init_fringe-current-line.el --- Bimap in fringe of line with point

;; ==============================================================================
;; Fringe Current Line Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-12-16 1109
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Package that puts a bitmap in the fringe to show the currently used line.

;;; Code:

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

(provide 'init_fringe-current-line)

;;; init_fringe-current-line.el ends here
