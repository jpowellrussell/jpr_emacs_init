;;; init_nov.el --- Set up Nov e-reader package
;;=============================================================================
;; Nov.el Set up for reading epubs
;;=============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-13
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Package for reading .epub files.  There might be a cleaner way to configure
;; the font set up function, but if so, I don't know it yet.  Font set up is
;; currently not working, so I'm messing with that

;;; Code:


(use-package nov
  :straight t
  :defer t
  :mode ("\\.epub\\'" .  nov-mode)
  :config (defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Crimson"
			   :height 1.25))
  :hook (nov-mode . my-nov-font-setup))

(provide 'init_nov)
;;; init_nov.el ends here
