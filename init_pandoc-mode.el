;;; init_pandoc-mode.el --- Interface for Using Pandoc within Emacs

;; ==============================================================================
;; Pandoc-Mode Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-06 1327
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; A package that allows use of Pandoc within Emacs using a hydra. Sounds great!

;;; Code:
(use-package pandoc-mode
  :defer t
  :ensure t
  :straight t
  :hook ((org-mode . pandoc-mode)
         (markdown-mode . pandoc-mode))
  )

(provide 'init_pandoc-mode)

;;; init_pandoc-mode.el ends here
