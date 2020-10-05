;;; init_rainbow-delimiters.el --- Set up package to color-code parentheses

;; ==============================================================================
;; Rainbow Delimiters Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1316
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Mode that colors parentheses by level of nesting

;;; Code:
(use-package rainbow-delimiters
  :straight t
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init_rainbow-delimiters)

;;; init_rainbow-delimiters.el ends here
