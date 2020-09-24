;;; init_telephone-line.el --- Sets up package for powerline customization

;; ==============================================================================
;; Telephone Line Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 1849
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Apparently the "powerline" is a thing from Evil/vi/vim? But it looks like it
;; might help make certain info more visible in the status bar, so I'm all for it.

;;; Code:
(use-package telephone-line
  :straight t
  :config
  (telephone-line-mode 1)
  )

(provide 'init_telephone-line)

;;; init_telephone-line.el ends here
