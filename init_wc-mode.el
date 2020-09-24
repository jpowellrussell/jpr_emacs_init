;;; init_wc-mode.el --- Set up package to show character, word, and line count

;; ==============================================================================
;; wc-mode Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-05-19 0957
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This package enables a minor mode to show character, word, and line count in
;; the mode line.

;;; Code:
(use-package wc-mode
  :straight t
  :init
  (wc-mode t))

(provide 'init_wc-mode)

;;; init_wc.el ends here
