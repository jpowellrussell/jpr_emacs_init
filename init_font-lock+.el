;;; init_font-lock+.el --- Sets up library to enhance standard font lock

;; ==============================================================================
;; Font-Lock+ Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 1620
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Font Lock is required by lots of modes I use, but interferes with some
;; functions, like highlighting text. This library adds a text property to text
;; that tells font lock to ignore it. My hope is that this allows me to have
;; highlights in Org documents, and maybe some other things useful for
;; incremental reading and/or progressive summarization.

;;; Code:
(use-package font-lock+
  :straight t
  :defer t
  )

(provide 'init_font-lock+)

;;; init_font-lock+.el ends here
