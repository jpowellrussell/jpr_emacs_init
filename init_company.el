;;; init_company.el --- In-buffer autocompletion tool

;; ==============================================================================
;; Company Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-21 1132
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Creates drop-downs in-buffer for autocomplete purposes, can be extended with a variety of backends.

;;; Code:
(use-package company
  :straight t
  :hook (after-init-hook . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 0)
  (push 'company-capf company-backends)
  )

(provide 'init_company)

;;; init_company.el ends here
