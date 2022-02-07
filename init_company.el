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
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
              ("M-n" . company-select-next)
              ("M-p" . company-select-previous))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (push 'company-capf company-backends)
  )

(provide 'init_company)

;;; init_company.el ends here
