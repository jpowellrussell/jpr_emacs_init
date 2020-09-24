;;; init_beacon.el --- Set up package for cursor-finding beacon

;; ==============================================================================
;; Beacon Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-05-15 1604
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This creates a colored overlay on the point whenever you scroll so you don't
;; lose it

;;; Code:
(use-package beacon
  :straight t
  :bind ("C-c b" . beacon-blink)
  :config
  (beacon-mode 1)
  (setq beacon-size 40)
  (setq beacon-color "#ccdc90")
  )

(provide 'init_beacon)

;;; init_beacon.el ends here
