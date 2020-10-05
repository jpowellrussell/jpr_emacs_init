;;; init_pdf-tools.el --- Set up fancy PDF Reader

;; ==============================================================================
;; PDF Tools Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1314
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Installed epdfinfo through brew tap dunn/emacs followed
;; by brew install pdf-tools From instructions found on stack
;; exchange, posted by 'Joe' here:
;; https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
;; To upgrade epdfinfo server, do 'brew upgrade pdf-tools' before
;; updating pacakge in Emacs. If something goes wrong, uninstall with
;; 'brew uninstall pdf-tools' and uninstall Emacs pacakge, then
;; reinstall both

;;; Code:
(use-package pdf-tools
  :straight t
  :defer t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install))

(provide 'init_pdf-tools)

;;; init_pdf-tools.el ends here
