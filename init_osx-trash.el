;;; init_osx-trash.el --- Makes delete-by-moving-to-trash work for Mac

;; ==============================================================================
;; OSX-Trash Setup
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2021-03-21 0923
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Emacs's default delete-by-moving-to-trash behavior doesn't actually send
;; files to the system trash folder on OSX (I'm hoping this still works fine now
;; that they're on OS 11).

;;; Code:
(use-package osx-trash
  :ensure t
  :straight t
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(provide 'init_osx-trash)

;;; init_osx-trash.el ends here
