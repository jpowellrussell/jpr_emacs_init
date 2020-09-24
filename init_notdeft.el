;;; init_notdeft.el --- Full-text search plugin for use with notes and such

;; ==============================================================================
;; NotDeft Setup
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-22 1518
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; A faster full-text search than Deft, meant for use with lots of files. Not
;; quite as handy as Deft for quick note-taking, but I'm not currently using
;; Deft for creating notes, so that's okay.

;;; Code:
(use-package notdeft
  :straight
  (:type git :host github :repo "hasu/notdeft")
  :hook
  (org-mode-local-variables-hook . default-notdeft-hook)
  (after-init-hook . notdeft-mode)
  :bind
  ("C-c n d" . notdeft)
  :config
  (require 'notdeft-autoloads)
  (setq notdeft-directories '("~/dropbox/khs/"))
  
  ;; Full path of "notdeft-xapian" executable.
  (let ((x
         (let ((default-directory
                 (file-name-directory
                  (file-truename (locate-library "notdeft")))))
           (file-truename "xapian/notdeft-xapian"))))
    (setq notdeft-xapian-program
          (and (file-executable-p x) x)))
  )

(provide 'init_notdeft)

;;; init_notdeft.el ends here
