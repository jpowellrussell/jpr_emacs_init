;;; init_treemacs.el --- Treemacs Package Set Up

;; =============================================================================
;; JPR's Treemacs Configuration
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-20 1907
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Treemacs is a visual directory tree tool divided into workspaces and projects

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-width 45
	  treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
     )
;;  It appears that Treemacs must have some of its own initialization code on
;;  truncate/visual-line mode, as when I try to set the below settings manually,
;;  they seem to mess other stuff up, and when I evaluate my settings, Treemacs
;;  gets set to visual line mode, but it doesn't on startup
;;  (when (eq major-mode 'treemacs-mode) (truncate-lines t))
;;  (when (eq major-mode 'treemacs-mode)(visual-line-mode -1))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(provide 'init_treemacs)
;;; init_treemacs.el ends here
