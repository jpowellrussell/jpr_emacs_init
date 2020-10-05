;;; init_magit.el --- Set up git tool for Emacs

;; ==============================================================================
;; Magit Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-05-12 0818
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Apparently a super awesome Git client for Emacs. If I'm ever going to do real
;; programming, I'll have to get comfortable with git.

;;; Code:
(use-package magit
  :straight t
  :defer t
  :bind (
         ("C-x g" . magit-status)
         )
  :config
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-diff-refine-hunk nil)

  )

(provide 'init_magit)
;;; init_magit.el ends here
