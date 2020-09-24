;;; init_ivy-bibtex.el --- Autocompletion for bibtex references

;; ==============================================================================
;; Ivy Bibtex Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-17 2018
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Ivy-based alternative to popular Helm-Bibtex package

;;; Code:
(use-package ivy-bibtex
  :straight
  (ivy-bibtex
   :type git :host github :repo "tmalsburg/helm-bibtex")
  :after (ivy)
  :config
  (setq bibtex-completion-bibliography
        '("~/dropbox/khs/works_list/wellsprings.bib"))
  (setq bibtex-completion-library-path '("~/dropbox/khs/increading")))

(provide 'init_ivy-bibtex)

;;; init_ivy-bibtex.el ends here
