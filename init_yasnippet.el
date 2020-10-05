;;; init_yasnippet.el --- Set up snippet completion tool

;; ==============================================================================
;; YASnippet Text Expansion Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1320
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:

;; Yasnippet is a snippet expander. I'm currently using it for slipbox layout,
;; my daily todo, and my initialization files, but I might find some more uses
;; for it, such as email templates.

;;; Code:p
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  ;; The below was giving me trouble when evaluating the buffer while emacs was
  ;; running, saying it was the wrong type argument. My only guess is that it
  ;; wants more than one directory? It doesn't seem to be necessary so long as I
  ;; only use snippets in the default directory, but if I ever want remotely
  ;; maintained snippets, I'll have to see if I can make this work.
  ;;(setq yas-snippet-dirs "~/.emacs.d/snippets/")
  )

(provide 'init_yasnippet)

;;; init_yasnippet.el ends here
