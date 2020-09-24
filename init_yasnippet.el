;; ;; =============================================================================
;; ;; YASnippet Text Expansion Set Up
;; ;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-15
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Yasnippet is a snippet expander. Apparently it can do a lot of great stuff, but right now I only have a snippet for my slip layout.
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

;; init_yas.el ends here
