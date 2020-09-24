;;; init_ivy.el --- Set up Ivy Autocompletion

;; ==============================================================================
;; Ivy Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-05-14 0953
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; One of several autocomplete options. I've rather liked Ido with some add-ons

;;; Code:
(use-package counsel
  :straight t
  :init
  (ido-mode -1)
  :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d%d) ")
  :bind
  (
   ("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y"     . counsel-yank-pop)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-libary)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> j"  . counsel-set-variable)
   ("C-s"     . swiper-isearch)
   ("C-x b"   . ivy-switch-buffer)
   ("C-x k"   . kill-buffer)
   ("C-c v"   . ivy-push-view)
   ("C-c V"   . ivy-pop-view)
   :map counsel-find-file-map
   ("RET"   . ivy-alt-done)
    )
  )

(use-package ivy-hydra
  :straight t
  :config
  (setq ivy-display-style 'fancy))
;
(provide 'init_ivy)

;;; init_ivy.el ends here
