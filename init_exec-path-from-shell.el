;; =============================================================================
;; JPR's Exec-Path-From-Shell Configuration
;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-14
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Apparently when MacOS opens Emacs through the GUI, it sets up some default
;; environment variables rather than inheriting what's in your actual path. This
;; package fixes that.
;; I don't currently do any command line stuff from within Emacs, but in the
;; long run I almost certainly want to change that

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-copy-env "PYTHONPATH")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
