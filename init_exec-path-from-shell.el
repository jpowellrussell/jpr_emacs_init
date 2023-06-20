;;; init_exec-path-from-shell.el --- Set up user terminal environment in GUI

;; ==============================================================================
;; Exec-Path-From-Shell Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1241
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Apparently when MacOS opens Emacs through the GUI, it sets up some default
;; environment variables rather than inheriting what's in your actual path. This
;; package fixes that.
;; I don't currently do any command line stuff from within Emacs, but in the
;; long run I almost certainly want to change that

;;; Code:
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :demand
  :config
  (setq exec-path-from-shell-variables '("GOPATH" "PATH"))
  (exec-path-from-shell-initialize))

(provide 'init_exec-path-from-shell)

;;; init_exec-path-from-shell.el ends here
