;;; init_tabbar.el --- Set up buffer tab bar package

;; ==============================================================================
;; Tabbar
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-29 1936
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Originally from Aquamacs, an alternate way to do a tab bar than the Awesome
;; Tabs package I tried first. Apparently, Emacs 27 will ship with a built in
;; tab-bar-mode, but who knows when that'll come out

;;; Code:
(use-package tabbar
  :straight t
  :config
  (tabbar-mode 1))

(provide 'init_tabbar)

;;; init_tabbar.el ends here
