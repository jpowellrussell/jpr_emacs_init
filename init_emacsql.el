;;; init_emacsql.el --- Package to do SQL commands in Emacs

;; ==============================================================================
;; Emacsql Setup
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2023-06-04 2147
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This is recommended by the developers of org-roam for folks using versions of
;; Emacs older than 29 as the database connector for org-roam to use.

;;; Code:
(use-package emacsql
  :straight t
  :demand t)

(use-package emacsql-sqlite
  :straight t
  :demand t)

(straight-use-package
 '(emacsql-sqlite-module :source melpa)
 :demand t)

(straight-use-package
 '(emacsql-sqlite-builtin)
 :demand t)

(provide 'init_emacsql)

;;; init_emacsql-sqlite-module.el ends here
