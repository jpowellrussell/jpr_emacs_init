;;; init_deft.el --- Deft package set up

;; =============================================================================
;; Deft Note Taking and Search Set Up
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-20 1626
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Setting up Deft, a package that works like NValt.
;; I've locally modified Deft to have separate definitons for a search directory
;; and a write directory.  I'm not entirely sure if I've broken anything, but so
;; far it seems to work as expected.  To work properly, deft-recursive must be
;; true, and the write directory must be a subdirectory of the search
;; directory.  As an example, for my own workflow, I have a directory for
;; different kinds of notes (general, gaming, and so forth), and one of those
;; directories is for "fleeting thoughts" - all notes on any topic that have not
;; yet been processed and organized.  It's helpful to search both fleeting and
;; processed notes, but I want all new notes to go to Fleeting thoughts.

;;; Code:
(use-package deft
  :bind ("C-c C-;" . deft)
  :config
  (setq deft-search-directory "~/dropbox/slipbox/" ; modified to have two dirs
        deft-write-directory "~/dropbox/slipbox/0-fleeting_thoughts"
        ;; Below is the default variable
        ;; deft-directory "~/dropbox/slipbox/"
	deft-recursive t
	deft-text-mode 'org-mode
	deft-default-extension "org"
	deft-extensions '("org" "md" "markdown" "txt" "text")
	;; Unfortunately, even in org mode, this doesn't quite work as desired -
        ;; it *does* set an unfound search string to the title of the note and
        ;; display that in the search, but it still saves the file name with its
        ;; default arbitrary string. Maybe I can modify the package some
	deft-use-filename-as-title t
        )
  )

(provide 'init_deft)
;;; init_deft.el ends here
