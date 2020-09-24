;; =============================================================================
;; Configuration for Emacs initialization
;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-13
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;; Method to organize init files based on Xah's here:
;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html Init file points
;; here, these are the settings that I pretty much want on to make Emacs usable
;; that do not rely on a package Packages are configured in their own init
;; files, called from a package-list init file. Is this sloppy? I dunno, I guess
;; I'll learn.


;; =============================================================================
;; Xah's Relative Path Function
;; =============================================================================
;; Code for Xah on how to do relative directories for other init files
;; Let's you easily split your config into multiple files. Note that I've
;; greatly reduced the docstring here to reduce screen real estate. See the full
;; thing at the URL below
;; Found here: http://ergoemacs.org/emacs/organize_your_dot_emacs.html

(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's
  file location.
  Written by Xah, found at http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat(file-name-directory (or load-file-name buffer-file-name))
	 @file-relative-path)
)


;; =============================================================================
;; Package Management Set Up
;; =============================================================================

;; After flailing around with el-get and not getting it to work right, I've
;; decided to go with straight.el for now. It might confuse the shit out of me
;; with its dependence on Git, but maybe that will make me learn Git.
;; Straight.el can be foudn here: https://github.com/raxod502/straight.el

;; Straight.el bootstrap code to install straight.el if not already installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package installation and set up
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)


;; =============================================================================
;; Load Other Init Files
;; =============================================================================

;; Method to organize init files based on Xah's here:
;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html Init file points
;; here, these are the settings that I pretty much want on to make Emacs usable
;; that do not rely on a package Packages are configured in their own init
;; files, called from a package-list init file. Is this sloppy? I dunno, I guess
;; I'll learn.

;; Load personally preferred settings that do not depend on packages
(load (xah-get-fullpath "jpr_emacs_settings.el"))

;; Load packages from specific configuration files
(load (xah-get-fullpath "jpr_package_list.el"))
