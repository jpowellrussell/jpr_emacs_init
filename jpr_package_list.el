;;; jpr_package_init --- personal package initialization
;;; Commentary:
;; =============================================================================
;; Jeff P. Russell's Emacs Packages
;; =============================================================================
;; Written by Jeff Russell
;; Updated 2020-04-13
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Code:
;; =============================================================================
;; Xah's Relative Path Function
;; =============================================================================
;; Code for Xah on how to do relative directories for other init files
;; Let's you easily split your config into multiple files.  Note that I've
;; greatly reduced the docstring here to reduce screen real estate.  See the full
;; thing at the URL below
;; Found here: http://ergoemacs.org/emacs/organize_your_dot_emacs.html

(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of @FILE-RELATIVE-PATH.
Written by Xah, found at http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat(file-name-directory (or load-file-name buffer-file-name))
	 @file-relative-path)
  )


;; =============================================================================
;; Packages to Load
;; =============================================================================

;; Load Org before other packages that require it due to some weirdness in how
;; it's built and the hack that Radon Rosborough has built into straight.el to
;; cope
(load (xah-get-fullpath "init_org.el"))

;; List of packages to initialize
;; -----------------------------------------------------------------------------
;; Mostly in alphabetical order, but some are re-ordered due to dependencies

(defvar jpr-pkg-init-list '("init_exec-path-from-shell.el" ; for dependencies
                            "init_magit.el" ; for dependencies
                            "init_zenburn-theme.el" ; other packages rely on
                            ;;                         ; default colors from this.
                            ;;                         ; Also so I don't have to
                            ;;                         ; look at the default theme
                            ;;                         ; when a package doesn't
                            ;;                         ; load.
                            "init_anki-editor.el"
                            "init_avy.el"
                            "init_awesome-tab.el"
                            "init_beacon.el"
			    "init_better-defaults.el"
			    "init_buffer-flip.el"
                            ;"init_company.el" ; disabled until I can work out a finer-grained initialization and/or backend, as right now it pops up on normal writing way more than I want
                            "init_dash.el"
			    ;; "init_deft.el"
                            "init_dired.el"
			    "init_elfeed.el"
			    "init_flycheck.el"
                            "init_flyspell.el"
                            "init_font-lock+.el"
                            "init_fringe-current-line.el"
			    ;"init_hc-zenburn-theme.el"
			    "init_highlight-symbol.el"
                            "init_hydra.el"
                            "init_ivy.el"
                            "init_jedi.el"
                            ;"init_kaolin-themes.el"
			    "init_markdown-mode.el"
			    "init_markdown-mode+.el"
			    "init_markdownfmt.el"
			    "init_mu4e.el"
                            "init_notdeft.el"
                            "init_numerologizer.el"
			    "init_nov.el"
			    "init_outlook.el"
			    ;"init_snails.el"
			    ;"init_org-brain.el"
			    "init_org-bullets.el"; turned off to test org visibility issue
			    ;"init_org-drill.el"
                            "init_org-fc.el"; turned off to test org visibility issue
			    ;"init_org-edna.el"
                            "init_org-journal.el"; turned off to test org visibility issue
                            "init_org-make-toc.el"; turned off to test org visibility issue
			    "init_org-noter.el"; turned off to test org visibility issue
			    ;"init_org-page.el"
                            ;"init_org-protocol.el"
			    ;"init_org-protocol-capture-html.el"
                            ;"init_org-ref.el"
                            "init_org-roam.el"
                            "init_org-web-tools.el"; turned off to test org visibility issue
                            "init_ov.el"
                            "init_ov-highlight.el"
                            "init_pandoc-mode.el"
			    "init_pdf-tools.el"
                            "init_pdf-tools-org.el"
			    "init_pdf-view-restore.el"
                            "init_pyvenv.el"
			    "init_rainbow-delimiters.el"
                            "init_s.el"
                            "init_slime.el"
                            "init_smart-mode-line.el"
                            "init_smex.el"
			    ;"init_toc-org.el"
			    "init_treemacs.el"
			    "init_winum.el"
			    "init_yasnippet.el"
                            "init_elpy.el" ; placed at end due to dependencies - slight slow down from this one, but not much
		           )
      )


;; Iterate through list to initialize each package
;; -----------------------------------------------------------------------------

;; Define a function to load each package in a list of packages
(defun jpr-load-list (list)
  "Run Xah's load full path for each relative path given in LIST."
    (dolist (element list)
      (load (xah-get-fullpath element))))

;; ;; Use a dolist macro to run load on each package from the list above
(jpr-load-list jpr-pkg-init-list)

(provide 'jpr_package_list)
;;; jpr_package_list.el ends here
