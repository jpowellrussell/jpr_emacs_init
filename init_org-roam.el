;;; init_org-roam.el --- Org-Based Personal Wiki and Mind Map

;; ==============================================================================
;; Org-Roam Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-08-15 1324
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This package emulates the web-based software Roam Research in Org. Key
;; attractive features include bidirectional linking and ease of creating new,
;; linked notes.

;;; Code:
(use-package org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam" :branch "v2")
  :ensure t
  :hook ;Loading at init slows down start up considerably, but it's important
        ;enough to me to be worth it
  (after-init . ivy-mode)
  :custom
  (org-roam-directory (file-truename "~/dropbox/khs/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
                                        ;("C-c n g" . org-roam-graph) ;graph not supported by v2 yet
         ("C-c n i" . org-roam-node-insert))
  :config
  ;; General Variables and Startup
  ;; ===========================================================================
  (org-roam-setup)
  (setq completion-ignore-case t)
  (setq org-roam-complete-everywhere nil)
;;  (setq org-roam-link-auto-replace nil)
;;  (setq org-roam-buffer-position 'right)
;;  (setq org-roam-graph-viewer "")

  ;; Org-Roam Protocol Setup
  ;; ===========================================================================
  (require 'org-roam-protocol)

  ;; Custom Functions
  ;; ===========================================================================

  ;; The two below come from nobiot on the org-roam forum and are a way to make
  ;; sure that no org-ids get "lost", such as from renaming a file or the like
  (defun my-org-id-update-org-roam-files ()
    "Update Org-ID locations for all Org-roam files."
    (interactive)
    (org-id-update-id-locations (org-roam--list-all-files)))

  (defun my-org-id-update-id-current-file ()
    "Scan the current buffer for Org-ID locations and update them."
    (interactive)
    (org-id-update-id-locations (list (buffer-file-name (current-buffer)))))

  ;; Capture Setup
  ;; ===========================================================================

  ;; This function generates the name for a new slip in my slipbox to be used in
  ;; the org-roam-capture template - This function is not necessary with "read-dir"
  ;; (defun jpr-org-roam-slip-name ()
  ;;   "Make a name for a new slip, including which slipbox it goes in, for org-roam-capture."
    ;; (concat "Slipbox/" (jpr-slipbox-select) "/" (jpr-pick-slip-id)))

  ;; Define some variables to use in capture templates, like directories
  (setq org-roam-slipbox-dir "Slipbox")

  ;; Define capture templates for org-roam (based on org-capture, but with some
  ;; customization based on org-roam functions)
  (setq org-roam-capture-templates
        `(
;;           ("b" "Book Slip" plain
;;            "*Link to Book*:
;; *Writers*:
;; *Leaves*:
;; *Looking Up*:
;; [[file:../Roam_Notes/Tags/20210311132041-books.org][Books]]

;; ** Times Read
;; mm/dd/yy - mm/dd/yy

;; ** Holding-On Askings
;; *What was the core thesis of this book?*
;; *How does this idea change my thinking about other ideas?*
;; *How does knowing this influence my practical abilities?*

;; ** Kind of Book

;; ** Thoughts after Reading
;; :PROPERTIES:
;; :VISIBILITY: all
;; :END:

;; ** Key Words and Thoughts

;; ** Things to Ankify

;; ** Outline
;; :PROPERTIES:
;; :VISIBILITY: all
;; :END:

;; *** Chapter Name
;; /One to Two Sentence Rundown of Chapter./

;; ** Thorough Rundown

;; ** Worthwhile Askings

;; ** Sayings

;; ** Folks and Works Named

;; ** Thoughts
;; :PROPERTIES:
;; :VISIBILITY: all
;; :END:

;; "
;;            :if-new (file+head "Slipbox/8-Books/${slug}.org"
;;                               "#+title: ${title}\n\n#+PROPERTY: DATE %<%Y-%m-%d %H%M>\n#+STARTUP: overview\n\n")
;;            :unnarrowed t)
          ("b" "Blog Post" entry "* ${title}\n:PROPERTIES:\n:HTML_HEADLINE_CLASS: Post\n:CUSTOM_ID: ${slug}\n:END:\n*Date*: %<%Y-%B-%d>\n\n%?\n\n/Did this post spark any thoughts? Have anything to ask or share? Feel free to send me an email at jeff DOT powell DOT russell AT gmail.com, and I'll add your thoughts below. You can also comment on the [][dreamwidth post]./\n\n
-----\n\n"
           :if-new (file+head "Undertakings/org_webstead/index.org"
                              "#+title: Jeff's Webstead\n\n")
           :prepend t
           :unnarrowed t)
          ("d" "Default" plain "%?"
           :if-new (file+head "Roam_Notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n\n")
          :unnarrowed t)
;;           ("e" "Engagement Report" plain
;;            :if-new (file+head "%(concat org-roam-directory \"/Engagement_Reports/%<%Y-%m-%d>-${slug}.org\")" "#+title: %<%Y-%m-%d>-${title}\n#+roam_alias: \n\n")
;;           "[[id:1EE2E69E-14DB-4835-8C81-5C9660A39CDE][Weekly Engagement Reports]]
;; - %?"
;;           :unnarrowed t)
;;           ("f" "Fleeting Thought" plain "%?"
;;            :if-new (file+head "%(concat org-roam-slipbox-dir \"/0-Fleeting_Thoughts/%<%Y%m%d%H%M%S>-${slug}.org\")"
;;                               "#+title: ${title}\n\n")
          ;; :unnarrowed t)
          ("p" "Person" plain
          "*Name*: ${title}\n*About*: %?"
          :if-new (file+head "Roam_Notes/Folks/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+roam_alias: \n\n")
          :unnarrowed t)
;;           ;; with new file-level property drawers, I like need to change the template below to put things like Slip_ID in the property drawer rather than as '#+PROPERTY'
;;           ("s" "Slip" plain
;;            "** Body
;;    :PROPERTIES:
;;    :VISIBILITY: all
;;    :END:
;; %i%?

;; ** Where From
;;    :PROPERTIES:
;;    :VISIBILITY: folded
;;    :END:

;; ** Links
;;    :PROPERTIES:
;;    :VISIBILITY: folded
;;    :END:

;; ** Tags
;;    :PROPERTIES:
;;    :VISIBILITY: folded
;;    :END:

;; "
;;            :if-new (file+head "%(read-directory-name \"path: \" (concat org-roam-directory org-roam-slipbox-dir))${slug}.org"
;;                               "#+title: ${title}\n\n#+PROPERTY: Slip_ID %(and (string-match \"^[a-z][a-z0-9]+\" \"${title}\")
;; (match-string 0 \"${title}\"))\n#+PROPERTY: Firstness 50\n#+PROPERTY: DATE %<%Y-%m-%d %H%M>\n#+STARTUP: overview\n\n")
;;            ;"%(jpr-org-roam-slip-name)-${slug}"; the way you put functions into this keyword is to put a '%' before the opening parenthesis, enclosed within a string
;;            :unnarrowed t)
;;           ("t" "Tag" plain
;;           "This is a slip to hold backlinks that have to do with ${title}. %?"
;;           :if-new (file+head "Roam_Notes/Tags/%<%Y%m%d%H%M%S>-${slug}.org"
;;                              "#+title: ${title}\n#+roam_alias: \n\n")
;;           :unnarrowed t)
          ("u" "Undertaking" plain "%?"
           :if-new (file+head "Undertakings/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n\n")
          :unnarrowed t)
          )
        )
  )
;; Org-Roam-Server Setup
;; ===========================================================================
;; Commented out for now, as v2 of org-roam does not yet work with server

;; (use-package org-roam-server
;;   :ensure t
;;   :straight t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20
;;         org-roam-server-network-vis-options nil ;This option stops the nodes from "dancing", which can be distracting and make it hard to click on nodes (json-encode (list (cons 'physics (list (cons 'enabled json-false)))))
;;         ))

(provide 'init_org-roam)

;;; init_org-roam.el ends here
