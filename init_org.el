;;; init_org.el --- Org Mode Set Up

;; =============================================================================
;; JPR's Org Mode Configuration
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-19 1535
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Setting up Org Mode, which lots of stuff has as a dependency, most exciting
;; of which to me is Org-Drill, which does spaced repetition, and which I may
;; use as the basis for building an incremental reading tool.  And by building,
;; right now I mean cobbling together packages and personal workflow, though
;; maybe one day I'll try to build my own thing.

;;; Code:
(use-package org
  :straight t
  :defer t
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :hook (org-mode . flyspell-mode)
        ;(org-mode . variable-pitch-mode)
        (org-mode . (lambda ()
                      (setq-local yas-trigger-key [tab])
                      (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))
  :bind
  (
      ("C-c 1" . org-store-link)
      ("C-c a" . org-agenda)
      ("C-c c" . org-capture)
      ("S-RET" . org-tree-open-in-right-frame)
      ("C-c p b" . jpr-webstead-publish))
  :config

  ;; Libraries to Require
  ;; ----------------------------------------
  ;; Enable Org Contrib (extra packages shipped with Org, but not part of default
  ;; build)
  (use-package org-contrib)
  
  ;; Enable Org-Protocol for interaction with other programs - I'm still sorting
  ;; out the kinks here
  (require 'org-protocol)

  ;; Enable org-tempo to allow some org-native snippets like structure templates
  (require 'org-tempo)

  ;; Enable Org-Element for parsing org files
  (require 'org-element)

  ;; Enable org-publishing
  (require 'ox-publish)

  ;; Enable RSS backend for org-publish
  (use-package ox-rss)

  ;; Enable ox-extra, specifically for ability to export a section without its
  ;; headline
  (use-package ox-extra
    :config
    (ox-extras-activate '(ignore-headlines)))

  ;; Adjust settings so that markup works over multiple lines
  (setcar (nthcdr 4 org-emphasis-regexp-components) 100)
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)

  ;; Appearance Customizations
  ;; ----------------------------------------
  ;; Fontify the whole line for headings, with a background color - last I
  ;; checked, this wasn't showing up as anything visible for me. I was worried
  ;; it was messing up my reading highlights, but even with it off, they are
  ;; disappearing when I reopen the file. The custom variables to store the
  ;; highlighting data might have gotten corrupted.
  (setq org-fontify-whole-heading-line t)

  ;; Hide the markup characters, such as for italic and bold.
  (setq org-hide-emphasis-markers t)

  ;; Set the headlines to have different sizes and look. Currently, the
  ;; headlines are black after I start up emacs, but evaluating this buffer one
  ;; more time fixes that. Ah, I wonder if it's because I'm using
  ;; "face-foreground 'default" before my theme has loaded. Maybe I need to move
  ;; my theme near the beginning?

  (let*   ((base-font-color (face-foreground 'default nil 'default))
           (headline `(:headline default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline :font "ETBEMBO"))))
     `(org-level-7 ((t (,@headline :font "ETBEMBO"))))
     `(org-level-6 ((t (,@headline :font "ETBEMBO"))))
     `(org-level-5 ((t (,@headline :font "ETBEMBO"))))
     `(org-level-4 ((t (,@headline :font "ETBEMBO" :height 1.1))))
     `(org-level-3 ((t (,@headline :font "ETBEMBO" :height 1.25))))
     `(org-level-2 ((t (,@headline :font "ETBEMBO" :height 1.5))))
     `(org-level-1 ((t (,@headline :font "ETBEMBO" :height 1.75))))
     `(org-document-title ((t (,@headline :font "ETBEMBO" :height 2.0 :underline nil))))
     )
   )

  ;; Use a tree buffer and a view buffer, similar to programs like Scrivener,
  ;; Devonthink, and Evernote. From Vim Valley here:
  ;; https://vimvalley.com/replacing-scrivener-with-emacs-and-vim/
  (defun org-tree-open-in-right-frame ()
    "Opens current level of Org tree in an indirect buffer."
    (interactive)
    (org-tree-to-indirect-buffer)
    (windmove-right))

  ;; Enable Letters in Plain Lists (such as "a. b. c.")
  (setq org-list-allow-alphabetical t)

  ;; Customize Plain List Bullet Demotion Scheme
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "*") ("*" . "-")))

  ;; Indent content with bullets
  (setq org-startup-indented t)

  ;; By setting the default to "folded", files respect my by-headline visibility
  ;; properties
  ;; (setq org-startup-folded "fold")

  ;; Navigation Customizations
  (setq org-return-follows-link t)

  ;; Todo Customizations
  ;; ----------------------------------------
  ;; (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING"
  ;;                                             "DONE")))

  ;; Org-Publish Customizations
  ;; ----------------------------------------
  ;; Setting up org-publish for publishing org files to other formats. My
  ;; initial use is to write my webstead in org and publish it as html to the
  ;; folder I sync with my github for posting it online.

  ;; The Publish Project Alist
  (setq org-publish-project-alist
        '(
          ("webstead-notes"
           :base-directory "~/dropbox/khs/undertakings/org_webstead"
           :base-extension "org"
           :publishing-directory "~/git/webstead/public"
           :recursive t
           :with-tags nil
           :publishing-function org-html-publish-to-html
           :headline-levels 6)
          ("webstead-static"
           :base-directory "~/dropbox/khs/undertakings/org_webstead"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
           :publishing-directory "~/git/webstead/public"
           :recursive t
           :publishing-function org-publish-attachment)
          ("webstead-rss"
           :base-directory "~/dropbox/khs/undertakings/org_webstead"
           :base-extension "org"
           :html-link-home "https://jpowellrussell.com/"
           :html-link-use-abs-url t
           :rss-extension "xml"
           :publishing-directory "~/git/webstead/public"
           :publishing-function (org-rss-publish-to-rss)
           :section-numbers nil
           :headline-levels 1
           :select-tags ("rss")
           :exclude-tags ("norss")
           :exclude ".*" ;to exclude all files
           :include ("index.org") ; ... except index
           :table-of-contents nil)
          ("webstead" :components ("webstead-notes" "webstead-static" "webstead-rss"))))

  ;; Some Variables for HTML Export that I want as default, so that I can
  ;; specify locally more specific things - I find the default settings way too
  ;; messy

  (setq org-html-head ""
        org-html-head-extra ""
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-preamble nil
        org-html-postamble nil
        org-html-use-infojs nil)

  ;; Custom Org-RSS Set Up
  ;; Define a custom version of some of the org-rss functions, I think, maybe
  ;; via advice rather than re-writing? The goal here is to avoid some of the
  ;; issues that seem hard-coded into its functions, like not being in the
  ;; location of the git repository after finishing RSS export, spending forever
  ;; indenting the file, and not recognizing the level of headlines where I put
  ;; post titles for generating the pubdate property, which makes them not get
  ;; exported to RSS. I've currently fixed that with an update to my capture
  ;; template, but I dunno if that's the best way to do it or not. Oh, and this
  ;; might need to go before the publishing project stuff above, not sure.

  ;; Custom Webstead Publish Command
  ;; This combines org-publish and staging, committing, and pushing to git

  (defun jpr-webstead-publish ()
  "One-step function to publish org-webstead to html and git to github."
  (interactive)
  (let ((default-directory "/Users/Jeff/git/webstead/public/")
        (webstead-file "/Users/Jeff/git/webstead/public/index.html")
        (webstead-rss "/Users/Jeff/git/webstead/public/index.xml"))
    (org-publish-project "webstead")
    (magit-stage-modified)
    (magit-call-git "add" webstead-file)
    (magit-call-git "commit" "-m" (concat "Webstead update " (format-time-string "%Y-%m-%d %H:%M")))
    (magit-call-git "add" webstead-rss)
    (magit-call-git "commit" "-m" (concat "Webstead RSS update " (format-time-string "%Y-%m-%d %H:%M")))
    (magit-call-git "push" "origin")
    (magit-refresh)
    (message "Update complete")
    ))

  ;; Org Capture Customizations
  ;; ----------------------------------------
  ;; I am still learning how to build capture templates and use capture
  ;; properly, but I think it's going to be helpful.

  ;; Define some variables to use in functions so that they are more robust if I
  ;; change my set up (especially file locations or formats or the like). I got
  ;; the idea for this from webbj74 here:
  ;; https://gist.github.com/webbj74/0ab881ed0ce61153a82e
  ;; but really it's just good programming practice.

  ;; Top level directory for Knowledge Handling Setup.
  (defconst *my-khs-dir* "~/dropbox/khs/")

  ;; Top level Slipbox directory for reading/thinking/writing notes
  (defconst *my-slipbox-dir* "~/dropbox/khs/slipbox/")

  ;; Fleeting thoughts directory and file for slipbox
  (defconst *my-fleeting-thoughts* "0-Fleeting_Thoughts/fleeting_thoughts.org")

  ;; Directory for incremental reading process files, like excerpts
  (defconst *my-increading-dir* "~/dropbox/khs/increading/")

  ;; File for increading notes
  (defconst *my-increading-file* "reading_notes.org")

  ;; Default directory for org files without any other home, including "Forethought"
  (defconst *my-org-dir* "~/dropbox/org")

  ;; Define Journal Files Directory
  (defconst *my-journal-dir* "daily_leaves/")

  ;; Define the title for today's journal file
  (defconst *todays-journal-file* "%Y-%m-%d.org")

  ;; Headline format within my Forethought file for daily TODOs, TODO capture
  ;; looks for this and inserts an individual TODO under it
  (defconst *my-todo-headline-format* "Daily Forethought")

  ;; This function should allow me to choose a slipbox name to use when I capture
  (defun jpr-slipbox-select ()
    "Select which slipbox in which to file a slip created with org-roam-capture."
    (interactive)
    (let ((choices '("0-Fleeting_Thoughts" "1-Main" "2-Fellhold" "3-Runes" "4-Slush" "5-Games" "6-Dreams" "7-Story_Toolbox")))
      (let ((slipbox (completing-read "Slipbox Name: " choices)))
        (format "%s" slipbox))))

  ;; This function should allow me to write out a Slip ID to incorporate into the filename
  (defun jpr-pick-slip-id ()
    "Write out a slip ID for a newly created slip."
    (interactive)
    (let ((slip-id (read-string "Slip ID: ")))
      (format "%s" slip-id)))

  ;; Function for user to enter a new file name for a capture template.
  (defun jpr-capture-name ()
    "Enter a name for a newly created file using an org-capture template."
    (interactive)
    (let ((file-name (read-string "File name: ")))
      (format "%s.org" file-name)))

  ;; Function to generate file name for "TODO" org-capture template. I created
  ;; this and the next function when I realized that embedding these functions
  ;; inside the template was getting them evaluated at compile time, rather than
  ;; at capture time. I might have been able to fix this with better
  ;; quoting/back-quoting, but this seemed like a cleaner way overall anyhow.

  (defun jpr-todo-file-name ()
    "Make a filename for my 'TODO' org-capture template."
    (concat *my-khs-dir* *my-journal-dir* (format-time-string *todays-journal-file*)))

  ;; Function to generate file name for "Slip" org-capture template
  (defun jpr-slip-file-name ()
    "Make a filename for my 'Slip' org-capture template."
    (concat *my-slipbox-dir* (jpr-slipbox-select) "/" (jpr-pick-slip-id) "-" (jpr-capture-name)))

  (setq org-capture-templates
        `(
          ;; The two below will likely get built out as org-roam capture
          ;; templates. I've left them here as comments to give me a starting
          ;; place.
          ;; ("e" "Excerpt"
          ;;  entry
          ;;  (file+headline (concat *my-increading-dir* *my-increading-file*) "Inbox")
          ;;  "* %f\n:PROPERTIES:\n:DATE_ADDED: %U\n:SOURCE: %F\n:END:\n%i\n%?"
          ;;  )
          ;; ("f" "Fleeting Thought"
          ;;  entry
          ;;  (file ,(concat *my-slipbox-dir* *my-fleeting-thoughts*))
          ;;  "* %U - \n:PROPERTIES:\n:SOURCE: %F\n:FIRSTNESS: 50\n:END:\n%?"
          ;;  )
          ("t" "TODO"
           entry
           (file+headline jpr-todo-file-name "Daily Forethought")
           "* TODO %?\n%a\n"
           )
;;           ("s" "Slip" plain (file jpr-slip-file-name) "** Body
;;    :PROPERTIES:
;;    :VISIBILITY: all
;;    :END:
;;    %i%?

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

;; ")
          ))

  ;; Tag Customizations
  ;; ----------------------------------------
  ;; Set up custom tags to added easily with the "insert tags" command
;;  (setq org-tag-alist '(("drill" . ?d)))

  ;; Below is sample code for defining global tags, including mutually exclusive tags
  ;; between the dummy tags "startgroup" and "endgroup". Disabled until I figure
  ;; out if I want to do more with tagging. Note that this is currently
  ;; redundant with the above code, so pick one or the other, not both.
  ;; (setq org-tag-alist '((:startgroup . nil)
  ;;                       ("@work" . ?w) ("@home" . ?h)
  ;;                       ("@tennisclub" . ?t)
  ;;                       (:endgroup . nil)
  ;;                       ("laptop" . ?l) ("pc")))
  )

(provide 'init_org.el)
;;; init_org.el ends here
