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
      ("S-RET" . org-tree-open-in-right-frame))
  :config

  ;; Libraries to Require
  ;; ----------------------------------------
  ;; Enable Org-Protocol for interaction with other programs - I'm still sorting
  ;; out the kinks here
  (require 'org-protocol)

  ;; Enable org-tempo to allow some org-native snippets like structure templates
  (require 'org-tempo)

  ;; Enable Org-Element for parsing org files
  (require 'org-element)

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
