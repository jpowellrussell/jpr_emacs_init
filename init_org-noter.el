;;; init_org-noter.el --- Set up Org interleaved annotation tool

;; ==============================================================================
;; Org-Noter Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-28 1542
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Allows for "interleaved" note-taking on PDFs, epubs, and other docs in a
;; linked org file.  I'm not sure whether this will become a part of my
;; incremental reading workflow or not, but I think at a minimum it will show me
;; some options on how to approach it.

;;; Code:
(use-package org-noter
  :straight t
  :defer t
  :config
  (setq-default org-noter-auto-save-last-location t)
  (setq org-noter-notes-search-path '("~/dropbox/khs/reading_thoughts"
                                      "~/dropbox/khs/increading"
                                      "~/dropbox/khs/increading/creativity"
                                      "~/dropbox/khs/increading/economics"
                                      "~/dropbox/khs/increading/history"
                                      "~/dropbox/khs/increading/learning"
                                      "~/dropbox/khs/increading/military"
                                      "~/dropbox/khs/increading/myth_and_religion"
                                      "~/dropbox/khs/increading/philosophy"
                                      "~/dropbox/khs/increading/politics"
                                      "~/dropbox/khs/increading/programming"
                                      "~/dropbox/khs/increading/psychology"
                                      "~/dropbox/khs/increading/science"
                                      "~/dropbox/khs/increading/technology"))
  )

(provide 'init_org-noter)

;;; init_org-noter.el ends here
