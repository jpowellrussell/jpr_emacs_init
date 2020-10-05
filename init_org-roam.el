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
  :straight t
  :ensure t
  :hook ;Loading at init slows down start up considerably, but it's important
        ;enough to me to be worth it
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/dropbox/khs/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
               :map org-mode-map
               (("C-c n i" . org-roam-insert))
               (("C-c n I" . org-roam-insert-immediate)))
  :config
  ;; Function to look for previously made tags in the format "#tag" and add them
  ;; as roam tags. I don't know if it works and I'm a bit worried to test it,
  ;; since I didn't really understand the code I mostly copied from org-roam.el,
  ;; from the function org-roam--extract-tags-prop

  ;; (defun org-roam--extract-tags-hashtags (_file)
  ;;     "Extract '#tags' from files as org-roam tags"
  ;;   (let* ((hashtags (cdr (assoc "#" (org-roam--extract-global-props '("#"))))))
  ;;     (condition-case nil
  ;;         (org-roam--str-to-list hashtags)
  ;;       (error
  ;;        (progn
  ;;          (lwarn '(org-roam) :error
  ;;                 "Failed to parse tags for buffer: %s. Skipping"
  ;;                 (or org-roam-file-name
  ;;                     (buffer-file-name)))
  ;;          nil)))))
  ;;  (setq org-roam-tag-sources '(prop last-directory hashtags))

  (setq org-roam-buffer-position 'right)
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain (function org-roam--capture-get-point)
          "%?"
          :file-name "Roam_Notes/%<%Y%m%d%H%M%S>-${slug}"
          :head "#+title: ${title}\n#+roam_alias: \n#+roam_tags: \n\n"
          :unnarrowed t)
          )
        )
  )

(provide 'init_org-roam)

;;; init_org-roam.el ends here
