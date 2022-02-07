;;; init_anki-editor.el --- Org-Based Editor for Anki Notes

;; ==============================================================================
;; Anki-Editor Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2021-07-02 1410
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This allows Anki notes to be made and edited in org-mode, then exported to
;; Anki.

;;; Code:
(use-package anki-editor
  :defer t
  :ensure t
  :straight t
  )

(provide 'init_anki-editor)

;;; init_anki-editor.el ends here
