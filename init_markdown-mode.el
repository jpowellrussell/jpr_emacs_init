;;; init_markdown-mode.el --- Set up markdown mode

;; ==============================================================================
;; Markdown Mode Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1302
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; ;; Markdown Mode for editing my current favorite human-readable plain text
;; formatting

;;; Code:
(use-package markdown-mode
  :straight t
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(provide 'init_markdown-mode)

;;; init_markdown-mode.el ends here
