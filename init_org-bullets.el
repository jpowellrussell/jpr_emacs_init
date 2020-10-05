;;; init_org-bullets.el --- Display org headlines with pretty bullets

;; ==============================================================================
;; Org Bullets Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-06-09 2008
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Everyone seems to love these bullets, I'm gonna give them more of a try now

;;; Code:
(use-package org-bullets
  :straight t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '(;;; Large
          "◉"
          "⚬"
          "✸"
          "✿"
          ;; ❂ ♥◉ ●◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
          ;;; Small
          ;; ► • ★ ▸
    )))

(provide 'init_org-bullets)

;;; init_org-bullets.el ends here
