;;; init_flyspell.el --- Flyspell spellchecker

;; ==============================================================================
;; Flyspell Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2022-02-12 2120
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Set up the package flyspell to do automatic spellcheck

;;; Code:
(use-package flyspell
  :defer t
  :straight t
  :config
  ;; Configure spellchecker using GNU Aspell, which apparently has good support
  ;; for personal libraries, which will be helpful when I work out my etymology
  ;; checker tool
  (setq ispell-program-name "aspell"
        ispell-default-dictionary "en_US")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :after flyspell
  :straight t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  :config (eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :straight t)

(provide 'init_flyspell)

;;; init_flyspell.el ends here
