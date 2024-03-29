;;; init_slime.el --- Configure Slime, an interactive common lisp minor mode

;; ==============================================================================
;; Slime Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-04 1739
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; This is recommended by the SBCL website, so let's see how it goes.

;;; Code:
(use-package slime
  :ensure t
  :straight t
  :defer t
  :init
  (defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)

  (defun slime-repl-font-lock-setup ()
    "Sets up the slime repl for syntax highlighting.
Found here: http://compgroups.net/comp.emacs/tweaking-slime/95455"
    (setq font-lock-defaults
          '(slime-repl-font-lock-keywords
            ;; From lisp-mode.el
            nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
            (font-lock-syntactic-face-function
             . lisp-font-lock-syntactic-face-function))))

    (defadvice slime-repl-insert-prompt (after font-lock-face activate)
    (let ((inhibit-read-only t))
      (add-text-properties
       slime-repl-prompt-start-mark (point)
       '(font-lock-face
         slime-repl-prompt-face
         rear-nonsticky
         (slime-repl-prompt read-only font-lock-face intangible)))))

    (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'init_slime)

;;; init_slime.el ends here
