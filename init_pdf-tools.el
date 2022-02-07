;;; init_pdf-tools.el --- Set up fancy PDF Reader

;; ==============================================================================
;; PDF Tools Set Up
;; ==============================================================================
;; Written by Jeff Russell
;; Updated: 2020-10-05 1314
;; jpowellrussell.com
;; ------------------------------------------------------------------------------

;;; Commentary:
;; Installed epdfinfo through brew tap dunn/emacs followed
;; by brew install pdf-tools From instructions found on stack
;; exchange, posted by 'Joe' here:
;; https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
;; To upgrade epdfinfo server, do 'brew upgrade pdf-tools' before
;; updating pacakge in Emacs. If something goes wrong, uninstall with
;; 'brew uninstall pdf-tools' and uninstall Emacs pacakge, then
;; reinstall both

;;; Code:
(use-package pdf-tools
  :straight t
  :defer t
  :init (pdf-loader-install)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))

;; The below code is meant to allow me to select text with the keyboard using
;; avy, but I haven't actually gone to the effort to understand it yet.
(defcustom pdf-links-convert-pointsize-scale 0.02
  "The scale factor for the -pointsize convert command.
This determines the relative size of the font, when interactively
reading links."
  :group 'pdf-links
  :type '(restricted-sexp :match-alternatives
                          ((lambda (x) (and (numberp x)
                                            (<= x 1)
                                            (>= x 0))))))

(defun pdf-links-read-char-action (query prompt)
  "Using PROMPT, interactively read a link-action.
BORROWED FROM `pdf-links-read-link-action'.
See `pdf-links-action-perform' for the interface."
  (pdf-util-assert-pdf-window)
  (let* ((links (pdf-info-search-string
                 query
                 (pdf-view-current-page)
                 (current-buffer)))
         (keys (pdf-links-read-link-action--create-keys
                (length links)))
         (key-strings (mapcar (apply-partially 'apply 'string)
                              keys))
         (alist (cl-mapcar 'cons keys links))
         (size (pdf-view-image-size))
         (colors (pdf-util-face-colors
                  'pdf-links-read-link pdf-view-dark-minor-mode))
         (args (list
                :foreground (car colors)
                :background "blue"
                :formats
                 `((?c . ,(lambda (_edges) (pop key-strings)))
                   (?P . ,(number-to-string
                           (max 1 (* (cdr size)
                                     pdf-links-convert-pointsize-scale)))))
                 :commands pdf-links-read-link-convert-commands
                 :apply (pdf-util-scale-relative-to-pixel
                         (mapcar (lambda (l) (car (cdr (assq 'edges l))))
                                 links)))))
    (print colors)

    (unless links
      (error "No links on this page"))
    (unwind-protect
        (let ((image-data nil))
          (unless image-data
            (setq image-data (apply 'pdf-util-convert-page args ))
            (pdf-cache-put-image
             (pdf-view-current-page)
             (car size) image-data 'pdf-links-read-link-action))
          (pdf-view-display-image
           (create-image image-data (pdf-view-image-type) t))
          (pdf-links-read-link-action--read-chars prompt alist))
      (pdf-view-redisplay))))

(defun avy-timed-input ()
  "BORROWED FORM `avy--read-candidates'"
  (let ((str "")
        char break)
    (while (and (not break)
                (setq char
                      (read-char (format "char%s (prefer multiple chars w.r.t. speed): "
                                         (if (string= str "")
                                             str
                                           (format " (%s)" str)))
                                 t
                                 (and (not (string= str ""))
                                      avy-timeout-seconds))))
      ;; Unhighlight
      (cond
       ;; Handle RET
       ((= char 13)
        (if avy-enter-times-out
            (setq break t)
          (setq str (concat str (list ?\n)))))
       ;; Handle C-h, DEL
       ((memq char avy-del-last-char-by)
        (let ((l (length str)))
          (when (>= l 1)
            (setq str (substring str 0 (1- l))))))
       ;; Handle ESC
       ((= char 27)
        (keyboard-quit))
       (t
        (setq str (concat str (list char))))))
    (print str)))

(defun get-coordinates (end)
  (let* ((query (avy-timed-input))
         (coords (list (or (pdf-links-read-char-action query "Please specify (SPC scrolls): ")
                           (error "No char selected")))))
    ;; (print coords)
  ;; (print (car (alist-get 'edges (car coords))))))
    (car (alist-get 'edges (car coords)))))

(defun pdf-keyboard-highlight ()
  (interactive)
  (let* ((start (get-coordinates nil))
         (end (get-coordinates t))
         (edges (append (cl-subseq start 0 2) (cl-subseq end 2 4))))
    (pdf-annot-add-markup-annotation
     edges 'highlight '"yellow") nil))

(provide 'init_pdf-tools)

;;; init_pdf-tools.el ends here
