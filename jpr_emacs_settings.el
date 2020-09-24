;;; jpr_emacs_settings.el --- JPR's preferred non-package settings

;; =============================================================================
;; Jeff P. Russell's Emacs Settings
;; =============================================================================
;; Written by Jeff Russell
;; Updated: 2020-04-19 1551
;; jpowellrussell.com
;; -----------------------------------------------------------------------------

;;; Commentary:
;; Method to organize init files based on Xah's here:
;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html Init file points
;; here, these are the settings that I pretty much want on to make Emacs usable
;; that do not rely on a package Packages are configured in their own init
;; files, called from a package-list init file.  Is this sloppy? I dunno, I guess
;; I'll learn.

;;; Code:
;; =============================================================================
;; Personal Global Preferences
;; =============================================================================
;; Very likely I'll merge this section with the Emacs4developers Recommended
;; Configuration as I come to be more comfortable with them and decide whether I
;; want to keep them. So far, so good.

;; My preferred fixed-width font
(set-frame-font "Inconsolata-12")

;; My preferred variable-pitch font
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 140))))
 '(fixed-pitch ((t (:family "Inconsolata" :height 100))))
 )

;; Removes the GUI tool bar at the top of the window, because it's ugly, and why
;; use that in Emacs?
(tool-bar-mode -1)

;; Opens frame maximized (can still see dock and menu bar on MacOS)
(add-to-list 'default-frame-alist '(fullscreen .  maximized))

;; Automatically insert line breaks in white space before the 80 column mark
(setq-default fill-column 80)
(add-hook 'python-mode-hook (lambda () fill-column 79))

;; I've disabled auto-fill and gone to visual line mode instead, mostly for more
;; attractive variable-pitch editing, but also for easier copy/pasting into
;; variable pitch editors like Outlook and Word. I may revisit this, as I think
;; actually filled columns improve editor-agnostic human readability of plain
;; text files, which helps their longevity and portability.
;; (setq-default auto-fill-function 'do-auto-fill)

;; Removes the assumption that two spaces follow a period at end of sentence,
;; since that convention is now less common and deprecated by many
(setq-default sentence-end-double-space nil)

;; Not sure if the "when" is needed here, but maybe it's good practice
;; Show rows and columns in all modes
(when (version<= "26.0.50" emacs-version )
   (global-display-line-numbers-mode)
   (column-number-mode))

;; I removed the scroll bars to get more screen real estate
(scroll-bar-mode -1)

;; If you typing or pasting over a selection, deletes the highlighted text,
;; making it behave more as expected from previous text-editor experience
(delete-selection-mode t)

;; Use visual line mode in all modes except Treemacs
;; I changed this to take away the code excluding Treemacs, which apparently
;; handles this itself
(global-visual-line-mode 1)

;; Allows minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Configure spellchecker using GNU Aspell, which apparently has good support
;; for personal libraries, which will be helpful when I work out my etymology
;; checker tool
(defmacro WhenMacOS (&rest body)
  "Check if running on MacOS, if true, run BODY."
  `(if (eq system-type 'darwin)
       (progn ,@body)
     nil
     )
  )
(WhenMacOS
 (setq-default ispell-program-name "aspell")
 )

;; Save content copied outside of emacs to the kill ring
(setq save-interprogram-paste-before-kill t)

;; Run Emacs server so that running instance of GUI Emacs will listen for
;; commands from the command line pointed at emacs
(unless (and (boundp 'server-process)
             (memq (process-status server-process) '(connect listen open run)))
  (server-start))

;; Ensures that if a frame is already open, executing GUI Emacs from the command
;; line will not open a new window
(x-focus-frame nil)

;; Point automatically goes to where it was last time file was visited
(require 'saveplace)
(setq-default save-place t)

;; Store saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Put all back up files in one place, and some other helpful customizations to
;; backups, from Andreas Spindler here:
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Settings to make Emacs play nice with the OS
;; From Vim valley here:
;; https://vimvalley.com/replacing-scrivener-with-emacs-and-vim/
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects somethign in another program to paste it into Emacs, but
      ;; kills something in Emacs before actually pasting it, this selection is gone
      ;; unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t
 )

;; Retrieve authorization from MacOS Keychain
(eval-after-load 'auth-source
  '(when (member window-system '(mac ns))
      (add-to-list 'auth-sources 'macos-keychain-internet)
      (add-to-list 'auth-sources 'macos-keychain-generic)))
(setq auth-source-debug t
      auth-source-do-cache nil)

;; =============================================================================
;; Custom utility functions
;; =============================================================================
;; These are some fairly simple functions that don't warrant a package on their
;; own but help make some common tasks easier

;; Easy commenting and uncommenting, from Vim Valley
;; https://vimvalley.com/replacing-scrivener-with-emacs-and-vim/
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Delete leading whitespace on a line, from here:
;;https://www.emacswiki.org/emacs/DeletingWhitespace

(defun my-delete-leading-whitespace (start end)
          "Delete whitespace at the beginning of each line in region."
          (interactive "*r")
          (save-excursion
            (if (not (bolp)) (forward-line 1))
            (delete-whitespace-rectangle (point) end nil)))
(global-set-key (kbd "C-x C-h") 'my-delete-leading-whitespace)

;; Allows for "unfilling" paragraphs, useful for when I compose someething in
;; Emacs and move it to a visual linewrapping program like Word or Excel

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line. This
  is useful, e.g. for use with 'visual line mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Key Binding for Above
(define-key global-map (kbd "C-M-q") 'unfill-region)

;; =============================================================================
;; Emacs4developers recommended configuration
;; =============================================================================
;; Some basic settings, found at emacs4developers. I will likely re-arrange once
;; I understand them and decide whether I wnat to keep them.

;; Basic information about me
(setq user-full-name "Jeff Russell")
(setq user-mail-address "jeff.powell.russell@gmail.com")

;; Ask "y" or "n" instead of "yes" or "no" for laziness
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; Automatically generate matching paired characters like parentheses and braces
(electric-pair-mode)

;; No hard tabs
(setq-default indent-tabs-mode nil)

;; Show trailing white space in Programming Mode Only
(defun turn-on-trailing-whitespace ()
  "Turn on trailing whitespace when called."
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'turn-on-trailing-whitespace)

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-systems 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(provide 'jpr-emacs-settings)
;;; jpr_emacs_settings.el ends here
