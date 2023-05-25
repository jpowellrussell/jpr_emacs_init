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
(set-frame-font "Inconsolata-14")

;; My preferred variable-pitch font
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 160))))
 '(fixed-pitch ((t (:family "Inconsolata" :height 100))))
 )

;; Removes the GUI tool bar at the top of the window, because it's ugly, and why
;; use that in Emacs?
(tool-bar-mode -1)

;; Opens frame maximized (can still see dock and menu bar on MacOS)
(add-to-list 'default-frame-alist '(fullscreen .  maximized))

;; Select Help Window after opening it (for easy closing, mostly)
(setq-default help-window-select t)

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

;;Turn of cl deprecation warning during startup.
(setq byte-compile-warnings '(cl-functions))

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
;; (global-set-key (kbd "C-;") 'toggle-comment-on-line)

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

;; Function to make selected region sentence case, which I am frankly surprised
;; is not a default Emacs commandw
;; By Xah, found here: http://ergoemacs.org/emacs/emacs_upcase_sentence.html
(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current text block or selection.

URL `http://ergoemacs.org/emacs/emacs_upcase_sentence.html'
Version 2020-11-30"
  (interactive)
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq $p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ((case-fold-search nil))
          ;; after period or question mark or exclamation
          (goto-char (point-min))
          (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
          ;; after a blank line, after a bullet, or beginning of buffer
          (goto-char (point-min))
          (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
          ;; for HTML. first letter after tag
          (goto-char (point-min))
          (while (re-search-forward "\\(<h[1-6]>[ \n]?\\|<p>[ \n]?\\|<li>\\|<dd>\\|<td>[ \n]?\\|<figcaption>[ \n]?\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
          (goto-char (point-min)))))))

;; Xah's function above assumes all lower case, whereas currently my use-case
;; (fixing some weird formatting from going from rich text to markdown to org,
;; includes lots of all caps I want to fix) is to work with sentences that may
;; or may not already be lower case, so I've written a simple function to
;; combine downcase-region and xah-upcase-sentence.
(defun jpr-sentence-case ()
  "Convert region to 'sentence case', even if curently uppercase.

Relies on xah-upcase-sentence, found here: http://ergoemacs.org/emacs/emacs_upcase_sentence.html"
  (interactive)
  (progn
    (let ($p1 $p2)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn
                (setq $p1 (point))
                (re-search-forward "\n[ \t]*\n"))
            (setq $p1 (point)))
          (progn
            (re-search-forward "\n[ \t]*\n" nil "move")
            (setq $p2 (point)))))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (downcase-region $p1 $p2))))
    ;; Below is a copy of xah-upcase-sentence above, because I could not figure
    ;; out a clean way to just call that function and make it work like I wanted
    (let ($p1 $p2)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn
                (setq $p1 (point))
                (re-search-forward "\n[ \t]*\n"))
            (setq $p1 (point)))
          (progn
            (re-search-forward "\n[ \t]*\n" nil "move")
            (setq $p2 (point)))))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (let ((case-fold-search nil))
            ;; after period or question mark or exclamation
            (goto-char (point-min))
            (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-z]\\)" nil "move")
              (upcase-region (match-beginning 2) (match-end 2))
              (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
            ;; after a blank line, after a bullet, or beginning of buffer
            (goto-char (point-min))
            (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil "move")
              (upcase-region (match-beginning 2) (match-end 2))
              (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
            ;; for HTML. first letter after tag
            (goto-char (point-min))
            (while (re-search-forward "\\(<h[1-6]>[ \n]?\\|<p>[ \n]?\\|<li>\\|<dd>\\|<td>[ \n]?\\|<figcaption>[ \n]?\\)\\([a-z]\\)" nil "move")
              (upcase-region (match-beginning 2) (match-end 2))
              (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
            (goto-char (point-min))))))))

;; Keybinding for above
(define-key global-map (kbd "C-c s c") 'jpr-sentence-case)

;; Function to convert contents of former tables into org-formatted tables in my Fellhold rules. There are enough of them that it was worth writing something instead of doing by hand, and rather than leaving it in scratch, this lets me learn from what I wrote.

(defun jpr-make-table ()
  "Take contents of tables originally formatted in RTF and make into org tables.

I created my Fellhold rules in Evernote originally, then imported them to Devonthink, and later converted them to Markdown, and then into org. So some of the finer bits (like tables) got screwed up. Luckily they got screwed up in a consistent way, so this function ought to let me highlight the contents of a former table and turn them into an org-formatted table."
  (interactive)
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (message "Please select a region")
        (return nil)))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        ;; defines a line to work with - I might need to put this in a while loop for as long as it finds valid lines
        (let ($l1 $l2)
          (goto-char (point-min))
          (setq $l1 (point-min))
          (setq $l2 (regex search for newline followed by number)))
        (save-restriction
          (narrow-to-region $l1 $l2)
          (while (re-search-forward "regexp for finding double new lines" nil "move")
            ;; Logic for finding a line: start a current point, search until newline followed by a digit
            ;; Logic for splitting line: find every double newline in "line" and replace wtih a comma
            (do something to put commas in)))
        (org-table-create-or-convert-from-region $p1 $p2)
        ;; thoughts on how to do this: return to point min, search for newline, insert '|-' followed by a newline
        (define header row)))))

;; I have been using "Better OneTab" for a while to manage my tabs, but I
;; recently (May 2021) discovered a new tool called "BrainTool" -
;; https://braintool.org/ . BrainTool allows you to import an org file to define
;; your links and notes, whereas Better OneTab exports your tab groups in a json
;; format. Note that for this to function, the Better OneTab export must be
;; modified to place the default top-level array inside an object and to give
;; the array the label "groups", and any URLs that have a percentage sign will
;; need to have those replaced by a double percentage sign '%%' so that the
;; format function works properly. I don't anticipate needing to use this again,
;; but I wanted to keep it here because I kept smashing my head against how to
;; read a json properly. Some of my confusion was just getting the syntax of
;; dolist wrong (I didn't enclose the arguments in a list at first), but it also
;; seems like you can't just read an array out as a list and use it, it seems to
;; default to making the top level an object, which by default is a hash table.
;; Once I put the array I wanted to work with inside an object and gave it a
;; label that I could access with 'get hash', everything worked fine.

(defun jpr-betteronetab-to-org (in-file out-file)
  "Convert a BetterOneTab json export to an Org file suitable to BrainTool.
IN-FILE should be a Better OneTab JSON export modified to have a top-level
object and with the array of tab groups named 'groups'. OUT-FILE should be an
org file."
(let* ((json-object-type 'hash-table)
       (json-array-type 'list)
       (json-key-type 'string)
       (json (json-read-file in-file))
       (groups (gethash "groups" json)))
  (with-temp-file out-file
      (dolist (group groups)
        (let ((group-title (gethash "title" group)))
          (insert (format "** %s\n" group-title)))
        (dolist (tab (gethash "tabs" group))
          (let ((url (gethash "url" tab))
                (link-title (gethash "title" tab)))
            (insert (format "*** [[%s][%s]]\n" url link-title))))))))

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
