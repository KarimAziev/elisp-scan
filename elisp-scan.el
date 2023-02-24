;;; elisp-scan.el --- Configure scan -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-scan
;; Keywords: lisp
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (project "0.9.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with scan

;; Commands

;; M-x `elisp-scan-ivy-read-unused-items'
;;      Read unused definitions of current file with ivy.
;;      To remove or backup batch of items, mark them.
;;      To remove one item without exiting minibuffer use `C-c C-d'
;;      or define new key for `elisp-scan-ivy-remove-item' in `elisp-scan-ivy-map'.

;; M-x `elisp-scan-ivy-remove-item'
;;      Remove current ivy item without exiting minibuffer.

;; M-x `elisp-scan-query-remove-unused'
;;      Query remove unused definitions.

;; M-x `elisp-scan-all-unused-defs'
;;      Check every project file for unused definitions.

;; M-x `elisp-scan-current-file'
;;      Show unused items in the current file.

;; Customization

;; `elisp-scan-permanent-dirs'
;;      In which directories always check usage.

;; `elisp-scan-archive-dir'
;;      Where to write backup files.

;; `elisp-scan-types-symbols'
;;      Symbols which should always be checked.

;;; Code:

(require 'project)
(declare-function ivy-read "ivy")
(declare-function ivy-state-current "ivy")
(declare-function ivy-update-candidates "ivy")
(declare-function ivy--kill-current-candidate "ivy")

(defcustom elisp-scan-types-symbols '(defun cl-defun defvar defconst defmacro
                                            defvar-local cl-defun cl-defmacro
                                            transient-define-suffix
                                            transient-define-argument
                                            transient-define-prefix
                                            transient-define-infix
                                            defun-ivy+)
  "Symbols which should always be checked."
  :type '(repeat symbol)
  :group 'elisp-scan)

(defcustom elisp-scan-archive-dir (expand-file-name
                                   ".elisp-scan-archives"
                                   user-emacs-directory)
  "Where to write backup files."
  :type 'file
  :group 'elisp-scan)


(defcustom elisp-scan-permanent-dirs `(,user-emacs-directory)
  "In which directories always check usage."
  :type '(repeat directory)
  :group 'elisp-scan)

(defmacro elisp-scan-with-temp-buffer (&rest body)
  "Execute BODY with in temp buffer with `emacs-lisp-mode-syntax-table'."
  (declare (indent 2)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (delay-mode-hooks
       (emacs-lisp-mode)
       (progn
         ,@body))))

(defun elisp-scan-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `elisp-scan-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-scan-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `elisp-scan-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-scan-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'elisp-scan-re-search-backward-inner)
               ((> count 0) #'elisp-scan-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun elisp-scan-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.

Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (elisp-scan-re-search-forward regexp bound noerror
                                (if count (- count) -1)))

(defun elisp-scan-make-re (name)
  "Convert NAME to regexp and surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" "\\(" (regexp-opt (list name
                                         (concat "@" name))
                                   t)
          "\\)"
          "\\_>"))

(defun elisp-scan-types-re ()
  "Return regexp for definitions from `elisp-scan-types-symbols'."
  (concat "\\("
          "[(]"
          "\\(" (mapconcat #'regexp-quote
                           (mapcar #'symbol-name elisp-scan-types-symbols)
                           "\\|")
          "\\)"
          "[\s\t\n]"
          "\\([^\s\t\n),]+\\)"
          "\\)"))

(defun elisp-scan-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun elisp-scan-backward-up-list (&optional n)
  "Move backward up across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-up-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun elisp-scan-top-level-lists ()
  "Return all Lisp lists at outermost position in current buffer.
An \"outermost position\" means one that it is outside of any syntactic entity:
outside of any parentheses, comments, or strings encountered in the scan."
  (let ((sexps))
    (goto-char (point-max))
    (while (elisp-scan-backward-list)
      (when-let ((sexp (unless (nth 4 (syntax-ppss (point)))
                         (list-at-point))))
        (push sexp sexps)))
    sexps))

;;;###autoload
(defun elisp-scan-top-level-maybe-unused-defs ()
  "Return alist with defined top level items which is unused in current buffer.
For example (SYMBOL-NAME . defun)."
  (let ((sexps))
    (goto-char (point-max))
    (while (elisp-scan-backward-list)
      (when-let ((sexp (unless (nth 4 (syntax-ppss (point)))
                         (list-at-point))))
        (when-let ((def-type (car (memq (nth 0 sexp) elisp-scan-types-symbols)))
                   (id (nth 1 sexp)))
          (let* ((command (eq 'interactive
                              (when (memq def-type '(defun cl-defun))
                                (if (stringp (nth 3 sexp))
                                    (car (nth 4 sexp))
                                  (car (nth 3 sexp))))))
                 (name (symbol-name id))
                 (re (elisp-scan-make-re name))
                 (item (when (null
                              (or
                               (save-excursion
                                 (elisp-scan-re-search-backward re nil t 1))
                               (save-excursion
                                 (forward-sexp 1)
                                 (elisp-scan-re-search-forward re nil t 1))))
                         (cons name (symbol-name (if command 'interactive
                                                   def-type))))))
            (when item
              (push item sexps))))))
    sexps))

(defun elisp-scan-find-project-files (&optional directory no-filter)
  "Return all elisp files from project instance in DIRECTORY.
If option
al argument NO-FILTER is non-nil, return all files, else
return only elisp files."
  (let* ((project-find-functions '(project-try-vc try))
         (pr (project-current t directory))
         (dirs (list (project-root pr)))
         (files (project-files pr dirs)))
    (if no-filter
        (seq-remove
         (apply-partially #'string-match-p "\\.elc\\|\\.gpg$")
         files)
      (mapcar
       (lambda (file)
         (expand-file-name file pr))
       (seq-filter (apply-partially #'string-match-p "\\.el$") files)))))

(defun elisp-scan-find-files-in-projects (directories)
  "Return all elisp files from project instances in DIRECTORIES."
  (seq-remove #'file-directory-p
              (seq-reduce (lambda (acc dir)
                            (append acc
                                    (elisp-scan-find-project-files
                                     dir t)))
                          directories '())))

(defun elisp-scan-get-file-or-buffer-content (buffer-or-file)
  "Return a substring of BUFFER-OR-FILE without text properties.
BUFFER-OR-FILE can be a buffer or file."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond ((bufferp buffer-or-file)
             (insert-buffer-substring-no-properties
              (get-buffer buffer-or-file)))
            ((file-exists-p buffer-or-file)
             (if-let ((buff (get-file-buffer buffer-or-file)))
                 (insert-buffer-substring-no-properties
                  buff)
               (insert-file-contents buffer-or-file))))
      (buffer-string))))

(defun elisp-scan-lists-in-files (files)
  "Find all Lisp forms in FILES and return alist of (FILE . FILE-LISTS)."
  (elisp-scan-with-temp-buffer
      (let ((founds))
        (dolist (file files)
          (when (file-exists-p file)
            (erase-buffer)
            (insert-file-contents file)
            (when-let ((sexps (elisp-scan-top-level-lists)))
              (push (cons file sexps) founds))))
        founds)))

(defun elisp-scan-make-overlay (beg end props)
  "Create a new overlay with range BEG to END propertized with PROPS."
  (let ((overlay (make-overlay beg end)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop (nth idx props))
               (val (plist-get props prop)))
          (overlay-put overlay prop val))))
    overlay))

(defvar elisp-scan-overlay nil)
(defun elisp-scan-highlight-bounds (start end)
  "Highlight region between START and END with FACE."
  (elisp-scan-cleanup-overlay)
  (goto-char start)
  (setq elisp-scan-overlay
        (elisp-scan-make-overlay
         start end
         '(face font-lock-warning-face))))

(defun elisp-scan-cleanup-overlay ()
  "Remove `elisp-scan-overlay'."
  (when (overlayp elisp-scan-overlay)
    (delete-overlay elisp-scan-overlay)
    (setq elisp-scan-overlay nil)))

(defun elisp-scan-get-prop (item property)
  "Get PROPERTY at ITEM.
ITEM should be a propertized string."
  (get-text-property 0 property item))

(defun elisp-scan-find-item-bounds (item)
  "Return bounds of ITEM list."
  (goto-char (point-min))
  (when (elisp-scan-re-search-forward (elisp-scan-make-re item) nil t 1)
    (elisp-scan-backward-up-list)
    (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (when (looking-back ";;;###autoload[\n]+" 0)
          (setq beg
                (elisp-scan-re-search-backward
                 ";;;###autoload" nil t 1)))
        (cons beg end)))))

(defun elisp-scan-copy-string-at-point ()
  "Return unquoted string at point."
  (when-let ((start (when (looking-at "\"")
                      (1+ (point)))))
    (forward-sexp 1)
    (buffer-substring-no-properties start (1- (point)))))

(defun elisp-scan-find-all-unsused-defs-0 ()
  "Find all unsused defs in current buffer."
  (let ((unused)
        (regexp (elisp-scan-types-re)))
    (goto-char (point-min))
    (while (elisp-scan-re-search-forward regexp nil t 1)
      (let ((id (match-string-no-properties 3))
            (type (match-string-no-properties 2))
            (re))
        (setq re (elisp-scan-make-re id))
        (unless (save-excursion
                  (elisp-scan-re-search-forward
                   re nil t 1))
          (when (null (save-excursion
                        (elisp-scan-re-search-backward
                         re nil t 2)))
            (let ((file (save-excursion
                          (goto-char (nth 1 (nth 9 (syntax-ppss (point)))))
                          (forward-char 1)
                          (elisp-scan-copy-string-at-point))))
              (push (propertize id
                                :file file
                                :type type
                                :id id)
                    unused))))))
    unused))

(defun elisp-scan-find-all-unsused-defs ()
  "Return unsused defs in project files."
  (elisp-scan-with-temp-buffer
      (insert (prin1-to-string
               (elisp-scan-lists-in-files
                (elisp-scan-find-project-files))))
      (reverse (elisp-scan-find-all-unsused-defs-0))))

(defun elisp-scan-remove-if-commands (unused)
  "Remove commands from UNUSED."
  (seq-remove (lambda (it)
                (commandp (intern it)))
              unused))

(defun elisp-scan-check-used-in-dirs-p (string &rest directories)
  "Return first filename with occurence of STRING in DIRECTORIES."
  (setq directories (delete nil (flatten-list directories)))
  (let ((founds)
        (lisp-re (elisp-scan-make-re string)))
    (while (and (null founds) directories)
      (with-temp-buffer
        (let ((default-directory (pop directories)))
          (shell-command
           (concat
            (string-join
             (list
              "ag --nogroup --nocolor -w"
              (concat "'" string "'"))
             "\s"))
           (current-buffer)
           (current-buffer))
          (let ((case-fold-search nil))
            (when (elisp-scan-re-search-forward lisp-re nil t 1)
              (beginning-of-line)
              (elisp-scan-re-search-forward "\\([^:]+\\):[0-9]+:" nil t 1)
              (setq founds
                    (expand-file-name
                     (match-string-no-properties 1)
                     default-directory)))))))
    founds))

(defun elisp-scan-if-yas-snipptes (unused)
  "Remove occurences from yas snippet directory from UNUSED."
  (when (executable-find "ag")
    (require 'yasnippet nil t)
    (if-let ((dirs (when (fboundp 'yas-snippet-dirs)
                     (yas-snippet-dirs))))
        (seq-remove (lambda (it)
                      (elisp-scan-check-used-in-dirs-p it
                                                       dirs))
                    unused)
      unused)))

(defun elisp-scan-group-by (prop items)
  "Group ITEMS by PROP and return alist."
  (seq-reduce (lambda (acc it)
                (let* ((key (elisp-scan-get-prop it prop))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list it))
                                (list it))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              items '()))

(defvar elisp-scan-unused-items nil)
(defun elisp-scan-highlight-confirm (&optional start end prompt)
  "Highlight region beetwen START and END with PROPS and ask PROMPT."
  (let ((last-overlay)
        (bounds (or (and start end
                         (cons start end))
                    (bounds-of-thing-at-point 'sexp))))
    (when bounds
      (unwind-protect
          (progn (goto-char (car bounds))
                 (recenter)
                 (setq last-overlay (elisp-scan-make-overlay
                                     (car bounds)
                                     (cdr bounds)
                                     '(face diff-indicator-removed)))
                 (if (functionp prompt)
                     (funcall prompt)
                   (yes-or-no-p (if (stringp prompt)
                                    prompt
                                  ""))))
        (when (and last-overlay
                   (overlayp last-overlay))
          (delete-overlay last-overlay))))))

(defun elisp-scan-jump-to-item (item)
  "Jump to propertized ITEM."
  (let ((file (elisp-scan-get-prop item :file))
        (id (elisp-scan-get-prop item :id)))
    (if (minibuffer-window-active-p (selected-window))
        (with-minibuffer-selected-window
          (with-current-buffer
              (find-file-noselect file)
            (when-let ((bounds (elisp-scan-find-item-bounds id)))
              (elisp-scan-highlight-bounds (car bounds)
                                           (cdr bounds))
              (unless (get-buffer-window (current-buffer))
                (pop-to-buffer-same-window (current-buffer))))))
      (with-current-buffer
          (find-file-noselect file)
        (when-let ((bounds (elisp-scan-find-item-bounds id)))
          (elisp-scan-highlight-bounds (car bounds)
                                       (cdr bounds))
          (unless (get-buffer-window (current-buffer))
            (pop-to-buffer-same-window (current-buffer))))))))

(defun elisp-scan-remove-definition (name &optional confirm)
  "Find and remove definition with NAME in current buffer.
If CONFIRM passed also prompt user."
  (goto-char (point-min))
  (when (elisp-scan-re-search-forward (elisp-scan-make-re name) nil t 1)
    (elisp-scan-backward-up-list)
    (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (when (looking-back ";;;###autoload[\n]+" 0)
          (setq beg (elisp-scan-re-search-backward
                     ";;;###autoload" nil t 1)))
        (unless (and confirm
                     (not (elisp-scan-highlight-confirm beg end
                                                        confirm)))
          (prog1 (buffer-substring-no-properties beg end)
            (delete-region beg (cdr bounds))
            (while (looking-at "^\n[\n]")
              (forward-line 1))
            (while (looking-back "^\n[\n]+" 0)
              (join-line))))))))

(defun elisp-scan-make-backup-file-name (file)
  "Generate backup filename for FILE."
  (if (file-directory-p elisp-scan-archive-dir)
      (expand-file-name
       (concat (if (file-name-absolute-p file)
                   (file-name-base file)
                 file)
               "~"
               (format-time-string "%F_%H%M%S")
               ".org")
       elisp-scan-archive-dir)
    elisp-scan-archive-dir))

(defun elisp-scan-read-action (prompt &optional extra)
  "Ask user to select an entry from choices and EXTRA promting with PROMPT."
  (car
   (read-multiple-choice
    prompt
    (append '((?y "yes")
              (?b "remove and backup")
              (?i "skip this file")
              (?n "no"))
            extra))))

(defun elisp-scan-remove-unused (items)
  "Remove occurrences of ITEMS.

As each match is found, the user must type a character saying
what to do with it."
  (let ((grouped (flatten-list
                  (mapcar #'cdr (elisp-scan-group-by :file items))))
        (exlcuded-files)
        (for-all)
        (answer)
        (new-file (elisp-scan-make-backup-file-name
                   "archive")))
    (dotimes (idx (length grouped))
      (let ((id (elisp-scan-get-prop (nth idx grouped) :id))
            (file (elisp-scan-get-prop (nth idx grouped) :file))
            (next (nth (1+ idx) grouped)))
        (unless (member file exlcuded-files)
          (let ((buff (or (get-file-buffer file)
                          (progn
                            (find-file-noselect file)
                            (get-file-buffer file)))))
            (when (and next
                       (stringp for-all)
                       (not (equal file (elisp-scan-get-prop next :file))))
              (setq for-all nil))
            (with-current-buffer buff
              (if buffer-read-only
                  (push file exlcuded-files)
                (when-let ((bounds (elisp-scan-find-item-bounds id)))
                  (elisp-scan-highlight-bounds (car bounds)
                                               (cdr bounds))
                  (pop-to-buffer (current-buffer))
                  (unless for-all
                    (let ((c (if answer
                                 (elisp-scan-read-action
                                  "Remove:\s" '((?r "for all in file")
                                                (?! "repeat for all files")))
                               (elisp-scan-read-action
                                "Remove:\s"))))
                      (if (member c '(?! ?r))
                          (setq for-all
                                (pcase c
                                  (?! (setq for-all t))
                                  (?r (setq for-all file))))
                        (setq answer c))))
                  (pcase answer
                    (?i (push file exlcuded-files))
                    (?y (elisp-scan-remove-definition id))
                    (?b
                     (let* ((backup
                             (concat (format "\n* %s\n" id)
                                     ":PROPERTIES:\n"
                                     (format
                                      ":ARCHIVE_TIME: %s\n"
                                      (format-time-string
                                       (substring
                                        "<%F %a %H:%M>" 1 -1)))
                                     (format
                                      ":ARCHIVE_FILE: %s\n"
                                      (abbreviate-file-name file))
                                     ":END:\n"
                                     (format "#+name: %s\n" id)
                                     (format
                                      "#+begin_src emacs-lisp\n%s\n#+end_src\n"
                                      (buffer-substring-no-properties
                                       (car bounds)
                                       (cdr bounds))))))
                       (unless (file-exists-p
                                (file-name-directory new-file))
                         (make-directory (file-name-directory new-file) t))
                       (write-region backup nil new-file t)
                       (elisp-scan-remove-definition id)))))))))))))

(define-derived-mode elisp-scan-report-mode tabulated-list-mode
  "Elisp Scan Report."
  "Show report gathered about unused definitions."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Type" 30 t)
         ("File" 30)])
  (tabulated-list-init-header))

(defun elisp-scan-button-action (button)
  "Run an action for BUTTON."
  (let ((current-window (selected-window))
        (item (get-text-property button 'item
                                 (marker-buffer button)))
        (report-buffer (current-buffer)))
    (with-selected-window current-window
      (let ((buff (progn (find-file-noselect
                          (elisp-scan-get-prop item :file))
                         (get-file-buffer
                          (elisp-scan-get-prop item :file)))))
        (switch-to-buffer-other-window buff)
        (when (with-current-buffer buff
                (elisp-scan-remove-definition
                 (elisp-scan-get-prop item :id)
                 "Remove?"))
          (when-let ((found (seq-find
                             (lambda (it)
                               (let ((id (car it)))
                                 (equal id
                                        (elisp-scan-get-prop item :id))))
                             (buffer-local-value
                              'tabulated-list-entries
                              report-buffer))))
            (with-current-buffer report-buffer
              (setq tabulated-list-entries (delete found
                                                   tabulated-list-entries))
              (tabulated-list-revert))))))))

(defun elisp-scan-report-convert (definition)
  "Return information about DEFINITION.

The information is formatted in a way suitable for
`elisp-scan-report-mode'."
  (list
   (elisp-scan-get-prop definition :id)
   (vector
    (cons
     (elisp-scan-get-prop definition :id)
     (list 'action #'elisp-scan-button-action
           'help-echo "Jump to item"
           'item definition))
    (elisp-scan-get-prop definition :type)
    (abbreviate-file-name
     (elisp-scan-get-prop definition :file)))))

(defun elisp-scan-display-report (items)
  "Show ITEMS in Tabulated List buffer."
  (with-current-buffer (get-buffer-create "*elisp-scan statistics*")
    (setq tabulated-list-entries
          (mapcar #'elisp-scan-report-convert items))
    (elisp-scan-report-mode)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(defun elisp-scan-get-files-to-check ()
  "Return files from current project and permanent projects.
Permanent projects defined in `elisp-scan-permanent-dirs'."
  (let ((project-find-functions '(project-try-vc try)))
    (if-let* ((curr (project-current t default-directory))
              (projects
               (seq-uniq (mapcar #'expand-file-name
                                 (append (cddr curr)
                                         elisp-scan-permanent-dirs)))))
        (elisp-scan-find-files-in-projects projects)
      (elisp-scan-find-project-files))))

(defun elisp-scan-find-unused-declarations ()
  "Search for unused `declare-function'."
  (let ((declare-re (concat "\\_<" "\\(" (regexp-opt
                                          (list "declare-function")
                                          t)
                            "\\)"
                            "\\_>"))
        (unused))
    (save-excursion
      (goto-char (point-min))
      (while (elisp-scan-re-search-forward declare-re nil t 1)
        (when-let* ((sexp (save-excursion
                            (elisp-scan-backward-up-list)
                            (sexp-at-point)))
                    (name (when (and
                                 (listp sexp)
                                 (eq (nth 0 sexp) 'declare-function)
                                 (symbolp (nth 1 sexp)))
                            (symbol-name (nth 1 sexp))))
                    (re (elisp-scan-make-re name)))
          (save-excursion
            (when (elisp-scan-backward-up-list)
              (unless (or (elisp-scan-re-search-backward re nil t 1)
                          (progn (forward-sexp 1)
                                 (elisp-scan-re-search-forward re nil t 1)))
                (push name unused)))))))
    unused))

(defun elisp-scan-unused-in-file (file &optional files)
  "Check defined items in FILE are unused both in FILE and FILES.
Return alist of (SYMBOL-NAME . DEFINITION-TYPE)."
  (let ((alist)
        (declarations))
    (elisp-scan-with-temp-buffer
        (insert (elisp-scan-get-file-or-buffer-content file))
        (setq alist (elisp-scan-top-level-maybe-unused-defs))
      (setq declarations
            (mapcar
             (lambda (d)
               (propertize
                d
                :id d
                :type "declare-function"
                :file file))
             (elisp-scan-find-unused-declarations))))
    (let ((files (delete file files))
          (not-used (mapcar #'car alist)))
      (elisp-scan-with-temp-buffer
          (insert
           (mapconcat (lambda (file)
                        (concat
                         ";;;###FILE:"
                         file
                         "\n"
                         (elisp-scan-get-file-or-buffer-content
                          file)))
                      files "\n\n"))
          (dolist (name not-used)
            (when-let ((found-file
                        (save-excursion
                          (when (elisp-scan-re-search-backward
                                 (elisp-scan-make-re name) nil t 1)
                            (when (re-search-backward
                                   "^;;;###FILE:\\([^\n]+\\)" nil t 1)
                              (match-string-no-properties 1))))))
              (setq not-used (delete name not-used)))))
      (append declarations
              (mapcar
               (lambda (name)
                 (let ((it (assoc name alist)))
                   (propertize
                    (car it)
                    :id (car it)
                    :type (cdr it)
                    :file file)))
               not-used)))))

;;;###autoload
(defun elisp-scan-current-file ()
  "Show unused items in the current file."
  (interactive)
  (let* ((file buffer-file-name)
         (files (if (and elisp-scan-permanent-dirs
                         (yes-or-no-p
                          (format "Include %s?"
                                  (string-join
                                   elisp-scan-permanent-dirs
                                   "\s"))))
                    (elisp-scan-get-files-to-check)
                  (elisp-scan-find-project-files)))
         (items (elisp-scan-unused-in-file
                 file
                 files)))
    (elisp-scan-display-report items)))

;;;###autoload
(defun elisp-scan-all-unused-defs ()
  "Check every project file for unused definitions."
  (interactive)
  (elisp-scan-display-report
   (if (yes-or-no-p "Include commands?")
       (elisp-scan-if-yas-snipptes
        (elisp-scan-find-all-unsused-defs))
     (elisp-scan-remove-if-commands
      (elisp-scan-find-all-unsused-defs)))))

;;;###autoload
(defun elisp-scan-query-remove-unused ()
  "Query remove unused definitions."
  (interactive)
  (save-some-buffers)
  (setq elisp-scan-unused-items (elisp-scan-if-yas-snipptes
                                 (elisp-scan-remove-if-commands
                                  (elisp-scan-find-all-unsused-defs))))
  (unwind-protect
      (elisp-scan-remove-unused elisp-scan-unused-items)
    (elisp-scan-cleanup-overlay)))

(defun elisp-scan-unused-trasnform-items (items)
  "Annotate ITEMS."
  (mapcar (lambda (it)
            (setq it (format "%s" it))
            (apply #'propertize
                   (concat
                    (propertize
                     (capitalize
                      (elisp-scan-get-prop it :type))
                     'face
                     'font-lock-builtin-face)
                    ": "
                    (elisp-scan-get-prop it :id)
                    "  "
                    (elisp-scan-get-prop it :file))
                   (text-properties-at 0 it)))
          items))

(defun elisp-scan-remove-item (item)
  "Remove ITEM which is propertized string with :id property."
  (elisp-scan-remove-definition
   (elisp-scan-get-prop item :id)))

(defvar ivy-last)
;;;###autoload
(defun elisp-scan-ivy-remove-item ()
  "Remove current ivy item without exiting minibuffer."
  (interactive)
  (when-let* ((item (ivy-state-current ivy-last))
              (file (elisp-scan-get-prop item :file)))
    (when-let ((cands
                (with-minibuffer-selected-window
                  (elisp-scan-remove-item item)
                  (ivy--kill-current-candidate)
                  (elisp-scan-unused-trasnform-items
                   (elisp-scan-unused-in-file
                    file
                    (elisp-scan-get-files-to-check))))))
      (ivy-update-candidates cands))))

(defvar elisp-scan-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'elisp-scan-ivy-remove-item)
    map))

;;;###autoload
(defun elisp-scan-ivy-read-unused-items (&optional arg)
  "Read unused definitions of current file with ivy.
To remove one item without exiting minibuffer \\<elisp-scan-ivy-map>\ use `\\[elisp-scan-ivy-remove-item]'.
To remove or backup batch of items, mark them.
With optional prefix ARG include only current file."
  (interactive "P")
  (require 'ivy nil t)
  (let ((marked)
        (file buffer-file-name))
    (ivy-read
     (substitute-command-keys
      "\\<elisp-scan-ivy-map>\ Use `\\[elisp-scan-ivy-remove-item]' to remove ")
     (lambda (&rest _args)
       (elisp-scan-unused-trasnform-items
        (elisp-scan-unused-in-file
         file
         (and (null arg)
              (elisp-scan-get-files-to-check)))))
     :action 'elisp-scan-jump-to-item
     :keymap elisp-scan-ivy-map
     :caller 'elisp-scan-ivy-read-unused-items
     :dynamic-collection t
     :unwind 'elisp-scan-cleanup-overlay
     :multi-action (lambda (cands)
                     (setq marked cands)))
    (elisp-scan-cleanup-overlay)
    (elisp-scan-remove-unused marked)))

(provide 'elisp-scan)
;;; elisp-scan.el ends here