;;; elisp-scan.el --- Configure elisp scan -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/KarimAziev/elisp-scan
;; Package-Requires: ((emacs "27.2") (ag "2.0"))

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

;; This is a tool for finding unused code in Emacs Lisp files.

;;; Code:

(require 'projectile)

(declare-function ivy-read "ivy")
(declare-function ivy--get-window "ivy")

(defcustom elisp-scan-types '("defun" "cl-defun" "defvar" "defconst"
                                 "defmacro" "defvar-local" "defcustom")
  "Types to check."
  :type '(repeat string)
  :group 'elisp-scan)

(defcustom elisp-scan-archive-dir (expand-file-name
                                      ".elisp-scan-archives"
                                      user-emacs-directory)
  "Where to write backup files."
  :type 'file
  :group 'elisp-scan)

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
         (cond ((< count 0) (setq count (- count))
                #'elisp-scan-re-search-backward-inner)
               ((> count 0) #'elisp-scan-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

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
  "Return regexp for definitions from `elisp-scan-types'."
  (concat "\\("
          "[(]"
          "\\(" (mapconcat 'regexp-quote elisp-scan-types "\\|") "\\)"
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

(defvar elisp-scan-files-cache (make-hash-table :test 'equal))

(defmacro elisp-scan-with-temp-buffer (&rest body)
  "Execute BODY with in temp buffer with `emacs-lisp-mode-syntax-table'."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (lisp-mode-variables nil)
     (set-syntax-table emacs-lisp-mode-syntax-table)
     (progn
       ,@body)))

(defun elisp-scan-set-file-cache (file value)
  "Put VALUE to `elisp-scan-files-cache' under FILE as key."
  (let ((tick (file-attribute-modification-time (file-attributes
                                                 file
                                                 'string))))
    (puthash file (list :tick tick :cache value)
             elisp-scan-files-cache)))

(defun elisp-scan-lists-in-files (files)
  "Find all Lisp forms in FILES and return alist of (FILE . FILE-LISTS)."
  (elisp-scan-with-temp-buffer
      (let ((founds))
        (dolist (file files)
          (erase-buffer)
          (insert-file-contents file)
          (when-let ((sexps (elisp-scan-top-level-lists)))
            (push (cons file sexps) founds)
            (elisp-scan-set-file-cache file sexps)))
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
        (unless (save-excursion (elisp-scan-re-search-forward
                                 re nil t 1))
          (when (null (save-excursion (elisp-scan-re-search-backward
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

(defun elisp-scan-find-project-files ()
  "Return list of elisp files in `user-emacs-directory.'."
  (require 'projectile)
  (when-let* ((project (projectile-project-root))
              (files (seq-filter (apply-partially 'string-match-p "\\.el$")
                                 (projectile-project-files
                                  (projectile-project-root)))))
    (mapcar (lambda (file) (expand-file-name file project)) files)))

(defun elisp-scan-find-all-unsused-defs ()
  "Return unsused defs in project files."
  (require 'pp)
  (elisp-scan-with-temp-buffer
      (insert (pp-to-string
               (elisp-scan-lists-in-files
                (elisp-scan-find-project-files))))
      (reverse (elisp-scan-find-all-unsused-defs-0))))

(defun elisp-scan-remove-if-commands (unused)
  "Remove commands from UNUSED."
  (seq-remove (lambda (it) (commandp (intern it)))
              unused))

(defun elisp-scan-check-used-in-dirs-p (string &rest directories)
  "Return first filename with occurence of STRING in DIRECTORIES."
  (setq directories (delete nil (flatten-list directories)))
  (let ((founds)
        (lisp-re (elisp-scan-make-re (regexp-quote string))))
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
    (if-let ((dirs (when (fboundp 'yas-snippet-dirs) (yas-snippet-dirs))))
        (seq-remove (lambda (it) (elisp-scan-check-used-in-dirs-p it
                                                                dirs))
                    unused)
      unused)))

(defun elisp-scan-group-by (prop items)
  "Group ITEMS by PROP and return alist."
  (seq-reduce (lambda (acc it)
                (let* ((key (elisp-scan-get-prop it prop))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell) (list it))
                                (list it))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              items '()))

(defun elisp-scan-unused-trasnform-items (items)
  "Annotate ITEMS."
  (mapcar (lambda (it)
            (setq it (format "%s" it))
            (apply 'propertize
                   (concat (elisp-scan-get-prop it :type) ": "
                           (elisp-scan-get-prop it :id)
                           "  "
                           (elisp-scan-get-prop it :file))
                   (text-properties-at 0 it)))
          items))

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
                                     (car bounds) (cdr bounds)
                                     '(face diff-indicator-removed)))
                 (if (functionp prompt)
                     (funcall prompt)
                   (yes-or-no-p (or prompt ""))))
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
              (progn
                (find-file-noselect file)
                (get-file-buffer file))
            (when-let ((bounds (elisp-scan-find-item-bounds id)))
              (elisp-scan-highlight-bounds (car bounds) (cdr bounds))
              (pop-to-buffer-same-window (current-buffer)))))
      (with-current-buffer
          (progn
            (find-file-noselect file)
            (get-file-buffer file))
        (when-let ((bounds (elisp-scan-find-item-bounds id)))
          (elisp-scan-highlight-bounds (car bounds) (cdr bounds))
          (pop-to-buffer-same-window (current-buffer)))))))

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
                     (not (elisp-scan-highlight-confirm beg end)))
          (prog1 (buffer-substring-no-properties beg end)
            (delete-region beg (cdr bounds))
            (while (looking-at "^\n[\n]")
              (forward-line 1))
            (while (looking-back "^\n[\n]+" 0)
              (join-line))))))))

(defun elisp-scan-make-backup-file-name (file)
  "Generate backup filename for FILE."
  (expand-file-name
   (concat (if (file-name-absolute-p file)
               (file-name-base file)
             file)
           "~"
           (format-time-string "%Y-%m-%d_%H%M%S")
           ".org")
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
                  (mapcar 'cdr (elisp-scan-group-by :file items))))
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
            (when (and (equal answer ?r)
                       for-all
                       next
                       (not (equal file (elisp-scan-get-prop next :file))))
              (setq for-all nil))
            (with-current-buffer buff
              (if buffer-read-only
                  (push file exlcuded-files)
                (when-let ((bounds (elisp-scan-find-item-bounds id)))
                  (elisp-scan-highlight-bounds (car bounds) (cdr bounds))
                  (pop-to-buffer (current-buffer))
                  (unless for-all
                    (let ((c (if answer
                                 (elisp-scan-read-action
                                  "Remove:\s" '((?r "for all in file")
                                                (?! "repeat for all files")))
                               (elisp-scan-read-action
                                "Remove:\s"))))
                      (if (member c '(?! ?r))
                          (setq for-all t)
                        (setq answer c))))
                  (pcase answer
                    (?i (push file exlcuded-files))
                    (?y (elisp-scan-remove-definition id))
                    (?b (let* ((backup
                                (concat (format "\n* %s\n" id)
                                        ":PROPERTIES:\n"
                                        (format
                                         "\n:ARCHIVE_TIME: %s\n"
                                         (format-time-string
                                          (substring
                                           "<%Y-%m-%d %a %H:%M>" 1 -1)))
                                        (format
                                         ":ARCHIVE_FILE: %s\n"
                                         (abbreviate-file-name file))
                                        (format "#+name %s\n" id)
                                        (format
                                         "#+begin_src emacs-lisp\n%s\n#+end_src\n"
                                         (buffer-substring-no-properties
                                          (car bounds) (cdr bounds))))))
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
         ("File" 30 t)
         ("Type" 20 t)])
  (tabulated-list-init-header))

(defun elisp-scan-button-action (button)
  "Run an action for BUTTON."
  (let ((current-window (selected-window))
        (item (get-text-property button 'item
                                 (marker-buffer button))))
    (with-selected-window current-window
      (let ((buff (progn (find-file-noselect
                          (elisp-scan-get-prop item :file))
                         (get-file-buffer
                          (elisp-scan-get-prop item :file)))))
        (switch-to-buffer-other-window buff)
        (with-current-buffer buff
          (elisp-scan-remove-definition
           (elisp-scan-get-prop item :id) t))))))

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
    (elisp-scan-get-prop definition :file)
    (elisp-scan-get-prop definition :type))))

;;;###autoload
(defun elisp-scan-report ()
  "Show current statistics gathered about unused definitions."
  (interactive)
  (with-current-buffer (get-buffer-create "*elisp-scan statistics*")
    (setq tabulated-list-entries
          (mapcar 'elisp-scan-report-convert
                  (if (yes-or-no-p "Include commands?")
                      (elisp-scan-if-yas-snipptes
                       (elisp-scan-find-all-unsused-defs))
                    (elisp-scan-if-yas-snipptes
                     (elisp-scan-remove-if-commands
                      (elisp-scan-find-all-unsused-defs))))))
    (elisp-scan-report-mode)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

;;;###autoload
(defun elisp-scan-query-remove-unused ()
  "Query remove unused definitions."
  (interactive)
  (save-some-buffers)
  (setq elisp-scan-unused-items (elisp-scan-if-yas-snipptes
                                    (elisp-scan-remove-if-commands
                                     (elisp-scan-find-all-unsused-defs))))
  (elisp-scan-remove-unused elisp-scan-unused-items))

;;;###autoload
(defun elisp-scan-ivy-read-unused-items ()
  "Remove marked unused definitions."
  (interactive)
  (require 'ivy)
  (setq elisp-scan-unused-items (elisp-scan-if-yas-snipptes
                                 (elisp-scan-remove-if-commands
                                  (elisp-scan-find-all-unsused-defs))))
  (let ((marked))
    (ivy-read "Unused:\s" (elisp-scan-unused-trasnform-items
                           elisp-scan-unused-items)
              :action 'elisp-scan-jump-to-item
              :caller 'elisp-scan-ivy-read-unused-items
              :unwind 'elisp-scan-cleanup-overlay
              :multi-action (lambda (cands) (setq marked cands)))
    (elisp-scan-cleanup-overlay)
    (elisp-scan-remove-unused marked)))

(provide 'elisp-scan)
;;; elisp-scan.el ends here