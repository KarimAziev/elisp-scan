;;; elisp-scan.el --- Configure scan -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-scan
;; Keywords: lisp
;; Version: 0.4.0
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

;; Usage

;; The most straightforward usage is to invoke transient popup M-x `elisp-scan-menu'.

;; Main commands:

;; - `elisp-scan-file' Scan the Elisp file and show the report.
;; - `elisp-scan-dir' Scan elisp files in the directory and show the report.
;; - `elisp-scan-project'  Scan project files and show the report.
;; - `elisp-scan-all-unused-defs' Same as `elisp-scan-project' but with predefined filters to show only unused items.

;; This commands will activate `elisp-scan-report-mode'.
;; M-x `elisp-scan-list-menu' - will dispatch transient menu for this mode.
;; Or see the keymap variable `elisp-scan-report-mode-map'.

;;; Customization

;; `elisp-scan-initial-filters' Default filters in reports modes
;; `elisp-scan-permanent-dirs' Directories to always check usage.
;; `elisp-scan-types-symbols' Symbols which should always be checked.

;;; Code:

(require 'project)
(require 'transient)


(declare-function text-property-search-backward "text-property-search")
(declare-function text-property-search-forward "text-property-search")
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

(defcustom elisp-scan-permanent-dirs nil
  "In which directories always check usage."
  :type '(repeat directory)
  :group 'elisp-scan)

(defcustom elisp-scan-initial-filters nil
  "Initial filters in report list mode."
  :type '(repeat
          (radio
           (function-item :tag "Show only unused" elisp-scan--unused-pred)
           (function-item :tag "Show only external references "
                          elisp-scan--ext-pred)
           (function-item :tag "Hide interactive commands"
                          elisp-scan--command-pred)
           (function :tag "Custom function")))
  :group 'elisp-scan)

(defvar elisp-scan-modes-types
  (list (cons 'define-minor-mode 2)
        (cons 'define-derived-mode 4)
        (cons 'define-generic-mode 8)
        (cons 'define-compilation-mode 3)
        (cons 'easy-mmode-define-minor-mode 2)) )

(defvar elisp-scan-interactive-types
  (list (cons 'defun 3)
        (cons 'defsubst 3)
        (cons 'cl-defun 3)
        (cons 'cl-defsubst 3)))

(defvar elisp-scan-non-defun-command-types
  (list (cons 'defhydra 3)
        (cons 'transient-define-prefix 3)))

(defvar elisp-scan-func-types
  (list (cons 'defun 3)
        (cons 'defmacro 3)
        (cons 'defsubst 3)
        (cons 'defalias 4)
        (cons 'defhydra 3)
        (cons 'transient-define-prefix 3)
        (cons 'transient-define-suffix 3)
        (cons 'transient-define-argument 3)
        (cons 'transient-define-infix 3)
        (cons 'cl-defun 3)
        (cons 'cl-defsubst 3)
        (cons 'cl-defmacro 3)
        (cons 'cl-defgeneric 3)
        (cons 'cl-defmethod 3)))

(defvar elisp-scan-var-types (list (cons 'defvar 3)
                                   (cons 'defconst 3)
                                   (cons 'defvar 3)))

(defvar elisp-scan-custom-types (list (cons 'defface 3)
                                      (cons 'defcustom 3)
                                      (cons 'defgroup 3)
                                      (cons 'deftheme 3)))

(defvar elisp-scan-def-type-poses
  (append
   (list (cons 'define-skeleton 2)
         (cons 'ert-deftest 3)
         (cons 'define-widget 3)
         (cons 'easy-mmode-define-minor-mode 2)
         (cons 'defclass 4)
         (cons 'cl-defstruct 3))
   elisp-scan-var-types
   elisp-scan-custom-types
   elisp-scan-func-types
   elisp-scan-modes-types))

(defmacro elisp-scan-with-temp-buffer (&rest body)
  "Execute BODY with in temp buffer with `emacs-lisp-mode-syntax-table'."
  (declare (indent 2)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let ((emacs-lisp-mode-hook nil))
       (emacs-lisp-mode)
       (progn
         ,@body))))

(defmacro elisp-scan-with-fake-file (file &rest body)
  "Execute BODY in the temp buffer with FILE as buffer file name."
  (declare (indent 2)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let ((buffer-file-name ,file))
       (insert-file-contents buffer-file-name)
       (if (equal (file-name-extension buffer-file-name) "el")
           (let ((emacs-lisp-mode-hook nil))
             (emacs-lisp-mode)
             (progn
               ,@body))
         (progn
           ,@body)))))

(defmacro elisp-scan-with-every-top-form (&rest body)
  "Bind VARS and eval BODY in current buffer on every top level form."
  (declare (indent 1)
           (debug t))
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char (point-max))
       (while (and (elisp-scan-backward-list)
                   (looking-at "[(]"))
         (save-excursion
           ,@body)))))

(defun elisp-scan-rpartial (fn &rest args)
  "Return a partial application of a function FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (lambda (&rest pre-args)
    (apply fn
           (append pre-args args))))

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

(defun elisp-scan-top-level-lists ()
  "Return all Lisp lists at outermost position in current buffer.
An \"outermost position\" means one that it is outside of any syntactic entity:
outside of any parentheses, comments, or strings encountered in the scan."
  (let ((sexps)
        (sexp))
    (goto-char (point-min))
    (while (setq sexp (ignore-errors (read (current-buffer))))
      (push sexp sexps))
    (reverse sexps)))

(defun elisp-scan-buffer-jump-to-form (type name)
  "Search for definition with TYPE and NAME in current buffer."
  (let ((symb (if (symbolp name)
                  name
                (intern name)))
        (type-symb (if (stringp type)
                       (intern type)
                     type))
        (re (elisp-scan-make-re (if (symbolp name)
                                    (symbol-name name)
                                  name)))
        (found))
    (save-excursion
      (goto-char (point-max))
      (while (and (not found)
                  (elisp-scan-re-search-backward re nil t))
        (let ((parse-sexp-ignore-comments t))
          (setq found (ignore-errors (forward-sexp -1)
                                     (when (eq type-symb (symbol-at-point))
                                       (backward-up-list 1)
                                       (when-let ((sexp
                                                   (elisp-scan-list-at-point)))
                                         (when (and (eq (car-safe sexp)
                                                        type-symb)
                                                    (eq (nth 1 sexp)
                                                        symb))
                                           (point)))))))))
    (when found
      (goto-char found)
      found)))

(defun elisp-scan-get-unused-in-buffer (&optional items)
  "Return alist of possibly unused ITEMS in current buffer."
  (let ((items (or items (elisp-scan-buffer)))
        (unused))
    (dolist (l items)
      (let ((type (car-safe l))
            (symb (nth 1 l)))
        (when (and
               (or
                (memq type elisp-scan-types-symbols)
                (memq type '(declare-function))))
          (when (elisp-scan-buffer-jump-to-form type symb)
            (let* ((name (symbol-name symb))
                   (re (elisp-scan-make-re name))
                   (used (or
                          (save-excursion
                            (elisp-scan-re-search-backward re nil t 1))
                          (save-excursion
                            (forward-sexp 1)
                            (elisp-scan-re-search-forward re nil t 1)))))
              (unless used
                (push (cons symb l) unused)))))))
    unused))

(defun elisp-scan-find-project-files (&optional directory no-filter)
  "Return all elisp files from project instance in DIRECTORY.
If optional argument NO-FILTER is non-nil, return all files, else
return only elisp files."
  (let* ((project-find-functions '(project-try-vc try))
         (pr (project-current t directory))
         (dirs (list (project-root pr)))
         (files (project-files pr dirs)))
    (if no-filter
        (seq-filter #'file-exists-p files)
      (seq-filter (elisp-scan-all-pass
                   (list (apply-partially #'string-suffix-p ".el")
                         #'file-exists-p))
                  files))))

(defun elisp-scan-find-files-in-projects (directories)
  "Return all elisp files from project instances in DIRECTORIES."
  (seq-remove #'file-directory-p
              (seq-reduce (lambda (acc dir)
                            (append acc
                                    (elisp-scan-find-project-files
                                     dir t)))
                          directories '())))

(defun elisp-scan-get-file-or-buffer-content (buffer-or-file)

  "Return a sub string of BUFFER-OR-FILE without text properties.
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
      (let ((founds)
            (percent 0)
            (len (length files)))
        (dotimes (k len)
          (let ((file (nth k files)))
            (when (file-exists-p file)
              (setq percent (elisp-scan-calc-percentage k len))
              (message "%d of %d (%d%%) extracting sexps %s"
                       k
                       len
                       percent (file-name-base file))
              (when (file-exists-p file)
                (erase-buffer)
                (insert-file-contents file)
                (when-let ((sexps (elisp-scan-top-level-lists)))
                  (push (cons file sexps) founds))))))
        founds)))

(defun elisp-scan-highlight-bounds (start end)
  "Highlight region between START and END with FACE."
  (goto-char start)
  (pulse-momentary-highlight-region
   start end
   'font-lock-warning-face))

(defun elisp-scan-get-prop (item property)
  "Get PROPERTY at ITEM.
ITEM should be a propertized string or a plist."
  (if (listp item)
      (plist-get item property)
    (get-text-property 0 property item)))

(defun elisp-scan-find-item-bounds (type name)
  "Return bounds of list with TYPE and NAME."
  (when-let ((beg (elisp-scan-buffer-jump-to-form type name)))
    (goto-char beg)
    (let ((end (save-excursion
                 (forward-sexp 1)
                 (point)))
          (autoload-start
           (save-excursion
             (forward-line -1)
             (when (looking-at ";;;###autoload")
               (let ((autoload-beg (point)))
                 (skip-chars-forward ";#a-z\s\t")
                 (when (or (not (looking-at "[(]autoload[\s\t]+'"))
                           (when (re-search-forward
                                  "[(]autoload[\s\t]+'" nil t 1)
                             (when-let ((sym (symbol-at-point)))
                               (eq sym (if (symbolp name) name (intern
                                                                name))))))
                   autoload-beg))))))
      (cons (or autoload-start beg) end))))

(defun elisp-scan-find-all-unsused-defs-0 ()
  "Find all unused definitions in current buffer."
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
            (let
                ((file
                  (save-excursion
                    (goto-char (nth 1 (nth 9 (syntax-ppss (point)))))
                    (forward-char 1)
                    (when-let ((start (1+ (point))))
                      (forward-sexp 1)
                      (buffer-substring-no-properties start (1- (point)))))))
              (push (propertize id
                                :file file
                                :type type
                                :name id)
                    unused))))))
    unused))

(defun elisp-scan-find-all-unused-defs ()
  "Return unused definitions in project files."
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

(defun elisp-scan-find-files-with-matches (str files &optional command)
  "Exec COMMAND (`ag' or `find') to find FILES with occurences of STR."
  (delete-dups
   (mapcar (lambda (it)
             (plist-get it :file))
           (elisp-scan-find-lines-with-matches
            str
            files
            command))))

(defun elisp-scan-find-lines-with-matches (str files &optional command)
  "Exec COMMAND (`ag' or `find') to find FILES with occurences of STR.
Result is list of plists."
  (when-let ((fn
              (pcase command
                ("ag" 'elisp-scan--exec-ag)
                ("find" 'elisp-scan--exec-find)
                ((guard (executable-find "ag"))
                 'elisp-scan--exec-ag)
                (_ 'elisp-scan--exec-find))))
    (let ((processed-dirs)
          (files-with-matches))
      (dolist (dir-or-file files)
        (let ((dir (if (file-directory-p dir-or-file)
                       (expand-file-name (file-name-as-directory dir-or-file))
                     (file-name-directory dir-or-file))))
          (unless (member dir processed-dirs)
            (setq processed-dirs (push dir processed-dirs))
            (when-let ((found (funcall fn str dir)))
              (setq found (mapcar (lambda (file)
                                    (expand-file-name
                                     file dir))
                                  found))
              (setq files-with-matches (nconc
                                        files-with-matches
                                        found))))))
      (let ((lisp-re (elisp-scan-make-re str)))
        (mapcar (apply-partially #'elisp-scan-rename-parse-line str)
                (seq-filter (apply-partially #'string-match-p lisp-re)
                            (delete-dups files-with-matches)))))))

(defun elisp-scan-check-used-in-dirs-p (string &rest directories)
  "Return first filename with occurrence of STRING in DIRECTORIES."
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


(defun elisp-scan-remove-if-in-yas-snippets (unused)
  "Check whether UNUSED is used in snippet files."
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
  "Ask user to select an entry from choices and EXTRA prompting with PROMPT."
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
      (let ((id (elisp-scan-get-prop (nth idx grouped) :name))
            (file (elisp-scan-get-prop (nth idx grouped) :file))
            (type (elisp-scan-get-prop (nth idx grouped) :type))
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
                (when-let ((bounds (elisp-scan-find-item-bounds
                                    type
                                    id)))
                  (unless for-all
                    (unless (get-buffer-window (current-buffer))
                      (pop-to-buffer (current-buffer)))
                    (let ((cover-ov (make-overlay (car bounds)
                                                  (cdr bounds))))
                      (unwind-protect
                          (progn
                            (overlay-put cover-ov 'face 'error)
                            (overlay-put cover-ov 'after-string
                                         (propertize (concat "\s")
                                                     'face 'success))
                            (let ((c (if answer
                                         (elisp-scan-read-action
                                          "Remove:\s"
                                          '((?r "for all in file")
                                            (?!
                                             "repeat for all files")))
                                       (elisp-scan-read-action
                                        "Remove:\s"))))
                              (if (member c '(?! ?r))
                                  (setq for-all
                                        (pcase c
                                          (?! (setq for-all t))
                                          (?r (setq for-all file))))
                                (setq answer c))))
                        (delete-overlay cover-ov))))
                  (pcase answer
                    (?i (push file exlcuded-files))
                    (?y (delete-region (car bounds)
                                       (cdr bounds))
                        (while (looking-at "^\n[\n]")
                          (forward-line 1))
                        (while (looking-back "^\n[\n]+" 0)
                          (join-line)))
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
                       (delete-region (car bounds)
                                      (cdr bounds))
                       (while (looking-at "^\n[\n]")
                         (forward-line 1))
                       (while (looking-back "^\n[\n]+" 0)
                         (join-line))))))))))))))

(defvar elisp-scan-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'elisp-scan-mark)
    (define-key map (kbd "u") #'elisp-scan-unmark)
    (define-key map (kbd "U") #'elisp-scan-unmark-all)
    (define-key map (kbd "C-c M-u") #'elisp-scan-mark-unused)
    (define-key map (kbd "C-c M-c") #'elisp-scan-mark-commands)
    (define-key map (kbd "C-c M-e") #'elisp-scan-mark-externals)
    (define-key map (kbd "D") #'elisp-scan-remove-marked)
    (define-key map (kbd "C-c C-n") #'elisp-scan-next-entry-line)
    (define-key map (kbd "C-c C-p") #'elisp-scan-prev-entry-line)
    (define-key map (kbd ".") #'elisp-scan-list-menu)
    (define-key map (kbd "C-.") #'elisp-scan-list-menu)
    (define-key map (kbd "K") #'elisp-scan-cancel-timer)
    (define-key map (kbd "?") #'elisp-scan-list-menu)
    (define-key map (kbd "RET") #'elisp-scan-push-button)
    (define-key map (kbd "C-j") #'elisp-scan-push-button)
    (define-key map [tab] #'elisp-scan-toggle-entry-at-point)
    (define-key map [backtab] #'elisp-scan-toggle-expand-all)
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Keymap used in tabulated views.")

(defun elisp-scan-button-action (item)
  "Jump to ITEM."
  (let ((current-window (selected-window))
        (line (elisp-scan-get-prop item :line))
        (buff (or
               (get-file-buffer
                (elisp-scan-get-prop item :file))
               (find-file-noselect
                (elisp-scan-get-prop item :file))))
        (type (elisp-scan-get-prop item :type))
        (id (elisp-scan-get-prop item :name)))
    (with-selected-window (or
                           (get-buffer-window buff)
                           (window-right current-window)
                           (window-left current-window)
                           (split-window-right))
      (with-current-buffer buff
        (if line
            (progn
              (goto-char (point-min))
              (forward-line (1- line))
              (when id
                (re-search-forward (elisp-scan-make-re id)
                                   (line-end-position) t))
              (unless (get-buffer-window buff)
                (pop-to-buffer-same-window buff))
              (pulse-momentary-highlight-one-line))
          (when-let ((beg (elisp-scan-buffer-jump-to-form type id)))
            (goto-char beg)
            (let ((end (save-excursion
                         (forward-sexp)
                         (point))))
              (when (looking-back ";;;###autoload[\n]+" 0)
                (setq beg
                      (elisp-scan-re-search-backward
                       ";;;###autoload" nil t 1)))
              (unless (get-buffer-window buff)
                (pop-to-buffer-same-window buff))
              (pulse-momentary-highlight-region beg end))))))))

(defun elisp-scan-goto-start-of-entry ()
  "Return button data props from first column."
  (when-let ((id (tabulated-list-get-id)))
    (let ((prev-id))
      (while (and
              (equal (setq prev-id (tabulated-list-get-id))
                     id)
              (= (forward-line -1) 0))
        prev-id)
      (unless (equal prev-id id)
        (forward-line 1)))))


(defun elisp-scan-button-data-props-at-point ()
  "Return button data props from first column."
  (save-excursion
    (elisp-scan-goto-start-of-entry)
    (when-let ((entry (tabulated-list-get-entry)))
      (plist-get (text-properties-at 0 (aref entry 0)) 'button-data))))

(defun elisp-scan-tabulated-marked (&optional char)
  "Return marked entries.
If optional CHAR is non-nil, then only return items
marked with that character."
  (let (c list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq c (char-after))
        (unless (eq c ?\s)
          (if char
              (when (eq c char)
                (push (tabulated-list-get-id) list))
            (push (elisp-scan-button-data-props-at-point) list)))
        (forward-line)))
    list))

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

(defun elisp-scan-unused-in-file (file &optional files)
  "Check defined items in FILE are unused both in FILE and FILES.
Return alist of (SYMBOL-NAME . DEFINITION-TYPE)."
  (let ((alist (elisp-scan-with-temp-buffer
                   (insert (elisp-scan-get-file-or-buffer-content file))
                   (elisp-scan-get-unused-in-buffer))))
    (let ((files (delete file files))
          (not-used))
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
          (dolist (cell alist)
            (let ((symb (car cell))
                  (type (cadr cell)))
              (if (eq type 'declare-function)
                  (push cell not-used)
                (unless (save-excursion
                          (when (elisp-scan-re-search-backward
                                 (elisp-scan-make-re (symbol-name symb)) nil t
                                 1)
                            (when (re-search-backward
                                   "^;;;###FILE:\\([^\n]+\\)" nil t 1)
                              (match-string-no-properties 1))))
                  (push cell not-used))))))
      (mapcar
       (lambda (cell)
         (let ((l (cdr cell))
               (name (copy-sequence (symbol-name (car cell)))))
           (propertize
            name
            :name (substring-no-properties name)
            :type (symbol-name (car l))
            :file file)))
       not-used))))

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
                    (elisp-scan-get-prop it :name)
                    "  "
                    (elisp-scan-get-prop it :file))
                   (text-properties-at 0 it)))
          items))

(defvar-local elisp-scan-timer nil)
(defvar-local elisp-scan-files nil)
(defvar-local elisp-scan-files-total nil)
(defvar-local elisp-scan-cached-entries nil)
(defvar-local elisp-scan-opened-local-refs nil)
(defvar-local elisp-scan-opened-external-refs nil)
(defvar-local elisp-scan-filters elisp-scan-initial-filters)

(defun elisp-scan-rename-parse-line (search-str line-string)
  "Parse LINE-STRING with SEARCH-STR from ag or find output to plist."
  (let* ((line-pos (string-match-p ":[0-9]+:" line-string))
         (file (substring-no-properties line-string 0 line-pos))
         (rest (split-string (substring-no-properties line-string line-pos) ":"
                             t))
         (line (pop rest))
         (txt (string-join rest ":")))
    (list
     :text txt
     :name search-str
     :file file
     :line
     (string-to-number line))))

(defun elisp-scan-line-plist-to-button (plist)
  "Convert PLIST to clickable reference."
  (let ((file (plist-get plist :file))
        (text (plist-get plist :text)))
    (let ((btn (apply #'propertize (concat (file-name-nondirectory file) ":"
                                           text)
                      (list
                       'mouse-face 'highlight
                       'help-echo "Jump to item"
                       'button t
                       'follow-link t
                       'category t
                       'button-data plist
                       'keymap button-map
                       'action 'elisp-scan-button-action))))
      btn)))


(defun elisp-scan-sexp-declare-p (sexp)
  "Return non-nil if SEXP is declared form."
  (pcase sexp
    (`(defvar ,name)
     (list 'defvar name :declared-variable))
    (`(declare-function ,name)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file
                        ,_args)
     (list 'declare-function name))
    (`(declare-function ,name ,_file ,_args ,_fileonly)
     (list 'declare-function name))))

(defun elisp-scan-get-doc-from-sexp (sexp)
  "Return documentation from SEXP."
  (when (proper-list-p sexp)
    (let* ((type (car-safe sexp))
           (pos (and type
                     (cdr
                      (assq type elisp-scan-def-type-poses)))))
      (pcase type
        ('defvar-keymap (plist-get sexp :doc))
        ((guard (and pos
                     (eq type 'cl-defmethod)
                     (memq (nth 2 sexp) '(:around :after
                                                  :before))))
         (let ((value (nth (1+ pos) sexp)))
           (when (stringp value)
             value)))
        ((guard (and pos))
         (let ((value (nth pos sexp)))
           (when (stringp value)
             value)))))))

(defun elisp-scan-list-at-point ()
  "Return list at point."
  (when-let ((sexp (sexp-at-point)))
    (when (proper-list-p sexp)
      sexp)))

(defun elisp-scan-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)


(defun elisp-scan-parse-at-point ()
  "Parse `sexp-at-point' at point."
  (let* ((item (elisp-scan-list-at-point))
         (type (car-safe item)))
    (when (and type
               (symbolp type)
               (not (nth 4 (syntax-ppss (point)))))
      (let ((doc (elisp-scan-get-doc-from-sexp item))
            (declaration (elisp-scan-sexp-declare-p item))
            (autoload-cookies
             (save-excursion
               (forward-line -1)
               (when (looking-at ";;;###")
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))))
        (pcase type
          ((guard declaration)
           (list declaration))
          ((or 'use-package 'use-package!)
           (let ((data (save-excursion
                         (elisp-scan-parse-sexp-from-backward)))
                 (v `(use-package ,(nth 1 item))))
             (if data
                 (push v data)
               (list v))))
          ((or 'with-eval-after-load 'eval-when-compile
               'eval-after-load
               'straight-use-package 'if 'progn
               'and
               'if-let 'when-let 'with-no-warnings
               'when 'unless 'eval-and-compile)
           (save-excursion
             (elisp-scan-parse-sexp-from-backward)))
          ((or 'setq 'setq-default)
           (remove nil (seq-map-indexed (lambda (v i)
                                          (if (eq (logand i 1) 0)
                                              (list type v)
                                            nil))
                                        (seq-drop item 1))))
          ((or 'require 'provide)
           (list (append (list (car-safe item)
                               (elisp-scan-unquote (nth 1 item)))
                         (seq-drop item 2))))
          ((guard
            (and (assq type elisp-scan-interactive-types)
                 (eq 'interactive
                     (if doc
                         (car-safe
                          (car-safe
                           (cdr-safe
                            (member doc
                                    item))))
                       (car-safe (nth
                                  (cdr
                                   (assq type
                                         elisp-scan-interactive-types))
                                  item))))))
           (list (list (car item)
                       (cadr item)
                       (caddr item)
                       doc
                       :interactive
                       :autoload autoload-cookies)))
          ((guard
            (assq type elisp-scan-non-defun-command-types))
           (list (append
                  (seq-take item
                            (cdr
                             (assq type
                                   elisp-scan-non-defun-command-types)))
                  (list doc :interactive
                        :autoload autoload-cookies))))
          ((guard (assq type (append
                              elisp-scan-custom-types
                              elisp-scan-var-types)))
           (let* ((i (cdr (assq type (append elisp-scan-custom-types
                                             elisp-scan-var-types))))
                  (value (seq-take item (1- i))))
             (list (nconc value (list doc :autoload autoload-cookies)))))
          ((guard (assq type elisp-scan-def-type-poses))
           (let ((value
                  (seq-take item (cdr (assq type elisp-scan-def-type-poses)))))
             (list (nconc value (list doc :autoload autoload-cookies)))))
          ((or 'add-hook 'remove-hook)
           (list item))
          ((guard (and (or (special-form-p type)
                           (macrop type))
                       (listp (cdr item))))
           (list (seq-take item 2))))))))

(defun elisp-scan-parse-sexp-from-backward ()
  "Scan sexp at point from backward."
  (forward-sexp 1)
  (backward-char 1)
  (elisp-scan-parse-backward))

(defun elisp-scan-parse-backward ()
  "Recursively scan backward forms."
  (let ((items))
    (while (and (elisp-scan-backward-list)
                (looking-at "[(]"))
      (setq items (nconc items (elisp-scan-parse-at-point))))
    items))

(defun elisp-scan-buffer ()
  "Scan current buffer."
  (let ((items))
    (elisp-scan-with-every-top-form
        (setq items (nconc items (elisp-scan-parse-at-point))))
    (nreverse items)))

(defun elisp-scan-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (with-temp-buffer
    (let ((status (apply #'call-process command nil t nil
                         (flatten-list args))))
      (let ((result (string-trim (buffer-string))))
        (if (= 0 status)
            result
          (message result) nil)))))

(defun elisp-scan--exec-ag (str dir)
  "Search for occurrence of STR with `ag' in DIR.
Return list of absolute files."
  (when-let* ((found  (elisp-scan-call-process
                       "ag" "--nogroup" "--nocolor"
                       "-Q"
                       "--"
                       str
                       dir)))
    (split-string found "[\n\r\f]" t)))

(defun elisp-scan--exec-find (str file)
  "Search for occurrence of STR with find in FILE.
Return list of absolute files."
  (when-let* ((output  (split-string
                        (shell-command-to-string
                         (string-join
                          (list "find"
                                (shell-quote-argument file)
                                "-type f -print0 | xargs -0 -e grep -nH -e"
                                (shell-quote-argument str))
                          "\s"))
                        "\n"
                        t)))
    (delq nil (mapcar (lambda (it)
                        (when-let ((line-start (string-match-p
                                                ":[0-9]+:" it)))
                          ;; (substring-no-properties it 0 line-start)
                          it))
                      output))))

(defun elisp-scan-find-refs-in-project (str &optional command)
  "Execute COMMAND to find project files with occurrences of STR."
  (elisp-scan-find-lines-with-matches str
                                      (elisp-scan-find-project-files)
                                      command))

(defun elisp-scan-file-pred (file plist)
  "Return t if PLIST's property :file equals to FILE."
  (when-let ((plist-file (elisp-scan-get-prop plist
                                              :file)))
    (file-equal-p file plist-file)))

(defun elisp-scan-read-symbol-to-rename ()
  "Read a symbol and new name from minibuffer.
Return cons with symbol to rename and new-name."
  (let ((name (or (thing-at-point 'symbol t)
                  (read-string
                   "Symbol to rename:")))
        (new-name))
    (while (eq t (catch 'other
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (use-local-map
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "C->")
                                        (lambda ()
                                          (interactive)
                                          (setq name nil)
                                          (throw 'other t)))
                            (set-keymap-parent map (current-local-map))
                            map))
                         (when (and name
                                    (minibuffer-window-active-p
                                     (selected-window)))
                           (insert name)
                           (backward-word)))
                     (unless name
                       (setq name (read-string
                                   "Symbol to rename:"
                                   name)))
                     (setq new-name (read-string
                                     (format "Rename %s to: " name)))))))
    (cons name new-name)))

(defun elisp-scan--replace (beg end &optional replacement replace-fn no-confirm)
  "Replace region between BEG and END with REPLACEMENT with confirmation.
If REPLACE-FN is a function, call it without arguments instead of
`replace-region-content' or `delete-region'.
If NO-CONFIRM is non nil, don't prompt."
  (let ((confirmed (or no-confirm (elisp-scan-confirm-replace beg end
                                                              replacement))))
    (when confirmed
      (if (and replace-fn (functionp replace-fn))
          (funcall replacement)
        (delete-region beg end)
        (insert replacement)))))

(defvar elisp-scan-batch-current-file nil)

(defun elisp-scan-calc-percentage (value total)
  "Calculate VALUE percentage of TOTAL as 100%."
  (round (/ (* 100 value)
            total)))

(defun elisp-scan-batch-pop-current-buffer (&rest _)
  "If buffer of `elisp-scan-batch-current-file' is not visible, show it."
  (when-let ((buff
              (and elisp-scan-batch-current-file
                   (get-file-buffer elisp-scan-batch-current-file))))
    (unless (get-buffer-window buff)
      (if (minibuffer-selected-window)
          (with-minibuffer-selected-window (pop-to-buffer buff)
                                           (when (and (bound-and-true-p
                                                       hs-minor-mode)
                                                      (fboundp 'hs-show-all))
                                             (hs-show-all)))
        (pop-to-buffer buff)
        (when (and (bound-and-true-p
                    hs-minor-mode)
                   (fboundp 'hs-show-all))
          (hs-show-all))))))

(defun elisp-scan-batch-with-every-file (fn files)
  "Call FN with every item from FILES and show PROGRESS-MESSAGE."
  (save-some-buffers)
  (unwind-protect
      (progn
        (add-hook 'minibuffer-setup-hook #'elisp-scan-batch-pop-current-buffer)
        (let ((max (length files))
              (percent 0))
          (dotimes (k max)
            (let ((file (nth k files)))
              (when (file-exists-p file)
                (setq elisp-scan-batch-current-file file)
                (setq percent (elisp-scan-calc-percentage k max))
                (message "%d of %d (%d%%) processing %s"
                         k
                         max
                         percent (file-name-base file))
                (delay-mode-hooks
                  (let ((buff (get-file-buffer file))
                        (buff-to-kill))
                    (unless buff
                      (setq buff-to-kill
                            (find-file-noselect file)))
                    (if
                        (eq (current-buffer)
                            (or buff buff-to-kill))
                        (funcall fn)
                      (with-current-buffer (or buff buff-to-kill)
                        (funcall fn)
                        (when (buffer-modified-p)
                          (message "File %s modified" file)
                          (save-buffer))))
                    (when buff-to-kill
                      (kill-buffer buff-to-kill)))))))))
    (remove-hook 'minibuffer-setup-hook #'elisp-scan-batch-pop-current-buffer)))


(defun elisp-scan-confirm-replace (beg end &optional replacement)
  "Make overlay between BEG and END and show REPLACEMENT at the END."
  (let ((cover-ov (make-overlay beg end)))
    (unwind-protect
        (progn
          (overlay-put cover-ov 'face 'error)
          (when (and replacement (not (string-empty-p replacement)))
            (overlay-put cover-ov 'after-string
                         (propertize (concat "\s"
                                             replacement)
                                     'face 'success)))
          (yes-or-no-p (if replacement "Replace?" "Remove?")))
      (delete-overlay cover-ov))))

(defun elisp-scan-render-chunk-in-buffer (buffer file)
  "Render symbols in FILE, used in other files to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (elisp-scan-cancel-timer)
      (message "Elisp-scan: Scanning %s" file)
      (when file
        (setq elisp-scan-cached-entries
              (nconc
               elisp-scan-cached-entries
               (elisp-scan-with-fake-file file
                   (elisp-scan-defs-with-refs))))
        (setq tabulated-list-entries (elisp-scan-render-defs
                                      elisp-scan-cached-entries
                                      elisp-scan-filters))
        (elisp-scan-rerender))
      (if-let ((next-file (pop elisp-scan-files)))
          (setq elisp-scan-timer
                (run-with-idle-timer 1 nil
                                     #'elisp-scan-render-chunk-in-buffer
                                     (current-buffer)
                                     next-file))
        (message "Elisp-scan: Done")))))

(defun elisp-scan-project-files-revert ()
  "Scan all project files in Tabulated List buffer."
  (elisp-scan-cancel-timer)
  (elisp-scan-project))

(defun elisp-scan-revert ()
  "Rescan and refresh tabulated view."
  (setq elisp-scan-cached-entries
        (seq-reduce
         (lambda (acc file)
           (message "Elisp-scan: rescanning %s" file)
           (setq acc (nconc
                      acc
                      (elisp-scan-with-fake-file file
                          (elisp-scan-defs-with-refs)))))
         (elisp-scan-get-files-from-cached-entries)
         '()))
  (setq tabulated-list-entries (elisp-scan-render-defs
                                elisp-scan-cached-entries
                                elisp-scan-filters)))

(defun elisp-scan-make-id (&rest props)
  "Make ID from PROPS."
  (string-join (seq-filter #'stringp props) "--"))
(defun elisp-scan-toggle-local-ref (plist)
  "Convert PLIST to clickable reference."
  (setq elisp-scan-opened-local-refs
        (if (member (plist-get plist :id) elisp-scan-opened-local-refs)
            (delete (plist-get plist :id) elisp-scan-opened-local-refs)
          (push (plist-get plist :id) elisp-scan-opened-local-refs)))
  (let ((refs elisp-scan-cached-entries))
    (setq tabulated-list-entries (elisp-scan-render-defs
                                  refs
                                  elisp-scan-filters))
    (elisp-scan-rerender)))

(defun elisp-scan-toggle-external-ref (plist)
  "Convert PLIST to clickable reference."
  (setq elisp-scan-opened-external-refs
        (if (member (plist-get plist :id) elisp-scan-opened-external-refs)
            (delete (plist-get plist :id) elisp-scan-opened-external-refs)
          (push (plist-get plist :id) elisp-scan-opened-external-refs)))
  (let ((refs elisp-scan-cached-entries))
    (setq tabulated-list-entries (elisp-scan-render-defs refs
                                                         elisp-scan-filters))
    (elisp-scan-rerender)))


(defun elisp-scan-get-files-from-cached-entries ()
  "Return list of uniq files from `elisp-scan-cached-entries'."
  (seq-uniq
   (mapcar (lambda (it)
             (plist-get it :file))
           elisp-scan-cached-entries)))


(defun elisp-scan-rerender-refs (&optional rescan)
  "Rerender tabulated entries.
If RESCAN is non nil recheck related file, othervise use cached items."
  (let ((refs (if rescan
                  (seq-reduce
                   (lambda (acc file)
                     (setq acc (nconc
                                acc
                                (elisp-scan-with-fake-file file
                                    (elisp-scan-defs-with-refs)))))
                   (elisp-scan-get-files-from-cached-entries)
                   '())
                elisp-scan-cached-entries))
        (filters elisp-scan-filters))
    (setq tabulated-list-entries
          (elisp-scan-render-defs refs
                                  filters))
    (elisp-scan-rerender)))

(defun elisp-scan-render-collapsed-content (refs)
  "Render expanded references REFS in tabulated views."
  (if (<= (length refs) 0)
      ""
    (let ((mapper (lambda (file)
                    (concat
                     (make-string 6 ?\ )
                     (elisp-scan-line-plist-to-button file)))))
      (concat "\n"
              (mapconcat mapper refs "\n")))))

(defun elisp-scan-defs-with-refs ()
  "Scan current file for definitions from `elisp-scan-types-symbols'.
Return list of plists.
Each plist consists of such props:
:sym - definition name, as symbol
:name - name of the symbol as string
:type-sym - symbol of definition type (e.g. defun, defvar etc)
:type - definition type converted to string
:id - generated id for tabulated view
:file - filename of the scanned buffer
:local-refs - references of name in scanned file
:ext-refs - external references of name in other files
:unused - t or nil, whether item is unused.
:interactive - t or nil, whether item is interactive."
  (let* ((file buffer-file-name)
         (items (elisp-scan-buffer))
         (unused (mapcar #'car (elisp-scan-get-unused-in-buffer items))))
    (mapcar
     (lambda (it)
       (let* ((sym (cadr it))
              (type-sym (car it))
              (type-name (symbol-name type-sym))
              (name (symbol-name sym))
              (all-refs
               (if elisp-scan-permanent-dirs
                   (delete-dups
                    (append (elisp-scan-find-refs-in-project
                             name)
                            (elisp-scan-find-lines-with-matches
                             name
                             elisp-scan-permanent-dirs)))
                 (elisp-scan-find-refs-in-project
                  name)))
              (ext-refs (seq-remove
                         (apply-partially #'elisp-scan-file-pred file)
                         all-refs)))
         (list
          :sym sym
          :interactive (and (memq :interactive it)
                            t)
          :name name
          :type-sym type-sym
          :type type-name
          :id (elisp-scan-make-id name type-name file)
          :file file
          :local-refs (seq-filter
                       (apply-partially #'elisp-scan-file-pred file)
                       all-refs)
          :ext-refs ext-refs
          :unused (and
                   (memq sym unused)
                   (not ext-refs)
                   t))))
     (seq-filter (lambda (it)
                   (memq (car-safe it)
                         elisp-scan-types-symbols))
                 items))))

(defun elisp-scan-all-pass (filters)
  "Create an unary predicate function from FILTERS.
Return t if every one of the provided predicates is satisfied by provided
 argument."
  (lambda (item)
    (not (catch 'found
           (dolist (filter filters)
             (unless (funcall filter item)
               (throw 'found t)))))))

(defun elisp-scan-make-indicator (count action data)
  "Create button with COUNT of children.
DATA should be an argument for ACTION."
  (funcall
   (if (fboundp 'buttonize) 'buttonize 'button-buttonize)
   (format "%s" count)
   action
   data
   "Click to expand"))

(defvar elisp-scan-unused-button
  (funcall
   (if (fboundp 'buttonize) 'buttonize 'button-buttonize)
   "UNUSED" 'elisp-scan-filter-unused))

(defun elisp-scan-plist-to-columns (plist)
  "Convert PLIST and REFS to list of columns."
  (let ((id (plist-get plist :id))
        (name (plist-get plist :name))
        (local-refs (plist-get plist :local-refs))
        (ext-refs (plist-get plist :ext-refs))
        (unused (plist-get plist :unused))
        (file (plist-get plist :file))
        (cmd (plist-get plist :interactive))
        (type (plist-get plist :type)))
    (list
     (funcall
      (if (fboundp 'buttonize) 'buttonize 'button-buttonize) name
      'elisp-scan-button-action
      plist)
     type
     (if cmd
         (funcall
          (if (fboundp 'buttonize) 'buttonize 'button-buttonize)
          "COMMAND"
          'elisp-scan-filter-commands
          plist)
       "")
     (if unused elisp-scan-unused-button "")
     (file-name-nondirectory file)
     (elisp-scan-make-indicator
      (length local-refs)
      'elisp-scan-toggle-local-ref plist)
     (concat
      (elisp-scan-make-indicator
       (length ext-refs)
       'elisp-scan-toggle-external-ref
       plist)
      (mapconcat #'elisp-scan-render-collapsed-content
                 (remove nil
                         (list
                          (and (member id
                                       elisp-scan-opened-external-refs)
                               ext-refs)
                          (and (member id
                                       elisp-scan-opened-local-refs)
                               local-refs)))
                 "")))))

(defun elisp-scan-render-defs (items &optional filters)
  "Render ITEMS with FILTERS."
  (let ((filtered-items
         (if filters
             (seq-filter (elisp-scan-all-pass filters)
                         items)
           items)))
    (mapcar
     (lambda (it)
       (list
        (plist-get it :id)
        (apply #'vector (elisp-scan-plist-to-columns
                         it))))
     filtered-items)))

(defun elisp-scan--unused-pred (plist)
  "Return non nil if PLIST's property :unused is non nil."
  (plist-get plist :unused))

(defun elisp-scan--command-pred (plist)
  "Return non nil if PLIST's property :interactive is non nil."
  (not (plist-get plist :interactive)))

(defun elisp-scan--ext-pred (plist)
  "Return non nil if PLIST's property :ext-refs is non nil."
  (plist-get plist :ext-refs))


(defun elisp-scan-toggle-filter (fn)
  "Push or pop symbol function FN from `elisp-scan-filters'."
  (setq elisp-scan-filters
        (if (memq fn elisp-scan-filters)
            (delq fn elisp-scan-filters)
          (push fn elisp-scan-filters))))

(defun elisp-scan-make-toggle-description (description value &optional on-label
                                                       off-label)
  "Enhance DESCRIPTION for VALUE with ON-LABEL or OFF-LABEL."
  (concat  (or description "")
           (propertize " "
                       'display
                       '(space :align-to 30))
           "("
           (if value
               (propertize  (or on-label "+") 'face
                            'transient-argument)
             (propertize  (or off-label "-") 'face
                          'transient-inactive-value))
           ")") )

(defun elisp-scan-mark-by-pred (pred)
  "Mark entries, that satisfies function PRED.
PRED should accepts one argument - plist."
  (require 'text-property-search)
  (save-excursion
    (goto-char (point-min))
    (while (text-property-search-forward 'tabulated-list-id)
      (when-let ((props (elisp-scan-button-data-props-at-point)))
        (when (funcall pred props)
          (tabulated-list-put-tag "*"))))))

(defun elisp-scan-rerender ()
  "Populate the current Tabulated List mode buffer and restore marked."
  (let ((marked-ids (mapcar (elisp-scan-rpartial #'plist-get :id)
                            (elisp-scan-tabulated-marked))))
    (tabulated-list-print t)
    (when marked-ids
      (elisp-scan-mark-by-pred (lambda (pl)
                                 (member
                                  (plist-get pl :id)
                                  marked-ids))))))

;;;###autoload
(defun elisp-scan-mark-commands ()
  "Mark interactive entries."
  (interactive)
  (elisp-scan-mark-by-pred (elisp-scan-rpartial
                            #'plist-get :interactive)))

;;;###autoload
(defun elisp-scan-mark-externals ()
  "Mark entries with external references."
  (interactive)
  (elisp-scan-mark-by-pred 'elisp-scan--ext-pred))

;;;###autoload
(defun elisp-scan-mark-unused ()
  "Mark unused entries."
  (interactive)
  (elisp-scan-mark-by-pred 'elisp-scan--unused-pred))

;;;###autoload
(defun elisp-scan-mark ()
  "Mark a tabulated entry at point and move to the next one."
  (interactive)
  (require 'text-property-search)
  (save-excursion
    (elisp-scan-goto-start-of-entry)
    (tabulated-list-put-tag "*" t))
  (text-property-search-forward 'tabulated-list-id))


;;;###autoload
(defun elisp-scan-unmark ()
  "Unmark a tabulated entry at point and move to the previous one."
  (interactive)
  (require 'text-property-search)
  (save-excursion
    (elisp-scan-goto-start-of-entry)
    (tabulated-list-put-tag " " t))
  (text-property-search-backward 'tabulated-list-id))

;;;###autoload
(defun elisp-scan-unmark-all (&optional char)
  "Unmark all marked entries.
If optional CHAR is non-nil, then only return items
marked with that character."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (pcase (char-after)
        (?\s nil)
        ((pred (apply-partially 'eq char))
         (tabulated-list-put-tag " "))
        (_ (tabulated-list-put-tag " ")))
      (forward-line))))


;;;###autoload
(defun elisp-scan-remove-marked ()
  "Mark a tabulated entry and move to the next line."
  (interactive)
  (let ((orig-buff (current-buffer))
        (groups (elisp-scan-group-by
                 :file
                 (or
                  (elisp-scan-tabulated-marked)
                  (when-let ((props
                              (elisp-scan-button-data-props-at-point)))
                    (list props)))))
        (removed-counter 0))
    (dolist (group groups)
      (let ((file (car group))
            (file-items (cdr group)))
        (dolist (item file-items)
          (setq item (pop file-items))
          (with-current-buffer (find-file-noselect file)
            (when-let ((bounds (elisp-scan-find-item-bounds
                                (plist-get item :type)
                                (plist-get item :sym))))
              (delete-region (car bounds)
                             (cdr bounds))
              (delete-blank-lines)
              (setq removed-counter (1+ removed-counter)))
            (unless file-items
              (save-buffer))))))
    (when (> removed-counter 0)
      (message "Removed %d" removed-counter)
      (when (buffer-live-p orig-buff)
        (with-current-buffer orig-buff
          (revert-buffer))))))


;;;###autoload
(defun elisp-scan-next-entry-line ()
  "Goto to the next entry."
  (interactive)
  (require 'text-property-search)
  (text-property-search-forward 'tabulated-list-id))


;;;###autoload
(defun elisp-scan-prev-entry-line ()
  "Go to the previous entry."
  (interactive)
  (require 'text-property-search)
  (text-property-search-backward 'tabulated-list-id))


;;;###autoload
(defun elisp-scan-push-button ()
  "Activate closest button at point."
  (interactive)
  (push-button (save-excursion
                 (skip-chars-forward "\s\t*")
                 (point))))

;;;###autoload
(defun elisp-scan-cancel-timer ()
  "Reset `elisp-scan-timer'."
  (interactive)
  (when (timerp elisp-scan-timer)
    (cancel-timer elisp-scan-timer))
  (setq elisp-scan-timer nil))


;;;###autoload
(defun elisp-scan-rename-symbol (&optional old-name new-name files)
  "Rename symbol with OLD-NAME to NEW-NAME in FILES or project files."
  (interactive)
  (pcase-let* ((`(,name . ,renamed-name)
                (cond ((and old-name new-name)
                       (cons old-name new-name))
                      ((and old-name)
                       (cons old-name
                             (read-string
                              (format "Rename %s to: " old-name)
                              old-name)))
                      ((and new-name)
                       (cons (read-string
                              (format (format "Symbol to rename to %s: "
                                              new-name)
                                      old-name))
                             new-name))
                      (t (elisp-scan-read-symbol-to-rename))))
               (regexp (elisp-scan-make-re name)))
    (unless files
      (setq files
            (elisp-scan-find-files-with-matches
             name
             (elisp-scan-get-files-to-check))))
    (elisp-scan-batch-with-every-file
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (let ((shown nil)
               (case-fold-search nil))
           (while (re-search-forward regexp nil t 1)
             (let ((beg (match-beginning 0))
                   (end (match-end 0)))
               (unless shown
                 (setq shown (current-buffer))
                 (pop-to-buffer-same-window shown))
               (elisp-scan--replace beg end renamed-name))))))
     files)))
;;;###autoload
(defun elisp-scan-all-unused-defs ()
  "Check every project file for unused definitions."
  (interactive)
  (with-current-buffer (elisp-scan-project)
    (setq elisp-scan-filters (list 'elisp-scan--unused-pred))
    (elisp-scan-rerender-refs)))

;;;###autoload
(defun elisp-scan-project (&optional arg)
  "Scan current project and show report.
When prefix ARG is non-nil, prompt project."
  (interactive "P")
  (let* ((default-directory (expand-file-name (if arg
                                                  (project-prompt-project-dir)
                                                (project-root
                                                 (project-current t)))))
         (files (if arg
                    (elisp-scan-find-project-files)))
         (buff (get-buffer-create (format "*elisp-scan-report-%s*"
                                          default-directory))))
    (with-current-buffer buff
      (elisp-scan-cancel-timer)
      (unless (derived-mode-p 'elisp-scan-report-mode)
        (elisp-scan-report-mode))
      (add-hook 'tabulated-list-revert-hook
                #'elisp-scan-project-files-revert nil t)
      (setq elisp-scan-files (elisp-scan-find-project-files))
      (setq elisp-scan-cached-entries nil)
      (setq elisp-scan-files-total (length files))
      (elisp-scan-render-chunk-in-buffer
       (current-buffer)
       (pop elisp-scan-files))
      (display-buffer buff))
    buff))


;;;###autoload
(defun elisp-scan-dir (directory)
  "Scan elisp files in the DIRECTORY and show the report."
  (interactive (list (read-directory-name "Directory")))
  (sit-for 0.5)
  (let* ((entries (seq-reduce
                   (lambda (acc file)
                     (message "Scanning %s" (file-name-nondirectory
                                             file))
                     (setq acc (nconc
                                acc
                                (elisp-scan-with-fake-file file
                                    (elisp-scan-defs-with-refs)))))
                   (directory-files-recursively
                    directory
                    "\\.el$" nil nil)
                   '())))
    (with-current-buffer
        (get-buffer-create
         (format "*elisp-scan-report-%s*" (abbreviate-file-name directory)))
      (elisp-scan-report-mode)
      (setq elisp-scan-cached-entries entries)
      (setq tabulated-list-entries (elisp-scan-render-defs
                                    elisp-scan-cached-entries
                                    elisp-scan-filters))
      (add-hook 'tabulated-list-revert-hook
                #'elisp-scan-revert nil t)
      (tabulated-list-print)
      (display-buffer (current-buffer)))))


;;;###autoload
(defun elisp-scan-file (file)
  "Scan the FILE and show the report."
  (interactive (list (read-file-name "File: " (when buffer-file-name
                                                (file-name-nondirectory
                                                 buffer-file-name)))))
  (let* ((entries (with-current-buffer (find-file-noselect file)
                    (elisp-scan-defs-with-refs))))
    (with-current-buffer
        (get-buffer-create
         (format "*elisp-scan-report-%s" file))
      (elisp-scan-report-mode)
      (setq elisp-scan-cached-entries entries)
      (setq tabulated-list-entries (elisp-scan-render-defs
                                    elisp-scan-cached-entries
                                    elisp-scan-filters))
      (add-hook 'tabulated-list-revert-hook
                #'elisp-scan-revert nil t)
      (tabulated-list-print)
      (display-buffer (current-buffer)))))

(defalias 'elisp-scan-current-file #'elisp-scan-file
  "Scan the FILE and show the report.")


;;;###autoload
(defun elisp-scan-toggle-entry-at-point ()
  "Collapse or expand references for entry at point."
  (interactive)
  (save-excursion
    (elisp-scan-goto-start-of-entry)
    (when-let* ((entry (tabulated-list-get-entry))
                (data (plist-get (text-properties-at 0 (aref entry 0))
                                 'button-data))
                (idx (plist-get data :id)))
      (let ((val (or (member idx elisp-scan-opened-external-refs)
                     (member idx elisp-scan-opened-local-refs))))
        (if val
            (progn (setq elisp-scan-opened-local-refs
                         (delete idx
                                 elisp-scan-opened-local-refs))
                   (setq elisp-scan-opened-external-refs
                         (delete idx
                                 elisp-scan-opened-external-refs)))
          (setq elisp-scan-opened-local-refs
                (push idx
                      elisp-scan-opened-local-refs))
          (setq elisp-scan-opened-external-refs
                (push idx
                      elisp-scan-opened-external-refs))))))
  (let ((refs elisp-scan-cached-entries))
    (setq tabulated-list-entries (elisp-scan-render-defs
                                  refs
                                  elisp-scan-filters))
    (elisp-scan-rerender)))

;;;###autoload
(defun elisp-scan-toggle-expand-all-local-refs ()
  "Toggle showing local references."
  (interactive)
  (setq elisp-scan-opened-local-refs
        (if elisp-scan-opened-local-refs
            nil
          (mapcar (lambda (pl)
                    (plist-get pl :id))
                  elisp-scan-cached-entries)))
  (let ((refs elisp-scan-cached-entries))
    (setq tabulated-list-entries
          (elisp-scan-render-defs refs
                                  elisp-scan-filters))
    (elisp-scan-rerender)))

;;;###autoload
(defun elisp-scan-toggle-expand-all-external-refs ()
  "Toggle showing external references."
  (interactive)
  (setq elisp-scan-opened-external-refs
        (if elisp-scan-opened-external-refs
            nil
          (mapcar (lambda (pl)
                    (plist-get pl :id))
                  elisp-scan-cached-entries)))
  (let ((refs elisp-scan-cached-entries))
    (setq tabulated-list-entries
          (elisp-scan-render-defs refs
                                  elisp-scan-filters))
    (elisp-scan-rerender)))

;;;###autoload
(defun elisp-scan-toggle-expand-all ()
  "Toggle showing all references."
  (interactive)
  (let ((vars '(elisp-scan-opened-external-refs
                elisp-scan-opened-local-refs))
        (val (or elisp-scan-opened-external-refs
                 elisp-scan-opened-local-refs))
        (ids))
    (setq ids (unless val
                (mapcar (elisp-scan-rpartial #'plist-get :id)
                        elisp-scan-cached-entries)))
    (dolist (var vars)
      (set var ids))
    (let ((refs elisp-scan-cached-entries))
      (setq tabulated-list-entries
            (elisp-scan-render-defs refs
                                    elisp-scan-filters))
      (elisp-scan-rerender))))

;;;###autoload
(defun elisp-scan-filter-externals (&rest _)
  "Toggle whether to show only items used in other files."
  (interactive)
  (setq elisp-scan-filters
        (if (memq 'elisp-scan--ext-pred elisp-scan-filters)
            (delq 'elisp-scan--ext-pred elisp-scan-filters)
          (delq 'elisp-scan--unused-pred elisp-scan-filters)
          (push 'elisp-scan--ext-pred elisp-scan-filters)))
  (elisp-scan-rerender-refs))

;;;###autoload
(defun elisp-scan-filter-commands (&rest _)
  "Toggle whether to show only unused items."
  (interactive)
  (elisp-scan-toggle-filter 'elisp-scan--command-pred)
  (elisp-scan-rerender-refs))


;;;###autoload
(defun elisp-scan-filter-unused (&rest _)
  "Toggle whether to show only unused items."
  (interactive)
  (elisp-scan-toggle-filter 'elisp-scan--unused-pred)
  (elisp-scan-rerender-refs))


;;;###autoload
(define-derived-mode elisp-scan-report-mode tabulated-list-mode
  "Elisp Scan Report."
  "Show report about scanned definitions."
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Type" 10 t)
         ("Interactive" 10 t)
         ("Status" 10 t)
         ("File" 15 t)
         ("Local" 5 (lambda (a b)
                      (> (string-to-number (aref (cadr a) 5))
                         (string-to-number (aref (cadr b) 5)))))
         ("External" 5 (lambda (a b)
                         (> (string-to-number (aref (cadr a) 6))
                            (string-to-number (aref (cadr b) 6)))))])
  (tabulated-list-init-header)
  (setq tabulated-list-padding 2)
  (setq-local imenu-prev-index-position-function
              (lambda ()
                (unless (bobp)
                  (forward-line -1))))
  (setq-local imenu-extract-index-name-function
              (lambda ()
                (unless (bobp)
                  (string-trim
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))))
  (use-local-map elisp-scan-report-mode-map))


;;;###autoload (autoload 'elisp-scan-menu "elisp-scan.el" nil t)
(transient-define-prefix elisp-scan-menu ()
  "Command dispatcher for `elisp-scan'."
  ["Report"
   [("f" "Scan file" elisp-scan-file)
    ("p" "Scan project" elisp-scan-project)
    ("d" "Scan directory" elisp-scan-dir)
    ("A" "Project for unused definitions"
     elisp-scan-all-unused-defs
     :inapt-if-not project-current)]]
  ["Misc"
   ("i" "Remove unused with ivy" elisp-scan-ivy-read-unused-items)
   ("r" "Rename symbol"
    elisp-scan-rename-symbol)])


;;;###autoload (autoload 'elisp-scan-list-menu "elisp-scan.el" nil t)
(transient-define-prefix elisp-scan-list-menu ()
  "Transient menu for `elisp-scan-report-mode'."
  :transient-non-suffix #'transient--do-exit
  ["Toggle filter"
   [("s" elisp-scan-filter-unused
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Show unused"
                     (memq 'elisp-scan--unused-pred elisp-scan-filters)))
     :transient t)
    ("e"
     elisp-scan-filter-externals
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Show external"
                     (memq 'elisp-scan--ext-pred elisp-scan-filters)))
     :transient t)
    ("i"
     elisp-scan-filter-commands
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Exclude interactive"
                     (memq 'elisp-scan--command-pred elisp-scan-filters)))
     :transient t)]]
  ["Show references"
   [("<tab>" "Toggle entry refs" elisp-scan-toggle-entry-at-point
     :transient t)
    ("a" elisp-scan-toggle-expand-all
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Expand all"
                     (and elisp-scan-opened-external-refs
                          elisp-scan-opened-local-refs)))
     :transient t)
    ("l" elisp-scan-toggle-expand-all-local-refs
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Expand local"
                     elisp-scan-opened-local-refs))
     :transient t)
    ("r"
     elisp-scan-toggle-expand-all-external-refs
     :description (lambda ()
                    (elisp-scan-make-toggle-description
                     "Expand external"
                     elisp-scan-opened-external-refs))
     :transient t)]]
  ["Marks"
   [("C-SPC" "Mark" elisp-scan-mark :transient t)
    ("u" "Unmark" elisp-scan-unmark :transient t)
    ("U" "Unmark all" elisp-scan-unmark-all :transient t)
    ("mu" "Mark unused" elisp-scan-mark-unused :transient t)
    ("mc" "Mark commands" elisp-scan-mark-commands :transient t)
    ("me" "Mark externals" elisp-scan-mark-externals :transient t)]]
  [("C-p" "Previous entry" elisp-scan-prev-entry-line
    :transient t)
   ("C-n" "Next entry" elisp-scan-next-entry-line
    :transient t)
   ("D" "Remove marked or item at point" elisp-scan-remove-marked)
   ("K" "Stop rendering" elisp-scan-cancel-timer
    :inapt-if-not
    (lambda ()
      (timerp elisp-scan-timer)))]
  (interactive)
  (if (derived-mode-p 'elisp-scan-report-mode)
      (transient-setup 'elisp-scan-list-menu)
    (elisp-scan-menu)))

;;; ivy
(defvar ivy-last)
(defvar elisp-scan-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'elisp-scan-ivy-remove-item)
    map))

;;;###autoload
(defun elisp-scan-query-remove-unused ()
  "Query remove unused definitions."
  (interactive)
  (elisp-scan-remove-unused (elisp-scan-remove-if-in-yas-snippets
                             (elisp-scan-remove-if-commands
                              (elisp-scan-find-all-unused-defs))))
  (save-some-buffers))

;;;###autoload
(defun elisp-scan-ivy-remove-item ()
  "Remove current ivy item without exiting minibuffer."
  (interactive)
  (when-let* ((item (ivy-state-current ivy-last))
              (file (elisp-scan-get-prop item :file))
              (cands
               (with-minibuffer-selected-window
                 (pcase-let ((`(,beg . ,end)
                              (elisp-scan-find-item-bounds
                               (elisp-scan-get-prop item :type)
                               (elisp-scan-get-prop item :name))))
                   (delete-region beg end)
                   (while (looking-at "^\n[\n]")
                     (forward-line 1))
                   (while (looking-back "^\n[\n]+" 0)
                     (join-line)))
                 (save-buffer)
                 (ivy--kill-current-candidate)
                 (elisp-scan-unused-trasnform-items
                  (elisp-scan-unused-in-file
                   file
                   (elisp-scan-get-files-to-check))))))
    (ivy-update-candidates cands)))


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
     :action (lambda (item)
               (let ((file (elisp-scan-get-prop item :file))
                     (id (elisp-scan-get-prop item :name))
                     (type (elisp-scan-get-prop item :type)))
                 (if (minibuffer-window-active-p (selected-window))
                     (with-minibuffer-selected-window
                       (with-current-buffer
                           (find-file-noselect file)
                         (when-let ((bounds
                                     (elisp-scan-find-item-bounds type id)))
                           (unless (get-buffer-window (current-buffer))
                             (pop-to-buffer-same-window (current-buffer)))
                           (elisp-scan-highlight-bounds (car bounds)
                                                        (cdr bounds)))))
                   (with-current-buffer
                       (find-file-noselect file)
                     (when-let ((bounds (elisp-scan-find-item-bounds type id)))
                       (unless (get-buffer-window (current-buffer))
                         (pop-to-buffer-same-window (current-buffer)))
                       (elisp-scan-highlight-bounds (car bounds)
                                                    (cdr bounds)))))))
     :keymap elisp-scan-ivy-map
     :caller 'elisp-scan-ivy-read-unused-items
     :dynamic-collection t
     :multi-action (lambda (cands)
                     (setq marked cands)))
    (elisp-scan-remove-unused marked)))

(provide 'elisp-scan)
;;; elisp-scan.el ends here