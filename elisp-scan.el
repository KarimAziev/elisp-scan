;;; elisp-scan.el --- Configure scan -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-scan
;; Keywords: lisp
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

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


;;; Commands

;; M-x `elisp-scan-report-project-refs'
;;      Scan project files for symbols, used in other files and print report.

;; M-x `elisp-scan-find-file-dependents' (file)
;;      Scan FILE for symbols, used in other files and print report.

;; M-x `elisp-scan-cancel-timer'
;;      Render symbols in FILE which is used in other files to BUFFER.

;; M-x `elisp-scan-ivy-read-unused-items' (&optional arg)
;;      Read unused definitions of current file with ivy.
;;      To remove one item without exiting minibuffer \<elisp-scan-ivy-map>use `\[elisp-scan-ivy-remove-item]'.
;;      To remove or backup batch of items, mark them.
;;      With optional prefix ARG include only current file.

;; M-x `elisp-scan-ivy-remove-item'
;;      Remove current ivy item without exiting minibuffer.

;; M-x `elisp-scan-query-remove-unused'
;;      Query remove unused definitions.

;; M-x `elisp-scan-all-unused-defs'
;;      Check every project file for unused definitions.

;; M-x `elisp-scan-current-file'
;;      Show unused items in the current file.

;; M-x `elisp-scan-render-file-report'
;;      Render file report.

;; M-x `elisp-scan-jump'
;;      Return list of filtered TYPES.

;;; Customization

;; `elisp-scan-permanent-dirs'
;;      In which directories always check usage.

;; `elisp-scan-archive-dir'
;;      Where to write backup files.

;; `elisp-scan-types-symbols'
;;      Symbols which should always be checked.

;;; Code:

(require 'project)
(require 'transient)
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

(defun elisp-scan-buffer-filter-types ()
  "Return list of filtered TYPES."
  (seq-filter (lambda (it)
                (memq (car it) elisp-scan-types-symbols))
              (elisp-scan-buffer)))

;;;###autoload
(defun elisp-scan-jump ()
  "Scan buffer and jump to item."
  (interactive)
  (let* ((items (elisp-scan-buffer))
         (alist (mapcar (lambda (it)
                          (cons (format "%s %s" (car it)
                                        (nth 1 it))
                                it))
                        items))
         (choice (cdr (assoc (completing-read "Item" alist) alist))))
    (elisp-scan-buffer-jump-to-form (car choice)
                                    (nth 1 choice))))

(defun elisp-scan-buffer-jump-to-form (type name)
  "Search for definition with TYPE and NAME."
  (let ((re (elisp-scan-make-re (symbol-name name)))
        (found))
    (save-excursion
      (goto-char (point-max))
      (while (and (not found)
                  (elisp-scan-re-search-backward re nil t))
        (let ((parse-sexp-ignore-comments t))
          (setq found (ignore-errors (forward-sexp -1)
                                     (when (eq type (symbol-at-point))
                                       (backward-up-list 1)
                                       (when-let ((sexp
                                                   (sexp-at-point)))
                                         (when (and (eq (car-safe sexp)
                                                        type)
                                                    (eq (nth 1 sexp)
                                                        name))
                                           (point)))))))))
    (when found
      (goto-char found)
      found)))

(defun elisp-scan-buffer-filter-get-unused-in-buffer ()
  "Return list of filtered TYPES."
  (let ((items (elisp-scan-buffer))
        (unused))
    (dolist (l items)
      (let ((type (car-safe l))
            (symb (nth 1 l)))
        (when (memq type elisp-scan-types-symbols)
          (when (elisp-scan-buffer-jump-to-form type symb)
            (let* ((name (symbol-name symb))
                   (re (elisp-scan-make-re (symbol-name symb)))
                   (used (or
                          (save-excursion
                            (elisp-scan-re-search-backward re nil t 1))
                          (save-excursion
                            (forward-sexp 1)
                            (elisp-scan-re-search-forward re nil t 1)))))
              (unless used
                (push (cons name (if (memq :interactive l)
                                     "interactive"
                                   (symbol-name type)))
                      unused)))))))
    unused))


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

(defun elisp-scan-if-yas-snipptes (unused)
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

(defvar elisp-scan-unused-items nil)

(defun elisp-scan-highlight-confirm (&optional start end prompt)
  "Highlight region between START and END with PROPS and ask PROMPT."
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
              (join-line))
            (save-buffer)))))))

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

(defvar elisp-scan-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'elisp-scan-render-file-report)
    map)
  "Keymap used in tabulated views.")

(define-derived-mode elisp-scan-report-mode tabulated-list-mode
  "Elisp Scan Report."
  "Show report gathered about unused definitions."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Type" 30 t)
         ("File" 30)])
  (tabulated-list-init-header)
  (use-local-map elisp-scan-report-mode-map))

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
              (elisp-scan-render-file-report))))))))

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

(defvar-local elisp-scan-related-files nil)
(defvar-local elisp-scan-report-file nil)

;;;###autoload
(defun elisp-scan-render-file-report ()
  "Render file report."
  (interactive)
  (let* ((buff (current-buffer))
         (items (when (and elisp-scan-report-file
                           elisp-scan-related-files)
                  (elisp-scan-unused-in-file
                   elisp-scan-report-file
                   elisp-scan-related-files))))
    (with-current-buffer buff
      (setq tabulated-list-entries
            (mapcar #'elisp-scan-report-convert items))
      (tabulated-list-print))))

(defun elisp-scan-display-file-report (file)
  "Check unused items in FILE and show report."
  (let* ((buff-name (format "*elisp-scan-%s*" file))
         (buff (get-buffer buff-name))
         (files (or (when (buffer-live-p buff)
                      (buffer-local-value
                       'elisp-scan-related-files buff))
                    (if (and elisp-scan-permanent-dirs
                             (yes-or-no-p
                              (format "Include %s?"
                                      (string-join
                                       elisp-scan-permanent-dirs
                                       "\s"))))
                        (elisp-scan-get-files-to-check)
                      (elisp-scan-find-project-files)))))
    (with-current-buffer (get-buffer-create buff-name)
      (unless (derived-mode-p 'elisp-scan-report-mode)
        (elisp-scan-report-mode)
        (add-hook 'tabulated-list-revert-hook
                  #'elisp-scan-render-file-report nil t))
      (setq elisp-scan-report-file file)
      (setq elisp-scan-related-files files)
      (elisp-scan-render-file-report)
      (unless (get-buffer-window (current-buffer))
        (display-buffer (current-buffer))))))

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
        (setq alist (elisp-scan-buffer-filter-get-unused-in-buffer))
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
  (elisp-scan-display-file-report (or buffer-file-name
                                      (read-file-name "File"))))

;;;###autoload
(defun elisp-scan-all-unused-defs ()
  "Check every project file for unused definitions."
  (interactive)
  (elisp-scan-display-report
   (if (yes-or-no-p "Include commands?")
       (elisp-scan-if-yas-snipptes
        (elisp-scan-find-all-unused-defs))
     (elisp-scan-remove-if-commands
      (elisp-scan-find-all-unused-defs)))))

;;;###autoload
(defun elisp-scan-query-remove-unused ()
  "Query remove unused definitions."
  (interactive)
  (save-some-buffers)
  (setq elisp-scan-unused-items (elisp-scan-if-yas-snipptes
                                 (elisp-scan-remove-if-commands
                                  (elisp-scan-find-all-unused-defs))))
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

(defvar-local elisp-scan-timer nil)
(defvar-local elisp-scan-files nil)
(defvar-local elisp-scan-files-total nil)

(defun elisp-scan-intern-cars (alist)
  "Intern cars in ALIST."
  (mapcar
   (lambda (it)
     (setcar it (intern-soft
                 (car it)))
     it)
   alist))


(defun elisp-scan-find-refs-filter-lines (str lines)
  "Return LINES with exact matches of STR."
  (let ((lisp-re (elisp-scan-make-re str)))
    (delete nil
            (seq-filter (apply-partially #'string-match-p lisp-re)
                        lines))))

(defun elisp-scan-rename-parse-line (line-string)
  "Parse LINE-STRING from ag or find output to plist."
  (let* ((line-pos (string-match-p ":[0-9]+:" line-string))
         (file (substring-no-properties line-string 0 line-pos))
         (rest (split-string (substring-no-properties line-string line-pos) ":"
                             t))
         (line (pop rest))
         (txt (string-join rest ":")))
    (list
     :text txt
     :rest txt
     :file file
     :line
     (string-to-number line))))

(defvar elisp-scan-export-types
  (mapcar #'intern '("provide")))

(defvar elisp-scan-modes-types
  (elisp-scan-intern-cars '(("define-minor-mode" . 2)
                         ("define-derived-mode" . 4)
                         ("define-generic-mode" . 8)
                         ("define-compilation-mode" .
                          3)
                         ("easy-mmode-define-minor-mode"
                          . 2))) )


(defvar elisp-scan-interactive-types
  (elisp-scan-intern-cars
   '(("defun" . 3)
     ("defsubst" . 3)
     ("cl-defun" . 3)
     ("cl-defsubst" . 3))))

(defvar elisp-scan-non-defun-command-types
  (elisp-scan-intern-cars
   '(("defhydra" . 3)
     ("transient-define-prefix" . 3))))

(defvar elisp-scan-func-types
  (elisp-scan-intern-cars
   '(("defun" . 3)
     ("defmacro" . 3)
     ("defsubst" . 3)
     ("defalias" . 4)
     ("defhydra" . 3)
     ("transient-define-prefix" . 3)
     ("transient-define-suffix" . 3)
     ("transient-define-argument" . 3)
     ("transient-define-infix" . 3)
     ("cl-defun" . 3)
     ("cl-defsubst" . 3)
     ("cl-defmacro" . 3)
     ("cl-defgeneric" . 3)
     ("cl-defmethod" . 3))))

(defvar elisp-scan-var-types (elisp-scan-intern-cars
                           '(("defvar" . 3)
                             ("defconst" . 3)
                             ("defvar-local" . 3))))

(defvar elisp-scan-custom-types (elisp-scan-intern-cars
                              '(("defface" . 3)
                                ("defcustom" . 3)
                                ("defgroup" . 3)
                                ("deftheme" . 3)))
  "Doc.")

(defvar elisp-scan-def-type-poses
  (append
   (elisp-scan-intern-cars
    '(("define-skeleton" . 2)
      ("ert-deftest" . 3)
      ("define-widget" . 3)
      ("easy-mmode-define-minor-mode"
       . 2)
      ("defclass" . 4)
      ("cl-defstruct" . 3)))
   elisp-scan-var-types
   elisp-scan-custom-types
   elisp-scan-func-types
   elisp-scan-modes-types))

(defun elisp-scan-sexp-declare-p (sexp)
  "Return non-nil if SEXP is declared form."
  (pcase sexp
    (`(defvar ,name)
     (list :declared-variable name))
    (`(declare-function ,name)
     (list :declare-function name))
    (`(declare-function ,name
                        ,_file)
     (list :declare-function name))
    (`(declare-function ,name
                        ,_file
                        ,_args)
     (list :declare-function name))
    (`(declare-function ,name ,_file ,_args ,_fileonly)
     (list :declare-function name))))

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
            (declaration (elisp-scan-sexp-declare-p item)))
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
                 (eq 'interactive (if doc
                                      (car-safe
                                       (car-safe
                                        (cdr-safe
                                         (member doc
                                                 item))))
                                    (nth
                                     (cdr
                                      (assq type
                                            elisp-scan-interactive-types))
                                     item)))))
           (list (list (car item)
                       (cadr item)
                       (caddr item)
                       :interactive
                       doc)))
          ((guard
            (assq type elisp-scan-non-defun-command-types))
           (list (append
                  (seq-take item (cdr
                                  (assq type
                                        elisp-scan-non-defun-command-types)))
                  (list :interactive))))
          ((guard (assq type (append
                              elisp-scan-custom-types
                              elisp-scan-var-types)))
           (let* ((i (cdr (assq type (append elisp-scan-custom-types
                                             elisp-scan-var-types))))
                  (value (seq-take item (1- i))))
             (list (nconc value (list doc)))))
          ((guard (assq type elisp-scan-def-type-poses))
           (let ((value
                  (seq-take item (cdr (assq type elisp-scan-def-type-poses)))))
             (list (nconc value (list doc)))))
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


(defun elisp-scan-pad-right (str limit)
  "Pad STR with spaces on the right to increase the length to LIMIT."
  (let ((width (string-width str)))
    (if (<= width limit)
        str
      (truncate-string-to-width str limit nil nil t t))))

(defun elisp-scan-buffer-group ()
  "Scan current buffer."
  (elisp-scan-group-with 'car (elisp-scan-buffer)
                         (lambda (it)
                           (elisp-scan-unquote (cadr it)))))

(defun elisp-scan-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defvar elisp-scan-columns
  '((:title "Type"
            :width 10)
    (:title "Name"
            :width 50)
    (:columns
     ((:title ""
              :width 1
              :value (lambda (&rest _) ""))
      (:title "File"
              :width 40
              :value
              (lambda (it)
                (buttonize
                 (abbreviate-file-name (plist-get it :file))
                 'elisp-scan-find-file-action
                 (seq-copy it)
                 "Find file")))
      (:title "Context"
              :value
              (lambda (it)
                (replace-regexp-in-string (regexp-quote (plist-get it :id))
                                          (propertize (plist-get it :id) 'face
                                                      'success)
                                          (plist-get it :text))))))))

(defun elisp-scan-find-file-action (props)
  "Jump to file specified in plist PROPS at line."
  (let ((file (plist-get props :file))
        (line (plist-get props :line))
        (wnd (selected-window)))
    (when file
      (with-selected-window wnd
        (find-file-other-window file)
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (pulse-momentary-highlight-one-line))))))

(defun elisp-scan-render-row (item &optional id columns total)
  "Render row ITEM with ID, COLUMNS and TOTAL.
TOTAL is max width and defaults to 100."
  (let* ((inhibit-read-only t)
         (total (or total 100))
         (open t)
         (col-len (length columns))
         (row-beg (point))
         (children)
         (sizes)
         (subcolumns)
         (indicator))
    (setq indicator "")
    (dotimes (idx (length columns))
      (let ((column (nth idx columns))
            (is-last (= (1+ idx) col-len)))
        (let ((field-name (or (plist-get column :field-name)
                              idx))
              (value
               (let ((value-getter (or (plist-get column :value)
                                       idx)))
                 (cond ((functionp value-getter)
                        (funcall value-getter item))
                       ((numberp value-getter)
                        (nth value-getter item)))))
              (width (if is-last
                         total
                       (plist-get column :width)))
              (format-fn (plist-get column :format))
              (formatted-value)
              (beg (point))
              (label)
              (subcols (plist-get column :columns)))
          (setq total (- total width))
          (when subcols
            (setq children value)
            (setq subcolumns subcols))
          (setq formatted-value
                (cond ((and subcols)
                       (concat indicator (format "%s " (length value))))
                      ((stringp format-fn)
                       (format format-fn value))
                      ((functionp format-fn)
                       (funcall format-fn value))
                      ((consp format-fn)
                       format-fn)
                      ((stringp value) value)
                      (t value)))
          (setq label (cond ((and subcols)
                             formatted-value)
                            ((stringp formatted-value)
                             (elisp-scan-pad-right (string-trim
                                                 formatted-value)
                                                width))
                            ((numberp formatted-value)
                             (elisp-scan-pad-right
                              (string-trim (format "%s" formatted-value))
                              formatted-value))
                            ((stringp (car-safe formatted-value))
                             (elisp-scan-pad-right (car formatted-value)
                                                width))
                            (t "")))
          (if (and (not subcols)
                   (consp label))
              (apply #'insert-button (list (car label)
                                          (cdr label)))
            (insert label))
          (push (1+ width) sizes)
          (unless is-last
            (let ((space (- (1+ width)
                            (string-width label))))
              (insert (make-string (if (> space 0) space 1) ?\s))))
          (add-text-properties beg (point)
                               `(field-name ,field-name
                                            tabulated-list-id
                                            ,id)))))
    (setq sizes (reverse sizes))
    (when (and open subcolumns children)
      (dolist (child children)
        (newline)
        (insert (make-string 1 ?\ ))
        (elisp-scan-render-row child id subcolumns)))
    (add-text-properties row-beg (point)
                         `(tabulated-list-id ,id))))

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
                          (substring-no-properties it 0 line-start)))
                      output))))

(defun elisp-scan-find-in-project (str)
  "Execute `ag' or `find' to find FILES with occurrences of STR."
  (when-let ((fn
              (pcase command
                ((guard (executable-find "ag"))
                 'elisp-scan--exec-ag)
                (_ 'elisp-scan--exec-find))))
    (let ((processed-dirs)
          (lines-with-matches)
          (files (let* ((pr (project-current t default-directory))
                        (pr-dir (expand-file-name (project-root pr)))
                        (all-files (project-files pr))
                        (all-dirs
                         (remove pr-dir
                                 (delete-dups (mapcar #'file-name-directory
                                                      all-files))))
                        (root-files
                         (seq-filter (lambda (it)
                                       (and (file-readable-p it)
                                            (not (file-directory-p it))))
                                     (directory-files
                                      pr-dir t
                                      directory-files-no-dot-files-regexp))))
                   (append (seq-intersection root-files all-files) all-dirs))))
      (dolist (dir-or-file files)
        (let ((dir (if (file-directory-p dir-or-file)
                       (expand-file-name (file-name-as-directory dir-or-file))
                     dir-or-file)))
          (unless (member dir processed-dirs)
            (setq processed-dirs (push dir processed-dirs))
            (when-let ((found (funcall fn str dir)))
              (setq found (mapcar (lambda (file)
                                    (expand-file-name
                                     file dir))
                                  found))
              (setq lines-with-matches (nconc
                                        lines-with-matches
                                        found))))))
      (mapcar (lambda (it)
                (when-let ((pl (elisp-scan-rename-parse-line it)))
                  (append pl (list :id str))))
              (elisp-scan-find-refs-filter-lines str
                                                 lines-with-matches)))))

(defun elisp-scan-find-references-in-file (file)
  "Return list of symbols from FILE used in other project files."
  (and file
       (file-exists-p file)
       (with-current-buffer (find-file-noselect
                             file)
         (let ((items (elisp-scan-buffer)))
           (delq nil
                 (mapcar
                  (lambda (it)
                    (when (assq (car it)
                                elisp-scan-def-type-poses)
                      (when-let* ((name
                                   (symbol-name
                                    (elisp-scan-unquote
                                     (nth 1 it))))
                                  (refs
                                   (seq-remove
                                    (lambda (it)
                                      (equal file
                                             (plist-get it
                                                        :file)))
                                    (elisp-scan-find-in-project
                                     name))))
                        (list (format "%s" (car it))
                              name
                              refs))))
                  items))))))

(defun elisp-scan-render-file-refs (file)
  "Render list of symbols from FILE used in other project files in BUFFER."
  (when-let ((group-items
              (and file
                   (elisp-scan-find-references-in-file file))))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert-button (abbreviate-file-name file)
                       'button-data `(:file ,file)
                       'face 'file-name-shadow
                       'action 'elisp-scan-find-file-action)
        (insert "\n")
        (dotimes (idx (length group-items))
          (elisp-scan-render-row (nth idx group-items) idx
                              elisp-scan-columns
                              100)
          (newline))))))

;;;###autoload
(defun elisp-scan-cancel-timer ()
  "Render symbols in FILE which is used in other files to BUFFER."
  (interactive)
  (when (timerp elisp-scan-timer)
    (cancel-timer elisp-scan-timer))
  (setq elisp-scan-timer nil))

(defun elisp-scan-render-chunk-in-buffer (buffer file)
  "Render symbols in FILE, used in other files to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (elisp-scan-cancel-timer)
      (let ((inhibit-read-only t))
        (when file
          (elisp-scan-render-file-refs file))
        (if-let ((next-file (pop elisp-scan-files)))
            (setq elisp-scan-timer
                  (run-with-idle-timer 1 nil
                                       'elisp-scan-render-chunk-in-buffer
                                       (current-buffer)
                                       next-file))
          (message "Elisp-scan: Done"))))))

(defun elisp-scan-report-refs-in-files (files)
  "Scan FILES for symbols, used in other files and print report."
  (let ((buff (with-current-buffer (get-buffer-create "*elisp-scan-report*")
                (when (timerp elisp-scan-timer)
                  (cancel-timer elisp-scan-timer))
                (setq buffer-read-only t)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (setq elisp-scan-files files)
                  (setq elisp-scan-files-total (length files))
                  (elisp-scan-render-chunk-in-buffer
                   (current-buffer)
                   (pop elisp-scan-files))
                  (current-buffer)))))
    (pop-to-buffer-same-window buff)))

;;;###autoload
(defun elisp-scan-find-file-dependents (file)
  "Scan FILE for symbols, used in other files and print report."
  (interactive (list (read-file-name "File: " nil nil t
                                     (when buffer-file-name
                                       (file-name-nondirectory
                                        buffer-file-name)))))
  (elisp-scan-report-refs-in-files (list file)))

;;;###autoload
(defun elisp-scan-report-project-refs ()
  "Scan project files for symbols, used in other files and print report."
  (interactive)
  (elisp-scan-report-refs-in-files (elisp-scan-get-files-to-check)))

;;;###autoload (autoload 'elisp-scan-transient "elisp-scan.el" nil t)
(transient-define-prefix elisp-scan-transient ()
  "Command dispatcher for `elisp-scan'."
  [[("c" "Show unused for current file" elisp-scan-current-file)
    ("i" "Remove unused with ivy" elisp-scan-ivy-read-unused-items)
    ("D" "Act on unused" elisp-scan-query-remove-unused)
    ("A" "Check whole project" elisp-scan-all-unused-defs)]
   [("f" "Scan file for symbols, used in other files"
     elisp-scan-find-file-dependents)
    ("p" "Scan all project files for cross-used symbols"
     elisp-scan-report-project-refs)]])

(provide 'elisp-scan)
;;; elisp-scan.el ends here