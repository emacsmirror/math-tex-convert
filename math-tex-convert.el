;;; math-tex-convert.el --- Convert LaTeX macros to unicode and back -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/math-tex-convert
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (math-symbol-lists "1.3") (auctex "12.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides some functions to interactively convert LaTeX
;; math macros into corresponding unicode characters and the other way
;; around.  You can also define your own translation rules that
;; override the defaults.

;;; Code:

;;; Dependencies

(require 'texmathp)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'math-symbol-lists)

;;; Predefined tables

(defvar math-tex-convert--macro-to-unicode-hash-table
  (make-hash-table :test 'equal
		   :size 3000)
  "Table mapping LaTeX macros to corresponding unicode characters.

Values are taken from `math-symbol-list-extended'.")

(defvar math-tex-convert--unicode-to-macro-hash-table
  (make-hash-table :test 'equal
		   :size 3000)
  "Table mapping unicode characters to corresponding LaTeX macros.

Values are taken from `math-symbol-list-extended'.")

(dolist (x math-symbol-list-extended)
  (when (string-prefix-p "\\" (nth 1 x))
    (puthash (nth 1 x) (nth 3 x)
	     math-tex-convert--macro-to-unicode-hash-table)))

(maphash (lambda (k v)
	   (puthash v k
		    math-tex-convert--unicode-to-macro-hash-table))
	 math-tex-convert--macro-to-unicode-hash-table)

;;; User settings

(defgroup math-tex-convert nil
  "Convert LaTeX macros to unicode and back."
  :prefix "math-tex-convert"
  :group 'convenience)

(defcustom math-tex-convert-user-defined-macro-to-unicode-map nil
  "Alist or hash-table of additional mappings.

The key is always a string that identifies a macro.  The value
can be a string, a list of strings or an empty string.

This map is looked up before the predefined one, so that the
values specified here override the predefined one.

For example (assuming this is an alist and not a hash table):

1.    (\"\\lambda\" . \"λ\")

2.    (\"\\lambda\" . nil)

3.    (\"\\lambda\" . (\"λ\" \"Λ\"))

The first mapping is self-explanatory.  Having mapping 2. means
that the macro \"\\lambda\" will always be ignored, regardless
of whether it is mapped to something in the predefined value in
`math-tex-convert--macro-to-unicode-hash-table'.

Having mapping 3. means that upon matching the macro \"\\lambda\"
you will be given the choice between \"λ\" and \"Λ\" in
interactive mode.  In non-interactive mode (i.e., when you opt to
\"convert all\"), the car of the list is always the replacement."
  :type '(alist :key-type (string  :tag "LaTeX macro")
                :value-type (repeat string))
  :group 'math-tex-convert)

(defcustom math-tex-convert-user-defined-unicode-to-macro-map nil
  "Alist or hash-table of additional mappings.

The key is always a string that identifies a macro.  The value
can be a string, a list of strings or an empty string.

This map is looked up before the predefined one, so that the
values specified here override the predefined one.

For example (assuming this is an alist and not a hash table):

1.    (\"λ\" . \"\\lambda\")

2.    (\"λ\" . nil)

3.    (\"λ\" . (\"\\lambda\" \"\\uplambda\"))

The first mapping is self-explanatory.  Having mapping 2. means
that \"λ\" will always be ignored, regardless
of whether it is mapped to something in the predefined value in
`math-tex-convert--unicode-to-macro-hash-table'.

Note that if you want replacement to ignore a given character,
say \"^\", only if it is escaped (i.e. preceded by a backslash),
you should add that to the list
`math-tex-convert-replace-only-if-escaped' instead.

Having mapping 3. means that upon matching \"λ\" you will be
given the choice between \"\\lambda\" and \"\\uplambda\" in
interactive mode.  In non-interactive mode (i.e., when you opt to
\"convert all\"), the car of the list is always the replacement."
  :type '(alist :key-type (string  :tag "Character")
                :value-type (repeat string))
  :group 'math-tex-convert)

(defcustom math-tex-convert-replace-only-if-escaped
  '("_" "^" "{" "}")
  "Strings to be converted only if escaped.

Escaped means that they are preceded by just one backslash."
  :type '(repeat string)
  :group 'math-tex-convert)

(defcustom math-tex-convert-ignore-predefined-tables nil
  "If non-nil, predifined tables are completely ignored.

All replacement are instead done by looking up
`math-tex-convert-user-defined-unicode-to-macro-map' or
`math-tex-convert-user-defined--macro-to-unicode-map'."
  :type 'boolean
  :group 'math-tex-convert)

(defcustom math-tex-convert-strings-never-to-be-replaced
  '("\\" "(" ")" "$" "[" "]")
  "List of strings that must never be replaced with a LaTeX macro.

Typically these are delimiters in math modes and special
characters."
  :type '(repeat string)
  :group 'math-tex-convert)

(defcustom math-tex-convert-before-replace-hook nil
  "Hooks to run before each replacement is performed."
  :type 'hook
  :group 'math-tex-convert)

(defcustom math-tex-convert-after-replace-hook nil
  "Hooks to run after each replacement is performed."
  :type 'hook
  :group 'math-tex-convert)



;;; Internal functions

(defsubst math-tex-convert--escape-string-p (string)
  "Return non-nil if length STRING is an odd integer."
  (eq (logand (length string) 1) 1))

(defsubst math-tex-convert--get-replacement (x)
  "Return car of X if X is a list, X otherwise."
  (if (listp x) (car x) x))

(defun math-tex-convert--option-loop (target replacement)
  "Ask user what to do with a potential target for substitution.

TARGET is a buffer substring at point that can be substituted.
REPLACEMENT is either a string or a list of strings."
  (let* ((options (if (listp replacement)
		      '((?n "skip")
			(32 "convert")
			(?/ "other replacement")
                        (61 "write in")
			(33 "convert all"))
		    '((?n "skip")
		      (32 "convert")
                      (61 "write in")
		      (33 "convert all"))))
         (choice
	  (read-multiple-choice
	   (format "%s ⟶ %s"
                   target
                   (math-tex-convert--get-replacement replacement))
           options)))
    (cond ((equal choice '(?n "skip"))
           '(skip . nil))
          ((equal choice '(61 "write in"))
           (let ((written (read-string ":")))
             `(convert . ,written)))
          ((equal choice '(32 "convert"))
           `(convert . ,(math-tex-convert--get-replacement replacement)))
	  ((equal choice '(33 "convert all"))
           `(all . ,(math-tex-convert--get-replacement replacement)))
	  ((and (equal choice '(?/ "other replacement"))
	        (listp replacement))
	   (math-tex-convert--option-loop target
				          (append (cdr replacement)
					          (list (car replacement))))))))

(defun math-tex-convert--replace (to-macro only-in-math)
  "Subroutine performing replacement between characters and LaTeX macros.

If TO-MACRO is non-nil, convert characters to LaTeX macros using
the values of `math-tex-convert--macro-to-unicode-hash-table' (LaTeX
macros will be converted to unicode characters).

If TO-MACRO is nil, convert LaTeX macros into characters using
the values of
`math-tex-convert--unicode-to-macro-hash-table' (unicode characters
will be converted to LaTeX macros).

If `math-tex-convert-user-defined-macro-to-unicode-map' or
`math-tex-convert-user-defined-unicode-to-macro-map' have a non-nil
value, use that value instead.

It ONLY-IN-MATH is non-nil, ignore matches that are not in a math
environment, as determined by `texmathp'."
  (let* ((table (cond (math-tex-convert-ignore-predefined-tables '())
                      (to-macro math-tex-convert--unicode-to-macro-hash-table)
                      (t math-tex-convert--macro-to-unicode-hash-table)))
         (user-table (if to-macro
                         math-tex-convert-user-defined-unicode-to-macro-map
                       math-tex-convert-user-defined-macro-to-unicode-map))
         (user-keys (map-keys user-table))
         (keys-re (thread-last
                    (append (map-keys table) user-keys)
                    (cl-delete-if
                     (lambda (x)
                       (member
                        x
                        math-tex-convert-strings-never-to-be-replaced)))
                    (regexp-opt)
                    (concat "\\(?1:\\\\*\\)")))
         (begin (if (use-region-p) (set-marker (make-marker)
                                               (region-beginning))
                  (set-marker (make-marker) (point-min))))
         (end (if (use-region-p)
                  (set-marker (make-marker) (region-end))
                (set-marker (make-marker) (point-max))))
         (number (count-matches keys-re begin end))
         (test (if only-in-math 'texmathp '(lambda () t)))
         (done 0)
         (wait t))
    (when (> 1 number) (user-error "Nothing to convert"))
    (when (use-region-p) (deactivate-mark))
    (goto-char begin)
    (unwind-protect
        (while (and (> (marker-position end) (point))
                    ;; If there is a replacement right at the end of the
                    ;; region then we should give up before trying to go
                    ;; on otherwise search will complain that the bound is
                    ;; before point
                    (re-search-forward keys-re end t))
          (let* ((end-of-escape (match-end 1))
                 (end-of-target (match-end 0))
                 (hlt (make-overlay end-of-escape end-of-target))
                 (target (buffer-substring end-of-escape end-of-target))
                 (escaped (math-tex-convert--escape-string-p
                           (match-string-no-properties 1)))
                 (only-if-esc (member target
                                      math-tex-convert-replace-only-if-escaped))
                 (replacement (if (member target user-keys)
                                  (map-elt user-table target)
                                ;; this way, if the key is mapped to
                                ;; nil in the user defined map, the
                                ;; replacement will be nil
                                (map-elt table target)))
                 (message-log-max nil))
            (overlay-put hlt 'face
                         `((nil (:background ,(face-attribute 'region
                                                              :background)))))
            (unwind-protect
                (when (and (save-match-data (funcall test))
                           (xor (and escaped only-if-esc)
                                (and (not escaped) (not only-if-esc)))
                           ;; do nothing and continue the loop if the
                           ;; replacement is nil
                           replacement)
                  (if wait
                      (let ((outcome (math-tex-convert--option-loop
                                      target
                                      replacement)))
                        (unless (equal (car outcome) 'skip)
                          (run-hooks 'math-tex-convert-before-replace-hook)
                          (let ((del-b (if escaped
                                           (1- end-of-escape)
                                         end-of-escape)))
                            (delete-region del-b end-of-target))
                          (when (and (not to-macro)
                                     (member
                                      (cdr outcome)
                                      math-tex-convert-replace-only-if-escaped))
                            ;; if the intended replacement unicode
                            ;; character is among the strings in
                            ;; math-tex-convert-replace-only-if-escaped,
                            ;; it means that we need to escape it!
                            (insert "\\"))
                          (insert (cdr outcome))
                          (run-hooks 'math-tex-convert-after-replace-hook)
                          (setq done (1+ done))
                          (when (equal (car outcome) 'all) (setq wait nil))))
                    (let ((del-b (if escaped
                                     (1- end-of-escape)
                                   end-of-escape))
                          (rep (math-tex-convert--get-replacement replacement)))
                      (delete-region del-b end-of-target)
                      (when (and (not to-macro)
                                 (member
                                  rep
                                  math-tex-convert-replace-only-if-escaped))
                        ;; same as above
                        (insert "\\"))
                      (insert rep))
                    (setq done (1+ done))))
              (delete-overlay hlt))))
      (message "%s replaced" done))))



;;; Interactive Functions

;;;###autoload
(defun math-tex-convert-to-unicode (&optional arg)
  "Replace math LaTeX macros with unicode characters.

If called while region is active, only replace in the region;
otherwise, on the whole accessible portion of the buffer.

If called with prefix argument ARG, only perform replacement
inside of LaTeX math environment (as determined by `texmathp')."
  (interactive "P")
  (save-excursion (math-tex-convert--replace nil arg)))

;;;###autoload
(defun math-tex-convert-to-macro (&optional arg)
  "Replace math characters with LaTeX macros.

If called while region is active, only replace in the region;
otherwise, on the whole accessible portion of the buffer.

If called with prefix argument ARG, only perform replacement
inside of LaTeX math environment (as determined by `texmathp')."
  (interactive "P")
  (save-excursion (math-tex-convert--replace t arg)))

;;;###autoload
(defun math-tex-convert-customize ()
  "Call the customize function with math-tex-convert as argument."
  (interactive)
  (customize-browse 'math-tex-convert))

(provide 'math-tex-convert)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; math-tex-convert.el ends here
