;;; orgtt.el --- Creating truth tables in org.       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nick Spain

;; Author: Nick Spain <nspain@arrax>
;; Version: 0.0.1
;; Keywords: Utility

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Creating truth tables in org.  I found it was pretty easy and very
;; useful to use org's spreadsheet capabilities to creat truth tables
;; for my assignments.  However, these was still a lot of copy pasting
;; and boring work filling in the tables, particuarly if there were
;; more than a few variables in the formula.  This is a great job for
;; a computer though, they don't get bored!  So here it is, truth
;; tables in org mode.

;; To use it, just call `orgtt-create-table', you will be prompted for
;; the formula you want to build the truth table for.  Enter this and
;; the rest will be done for you!

;; Formula syntax:
;; -   - negation
;; +   - logical or
;; .   - logical and
;; ->  - implication
;; <-> - biimplication
;; <+> - exclusive or
;;
;; You can add your own syntax and define your own connectives by
;; adding a mapping to `orgtt-connective-alist'.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'org-table)

(defgroup orgtt nil
  "Customisation group for orgtt."
  :group 'utility)

(defcustom orgtt-connective-alist '(("-"   . orgtt--negate)
				    ("+"   . orgtt--lor)
				    ("."   . orgtt--land)
				    ("->"  . orgtt--implication)
				    ("<->" . orgtt--biimplication)
				    ("<+>" . orgtt--xor))
  "Associative list of allowed connective in formulae."
  :type 'alist
  :group 'orgtt)

(defcustom orgtt-use-binary nil
  "Display boolean values as 1 or 0 in tables."
  :type 'boolean
  :group 'orgtt)

(defun orgtt--to-bool (x)
  "Convert X to a boolean (t/nil).

We want to convert everything to booleans because it will make
them easier to work with.  Emacs Lisp has short circuit
evaluation for its functions like `and' and `or', if we're not
using t or nil then we will get funny return values."
  (cond ((booleanp x) x)
	((and (stringp x) (or (string-equal x "t") (string-equal x "nil")))
	 (string-equal x "t"))
	((and (numberp x) (or (= x 1) (= x 0))) (= x 1))
	(t (user-error (s-join " " '("You are trying to convert something that is"
				"not the strings 't' or 'nil', the digits 1 or 0, or a"
				"boolean.  I have stopped because the results could be"
				"confusing"))))))

(defun orgtt--bool-to-binary (b)
  "Convert a boolean value B to binary (i.e. t -> 1, nil -> 0)."
  (if (not (booleanp b))
      (user-error "Cannot convert non-boolean type to binary")
    (if b 1 0)))

(defun orgtt--pad-list (l n pad)
  "Left pad a list L so it has length N, list is filled with PAD."
  (if (> n (length l))
      (cl-concatenate 'list (make-list (- n (length l)) pad) l)
    l))

(defun orgtt--get-valuations (n)
  "Return a list containg a list of all valuations for N variables."
  (mapcar #'(lambda (l) (mapcar #'orgtt--to-bool (orgtt--pad-list l n 0)))
	  (cl-loop for i from 0 upto (- (expt 2 n) 1) collect (orgtt--to-binary-list i))))

(defun orgtt--string-head (s)
  "Get the first character in S as a string."
  (s-left 1 s))
(defun orgtt--string-tail (s)
  "Get all but the first characters in a string S."
  (s-right (-1 (length s)) s))

(defun orgtt--get-vars (formula)
  "Get the variables in FORMULA."
  (s-split "" (cl-remove-if #'(lambda (c) (or (< c 65) (> c 90)))
			    (s-upcase formula))
	   t))

(defun orgtt--to-binary-list (n)
  "Convert N to list of its binary digits."
  (cond ((= n 0) (list 0))
	((= n 1) (list 1))
	(t (nconc (orgtt--to-binary-list (truncate n 2))
		  (list (mod n 2))))))

(defun orgtt--org-table-align-string (s)
  "Align a string representation of a table S using `org-table-align'."
  (with-temp-buffer
    (insert s)
    (org-table-align)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun orgtt--negate (x)
  "Negate X."
  (not (orgtt--to-bool x)))

(defun orgtt--lor (x y)
  "Logical or between X and Y."
  (or (orgtt--to-bool x) (orgtt--to-bool y)))

(defun orgtt--land (x y)
  "Logical and between X and Y."
  (and (orgtt--to-bool x) (orgtt--to-bool y)))

(defun orgtt--implication (x y)
  "X implies Y."
  (or (orgtt--negate x) (orgtt--to-bool y)))

(defun orgtt--biimplication (x y)
  "X iff Y."
  (and (orgtt--implication x y) (orgtt--implication y x)))

(defun orgtt--xor (x y)
  "Exclusive or between X and Y."
  (let ((x (orgtt--to-bool x))
	(y (orgtt--to-bool y)))
    (or (and (not x) y) (and x (not y)))))

(defun orgtt--build-table-row (elements)
  "Build a table row for `org-mode' containing ELEMENTS."
  (if (null elements)
      "|"
    (concat (format "| %s " (car elements))
	    (orgtt--build-table-row (cdr elements)))))


(defun orgtt--build-table-header (vars formula)
  "Build the header for a truth table for VARS for FORMULA."
  (orgtt--build-table-row (reverse (cons formula (reverse vars)))))

(defun orgtt--build-table-body (vars &optional binaryp)
  "Build the body for a truth tables with VARS in binary depending BINARYP."
    (s-join "\n" (mapcar #'(lambda (xs)
			     (s-concat (orgtt--build-table-row
					(if binaryp
					    (mapcar #'orgtt--bool-to-binary xs)
					xs)) " |"))
			 (orgtt--get-valuations (length vars)))))

(defun orgtt--build-orgtbl-formula-aux (cursor rest connectives)
  "`orgtt--build-orgtbl-formula' helper, takes CURSOR, REST and CONNECTIVES.

This function does the majority of the work for
`orgtt--build-orgtbl-formula' by translating a given formula into
one which can be used by orgtbl."
  (cond (;; Add the next part of the formula to the cursor if the
	 ;; cursor string is a substring of multiple keys in the
	 ;; alist so that we can narrow down the possible
	 ;; connectives.
	 (orgtt--multiple-pos-keys cursor connectives)
	 (orgtt--build-orgtbl-formula-aux (concat cursor (orgtt--string-head rest))
					  (orgtt--string-tail rest)
					  connectives))))

(defun orgtt--build-orgtbl-formula (formula &optional connective-alist)
  "Build the orgtbl formula to calculate FORMULA outcomes with CONNECTIVE-ALIST."
  (let ((connective-list (or connective-alist orgtt-connective-alist)))))

(defun orgtt--build-table-divider (n)
  "Build a divider for N variables to go between the header and the body."
  (s-concat "|" (s-repeat (- n 1) "-+") "-|"))

(defun orgtt--create-table (formula &optional binaryp)
  "Create and return the truth table for FORMULA as a string in binary if BINARYP."
  (let ((vars (orgtt--get-vars formula)))
    (orgtt--org-table-align-string
     (s-join "\n" (list (orgtt--build-table-header vars formula)
			(orgtt--build-table-divider (length vars))
			(orgtt--build-table-body vars binaryp))))))

(defun orgtt--create-table-and-solve (formula &optional binaryp)
  "Create the fully completed truth table for FORMULA as a string, in binary if BINARYP."
  (let ((vars (orgtt--get-vars formula)))
    (orgtt--org-table-align-string
     (s-join "\n" (list (orgtt--build-table-header vars formula)
			(orgtt--build-table-divider (length vars))
			(orgtt--build-table-body vars binaryp)
			(format "TBLFM: $%d=%s"
				(+ (length vars) 1)
				(orgtt--build-orgtbl-formula formula)))))))

;;;###autoload
(defun orgtt-create-table (formula)
  "Insert the truth table for FORMULA.

Formula should be made up of variables (letters) and connectives
defined in `orgtt-connective-alist'."
  (interactive "sFormula: ")
  (insert (orgtt--create-table formula orgtt-use-binary)))

;;;###autoload
(defun orgtt-create-table-and-solve (formula)
  "Insert a filled out truth table for FORMULA.

Formula should be made up of variables (letters) and connectives
defined in `orgtt-connective-alist'."
  (interactive "sFormula: ")
  (insert (orgtt--create-table-and-solve formula orgtt-use-binary)))

(provide 'orgtt)
;;; orgtt.el ends here
