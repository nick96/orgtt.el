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

(defsubst orgtt--to-bool (x)
  "Convert X to a 'boolean' (t/nil).

We want to convert everything to booleans because it will make
them easier to work with.  Emacs Lisp has short circuit
evaluation for its functions like `and' and `or', if we're not
using t or nil then we will get funny return values."
  (if x t nil))

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

(defun orgtt--get-vars (formula)
  "Get the variables in FORMULA."
  (s-split "" (cl-remove-if #'(lambda (c) (or (< c 65) (> c 90)))
			    (s-upcase formula))
	   t))

(defun orgtt--build-table-row (elements)
  "Build a table row for `org-mode' containing ELEMENTS."
  (if (null elements)
      "|"
    (concat (format "| %s " (car elements))
	    (orgtt--build-table-row (cdr elements)))))

(defun orgtt--to-binary-list (n)
  "Convert N to list of its binary digits."
  (cond ((= n 0) (list 0))
			     ((= n 1) (list 1))
			     (t (nconc (orgtt--to-binary-list (truncate n 2))
				       (list (mod n 2))))))

(defun orgtt--pad-list (l n pad)
  "Pad a list L so it has length N, list is filled with PAD."
  (if (> n (length l))
      (cl-concatenate 'list (make-list (- n (length l)) pad) l)
    l))

(defun orgtt--get-valuations (n)
  "Return a list containg a list of all valuations for N variables."
  (mapcar #'(lambda (l) (orgtt--pad-list l n 0))
	  (cl-loop for i from 0 upto (- (expt 2 n) 1) collect (orgtt--to-binary-list i)))):

(defun orgtt--build-table-header (vars formula)
  "Build the header for a truth table for VARS for FORMULA."
  (orgtt--build-table-row (reverse (cons formula (reverse vars)))))

(defun orgtt--build-table-body (vars)
  "Build the body for a truth tables with VARS."
    (s-join "\n" (mapcar #'orgtt--build-table-row (orgtt--get-valuations (length vars)))))

;; TODO: Draw state machine to model parsing of formula
(defun orgtt--build-orgtbl-formula (formula)
  "Build the orgtbl formula to calculate FORMULA outcomes."
  formula)

(defun orgtt--create-table (formula)
  "Create and return the truth table for FORMULA as a string."
  (let ((vars (orgtt--get-vars formula)))
    (s-join "\n" (list (orgtt--build-table-header vars formula)
		       (orgtt--build-table-body vars)
		       (format "TBLFM: $%d=%s"
			       (+ (length vars) 1)
			       (orgtt--build-orgtbl-formula formula))))))

;;;###autoload
(defun orgtt-create-table (formula)
  "Insert a filled out truth table for FORMULA.

Formula should be made up of variables (letters) and connectives
defined in `orgtt-connective-alist'."
  (insert (orgtt--create-table formula)))

(provide 'orgtt)
;;; orgtt.el ends here
