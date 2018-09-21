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

;;; TODO:

;; - Fix parsing of negations

;;; Code:

(require 'cl-lib)
(require 's)
(require 'org-table)
(require 'dash)

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
  (s-right (- (length s) 1) s))

(defun orgtt--get-vars (formula)
  "Get the variables in FORMULA."
  (-distinct (-sort 's-less-p (s-split "" (cl-remove-if #'(lambda (c) (or (< c 65) (> c 90)))
							(s-upcase formula))
				       t))))

(defun orgtt--to-binary-list (n)
  "Convert N to list of its binary digits."
  (cond ((= n 0) (list 0))
	((= n 1) (list 1))
	(t (nconc (orgtt--to-binary-list (truncate n 2))
		  (list (mod n 2))))))

(defun orgtt--org-table-align-string (s)
  "Align a string representation of a table S using `org-table-align'."
  (s-chomp (with-temp-buffer
	     (insert s)
	     (goto-char (point-min))
	     (org-table-align)
	     (buffer-substring-no-properties (point-min) (point-max)))))

(defun orgtt--insert-no-move (s)
  "Insert a string S into the current buffer but keep the cursor where it is."
  (save-excursion (insert s)))

(defmacro orgtt-defun-connective (name args &optional docstring &rest body)
  "Define a connective with NAME and ARGS, BODY defines what it does."
  (declare (indent defun)
	   (debug t))
  (let ((binaryp-exp
	 (-concat '(and)
		  (mapcar #'(lambda (x) `(and (numberp ,x) (memq ,x '(0 1))))
			  args)))
	(booleanised-args (mapcar #'(lambda (x) `(,x (orgtt--to-bool ,x)))
				  args)))
    `(defun ,name ,args
       ,docstring
       (let ((binaryp ,binaryp-exp))
	 (let (,@booleanised-args)
	   (if binaryp
	       (orgtt--bool-to-binary ,@body)
	     ,@body))))))


(orgtt-defun-connective orgtt--negate (x)
  "Negate x"
  (not x))

(orgtt-defun-connective orgtt--lor (x y)
  "Logical or between X and Y."
  (or x y))

(orgtt-defun-connective orgtt--land (x y)
  "Logical and between X and Y."
  (and x y))

(orgtt-defun-connective orgtt--implication (x y)
  "X implies Y."
  (or (orgtt--negate x) y))

(orgtt-defun-connective orgtt--biimplication (x y)
  "X iff Y."
  (and (orgtt--implication x y) (orgtt--implication y x)))

(orgtt-defun-connective orgtt--xor (x y)
  "Exclusive or between X and Y."
  (or (and (not x) y) (and x (not y))))

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

(defun orgtt--convert-to-prefix-notation (formula)
  "Convert FORMULA to prefix notation."
  formula)

(defun orgtt--replacement-mapping (s mapping-alist)
  "Replace occurances of keys in MAPPING-ALIST in string S with corresponding values."
  (let ((keys (mapcar #'car mapping-alist)))
    (dolist (key keys s)
      (setq s (s-replace key (symbol-name (cdr (assoc key mapping-alist))) s)))))

(defun orgtt--skip-parened-region (formula &optional paren-count)
  "Skip over a parenthesised region in FORMULA, keeping state with PAREN-COUNT."
  (let ((paren-count (or paren-count 0))
	(formula (if (listp formula)
		     formula
		   (s-split "" formula t))))
    (pcase formula
      (`("(" . ,xs) (orgtt--skip-parened-region xs (+ paren-count 1)))
      (`(")" . ,xs) (if (= paren-count 1)
			xs
		      (orgtt--skip-parened-region xs (- paren-count 1))))
      (`(,_ . ,xs) (orgtt--skip-parened-region xs paren-count)))))

(defun orgtt--remove-surrounding-parens (formula)
  "Remove parentheses surroundign FORMULA."
  (cond ((stringp formula)
	 (if (and (s-equals-p (s-left 1 formula) "(")
		  (s-equals-p (s-right 1 formula) ")"))
	     (substring formula 1 (- (length formula) 2))
	   formula))
	((listp formula)
	 (if (and (s-equals-p (-first-item formula) "(")
		  (s-equals-p (-last-item formula) ")"))
	     (-slice formula 1 (- (length formula) 2))
	   formula))))

(defun orgtt--build-prefix-formula (formula &optional connective-alist queue connective)
  "Convert FORMULA to prefix from with CONNECTIVE-ALIST, QUEUE and CONNECTIVE.

Currently this function will not work if FROMULA is surrounded in parentheses."
  (let ((connective-alist (or connective-alist orgtt-connective-alist))
	(queue (or queue '()))
	(connective (or connective ""))
	(formula (if (stringp formula)
		     (s-split "" formula t)
		   formula)))
    (pcase formula
      ;; When we reach an openning parenthesis, this signals the start
      ;; of a subexpression. We want to be able to use this as one of
      ;; the arguments to the connective of the current expression, so
      ;; we get it by calling this function on that region (it
      ;; terminates on a ')') and adding it to the variables queue. We
      ;; then use `orgtt--skip-parened-region' to jump over the
      ;; subexpression we've already parse and continue parsing the
      ;; expression.
      (`("(" . ,xs) (progn
		      (setq queue (-snoc queue (orgtt--build-prefix-formula xs connective-alist)))
		      (orgtt--build-prefix-formula (orgtt--skip-parened-region xs)
						   connective-alist queue connective)))
      ;; A closing parenthesis signals that we've reached the end of
      ;; the current expression. We should now have a know connective
      ;; in the CONNECTIVE argument (known connectives are in the
      ;; CONNECTIVE-ALIST). If this is not the case, something has
      ;; gone wrong, so we throw an error. Otherwise, we will
      ;; determine the arity of the the function which the connective
      ;; maps to in CONNECTIVE-ALIST and pop that many
      ;; variables/subexpressions off the top of QUEUE. Again, if
      ;; queue is not long enough for this then there is a problem, so
      ;; we throw an error. If all goes to plan, then we just create
      ;; an expression lead by the connective function name with all
      ;; the arguments in the appropriate order.
      (`(")" . ,xs) (progn
		      (let ((connective-fn (cdr (assoc connective connective-alist))))
			(if (not connective-fn)
			    (user-error "No known connective for %S, check your formula"
					connective)
			  (let* ((connective-arity
				  (length (help-function-arglist connective-fn)))
				 (args (s-join " " (-take connective-arity queue))))
			    (setq queue (-drop connective-arity queue))
			    (format "(%s %s)" connective-fn args))))))
      ;; Whitespace is not useful to us so we just ignore it.
      ((and `(,x . ,xs) (guard (s-blank-str-p x)))
       (orgtt--build-prefix-formula xs connective-alist queue connective))
      ;; Capital letters signify a literal, so they get added to
      ;; QUEUE.
      ((and `(,x . ,xs) (guard (s-matches-p "^[A-Z]$" x)))
       (orgtt--build-prefix-formula xs connective-alist (-snoc queue x) connective))
      ;; If we've go this far and X still hasn't been used, it must be
      ;; part of a connective, so we add it to the end of the
      ;; CONNECTIVE parameter which keeps track of this.
      (`(,x . ,xs) (orgtt--build-prefix-formula xs connective-alist queue (s-append x connective)))
      ;; If we reach here, that means that the formula was not
      ;; surrunded in parentheses because the final ')' would have
      ;; been caught by the corresponding condition above. We'll just
      ;; make a recursive call with the same state except with FORMULA
      ;; being ")". This allows formulae to not have to be wrapped in
      ;; parens but for the parser to still have the niceness of
      ;; terminating on a specific character.
      (_ (orgtt--build-prefix-formula ")" connective-alist queue connective)))))

(defun orgtt--replace-vars-with-placeholders (vars formula)
  "Replace in VARS in FORMULA with placeholders of the form $<index>.

This function is used to do the final work required of making
FORMULA usable with orgtbl."
  (let ((case-fold-search nil)
	(placeholder-map (-map (lambda (x) (cons (car x) (format "$%d" (+ (cdr x) 1))))
			       (-zip vars (cl-loop for i from 0 below (length vars) collect i)))))
    (dolist (placeholder placeholder-map formula)
      (setq formula (s-replace (car placeholder) (cdr placeholder) formula)))))

(defun orgtt--build-orgtbl-formula (formula &optional binaryp connective-alist queue connective)
  "Build an orgtbl formula representation of FORMULA with BINARYP, CONNECTIVE-ALIST, QUEUE and CONNECTIVE."
  (format "'%s%s" (orgtt--replace-vars-with-placeholders
		   (orgtt--get-vars formula)
		   (orgtt--build-prefix-formula (orgtt--remove-surrounding-parens formula)
						connective-alist queue connective))
	  (if binaryp ";N" "")))

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
  (let* ((vars (orgtt--get-vars formula))
	 (table (orgtt--org-table-align-string
		 (s-join "\n"
			 (list (orgtt--build-table-header vars formula)
			       (orgtt--build-table-divider (length vars))
			       (orgtt--build-table-body vars binaryp)))))
	 (orgtbl-formula (format "#+TBLFM: $%d=%s" (+ (length vars) 1)
				 (orgtt--build-orgtbl-formula formula binaryp)))
	 (tbl (s-concat table "\n" orgtbl-formula)))
    (with-temp-buffer
      (insert tbl)
      (goto-char (point-max))
      (org-table-calc-current-TBLFM)
      (goto-char (point-min))
      (org-table-align)
      (buffer-substring-no-properties (point-min) (point-max)))))

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
