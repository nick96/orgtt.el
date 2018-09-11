;;; orgtt.el-test.el --- Tests for orgtt.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'orgtt)
(require 's)

;; Test helper functions
(ert-deftest orgtt-test-to-bool ()
  "Tests the conversion of things to boolean."
  (should (orgtt--to-bool t))
  (should-not (orgtt--to-bool nil))
  (should (orgtt--to-bool 1))
  (should-not (orgtt--to-bool 0))
  (should (orgtt--to-bool "t"))
  (should-not (orgtt--to-bool "nil"))
  (should-error (orgtt--to-bool "x")))

(ert-deftest orgtt-test-get-valuations ()
  "Test getting all possible valuations for N variables."
  (should (equal (orgtt--get-valuations 2)
		 '((nil nil) (nil t) (t nil) (t t)))))

(ert-deftest orgtt-test-get-vars ()
  "Test getting all variables from a formula."
  (should (equal (orgtt--get-vars "A + B") '("A" "B")))
  (should (equal (orgtt--get-vars "B . A") '("A" "B")))
  (should (equal (orgtt--get-vars "A + (B . A)") '("A" "B"))))

;; Test the functions called for the default connectives
(ert-deftest orgtt-test-negate-boolean ()
  "Tests for negation with booleans."
  (should (orgtt--negate nil))
  (should-not (orgtt--negate t)))

(ert-deftest orgtt-test-negate-binary ()
  "Tests for negation with 1s and 0s."
  (should (= (orgtt--negate 1) 0))
  (should (= (orgtt--negate 0) 1)))

(ert-deftest orgtt-test-land-boolean ()
  "Tests for logical and with booleans."
  (should (orgtt--land t t))
  (should-not (orgtt--land t nil))
  (should-not (orgtt--land nil t))
  (should-not (orgtt--land nil nil)))

(ert-deftest orgtt-test-land-binary ()
  "Tests for logical and with 1s and 0s."
  (should (= (orgtt--land 1 1) 1))
  (should (= (orgtt--land 1 0) 0))
  (should (= (orgtt--land 0 1) 0))
  (should (= (orgtt--land 1 0) 0)))

(ert-deftest orgtt-test-lor-boolean ()
  "Tests for logical or with booleans."
  (should (orgtt--lor t t))
  (should (orgtt--lor t nil))
  (should (orgtt--lor nil t))
  (should-not (orgtt--lor nil nil)))

(ert-deftest orgtt-test-lor-binary ()
  "Tests for logical or with 1s and 0s."
  (should (= (orgtt--lor 1 1) 1))
  (should (= (orgtt--lor 1 0) 1))
  (should (= (orgtt--lor 0 1) 1))
  (should (= (orgtt--lor 0 0) 0)))

(ert-deftest orgtt-test-implication-boolean ()
  "Tests for material implication with booleans."
  (should (orgtt--implication t t))
  (should-not (orgtt--implication t nil))
  (should (orgtt--implication nil t))
  (should (orgtt--implication nil nil)))

(ert-deftest orgtt-test-implication-binary ()
  "Tests for material implication with 1s and 0s"
  (should (= (orgtt--implication 1 1) 1))
  (should (= (orgtt--implication 1 0) 0))
  (should (= (orgtt--implication 0 1) 1))
  (should (= (orgtt--implication 0 0) 1)))

(ert-deftest orgtt-test-biimplication-boolean ()
  "Tests for biimplication with booleans."
  (should (orgtt--biimplication t t))
  (should-not (orgtt--biimplication t nil))
  (should-not (orgtt--biimplication nil t))
  (should (orgtt--biimplication nil nil)))

(ert-deftest orgtt-test-biimplication-binary ()
  "Tests for biimplication with 1s and 0s."
  (should (= (orgtt--biimplication t t) 1))
  (should (= (orgtt--biimplication t nil) 0))
  (should (= (orgtt--biimplication nil t) 0))
  (should (= (orgtt--biimplication nil nil) 1)))

(ert-deftest orgtt-test-xor-boolean ()
  "Tests for exclusive or with booleans."
  (should-not (orgtt--xor t t))
  (should (orgtt--xor t nil))
  (should (orgtt--xor nil t))
  (should-not (orgtt--xor nil nil)))

(ert-deftest orgtt-test-xor-binary ()
  "Tests for exclusive or with 1s and 0s."
  (should (= (orgtt--xor 1 1) 0))
  (should (= (orgtt--xor 1 0) 1))
  (should (= (orgtt--xor 0 1) 1))
  (should (= (orgtt--xor 0 0) 0)))

;; Test the functions for parsing the formula in its string form
(ert-deftest orgtt-test-get-vars ()
  "Test `orgtt--get-vars'."
  (should (equal (orgtt--get-vars "A + B") '("A" "B")))
  (should (equal (orgtt--get-vars "(A . B) + -(C <+> D -> (E <-> F))")
		 '("A" "B" "C" "D" "E" "F"))))

;; Test the function used to build the truth table
(ert-deftest orgtt-test-build-table-header ()
  "Test `orgtt--build-table-header'."
  (should (equal (orgtt--build-table-header '("A" "B") "A + B")
		 "| A | B | A + B |"))
  (should (equal (orgtt--build-table-header '("A" "B" "C" "D" "E") "(A <+> B) . C + (D -> E)")
		 "| A | B | C | D | E | (A <+> B) . C + (D -> E) |")))

(ert-deftest orgtt-test-build-table-body-boolean ()
  "Test `orgtt--build-table-body'."
  (should (equal (orgtt--build-table-body '("A" "B"))
		 (s-join "\n" '("| nil | nil |" "| nil | t |" "| t | nil |" "| t | t |")))))

(ert-deftest orgtt-test-build-table-body-binary ()
  "Test `orgtt--build-table-body' with binary enabled."
  (should (equal (orgtt--build-table-body '("A" "B") t)
		 (s-join "\n" '("| 0 | 0 |" "| 0 | 1 |" "| 1 | 0 |" "| 1 | 1 |")))))

(ert-deftest orgtt-test-build-orgtbl-formula-unnested ()
  "Test `orgtt--build-orgtbl-formula' with un-nested formulae."
  (should (equal (orgtt--build-orgtbl-formula "A + B")
		 "'(orgtt--lor $1 $2"))
  (should (equal (orgtt--build-orgtbl-formula "A -> B")
		 "'(orgtt--implication $1 $2)"))
  (should (equal (orgtt--build-orgtbl-formula "B . A")
		 "'(orgtt--land $2 $1)")))

(ert-deftest orgtt-test-build-orgtbl-formula-nested ()
  "Test `orgtt--build-orgtbl-formula' with nested formulae."
  (should (equal (orgtt--build-orgtbl-formula "A -> (B -> C)")
		 "'(orgtt--implication $1 (orgtt--implication $2 $3))"))
  (should (equal (orgtt--build-orgtbl-formula "(A + (B . (C -> (A <-> (D <+> B)))))")
		 "'(orgtt--lor $1 (orgtt--land $2 (orgtt--implication $3 (orgtt--biimplication $1 (orgtt--xor $4 $2)))))"))
  (should (equal (orgtt--build-orgtbl-formula "(A . B) + (-B . -A)")
		 "'(orgtt--lor (orgtt--land $1 $2) (orgtt--land (orgtt--negate $2) (orgtt--negate $1)))")))

(ert-deftest orgtt--test-build-orgtbl-formula-binary ()
  "Test `orgtt--build-orgtbl-formula' with binary representation enabled."
  (should (equal (orgtt--build-orgtbl-formula "A -> (B -> C)" t)
		 "'(orgtt--implication $1 (orgtt--implication $2 $3));N"))
  (should (equal (orgtt--build-orgtbl-formula "(A + (B . (C -> (A <-> (D <+> B)))))" t)
		 "'(orgtt--lor $1 (orgtt--land $2 (orgtt--implication $3 (orgtt--biimplication $1 (orgtt--xor $4 $2)))));N"))
  (should (equal (orgtt--build-orgtbl-formula "(A . B) + (-B . -A)" t)
		 "'(orgtt--lor (orgtt--land $1 $2) (orgtt--land (orgtt--negate $2) (orgtt--negate $1)));N")))

(ert-deftest orgtt-test-build-orgtbl-formula-custom-connectives ()
  "Test `orgtt--build-orgtbl-formula' using custom connectives."
  (let* ((land (lambda (x y) (orgtt--land x y)))
	 (lor (lambda (x y) (orgtt--lor x y)))
	 (neg (lambda (x) (orgtt--negate x)))
	 (connective-alist '(("+" . land)
			     ("-" . lor)
			     ("~" . neg))))
    (should (equal (orgtt--build-orgtbl-formula "A + B + C" connective-alist)
		   "'(land $1 (land $2 $3))"))
    (should (equal (orgtt--build-orgtbl-formula "~A - ~B" connective-alist)
		   "'(lor (neg $1) (neg $2))"))))

(ert-deftest orgtt-test-build-orgtbl-formula-custom-connectives-lambda ()
  "Test `orgtt--build-orgtbl-formula' using custom connectives defined as lambdas."
  :expected-result :failed
  (let ((connective-alist '(("," . (lambda (x y) (orgtt--land x y)))
			    (";" . (lambda (x y) (orgtt--lor x y)))
			    ("-" . (lambda (x) (orgtt--negate x))))))
    (should (equal (orgtt--build-orgtbl-formula "A, B, C" connective-alist)
		   "'(orgtt--custom-op1 $1 (orgtt--custom-op1 $2 $3))"))
    (should (equal (orgtt--build-orgtbl-formula "(A , B) ; (-A , -B)" connective-alist)
		   "'(orgtt--custom-op2 (orgtt--custom-op1 $1 $2) (orgtt--custom-op1 (orgtt--custom-op3 $1) (orgtt--custom-op3 $2)))"))))

(ert-deftest orgtt-test-create-table-and-solve ()
  "Test `orgtt--create-table-and-solve'."
  (let ((table1 (test-helper-get-solved-table 1))
	(table2 (test-helper-get-solved-table 2))
	(table3 (test-helper-get-solved-table 3))
	(table4 (test-helper-get-solved-table 4)))
    (should (equal (orgtt--create-table-and-solve "A + B") table1))
    (should (equal (orgtt--create-table-and-solve "A -> (B . C)") table2))
    (should (equal (orgtt--create-table-and-solve "A + B" t) table3))
    (should (equal (orgtt--create-table-and-solve "A -> (B . C)" t) table4))))

(ert-deftest orgtt-test-create-table ()
  "Test `orgtt--create-table'."
  (let ((table1 (test-helper-get-table 1))
	(table2 (test-helper-get-table 2))
	(table3 (test-helper-get-table 3))
	(table4 (test-helper-get-table 4)))
    (should (equal (orgtt--create-table "A + B") table1))
    (should (equal (orgtt--create-table "A -> (B . C)") table2))
    (should (equal (orgtt--create-table "A + B" t) table3))
    (should (equal (orgtt--create-table "A -> (B. C)" t) table4))))
;;; orgtt-test.el ends here
