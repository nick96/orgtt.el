;;; orgtt.el-test.el --- Tests for orgtt.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'orgtt)
(require 's)

;; Test the functions called for the default connectives
(ert-deftest orgtt-test-negate ()
  "Tests for negation."
  (should (orgtt--negate nil))
  (should (not (orgtt--negate t))))

(ert-deftest orgtt-test-land ()
  "Tests for logical and."
  (should (orgtt--land t t))
  (should (not (orgtt--land t nil)))
  (should (not (orgtt--land nil t)))
  (should (not (orgtt--land nil nil))))

(ert-deftest orgtt-test-lor ()
  "Tests for logical or."
  (should (orgtt--lor t t))
  (should (orgtt--lor t nil))
  (should (orgtt--lor nil t))
  (should (not (orgtt--lor nil nil))))

(ert-deftest orgtt-test-implication ()
  "Tests for implication."
  (should (orgtt--implication t t))
  (should (not (orgtt--implication t nil)))
  (should (orgtt--implication nil t))
  (should (orgtt--implication nil nil)))

(ert-deftest orgtt-test-biimplication ()
  "Tests for biimplication."
  (should (orgtt--biimplication t t))
  (should (not (orgtt--biimplication t nil)))
  (should (not (orgtt--biimplication nil t)))
  (should (orgtt--biimplication nil nil)))

(ert-deftest orgtt-test-xor ()
  "Tests for exclusive or."
  (should (not (orgtt--xor t t)))
  (should (orgtt--xor t nil))
  (should (orgtt--xor nil t))
  (should (not (orgtt--xor nil nil))))

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

(ert-deftest orgtt-test-build-table-body ()
  "Test `orgtt--build-table-body'."
  (should (equal (orgtt--build-table-body
		  '("A" "B"))
		 (s-join "\n" '("| 0 | 0 |" "| 0 | 1 |" "| 1 | 0 |" "| 1 | 1 |")))))

(ert-deftest orgtt-test-build-orgtbl-formula ()
  "Test `orgtt--build-orgtbl-formula'.")

(ert-deftest orgtt-test-create-table ()
  "Test `orgtt--create-table'.")

;; Test the interface as the user would use it
(ert-deftest orgtt-test ()
  "Test `orgtt-create-table'.")

;;; orgtt.el-test.el ends here
