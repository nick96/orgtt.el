;;; orgtt-integration-test.el --- Integration tests for orgtt.el

;;; Commentary:

;;; Code:


;; Test the interface as the user would use it
(ert-deftest orgtt-integration-test-create-table-and-solve-boolean ()
  "Test `orgtt-create-table-and-solve' with boolean representation."
  (let ((orgtt-use-binary nil))
    (with-temp-buffer
      (orgtt-create-table-and-solve "A + B")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
		     (test-helper-get-solved-table 1))))
    (with-temp-buffer
      (orgtt-create-table-and-solve "A -> (B . C)")
      (should (equal (test-helper-buffer-no-properties)
		     (test-helper-get-solved-table 2))))))

(ert-deftest orgtt-integration-test-create-table-and-solve-binary ()
  "Test `orgtt-create-table-and-solve' with binary representation."
  (let ((orgtt-use-binary t))
    (with-temp-buffer
      (orgtt-create-table-and-solve "A + B")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
		     (test-helper-get-solved-table 3))))
    (with-temp-buffer
      (orgtt-create-table-and-solve "A -> (B . C)")
      (should (equal (test-helper-buffer-no-properties)
		     (test-helper-get-solved-table 4))))))

(ert-deftest orgtt-integration-test-create-table-boolean ()
  "Test `orgtt-create-table' with boolean representation."
  (let ((orgtt-use-binary nil))
    (with-temp-buffer
    (orgtt-create-table "A + B")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
		   (test-helper-get-table 1))))
    (with-temp-buffer
      (orgtt-create-table "A -> (B . C)")
      (should (equal (test-helper-buffer-no-properties)
		     (test-helper-get-table 2))))))

(ert-deftest orgtt-integration-test-create-table-binary ()
  "Test `orgtt-create-table'."
  (let ((orgtt-use-binary t))
    (with-temp-buffer
      (orgtt-create-table "A + B")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
		     (test-helper-get-table 3))))
    (with-temp-buffer
      (orgtt-create-table "A -> (B . C)")
      (should (equal (test-helper-buffer-no-properties)
		     (test-helper-get-table 4))))))

;;; orgtt-integration-test.el ends here
