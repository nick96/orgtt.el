;;; test-helper.el --- Helpers for orgtt.el-test.el

;;; Commentary:

;;; Code:

(require 'f)

(defconst test-helper-test-dir (f-parent (f-this-file)))

(defun test-helper-get-table (n)
  "Get contents of file 'expected-tables/tabel$N.org'."
  (let ((table-name (format (f-join test-helper-test-dir
				    "expected-tables/table%d.org")
			    n)))
    (s-chomp (with-temp-buffer
	       (insert-file-contents table-name)
	       (test-helper-buffer-no-properties)))))

(defun test-helper-get-solved-table (n)
  "Get contents of file 'expected-tables/solved-tabel$N.org'."
  (let ((table-name (format (f-join test-helper-test-dir
				    "expected-tables/solved-table%d.org")
			    n)))
    (s-chomp (with-temp-buffer
	       (insert-file-contents table-name)
	       (test-helper-buffer-no-properties)))))

(defun test-helper-buffer-no-properties ()
  "Get the contents of the current buffer without any properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'test-helper)
;;; test-helper.el ends here
