;;; test-helper.el --- Helpers for orgtt.el-test.el

;;; Commentary:

;;; Code:

(require 'f)
;; I know this is gross but I need a way of being able to easily refer
;; to the absolute file name so that tests can be run interactively in
;; emacs and from cask.
(require 'projectile)


(defun test-helper-get-table (n)
  "Get contents of file 'expected-tables/tabel$N.org'."
  (let ((table-name (format (f-join (projectile-project-root)
				    "test/expected-tables/table%d.org")
			    n)))
    (s-chomp (with-temp-buffer
	       (insert-file-contents table-name)
	       (test-helper-buffer-no-properties)))))

(defun test-helper-get-solved-table (n)
  "Get contents of file 'expected-tables/solved-tabel$N.org'."
  (let ((table-name (format (f-join (projectile-project-root)
				    "test/expected-tables/solved-table%d.org")
			    n)))
    (s-chomp (with-temp-buffer
	       (insert-file-contents table-name)
	       (test-helper-buffer-no-properties)))))

(defun test-helper-buffer-no-properties ()
  "Get the contents of the current buffer without any properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'test-helper)
;;; test-helper.el ends here
