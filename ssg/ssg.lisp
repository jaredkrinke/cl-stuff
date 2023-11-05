(defpackage ssg
  (:use :cl))

(in-package :ssg)

(defun for-each-file-in-directory (directory function &key (process-file-p (constantly t)))
  "Calls FUNCTION on each file (directly) within BASE-DIRECTORY, optionally filtering out files"
  (loop for file in (uiop:directory-files directory)
	do (when (funcall process-file-p file)
	     (funcall function file))))

(defun for-each-file (base-directory function &key (process-directory-p (constantly t)) (process-file-p (constantly t)))
  "Calls FUNCTION for each file under (the entire tree of) BASE-DIRECTORY, optionally filtering out directories and files"
  (uiop:collect-sub*directories base-directory
				process-directory-p
				process-directory-p
				(lambda (directory)
				  (for-each-file-in-directory directory
							      function
							      :process-file-p process-file-p))))

(defun relevant-directory-p (directory)
  "Returns non-NIL if the directory is 'relevant', meaning not part of source control, etc."
  (let ((directory-name (first (last (pathname-directory directory)))))
    (not (equal directory-name ".git"))))

;;; TODO: Is there a Common Lisp Markdown parser that supports tables and GitHub's header-to-id logic? Ideally, one that has an intermediate (possibly list) representation I could use for handling links
