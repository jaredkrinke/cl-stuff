;;;; dirmon is a library for detecting changes within a directory in the file system

(defpackage dirmon
  (:use :cl))

(in-package :dirmon)

;;; Utilities
(defun diff-alists (a b &key (test 'eql))
  "Returns: rows only in A, rows in both, and rows only in B (according to TEST on each key)"
  ;; TODO: Consider using temporary hash tables for speed
  ;; TODO: An actual algorithm
  (let* ((a-firsts (mapcar #'first a))
	 (b-firsts (mapcar #'first b))
	 (only-a (set-difference a-firsts b-firsts :test test))
	 (both (intersection a-firsts b-firsts :test test))
	 (only-b (set-difference b-firsts a-firsts :test test)))
    (break)
    (values (mapcar (lambda (first) (assoc first a)) only-a)
	    (mapcar (lambda (first) (cons (assoc first a) (assoc first b))) both)
	    (mapcar (lambda (first) (assoc first b)) only-b))))

;;; File system utilities
(defun for-each-file-in-directory (directory function &key (process-file-p (constantly t)))
  "Calls FUNCTION on each file (directly) within BASE-DIRECTORY, optionally filtering out files"
  (loop for file in (uiop:directory-files directory)
	do (when (funcall process-file-p file)
	     (funcall function file))))

(defun for-each-file-under-directory (base-directory function
				      &key (process-directory-p (constantly t))
					(process-file-p (constantly t)))
  "Calls FUNCTION for each file under (the entire tree of) BASE-DIRECTORY, with optional filtering"
  (flet ((process-directory (directory)
	   (for-each-file-in-directory directory
				       function
				       :process-file-p process-file-p)))
    (uiop:collect-sub*directories base-directory
				  process-directory-p
				  process-directory-p
				  #'process-directory)))

;; (defun path-relative-to (base-directory pathname)
;;   "Returns PATHNAME relative to BASE-DIRECTORY"
;;   ;; TODO: Can this handle deleted files?
;;   (enough-namestring (truename pathname) (truename base-directory)))

;; Data model
(defun for-each-change (function old new)
  "Diffs two snapshots, calling FUNCTION for each difference"
  (multiple-value-bind (only-a both only-b) (diff-alists old new :test 'equal)
    (loop for row in only-a do (funcall function :delete (first row) nil))
    (loop for (row-old . row-new) in both do
      (when (> (rest row-new) (rest row-old))
	(funcall function :update (first row-new) (rest row-new))))
    (loop for row in only-b do (funcall function :add (first row) (rest row))))
  nil)

;;; Public API
(defun for-each-change-in-directory (function directory
				     &key previous-snapshot
				       (process-directory-p (constantly t))
				       (process-file-p (constantly t)))
  "Crawls DIRECTORY and calls FUNCTION with 3 arguments:

1. :ADD, :UPDATE, or :DELETE
2. PATHNAME for the file (relative to DIRECTORY)
3. The last modification time for the file (or NIL for :DELETEs)

Returns an updated snapshot (which can be passed to future calls to detect changes)."
  (let ((snapshot nil)
	(directory-truename (truename directory)))
    (flet ((relative-path (file) (enough-namestring file directory-truename)))
      (for-each-file-under-directory directory
				     (lambda (file) (push (cons (relative-path file)
								(file-write-date file))
							  snapshot))
				     :process-directory-p process-directory-p
				     :process-file-p process-file-p)
      (for-each-change (lambda (change-type file time)
			 (funcall function change-type file time))
		       previous-snapshot
		       snapshot)
      snapshot)))
