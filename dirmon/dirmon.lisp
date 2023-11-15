;;;; dirmon is a library for detecting changes within a directory in the file system

(defpackage dirmon
  (:use :cl)
  (:export #:for-each-change-in-directory
	   #:read-snapshot
	   #:write-snapshot))

(in-package :dirmon)

(defparameter *wildcard-pathname* (make-pathname :name :wild
						 :type :wild))

(defun enumerate-files (directory)
  (declare (type pathname directory))
  (let ((items (list directory))
	(files nil))
    (loop while items do
      (let ((item (pop items)))
	(declare (type pathname item))
	(if (pathname-name item)
	    (push item files)
	    (loop for child in (directory (merge-pathnames *wildcard-pathname* item)) do
	      (push child items)))))
    files))

;;; SBCL+Windows only
(defun for-each-item-in-directory (function directory)
  "Calls FUNCTION with (string, (or :file :directory)) for each item in DIRECTORY"
  (declare (type function function))
  (sb-win32::native-call-with-directory-iterator
   (lambda (next)
     (declare (type function next))
     (loop for (name kind) = (multiple-value-list (funcall next))
	   while name
	   do (funcall function name kind)))
   (namestring directory)
   nil))

(defun enumerate-files (directory)
  (declare (type pathname directory))
  (let ((items (list directory))
	(files nil))
    (loop while items do
      (let ((item (pop items)))
	(declare (type pathname item))
	(if (pathname-name item)
	    (push item files)
	    (for-each-item-in-directory
	     (lambda (name kind)
	       (push (merge-pathnames
		      (ecase kind
			(:file (parse-namestring name))
			(:directory (make-pathname :directory (list :relative name))))
		      item)
		     items))
	     item))))
    files))

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

;;; Snapshots (just a hash map of relative file path to modification time)
(defparameter *empty-snapshot* (make-hash-table :test 'equal))

(defun make-snapshot (directory &key (process-directory-p (constantly t)) (process-file-p (constantly t)))
  "Creates a new directory snapshot for DIRECTORY, with optional filtering"
  (let ((snapshot (make-hash-table :test 'equal))
	(directory-truename (truename directory)))
    (flet ((relative-path (file) (enough-namestring file directory-truename)))
      (for-each-file-under-directory directory
				     (lambda (file) (setf (gethash (relative-path file) snapshot)
							  (file-write-date file)))
				     :process-directory-p process-directory-p
				     :process-file-p process-file-p))
    snapshot))

(defun for-each-difference (old new function)
  "Calls FUNCTION with ((OR :CREATE :UPDATE :DELETE), PATHNAME) for each difference between A and B"
  (unless old (setf old *empty-snapshot*))
  (loop for file-old being the hash-keys in old using (hash-value time-old)
	for time-new = (gethash file-old new)
	do (if time-new
	       (when (> time-new time-old)
		 (funcall function :update file-old))
	       (funcall function :delete file-old)))
  (loop for file-new being the hash-keys in new
	do (unless (gethash file-new old)
	     (funcall function :create file-new))))

;;; Public API
(defun for-each-change-in-directory (function directory
				     &key previous-snapshot
				       (process-directory-p (constantly t))
				       (process-file-p (constantly t)))
  "Crawls DIRECTORY and calls FUNCTION with 2 arguments:

1. :CREATE, :UPDATE, or :DELETE
2. PATHNAME for the file (relative to DIRECTORY)

Returns an updated snapshot (which can be passed to future calls to detect changes)."
  (let ((snapshot (make-snapshot directory
				 :process-directory-p process-directory-p
				 :process-file-p process-file-p)))
    (for-each-difference previous-snapshot snapshot function)
    snapshot))

(defun write-snapshot (snapshot &optional (stream *standard-output*))
  "Writes SNAPSHOT out to STREAM"
  (error "TODO: Not implemented"))

(defun read-snapshot (&optional (stream *standard-input*))
  "Reads a snapshot (prevoiusly written using WRITE-SNAPSHOT) from STREAM"
  (error "TODO: Not implemented"))
