(defpackage :halp
  (:use :cl)
  (:export #:with-gensyms
	   #:substring-starts-with-p
	   #:string-join
	   #:alist-path
	   #:find-max
	   #:find-min
	   #:pairs->hash-table))

(in-package #:halp)

;;; Macro helpers
(defmacro with-gensyms ((&rest names) &body body)
  "Creates variables named NAMES via GENSYM and runs BODY"
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

;;; String helpers
(defun substring-starts-with-p (prefix string &key (start 0))
  (and (>= (length string)
	   (+ start (length prefix)))
       (loop for prefix-character across prefix
	     for string-index upfrom start
	     for string-character = (aref string string-index)
	     if (not (char= prefix-character string-character))
	       do (return nil)
	     finally (return t))))

(defun string-join (strings &optional (delimiter ""))
  "Concatenates STRINGS with an optional delimiter in between"
  (let ((stream (make-string-output-stream)))
    (loop with first = t
	  for string in strings
	  do (if first
		 (setf first nil)
		 (write-string delimiter stream))
	     (write-string string stream))
    (get-output-stream-string stream)))

;;; Collection helpers
(defun alist-path (alist &rest path)
  "Returns the alist value associated with PATH (note: multiple values will be searched recursively)"
  (loop with result = alist
	for key in path
	do (setf result (rest (assoc key result)))
	finally (return result)))

(defun find-max (get-value list)
  "Returns the item from LIST with the highest value when passed to the function GET-VALUE"
  (loop with max-value = nil
	with best = nil
	for item in list
	for value = (funcall get-value item)
	do (when (or (not max-value)
		     (> value max-value))
	     (setf max-value value)
	     (setf best item))
	finally (return (values best max-value))))

(defun find-min (get-value list)
  "Returns the item from LIST with the smallest value when passed to the function GET-VALUE"
  (loop with min-value = nil
	with best = nil
	for item in list
	for value = (funcall get-value item)
	do (when (or (not min-value)
		     (< value min-value))
	     (setf min-value value)
	     (setf best item))
	finally (return (values best min-value))))

(defun pairs->hash-table (pairs &key (test 'eql))
  "Creates a hash table and populate it with PAIRS (a list of (KEY . VALUE))"
  (let ((hash-table (make-hash-table :test test)))
    (loop for (key . value) in pairs
	  do (setf (gethash key hash-table) value))
    hash-table))
