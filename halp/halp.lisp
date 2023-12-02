(defpackage #:halp
  (:use #:cl))

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
