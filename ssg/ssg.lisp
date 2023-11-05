(defpackage ssg
  (:use :cl))

(in-package :ssg)

;;; File system
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

;;; HTML templates
(defparameter *html-escapes* '((#\& . "&amp;")
			       (#\< . "&lt;")
			       (#\> . "&gt;")
			       (#\' . "&apos;")
			       (#\" . "&quot;")))
			       

(defun write-escaped-string (string &optional (stream *standard-output*))
  "Escapes STRING for use in an HTML document, and writes it to STREAM"
  (loop for character across string do
    (let ((row (assoc character *html-escapes*)))
      (if row
	  (write-string (cdr row) stream)
	  (write-char character stream)))))

(defun keyword->tag (keyword)
  "Converts e.g. :HTML to 'html'"
  (string-downcase (symbol-name keyword)))

(defun process-item (stream item)
  "Renders ITEM (a list representing HTML, or a string) to the given stream"
  ;;; TODO: DOCTYPE, void tags
  ;;; TODO: Verbatim HTML -- needed?
  (let ((tag (keyword->tag (first item)))
	(children (rest item)))
    (write-char #\< stream)
    (write-string tag stream)
    (loop for child = (first children)
	  while (keywordp child) do
	    (write-char #\Space stream)
	    (write-string (keyword->tag child))
	    (write-string "=\"" stream)
	    (write-escaped-string (second children) stream)
	    (write-char #\" stream)
	    (setf children (cddr children)))
    (write-string ">" stream)
    (loop for child in children do
      (cond ((stringp child) (write-escaped-string child stream))
	    ((listp child) (process-item stream child))
	    (t (error "Unexpected child: ~a (~a)" child (type-of child)))))
    (write-string "</" stream)
    (write-string tag stream)
    (write-char #\> stream)))

(defun html (fragment)
  "Converts a list representing an HTML document or fragment into a string that encodes the HTML document"
  (let ((stream (make-string-output-stream)))
    (process-item stream fragment)
    (get-output-stream-string stream)))
