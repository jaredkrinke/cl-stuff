(defpackage ssg
  (:use :cl))

(in-package :ssg)

;;; Utilities
(defmacro deletef (object place)
  "(Destructively) DELETE OBJECT from PLACE"
  `(setf ,place (delete ,object ,place)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

(defmacro append-itemf (item place)
  "Appends ITEM to the end of LIST"
  (with-gensyms (cell)
    `(let ((,cell (cons ,item nil)))
       (if ,place
	   (setf (rest (last ,place)) ,cell)
	   (setf ,place ,cell))
       nil)))

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

(defun path-relative-to (base-directory pathname)
  (enough-namestring (truename pathname) (truename base-directory)))

;;; Processing and dependency graph
(defclass item ()
  ((id :documentation "Path/id for the item"
       :accessor item-id
       :initarg :id)
   (content :documentation "Raw content of this item" ; TODO: This should eventually be a byte array, but will be a string for now
	    :accessor item-content
	    :initarg :content)
   ;; TODO: a-list or subclassing?
   (metadata :documentation "Properties a-list associated with this item"
	     :accessor item-metadata
	     :initarg :metadata))
  (:documentation "Represents an item, optionally with metadata"))

(defclass node ()
  ((input :documentation "Name or pattern indicating inputs"
	  :accessor node-input
	  :initarg :input))
  (:documentation "Represents an arbitrary node in the processing graph"))

(defclass transform-node (node)
  ()
  (:documentation "Represents a 1:N processing node in the graph"))

(defclass aggregate-node (node)
  ()
  (:documentation "Represents an M:N processing node in the graph"))

(defgeneric process (node &optional items)
  (:documentation "Processes a node in the pipeline graph"))

(defgeneric transform (node item)
  (:documentation "Transforms a single item for a transform node"))

(defmethod process ((node transform-node) &optional items)
  (loop for item in items collect (transform node item)))

;;; Built-in nodes
(defparameter *source-directory* nil "Directory to read input files from for the READ-FILES source code")
(defparameter *destination-directory* nil "Directory to write files out to for the WRITE-FILES node")

;; TODO: Should support reading bytes, strings, and objects
(defclass read-files (aggregate-node)
  ()
  (:documentation "Source node for reading input files from *SOURCE-DIRECTORY*"))

(defmethod process ((node read-files) &optional (items nil))
  (declare (ignore items)) ; TODO: Needed?
  (let ((result nil))
    (for-each-file *source-directory*
		   (lambda (file)
		     (push (make-instance 'item
					  :id (path-relative-to *source-directory*
								file)
					  :content (with-open-file (stream file)
						     (read stream)))
			   result)))
    result))

(defclass write-files (transform-node)
  ()
  (:documentation "Sink node for writing items out to *DESTINATION-DIRECTORY*"))

(defmethod transform ((node write-files) (item item))
  ;; TODO: Handle subdirectories!
  (let ((output-pathname (merge-pathnames (item-id item) *destination-directory*)))
    (with-open-file (stream output-pathname
			    :direction :output
			    :if-exists :supersede)
      (write-string (item-content item) stream)))
  item)

;;; HTML template nodes
(defclass list-to-html (transform-node)
  ()
  (:documentation "Transform that converts HTML in list form to an HTML string"))

(defmethod transform ((node list-to-html) item)
  (setf (item-content item) (html (item-content item)))
  item)

;;; Processing pipeline graph
(defparameter *pipeline*
  '((read-files . (list-to-html))
    (list-to-html . (write-files)))
  "Processing pipeline as a directed acyclic graph, represented as list of (NODE-NAME DOWNSTREAM-NODE-NAME-1 ...)")

(defun pipeline-edges (pipeline)
  "Returns a (new) list of edges in PIPELINE"
  (let ((edges nil))
    (loop for row in pipeline
	  for from = (first row)
	  do (loop for to in (rest row)
		   do (push (cons from to) edges)))
    edges))

(defun pipeline-nodes (pipeline)
  "Returns a (new) list of nodes in PIPELINE"
  (let ((nodes nil))
    (loop for row in pipeline do
      (pushnew (first row) nodes)
      (loop for child in (rest row) do
	(pushnew child nodes)))
    nodes))

(defun pipeline-sort (pipeline)
  "Returns a topological sort of the nodes in PIPELINE"
  (let ((edges (pipeline-edges pipeline))
	(start-nodes (pipeline-nodes pipeline))
	(sorted-nodes nil))
    (loop for edge in edges do (deletef (rest edge) start-nodes))
    (loop while start-nodes do
      (let ((node (pop start-nodes)))
	(push node sorted-nodes)
	(loop for edge in (loop for edge in edges
			     if (equal node (first edge))
			       collect edge)
	      for to = (rest edge)
	      do (deletef edge edges)
		 (unless (find-if (lambda (e) (equal to (rest e))) edges)
		   (push to start-nodes)))))
    (if edges
	(error "PIPELINE has a cycle!")
	(nreverse sorted-nodes))))

(defun pipeline-reverse (pipeline)
  "Takes PIPELINE as list of (PARENT . (CHILD1 ...)) and returns a list of (CHILD . (PARENT1 ...))"
  (let ((edges (pipeline-edges pipeline))
	(result nil))
    (loop for (parent . child) in edges do
      (let ((row (assoc child result)))
	(if row
	    (append-itemf parent row)
	    (push (list child parent) result))))
    result))

;; TODO: Build file-level dependency graph!
(defun run (pipeline)
  "Runs the given pipeline"
  (let ((reversed (pipeline-reverse pipeline))
	(results nil))
    (loop for (name . node) in (mapcar (lambda (name) (cons name
							    (make-instance name)))
				       (pipeline-sort pipeline))
	  for parents = (rest (assoc name reversed))
	  for input = (if parents
			  (loop for parent in parents
				nconc (rest (assoc parent results)))
			  nil)
	  for output = (process node input)
	  do (format t "~a: ~s -> ~s~%" name (mapcar #'item-id input) (mapcar #'item-id output))
	     (push (cons name output) results))))

;;; TODO: Is there a Common Lisp Markdown parser that supports tables and GitHub's header-to-id logic? Ideally, one that has an intermediate (possibly list) representation I could use for handling links

;;; HTML templates
(defparameter *html-escapes* '((#\& . "&amp;")
			       (#\< . "&lt;")
			       (#\> . "&gt;")))

(defparameter *html-attribute-escapes* `((#\' . "&apos;")
					 (#\" . "&quot;")
					 . ,*html-escapes*))

(defparameter *html-void-tags* '(:area
				 :base
				 :br
				 :col
				 :embed
				 :hr
				 :img
				 :input
				 :link
				 :meta
				 :param
				 :source
				 :track
				 :wbr))

(defun void-tag-p (keyword)
  "Returns non-NIL if the given keyword maps to an HTML void tag (e.g. :BR for <br>)"
  (member keyword *html-void-tags*))

(defun write-escaped-string (string escapes &optional (stream *standard-output*))
  "Escapes STRING using ESCAPES, writing to STREAM"
  (loop for character across string do
    (let ((row (assoc character escapes)))
      (if row
	  (write-string (cdr row) stream)
	  (write-char character stream)))))

(defun write-escaped-text (text &optional (stream *standard-output*))
  "Escapes TEXT for use in an HTML document, and writes it to STREAM"
  (write-escaped-string text *html-escapes* stream))

(defun write-escaped-attribute (attribute-value &optional (stream *standard-output*))
  "Escapes ATTRIBUTE-VALUE for use in an HTML attribute, and writes it to STREAM"
  (write-escaped-string attribute-value *html-attribute-escapes* stream))

(defun keyword->tag (keyword)
  "Converts e.g. :HTML to 'html'"
  (string-downcase (symbol-name keyword)))

(defun write-html (fragment &optional (stream *standard-output*) inline)
  "Writes a list representing an HTML document or fragment as 'text/html' to STREAM"
  ;;; TODO: Verbatim HTML -- needed?
  (let* ((keyword (first fragment))
	 (tag (keyword->tag keyword))
	 (children (rest fragment))
	 (void (void-tag-p keyword))
	 (root (eql keyword :html)))
    (unless inline
      (when root
	(write-string "<!DOCTYPE html>" stream))
      (fresh-line stream))
    (write-char #\< stream)
    (write-string tag stream)
    (loop for child = (first children)
	  while (keywordp child) do
	    (write-char #\Space stream)
	    (write-string (keyword->tag child) stream)
	    (write-string "=\"" stream)
	    (write-escaped-attribute (second children) stream)
	    (write-char #\" stream)
	    (setf children (cddr children)))
    (write-string ">" stream)
    (unless void
      (loop with inline = nil
	    for child in children do
	      (cond ((stringp child)
		     (setf inline t)
		     (write-escaped-text child stream))
		    ((listp child) (write-html child stream inline))
		    (t (error "Unexpected child: ~a (~a)" child (type-of child)))))
      (write-string "</" stream)
      (write-string tag stream)
      (write-char #\> stream)
      (unless inline (fresh-line stream)))))

(defun html (fragment)
  "Converts a list representing an HTML document or fragment into a string that encodes the HTML document"
  (let ((stream (make-string-output-stream)))
    (write-html fragment stream)
    (get-output-stream-string stream)))
