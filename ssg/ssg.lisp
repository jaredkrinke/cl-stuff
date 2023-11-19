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

;;; Logging
(defvar *debug* nil "Non-nil enables debug logging")

(defmacro when-logging (&body body)
  `(and *debug* (progn ,@body)))

(defmacro spew (format-string &rest arguments)
  `(when-logging (format t ,format-string ,@arguments)))

;;; TODO: Needed?
(defun relevant-directory-p (directory)
  "Returns non-NIL if the directory is 'relevant', meaning not part of source control, etc."
  (let ((directory-name (first (last (pathname-directory directory)))))
    (not (equal directory-name ".git"))))

;;; Processing and dependency graph
(defclass item ()
   ((content :documentation "Raw content of this item" ; TODO: This should eventually be a byte array, but will be a string for now--actuallly, maybe not. Could just have multiple reader nodes
	    :accessor item-content
	    :initarg :content
	    :initform nil)
   ;; TODO: a-list or subclassing?
   (metadata :documentation "Properties a-list associated with this item"
	     :accessor item-metadata
	     :initarg :metadata
	     :initform nil))
  (:documentation "Represents an item, optionally with metadata"))

;; (defun item-clone (item)
;;   "Clones an item (shallowly) by duplicating its properties"
;;   (make-instance 'item
;; 		 ;; :id (item-id item)
;; 		 :content (item-content item)
;; 		 :metadata (item-metadata item)))

(defclass node ()
  ((include :documentation "Function taking a UNIX-style pathstring, returning non-NIL if the item should be included"
	  :accessor node-include
	    :initarg :include
	    :initform (constantly t))
   (exclude :documentation "Function taking a UNIX-style pathstring, returning non-NIL if the item should be excluded"
	    :accessor node-exclude
	    :initarg :exclude
	    :initform (constantly nil))
   (cached-result :documentation "Cached result of the most recent update"
		  :accessor node-cached-result
		  :initarg :cached-result
		  :initform nil)
   (snapshot :documentation "Cached snapshot (hash of pathstring to item) from previous updates"
	     :accessor node-snapshot
	     :initarg :snapshot
	     :initform (make-hash-table :test 'equal)))
  (:documentation "Represents an arbitrary node in the processing graph"))

(defclass source-node (node)
  ()
  (:documentation "Represents a node that initially enumerates items from a data source (note: these nodes *always* must run)"))

(defclass transform-node (node)
  ((cached-outputs :initform (make-hash-table :test 'equal)
		   :accessor transform-node-cached-outputs
		   :documentation "Cached hash of input to output(s), needed for handling deletions"))
  (:documentation "Represents a 1:N processing node in the graph"))

;; TODO: Consider adding direct-transform-node and content-transform-node subclasses

(defclass aggregate-node (node)
  ()
  (:documentation "Represents an M:N processing node in the graph"))

(defclass sink-node (node)
  ()
  (:documentation "Represents a sink node in the graph, e.g. for writing files to disk"))

(defun filter-changes (node changes)
  "Filters CHANGES based on the node's include/exclude filters"
  (with-accessors ((include node-include) (exclude node-exclude)) node
    (loop for (event path item) in changes
	  if (and (funcall include path)
		  (not (funcall exclude path)))
	    collect (list event path item))))

(defgeneric update (node changes)
  (:documentation "Updates a node in the pipeline graph in response to changes"))

(defgeneric transform (node path item)
  (:documentation "Updates a single item in response to a change for a transform node"))

(defgeneric aggregate (node changes)
  (:documentation "Updates items based on changes and aggregates the items into one or more result items"))

(defun resolve-results (old-paths results snapshot)
  "Removes OLD-PATHS from SNAPSHOT, adds RESULTS, and returns list of changes"
  (let ((changes nil)
	(removed-paths (copy-list old-paths)))
    (loop for path in old-paths do
      (remhash path snapshot))
    (loop for (path . item) in results do
      ;; TODO: Check for differences?
      (setf (gethash path snapshot) item)
      (push (list :update path item) changes)
      (deletef path removed-paths))
    (loop for path in removed-paths do
      (push (list :delete path nil) changes))
    changes))

(defmethod update :around ((node node) changes)
  (call-next-method node (filter-changes node changes)))

(defmethod update ((node transform-node) changes)
  (let ((snapshot (node-snapshot node))
	(cached-outputs (transform-node-cached-outputs node)))
    (loop for (event input-path input-item) in changes
	  nconc (resolve-results
		 (gethash input-path cached-outputs)
		 (ecase event
		   (:update
		    (let ((outputs (multiple-value-list (transform node input-path input-item))))
		      (setf (gethash input-path cached-outputs) (mapcar #'first outputs))
		      outputs))
		   (:delete
		    nil))
		 snapshot))))

(defmethod update ((node aggregate-node) changes)
  (let ((snapshot (node-snapshot node)))
    (resolve-results (loop for path being the hash-keys in snapshot collect path)
		     (aggregate node changes)
		     snapshot)))

;;; Filtering helpers
(defun create-type-filter (type)
  "Creates a pathstring filter for TYPE"
  (lambda (pathstring)
    (let* ((pathname (uiop:parse-unix-namestring pathstring))
	   (item-type (pathname-type pathname)))
      (string= type item-type))))

;;; Built-in nodes
(defparameter *source-directory* #p"input/" "Directory to read input files from for the READ-FROM-DIRECTORY source code")
(defparameter *destination-directory* #p"output/" "Directory to write files out to for the WRITE-FILES node")

(defclass read-from-directory (source-node)
  ((directory-snapshot :initform nil
		      :initarg :directory-snapshot
		      :accessor read-from-directory-directory-snapshot
		      :documentation "Previous directory snapshot"))
  (:documentation "Source node for enumerating files from *SOURCE-DIRECTORY*"))

(defmethod update ((node read-from-directory) input-changes)
  (multiple-value-bind (changes snapshot) (dirmon:get-changes-in-directory
					   *source-directory*
					   :previous-snapshot (read-from-directory-directory-snapshot node))
    (setf (read-from-directory-directory-snapshot node) snapshot)
    (loop for (event pathname) in changes
	  collect (list (if (equal event :delete) :delete :update)
			(uiop/pathname:unix-namestring pathname)
			nil))))

(defclass write-to-directory (sink-node)
  ()
  (:documentation "Sink node for writing files to *DESTINATION-DIRECTORY*"))

(defmethod update ((node write-to-directory) input-changes)
  (loop for (event path item) in input-changes do
    (let ((output-path (merge-pathnames (uiop:parse-unix-namestring path)
					*destination-directory*)))
      (ecase event
	(:update
	 (ensure-directories-exist output-path)
	 (with-open-file (stream output-path
				 :direction :output
				 :if-exists :supersede)
	   ;; TODO: Support other formats
	   (write-string (item-content item) stream)))
	(:delete
	 (delete-file output-path))))))

;; TODO: Should support reading bytes, strings, and objects
(defclass read-as-string (transform-node)
  ()
  (:documentation "Source node for reading files as text from *SOURCE-DIRECTORY*"))

(defmethod transform ((node read-as-string) pathstring item)
  (cons pathstring
	(make-instance 'item
		       :content
		       (uiop:read-file-string (merge-pathnames
					       (uiop/pathname:parse-unix-namestring pathstring)
					       *source-directory*)))))

;;; HTML template nodes
;; (defclass list-to-html (transform-node)
;;   ()
;;   (:documentation "Transform that converts HTML in list form to an HTML string"))

;; (defmethod transform ((node list-to-html) (item item))
;;   ;; TODO: This needs to clone the item instead of overwriting a property!
;;   (setf (item-content item) (html (item-content item)))
;;   item)

;;; Processing pipeline graph
(defparameter *pipeline*
  '((read-from-directory . (read-as-string))
    (read-as-string . (write-to-directory)))
  "Processing pipeline as a directed acyclic graph, represented as list of (NODE-NAME DOWNSTREAM-NODE-NAME-1 ...)")

(defvar *name-to-nodes* nil "A-list mapping node names to the nodes themselves")
(defvar *reversed-pipeline* nil "*PIPELINE* with edges reversed, to find nodes' prerequisites")

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

;;; TODO: Instead of doing this, attach parents directly to the NODE objects
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

(defun initialize-pipeline ()
  "Initializes the processing graph from *PIPELINE*"
  ;; TODO: Eventually, results should be persisted to disk and loaded here
  (setf *name-to-nodes* (loop for name in (pipeline-sort *pipeline*)
			      collect (cons name (make-instance name))))
  (setf *reversed-pipeline* (pipeline-reverse *pipeline*)))

(defun run-pipeline ()
  "Runs the given pipeline"
  (loop for (name . node) in *name-to-nodes*
	do (setf (node-cached-result node) nil))
  (loop with name-to-results = (make-hash-table)
	for (name . node) in *name-to-nodes*
	for parent-names = (rest (assoc name *reversed-pipeline*))
	for input-changes = (loop for parent-name in parent-names
				  for parent = (rest (assoc parent-name *name-to-nodes*))
				  nconc (node-cached-result parent))
	for output-changes = (update node input-changes)
	do (when-logging
	     (spew "~a:~%" name)
	     (loop for (event path) in input-changes do (spew "  ~a:~a~%" event path))
	     (spew " -->~%")
	     (loop for (event path) in output-changes do (spew "  ~a:~a~%" event path))
	     (spew "~%"))
	   (setf (node-cached-result node) output-changes)))

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
