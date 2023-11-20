(defpackage ssg
  (:use :cl))

(in-package :ssg)

;;; Utilities
(defmacro deletef (object place &key (test ''eql))
  "(Destructively) DELETE OBJECT from PLACE"
  `(setf ,place (delete ,object ,place :test ,test)))

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
  "Run BODY only when *DEBUG* is non-NIL"
  `(and *debug* (progn ,@body)))

(defmacro spew (format-string &rest arguments)
  "Log only when *DEBUG* is non-NIL"
  `(when-logging (format t ,format-string ,@arguments)))

;;; TODO: Needed?
(defun relevant-directory-p (directory)
  "Returns non-NIL if the directory is 'relevant', meaning not part of source control, etc."
  (let ((directory-name (first (last (pathname-directory directory)))))
    (not (equal directory-name ".git"))))

;;; Processing and dependency graph
(defclass item ()
  ((content :documentation "Raw content of this item; see ITEM-READ-CONTENT for supported types"
	    :initarg :content
	    :accessor item-content
	    :initform nil)
   (metadata :documentation "Properties a-list associated with this item"
	     :accessor item-metadata
	     :initarg :metadata
	     :initform nil))
  (:documentation "Represents an item, optionally with metadata"))

(defgeneric read-content-as-string (content)
  (:documentation "Reads CONTENT as a STRING"))

(defmethod read-content-as-string ((content string))
  content)

(defmethod read-content-as-string ((content pathname))
  (uiop:read-file-string content))

(defgeneric read-content-as-object (content)
  (:documentation "Reads CONTENT as a Lisp object"))

(defmethod read-content-as-object ((content pathname))
  (uiop:safe-read-file-form content))

(defun item-read-content (item &key (type :string))
  "Reads ITEM's content. The underlying slot can be of type (OR PATHNAME STRING), and this will be read/coerced into TYPE.

Supported types:

* :STRING
* :OBJECT"
  (let ((content (item-content item)))
    (ecase type
      (:string (read-content-as-string content))
      (:object (read-content-as-object content)))))

(defun items-equal (a b)
  "Returns non-NIL if A and B are equivalent"
  ;; Note: this isn't 100% accurate due to how a-lists shadow properties, but should be fine for detecting identical processing output
  (and (equal (item-metadata a) (item-metadata b))
       (equal (item-content a) (item-content b))

       ;; Treat PATHNAME content as different because it's just the path and doesn't reflect actual content
       (not (typep (item-content a) 'pathname))))

(defun item-clone (item)
  "Clones an item (shallowly) by duplicating its properties"
  (make-instance 'item
		 :content (item-content item)
		 :metadata (item-metadata item)))

(defclass node ()
  ((include :documentation "Filters for items to include (see FILTER-TEST for syntax)"
	    :accessor node-include
	    :initarg :include
	    :initform :rest)
   (exclude :documentation "Filters for items to exclude (see FILTER-TEST for syntax); note: this overrides INCLUDE"
	    :accessor node-exclude
	    :initarg :exclude
	    :initform nil)
   (input :documentation "Pending changes coming into this node"
	  :accessor node-input
	  :initform nil)
   (cached-input :documentation "Cached input (hash table of path to item) to this node"
		 :accessor node-cached-input
		 :initarg :cached-input
		 :initform (make-hash-table :test 'equal))
   (cached-output :documentation "Cached ouput (hash table of path to item) from this node"
		  :accessor node-cached-output
		  :initarg :cached-output
		  :initform (make-hash-table :test 'equal)))
  (:documentation "Represents an arbitrary node in the processing graph"))

(defclass source-node (node)
  ()
  (:documentation "Represents a node that initially enumerates items from a data source (note: these nodes *always* must run)"))

(defclass transform-node (node)
  ((cached-outputs :initform (make-hash-table :test 'equal)
		   :accessor transform-node-cached-outputs
		   :documentation "Cached hash table of input to output(s), needed for handling deletions"))
  (:documentation "Represents a 1:N processing node in the graph"))

;; TODO: Consider adding direct-transform-node and content-transform-node subclasses

(defclass aggregate-node (node)
  ()
  (:documentation "Represents an M:N processing node in the graph"))

(defclass sink-node (node)
  ()
  (:documentation "Represents a sink node in the graph, e.g. for writing files to disk"))

(defgeneric update (node)
  (:documentation "Updates a node in the pipeline graph in response to changes"))

(defgeneric transform (node path item)
  (:documentation "Updates a single item in response to a change for a transform node"))

(defgeneric aggregate (node snapshot)
  (:documentation "Aggregates the input items into one or more result items"))

(defmethod update :after ((node node))
  (setf (node-input node) nil))

(defun resolve-results (old-paths results cached-output)
  "Removes OLD-PATHS from CACHED-OUTPUT, adds RESULTS, and returns list of changes"
  (let ((changes nil)
	(removed-paths (copy-list old-paths)))
    (loop for (path . item) in results
	  for old-item = (gethash path cached-output)
	  do (deletef path removed-paths :test 'equal) ; Not deleted
	     ;; Only report :UPDATE when there's actually a change
	     (unless (and old-item (items-equal item old-item))
	       (setf (gethash path cached-output) item)
	       (push (list :update path item) changes)))

    ;; Any previous paths that aren't seen have been :DELETED
    (loop for path in removed-paths do
      (remhash path cached-output)
      (push (list :delete path nil) changes))
    changes))

(defmethod update ((node transform-node))
  (let ((changes (node-input node))
	(cached-output (node-cached-output node))
	(cached-outputs (transform-node-cached-outputs node)))
    ;; Transform nodes process items individually; collect *all* outputs
    (loop for (event input-path input-item) in changes
	  nconc (resolve-results
		 (gethash input-path cached-outputs)
		 (ecase event
		   (:update
		    ;; Process one item (note: it may result in multiple outputs)
		    (let ((outputs (multiple-value-list (transform node input-path input-item))))
		      (setf (gethash input-path cached-outputs) (mapcar #'first outputs))
		      outputs))
		   (:delete
		    nil))
		 cached-output))))

(defmethod update ((node aggregate-node))
  (let ((changes (node-input node))
	(cached-input (node-cached-input node))
	(cached-output (node-cached-output node)))
    ;; Update cached input
    (loop for (event path item) in changes do
      (ecase event
	(:update (setf (gethash path cached-input) item))
	(:delete (remhash path cached-input))))
    (resolve-results (loop for path being the hash-keys in cached-output collect path)
		     (aggregate node cached-input)
		     cached-output)))

(defun filter-test (change filter)
  "Returns non-NIL if CHANGE matches FILTER; supported filters:

* NIL matches nothing
* T matches everything
* :REST matches anything that wasn't specifically included by another sibling
* (:TYPE \"foo\" matches items base on file type (\"foo\" in this example)"
  ;; TODO: Some way to match based on path?
  ;; TODO: Compile these!
  ;; TODO: Allow list of these (would these be intersection or union?)
  (cond ((null filter) nil)
	((eql filter t) t)
	((eql filter :rest) (error "Unexpected :REST (exclusion?) filter!"))
	((listp filter)
	 (ecase (first filter)
	   (:type (equal (second filter)
			 (pathname-type (uiop:parse-unix-namestring (second change)))))))
	(t (error "Unexpected filter: ~a" filter))))

(defun should-include (change node)
  "Returns non-NIL if CHANGE should be processed by NODE (based on include/exclude filters)"
  (and (filter-test change (node-include node))
       (not (filter-test change (node-exclude node)))))

;;; Node helpers
(defun change-type (pathstring new-type)
  "Modifies PAHTHSTRING so that it has type NEW-TYPE"
  (let* ((pathname (uiop:parse-unix-namestring pathstring))
	 (new-pathname (merge-pathnames (make-pathname :type new-type) pathname)))
    (uiop:unix-namestring new-pathname)))

;;; Built-in nodes
(defparameter *source-directory* #p"input/" "Directory to read input files from for the READ-FROM-DIRECTORY source code")
(defparameter *destination-directory* #p"output/" "Directory to write files out to for the WRITE-FILES node")

;; TODO: Consider renaming to "from-source" and "to-destination"
(defclass read-from-directory (source-node)
  ((snapshot :initform nil
	     :initarg :snapshot
	     :accessor read-from-directory-snapshot
	     :documentation "Previous directory snapshot"))
  (:documentation "Source node for enumerating files from *SOURCE-DIRECTORY*"))

(defmethod update ((node read-from-directory))
  (multiple-value-bind (changes snapshot) (dirmon:get-changes-in-directory
					   *source-directory*
					   :previous-snapshot (read-from-directory-snapshot node))
    (setf (read-from-directory-snapshot node) snapshot)
    (loop for (event pathname) in changes
	  collect (list (if (equal event :delete) :delete :update)
			(uiop/pathname:unix-namestring pathname)
			(make-instance 'item
				       :content (merge-pathnames (parse-namestring pathname)
								 *source-directory*))))))

(defclass write-to-directory (sink-node)
  ()
  (:documentation "Sink node for writing files to *DESTINATION-DIRECTORY*"))

(defgeneric write-content (content output-path)
  (:documentation "Writes CONTENT to OUTPUT-PATH, specialized on the type of CONTENT"))

(defmethod write-content ((content pathname) output-path)
  (spew "Copying ~a to ~a...~%" content output-path)
  (uiop:copy-file content output-path))

(defmethod write-content ((content string) output-path)
  (spew "Writing to ~a...~%" output-path)
  (with-open-file (stream output-path
			  :direction :output
			  :if-exists :supersede)
    (write-string content stream)))

(defmethod update ((node write-to-directory))
  (loop for (event path item) in (node-input node) do
    (let ((output-path (merge-pathnames (uiop:parse-unix-namestring path)
					*destination-directory*)))
      (ecase event
	(:update
	 (ensure-directories-exist output-path)
	 (write-content (item-content item) output-path))
	(:delete
	 (delete-file output-path))))))

;;; HTML template nodes
(defclass list-to-html (transform-node)
  ((include :initform '(:type "lhtml")))
  (:documentation "Transform that converts HTML in list form to an HTML string"))

(defmethod transform ((node list-to-html) pathstring input-item)
  (let ((item (item-clone input-item)))
    (setf (item-content item) (html (item-read-content item :type :object)))
    (cons (change-type pathstring "html")
	  item)))

;;; Processing pipeline graph
(defparameter *pipeline*
  '((read-from-directory . (list-to-html
			    write-to-directory))
    (list-to-html . (write-to-directory)))
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

(defun propagate-changes (changes children)
  "Propagates CHANGES to CHILDREN, respecting filters"
  ;; Separate children into "accepts :REST" vs. "explicit opt-in"
  (let ((explicit-children nil)
	(rest-children nil))
    (loop for child in children do
      (if (equal (node-include child) :rest)
	  (push child rest-children)
	  (push child explicit-children)))
    ;; Actually decide which children to propagate to
    (loop for change in changes
	  for taken = nil
	  do ;; Check for an explicit inclusion
	     (loop for child in explicit-children
		   do (when (should-include change child)
			(setf taken t)
			(push change (node-input child))))
	     (unless taken
	       (loop for child in rest-children
		     do (push change (node-input child)))))))

(defun log-update (name input-changes output-changes)
  "Logs input and output from node update"
  (spew "~a:~%" name)
  (loop for (event path) in input-changes do (spew "  ~a:~a~%" event path))
  (spew " -->~%")
  (loop for (event path) in output-changes do (spew "  ~a:~a~%" event path))
  (spew "~%"))

(defun run-pipeline ()
"Runs the given pipeline"
  (loop for (name . node) in *name-to-nodes*
	for children = (rest (assoc name *pipeline*))
	for input-changes = (node-input node)
	for output-changes = (update node)
	do ;; Debug logging
	   (when-logging (log-update name input-changes output-changes))
	   ;; Push changes to children
	   (propagate-changes output-changes
			      (loop for child-name in children
				    collect (rest (assoc child-name *name-to-nodes*))))))

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
