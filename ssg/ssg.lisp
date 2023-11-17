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
;; (defun for-each-file-in-directory (directory function &key (process-file-p (constantly t)))
;;   "Calls FUNCTION on each file (directly) within BASE-DIRECTORY, optionally filtering out files"
;;   (loop for file in (uiop:directory-files directory)
;; 	do (when (funcall process-file-p file)
;; 	     (funcall function file))))

;; (defun for-each-file (base-directory function &key (process-directory-p (constantly t)) (process-file-p (constantly t)))
;;   "Calls FUNCTION for each file under (the entire tree of) BASE-DIRECTORY, optionally filtering out directories and files"
;;   (uiop:collect-sub*directories base-directory
;; 				process-directory-p
;; 				process-directory-p
;; 				(lambda (directory)
;; 				  (for-each-file-in-directory directory
;; 							      function
;; 							      :process-file-p process-file-p))))

(defun relevant-directory-p (directory)
  "Returns non-NIL if the directory is 'relevant', meaning not part of source control, etc."
  (let ((directory-name (first (last (pathname-directory directory)))))
    (not (equal directory-name ".git"))))

;; (defun path-relative-to (base-directory pathname)
;;   (enough-namestring (truename pathname) (truename base-directory)))

;;; Processing and dependency graph
(defclass item ()
  ;; ((id :documentation "Path/id for the item"
  ;;      :accessor item-id
  ;;      :initarg :id)
   ;; (time :documentation "'Last modified time' for this item"
   ;; 	 :accessor item-time
   ;; 	 :initarg :time)
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
  ((input :documentation "Name or pattern indicating inputs"
	  :accessor node-input
	  :initarg :input)
   ;; (time :documentation "Time of last (actual) update"
   ;; 	 :accessor node-time
   ;; 	 :initform nil)
   (snapshot :documentation "If available, cached snapshot (hash of path to item) from previous runs"
	     :accessor node-snapshot
	     :initarg :snapshot
	     :initform nil))
  (:documentation "Represents an arbitrary node in the processing graph"))

(defclass source-node (node)
  ()
  (:documentation "Represents a node that initially enumerates items from a data source (note: these nodes *always* must run)"))

(defclass transform-node (node)
  ((cached-outputs :initform (make-hash-table :test 'equal)
		   :accessor transform-node-cached-outputs
		   :documentation "Cached hash of input to output(s), needed for handling deletions"))
  (:documentation "Represents a 1:N processing node in the graph"))

(defclass aggregate-node (node)
  ()
  (:documentation "Represents an M:N processing node in the graph"))

(defclass sink-node (node)
  ()
  (:documentation "Represents a sink node in the graph, e.g. for writing files to disk"))

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
      (setf (gethash path snapshot) item)
      (push (list :update path item) changes)
      (deletef path removed-paths))
    (loop for path in removed-paths do
      (push (list :delete path nil) changes))
    changes))

(defmethod update ((node transform-node) changes)
  (let ((snapshot (node-snapshot node))
	(cached-outputs (transform-node-cached-outputs node)))
    (loop for (event input-path input-item) in changes
	  nconc (resolve-results (gethash input-path cached-outputs)
				 (ecase event
				   (:update (transform node input-path input-item))
				   (:delete nil))
				 snapshot))))

(defmethod update ((node aggregate-node) changes)
  (let ((snapshot (node-snapshot node)))
    (resolve-results (loop for path being the hash-keys in snapshot collect path)
		     (aggregate node changes)
		     snapshot)))

;;; Built-in nodes
(defparameter *source-directory* #p"input/" "Directory to read input files from for the READ-DIRECTORY source code")
(defparameter *destination-directory* #p"output/" "Directory to write files out to for the WRITE-FILES node")

(defclass read-directory (source-node)
  ((previous-snapshot :initform nil
		      :initarg :previous-snapshot
		      :accessor read-directory-previous-snapshot
		      :documentation "Previous directory snapshot"))
  (:documentation "Source node for enumerating files from *SOURCE-DIRECTORY*"))

(defmethod update ((node read-directory) (input-changes null))
  (multiple-value-bind (changes snapshot) (dirmon:get-changes-in-directory
					   *source-directory*
					   :previous-snapshot (read-directory-previous-snapshot node))
    (setf (read-directory-previous-snapshot node) snapshot)
    (loop for (event . path) in changes
	  collect (list (if (equal event :delete) :delete :update)
			path
			nil))))

;; TODO: Should support reading bytes, strings, and objects
;; (defclass read-as-text (transform-node)
;;   (:documentation "Source node for reading files as text from *SOURCE-DIRECTORY*"))

;; (defmethod transform ((node read-as-text) path item)
;;   (with-open-file (stream (merge-pathnames path *source-directory*))
;;     (read

;; (defmethod aggregate ((node read-files) (items null))
;;   (let ((result nil))
;;     (for-each-file *source-directory*
;; 		   (lambda (file)
;; 		     (push (make-instance 'item
;; 					  :id (path-relative-to *source-directory*
;; 								file)
;; 					  :time (file-write-date file)
;; 					  :content (with-open-file (stream file)
;; 						     (read stream)))
;; 			   result)))
;;     result))

;; ;; TODO: Needs to be aggregate in order to catch deleted files!
;; (defclass write-files (transform-node)
;;   ()
;;   (:documentation "Sink node for writing items out to *DESTINATION-DIRECTORY*"))

;; (defmethod transform ((node write-files) (item item))
;;   ;; TODO: Handle subdirectories!
;;   (let ((output-pathname (merge-pathnames (item-id item) *destination-directory*)))
;;     (with-open-file (stream output-pathname
;; 			    :direction :output
;; 			    :if-exists :supersede)
;;       (write-string (item-content item) stream)))
;;   item)

;;; HTML template nodes
;; (defclass list-to-html (transform-node)
;;   ()
;;   (:documentation "Transform that converts HTML in list form to an HTML string"))

;; (defmethod transform ((node list-to-html) (item item))
;;   ;; TODO: This needs to clone the item instead of overwriting a property!
;;   (setf (item-content item) (html (item-content item)))
;;   item)

;;; Processing pipeline graph
;; (defparameter *pipeline*
;;   '((read-files . (list-to-html))
;;     (list-to-html . (write-files)))
;;   "Processing pipeline as a directed acyclic graph, represented as list of (NODE-NAME DOWNSTREAM-NODE-NAME-1 ...)")

;; (defvar *name-to-nodes* nil "A-list mapping node names to the nodes themselves")
;; (defvar *reversed-pipeline* nil "*PIPELINE* with edges reversed, to find nodes' prerequisites")

;; (defun pipeline-edges (pipeline)
;;   "Returns a (new) list of edges in PIPELINE"
;;   (let ((edges nil))
;;     (loop for row in pipeline
;; 	  for from = (first row)
;; 	  do (loop for to in (rest row)
;; 		   do (push (cons from to) edges)))
;;     edges))

;; (defun pipeline-nodes (pipeline)
;;   "Returns a (new) list of nodes in PIPELINE"
;;   (let ((nodes nil))
;;     (loop for row in pipeline do
;;       (pushnew (first row) nodes)
;;       (loop for child in (rest row) do
;; 	(pushnew child nodes)))
;;     nodes))

;; (defun pipeline-sort (pipeline)
;;   "Returns a topological sort of the nodes in PIPELINE"
;;   (let ((edges (pipeline-edges pipeline))
;; 	(start-nodes (pipeline-nodes pipeline))
;; 	(sorted-nodes nil))
;;     (loop for edge in edges do (deletef (rest edge) start-nodes))
;;     (loop while start-nodes do
;;       (let ((node (pop start-nodes)))
;; 	(push node sorted-nodes)
;; 	(loop for edge in (loop for edge in edges
;; 			     if (equal node (first edge))
;; 			       collect edge)
;; 	      for to = (rest edge)
;; 	      do (deletef edge edges)
;; 		 (unless (find-if (lambda (e) (equal to (rest e))) edges)
;; 		   (push to start-nodes)))))
;;     (if edges
;; 	(error "PIPELINE has a cycle!")
;; 	(nreverse sorted-nodes))))

;; ;;; TODO: Instead of doing this, attach parents directly to the NODE objects
;; (defun pipeline-reverse (pipeline)
;;   "Takes PIPELINE as list of (PARENT . (CHILD1 ...)) and returns a list of (CHILD . (PARENT1 ...))"
;;   (let ((edges (pipeline-edges pipeline))
;; 	(result nil))
;;     (loop for (parent . child) in edges do
;;       (let ((row (assoc child result)))
;; 	(if row
;; 	    (append-itemf parent row)
;; 	    (push (list child parent) result))))
;;     result))

;; (defun initialize-pipeline ()
;;   "Initializes the processing graph from *PIPELINE*"
;;   ;; TODO: Eventually, results should be persisted to disk and loaded here
;;   (setf *name-to-nodes* (loop for name in (pipeline-sort *pipeline*)
;; 			      collect (cons name (make-instance name))))
;;   (setf *reversed-pipeline* (pipeline-reverse *pipeline*)))

;; (defun newest-item-time (items)
;;   "Returns the newest ITEM-TIME in ITEMS"
;;   (reduce #'max (mapcar #'item-time items) :initial-value 0))

;; (defun results-differ-p (old new)
;;   "Returns non-NIL if result set NEW is newer than result set OLD (either contains newer files, or file set is different)"
;;   (if (eql old new)
;;       nil
;;       (or (let ((newest-old (newest-item-time old))
;; 		(newest-new (newest-item-time new)))
;; 	    (> newest-new newest-old))
;; 	  (let ((ids-old (mapcar #'item-id old))
;; 		(ids-new (mapcar #'item-id new)))
;; 	    (set-difference ids-old ids-new :test 'equal)))))

;; (defun run-pipeline ()
;;   "Runs the given pipeline"
;;   (loop for (name . node) in *name-to-nodes*
;; 	for parents = (rest (assoc name *reversed-pipeline*))
;; 	for node-time = (node-time node)
;; 	for input-time = (if parents
;; 			     (reduce #'max
;; 				     (mapcar #'node-time
;; 					     ;; TODO: This could be avoided by linking nodes instead of names!
;; 					     (mapcar (lambda (parent) (rest (assoc parent *name-to-nodes*)))
;; 						     parents))
;; 				     :initial-value 0)
;; 			     (1+ (get-universal-time)))
;; 	do (when (or (not node-time)
;; 		     (> input-time node-time))
;; 	     (let* ((snapshots (node-snapshot node))
;; 		    (input (loop for parent in parents
;; 				 append (node-snapshot (rest (assoc parent *name-to-nodes*)))))
;; 		    (results (update node input)))
;; 	       (format t "~a: ~s -> ~s~%" name (mapcar #'item-id input) (mapcar #'item-id results))
;; 	       (when (results-differ-p snapshots results)
;; 		 (setf (node-snapshot node) results)
;; 		 (setf (node-time node) (get-universal-time)))))))

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
