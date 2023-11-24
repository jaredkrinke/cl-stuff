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

;; TODO: specialize on both content *and* a type parameter
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
   (children :documentation "List of child nodes in the pipeline"
	     :accessor node-children
	     :initarg :children
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
    (when changes
      (loop for (event path item) in changes do
	(ecase event
	  (:update (setf (gethash path cached-input) item))
	  (:delete (remhash path cached-input))))
      (resolve-results (loop for path being the hash-keys in cached-output collect path)
		       (aggregate node cached-input)
		       cached-output))))

(defun filter-test (change filter)
  "Returns non-NIL if CHANGE matches FILTER; supported filters:

* NIL matches nothing
* T matches everything
* \"foo\" matches (only) the exact (UNIX-style) path
* :REST matches anything that wasn't specifically included by another sibling
* (:TYPE \"foo\" matches items base on file type (\"foo\" in this example)"
  ;; TODO: Some way to match based on path?
  ;; TODO: Compile these!
  ;; TODO: Allow list of these (would these be intersection or union?)
  (cond ((null filter) nil)
	((eql filter t) t)
	((eql filter :rest) (error "Unexpected :REST (exclusion?) filter!"))
	((stringp filter) (equal filter (second change)))
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

(defclass read-from-directory (source-node)
  ((directory :initarg :directory
	      :accessor read-from-directory-directory
	      :documentation "Source directory for this node")
   (snapshot :initform nil
	     :initarg :snapshot
	     :accessor read-from-directory-snapshot
	     :documentation "Previous directory snapshot"))
  (:documentation "Source node for enumerating files from a directory"))

(defmethod update ((node read-from-directory))
  (with-slots (directory read-from-directory-directory) node
    (multiple-value-bind (changes snapshot) (dirmon:get-changes-in-directory
					     directory
					     :previous-snapshot (read-from-directory-snapshot node))
      (setf (read-from-directory-snapshot node) snapshot)
      (loop for (event pathname) in changes
	    collect (list (if (equal event :delete) :delete :update)
			  (uiop/pathname:unix-namestring pathname)
			  (make-instance 'item
					 :content (merge-pathnames (parse-namestring pathname)
								   directory)))))))

(defclass source (read-from-directory)
  ((directory :initform *source-directory*))
  (:documentation "Source node for reading from *SOURCE-DIRECTORY*"))

(defclass suppress-output (sink-node)
  ()
  (:documentation "Sink node that just drops files"))

(defclass write-to-directory (sink-node)
  ((directory :initarg :directory
	      :accessor write-to-directory-directory
	      :documentation "Output directory for this node"))
  (:documentation "Sink node for writing to a directory"))

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
  (with-slots (directory write-to-directory-directory) node
    (loop for (event path item) in (node-input node) do
      (let ((output-path (merge-pathnames (uiop:parse-unix-namestring path)
					  directory)))
	(ecase event
	  (:update
	   (ensure-directories-exist output-path)
	   (write-content (item-content item) output-path))
	  (:delete
	   (delete-file output-path)))))))

(defclass destination (write-to-directory)
  ((directory :initform *destination-directory*))
  (:documentation "Sink node for writing files to *DESTINATION-DIRECTORY*"))

;;; HTML template nodes
(defclass list-to-html (transform-node)
  ((include :initform '(:type "lhtml")))
  (:documentation "Transform that converts HTML in list form to an HTML string"))

(defmethod transform ((node list-to-html) pathstring input-item)
  (let ((item (item-clone input-item)))
    (setf (item-content item) (html (item-read-content item :type :object)))
    (cons (change-type pathstring "html")
	  item)))

;;; make-blog nodes
(defun write-string-to-file (string filespec)
  "Writes STRING to FILESPEC"
  (with-open-file (stream filespec :direction :output :if-exists :supersede)
    (write-string string stream)))

(defun item-from-string (string)
  (make-instance 'item :content string))

(defun item-from-file (filespec)
  (item-from-string (uiop:read-file-string filespec)))

(defun extract-post-path (path)
  (subseq path 0 (position #\. path :from-end t)))

(defclass front-matter (transform-node)
  ((include :initform '(:type "md")))
  (:documentation "Reads Markdown posts and outputs metadata and Markdown content"))

(defmethod transform ((node front-matter) path item)
  (let ((post-path (extract-post-path path)))
    (uiop:with-temporary-file (:pathname metadata-pathname)
      (uiop:with-temporary-file (:pathname markdown-pathname)
	;; TODO: Could this be done with an in-memory stream?
	(uiop:with-temporary-file (:pathname input-pathname)
	  (write-string-to-file (item-read-content item :type :string) input-pathname)
	  (uiop:run-program (format nil
				    "leano front-matter.js ~a ~a.html ~a ~a"
				    input-pathname
				    post-path
				    metadata-pathname
				    markdown-pathname))
	  (cons (format nil "~a.content-md" post-path)
		(make-instance 'item
			       :content (uiop:read-file-string markdown-pathname)
			       :metadata (list (cons :json (uiop:read-file-string metadata-pathname))))))))))

(defclass markdown (transform-node)
  ((include :initform '(:type "content-md")))
  (:documentation "Reads Markdown content and outputs HTML content"))

(defmethod transform ((node markdown) path item)
  (let ((post-path (extract-post-path path)))
    (uiop:with-temporary-file (:pathname html-pathname)
      (uiop:with-temporary-file (:pathname input-pathname)
	(write-string-to-file (item-read-content item :type :string) input-pathname)
	(uiop:run-program (format nil
				  "deno run --allow-read --allow-write markdown.ts ~a ~a"
				  input-pathname
				  html-pathname))
	(cons (format nil "~a.content-html" post-path)
	      (make-instance 'item
			     :content (uiop:read-file-string html-pathname)
			     :metadata (item-metadata item)))))))

(defclass template-posts (transform-node)
  ((include :initform '(:type "content-html")))
  (:documentation "Reads HTML post content and applies templates, resulting in final HTML"))

(defmethod transform ((node template-posts) path item)
  (let ((post-path (extract-post-path path)))
    (uiop:with-temporary-file (:pathname html-pathname)
      (uiop:with-temporary-file (:pathname input-pathname)
	(uiop:with-temporary-file (:pathname input-metadata-pathname)
	  (write-string-to-file (item-read-content item :type :string) input-pathname)
	  (write-string-to-file (rest (assoc :json (item-metadata item))) input-metadata-pathname)
	  (uiop:run-program (format nil
				    "leano template-post.js cache/site.json ~a ~a ~a"
				    input-metadata-pathname
				    input-pathname
				    html-pathname))
	  (cons (format nil "~a.html" post-path)
		(item-from-file html-pathname)))))))

(defclass index-posts (aggregate-node)
  ((include :initform '(:type "content-md")))
  (:documentation "Reads post metadata and produces an index"))

(defmethod aggregate ((node index-posts) snapshot)
  (uiop:with-temporary-file (:pathname path-pathname)
    (uiop:with-temporary-file (:pathname json-pathname)
      (with-open-file (path-stream path-pathname :direction :output :if-exists :supersede)
	(with-open-file (json-stream json-pathname :direction :output :if-exists :supersede)
	  ;; Produce a list of paths and JSON (one file per line) for consumption by script
	  (maphash (lambda (path item)
		     (fresh-line path-stream)
		     (write-string path path-stream)
		     (fresh-line json-stream)
		     (write-string (rest (assoc :json (item-metadata item))) json-stream))
		   snapshot)))
      ;; Consume via script
      (uiop:with-temporary-file (:pathname index-pathname)
	(uiop:run-program (format nil
				  "leano index.js ~a ~a ~a"
				  path-pathname
				  json-pathname
				  index-pathname))
	(list (cons "posts.index-json"
		    (item-from-file index-pathname)))))))

(defmacro with-temporary-directory ((name id) &body body)
  "Runs BODY with a directory named ID bound to NAME"
  (with-gensyms (result)
    `(let ((,result nil)
	   (,name (make-pathname :directory '(:relative "cache" ,id))))
       (ensure-directories-exist ,name)
       (setf ,result (progn ,@body))
       (uiop:delete-directory-tree ,name :validate t)
       ,result)))

(defclass template-indexes (aggregate-node)
  ((include :initform "posts.index-json"))
  (:documentation "Reads index and generates HTML files for each"))

(defmethod aggregate ((node template-indexes) snapshot)
  (let ((index-item (gethash "posts.index-json" snapshot))
	(output nil))
    (uiop:with-temporary-file (:pathname index-pathname)
      (with-temporary-directory (temporary-directory "tmp-index")
	(write-string-to-file (item-read-content index-item :type :string) index-pathname)
	(uiop:run-program (format nil
				  "leano template-indexes.js cache/site.json ~a ~a"
				  index-pathname
				  temporary-directory))
	(loop for (event file) in (dirmon:get-changes-in-directory temporary-directory) do
	  (push (cons file
		      (make-instance 'item
				     :content (uiop:read-file-string
					       (merge-pathnames (parse-namestring file)
								temporary-directory))))
		output)))
      output)))

(defclass template-feed (aggregate-node)
  ((include :initform t))
  (:documentation "Reads index and HTML content and generates an Atom feed"))

(defmethod aggregate ((node template-feed) snapshot)
  (let ((index-item (gethash "posts.index-json" snapshot)))
    (uiop:with-temporary-file (:pathname feed-pathname)
      (uiop:with-temporary-file (:pathname index-pathname)
	(with-temporary-directory (temporary-directory "tmp-feed")
	  (write-string-to-file (item-read-content index-item :type :string) index-pathname)
	  ;; Write files to temporary directory
	  (maphash (lambda (path item)
		     (let ((pathname (merge-pathnames (uiop:parse-unix-namestring path)
						      temporary-directory)))
		       (ensure-directories-exist pathname)
		       (write-string-to-file (item-read-content item :type :string)
					     pathname)))
		   snapshot)
	  (uiop:run-program (format nil
				    "leano template-feed.js ~a cache/site.json ~a ~a"
				    temporary-directory
				    index-pathname
				    feed-pathname))
	  (list (cons "feed.xml"
		      (item-from-file feed-pathname))))))))

(defclass template-misc (aggregate-node)
  ((include :initform "site.json"))
  (:documentation "Creates CSS and 404 page"))

(defmethod aggregate ((node template-misc) snapshot)
  (uiop:with-temporary-file (:pathname 404-pathname)
    (uiop:with-temporary-file (:pathname css-pathname)
      (uiop:run-program (format nil
				"deno run --allow-read --allow-write template-404.ts cache/site.json ~a"
				404-pathname))
      (uiop:run-program (format nil
				"deno run --allow-read --allow-write template-css.ts cache/site.json ~a"
				css-pathname))
      (list (cons "404.html"
		  (item-from-file 404-pathname))
	    (cons "css/style.css"
		  (item-from-file css-pathname))))))

;;; Processing pipeline graph
(defun add-edge (node child)
  "Adds an edge from NODE to CHILD"
  (push child (node-children node)))

(defun get-edges (nodes)
  "Returns a list of edges from the graph represented by NODES"
  (loop for node in nodes
	nconc (loop for child in (node-children node)
		    collect (cons node child))))

(defun pipeline-sort (nodes)
  "Returns a topological sort of the graph represented by NODES"
  (let ((edges (get-edges nodes))
	(start-nodes (copy-list nodes))
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

(defstruct node-info
  node
  parents
  children)

(defun make-pipeline (node-graph)
  "Creates a pipeline from the description, NODE-GRAPH. Each item in the (quoted) list describes a node in the following format:

(NAME &KEY (:PARENTS NIL)
           (:CHILDREN NIL)
      &ALLOW-OTHER-KEYS)

The named arguments are passed to (MAKE-INSTANCE 'NAME), except for :PARENTS and :CHILDREN which are used to construct edges in the graph. Either :PARENTS or :CHILDREN is supported; use whichever seems most convenient or easiest to understand.

Note: the result is a topologically sorted list of node instances."
  (let ((pipeline nil)
	(name-to-info (make-hash-table)))
    ;; Initialize nodes
    (loop for (name . arguments) in node-graph do
      (let ((actual-arguments (copy-list arguments)))
	(remf actual-arguments :parents)
	(remf actual-arguments :children)
	;; TODO: Arguments need to be evaluated, right?
	(let ((node (apply #'make-instance (cons name actual-arguments))))
	  (push node pipeline)
	  (setf (gethash name name-to-info)
		(make-node-info :parents (getf arguments :parents)
				:children (getf arguments :children)
				:node node)))))
    ;; Add edges
    (loop for info being the hash-values in name-to-info do
      (let ((node (node-info-node info)))
	(loop for parent-name in (node-info-parents info) do
	  (add-edge (node-info-node (gethash parent-name name-to-info))
		    node))
	(loop for child-name in (node-info-children info) do
	  (add-edge node
		    (node-info-node (gethash child-name name-to-info))))))
    ;; Sort
    (pipeline-sort pipeline)))

(defvar *pipeline*
  (make-pipeline '((source :children (front-matter
				      template-misc
				      destination))
		   (front-matter :children (markdown
					    index-posts))
		   (markdown :children (template-posts
					template-feed))
		   (index-posts :children (template-indexes
					   template-feed))
		   (template-posts :children (destination))
		   (template-indexes :children (destination))
		   (template-feed :children (destination))
		   (template-misc :children (destination))
		   (destination))))

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

(defun log-update (node input-changes output-changes)
  "Logs input and output from node update"
  (spew "~a:~%" node)
  (loop for (event path) in input-changes do (spew "  ~a:~a~%" event path))
  (spew " -->~%")
  (loop for (event path) in output-changes do (spew "  ~a:~a~%" event path))
  (spew "~%"))

(defun run (&optional (pipeline *pipeline*))
  "Runs PIPELINE"
  (loop for node in pipeline
	for children = (node-children node)
	for input-changes = (node-input node)
	for output-changes = (update node)
	do ;; Debug logging
	   (when-logging (log-update node input-changes output-changes))
	   ;; Push changes to children
	   (propagate-changes output-changes
			      (node-children node))))

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
