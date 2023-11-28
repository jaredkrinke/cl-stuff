(defpackage #:ssg
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

(defmacro gethash-or (key hash-table &key initform)
  "Same as GETHASH, except if there is no value for KEY, INITFORM is added as the value and returned"
  (with-gensyms (key-s hash-table-s value-s found-s)
    `(let ((,key-s ,key)
	   (,hash-table-s ,hash-table))
       (multiple-value-bind (,value-s ,found-s) (gethash ,key-s ,hash-table-s)
	 (unless ,found-s
	   (setf ,value-s ,initform)
	   (setf (gethash ,key-s ,hash-table-s) ,value-s))
	 ,value-s))))

(defmacro do-hash ((hash-table key value) &body body)
  "Runs BODY with KEY and VALUE set to each pair from HASH-TABLE"
  `(maphash (lambda (,key ,value) ,@body)
	    ,hash-table))

(defun alist-path (alist &rest path)
  "Returns the alist value associated with PATH (note: multiple values will be searched recursively)"
  (loop with result = alist
	for key in path
	do (setf result (rest (assoc key result)))
	finally (return result)))

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

(defmethod read-content-as-string ((content cons))
  (let ((stream (make-string-output-stream)))
    (prin1 content stream)
    (get-output-stream-string stream)))

(defgeneric read-content-as-object (content)
  (:documentation "Reads CONTENT as a Lisp object"))

(defmethod read-content-as-object ((content pathname))
  (uiop:safe-read-file-form content))

(defmethod read-content-as-object ((content list))
  content)

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

;;; Debugging nodes
(defclass log-items (transform-node)
  ()
  (:documentation "Debugging node that logs items"))

(defmethod transform ((node log-items) pathstring item)
  (format t "~a:~%" pathstring)
  (format t "~s~%" (item-metadata item))
  (format t "~a~%" (item-read-content item :type :string))
  (cons pathstring item))

;;; HTML template nodes
(defclass lhtml (transform-node)
  ((include :initform '(:type "lhtml")))
  (:documentation "Transform that converts HTML in list form to an HTML string"))

(defmethod transform ((node lhtml) pathstring input-item)
  (let ((item (item-clone input-item)))
    (setf (item-content item) (html (item-read-content item :type :object)))
    (cons (change-type pathstring "html")
	  item)))

;;; Blog nodes
(defclass front-matter (transform-node)
  ((include :initform '(:type "md")))
  (:documentation "Extracts front matter from Markdown files and adds it (along with :PATH-TO-ROOT) to item metadata"))

(defparameter *front-matter-pattern* (ppcre:create-scanner "^'''\\r?\\n(.*?\\r?\\n)'''\\r?\\n"
							   :single-line-mode t))

(defun create-path-to-root (pathstring)
  "Creates a pathstring to the root from PATHSTRING"
  (let* ((up "../")
	 (up-length (length up))
	 (depth (length (rest (pathname-directory (uiop:parse-unix-namestring pathstring)))))
	 (length (* up-length depth))
	 (result (make-string length)))
    (loop for i from 0 upto (1- length) do
      (setf (elt result i) (elt up (mod i up-length))))
    result))

(defmethod transform ((node front-matter) pathstring input-item)
  (let* ((item (item-clone input-item))
	 (content (item-read-content item :type :string)))
    (multiple-value-bind (start end starts ends) (funcall *front-matter-pattern* content 0 (length content))
      (when start
	(let ((front-matter (subseq content (elt starts 0) (elt ends 0)))
	      (metadata (item-metadata item)))
	  (loop for (key value) on (read-from-string front-matter) by 'cddr
		do (push (cons key value) metadata))
	  (push (cons :path-to-root (create-path-to-root pathstring)) metadata)
	  (push (cons :path-from-root (change-type pathstring "html")) metadata)
	  (setf (item-metadata item) metadata)
	  (setf (item-content item) (subseq content end)))))
    (cons pathstring item)))

(defclass markdown (transform-node)
  ((include :initform '(:type "md")))
  (:documentation "Parses Markdown into a document tree"))

(defun plist-replace (plist key value)
  "Replaces the value for KEY in PLIST with VALUE; returning a new copy of PLIST"
  (let ((plist2 (copy-tree plist)))
    (setf (getf plist2 key) value)
    plist2))

;; TODO: Could be destructive!
(defmacro process-tree ((var tree) &body cases)
  "Walks TREE running CASES (a list of (TAG &BODY), where TREE is set to the matching node and the output is spliced into the tree)"
  (with-gensyms (f)
    `(labels ((,f (,var)
		(cond ((null ,var) nil)
		      ((consp ,var)
		       (case (car ,var)
			 ,@cases
			 (t (cons (car ,var)
				  (mapcar #',f (cdr ,var))))))
		      (t ,var))))
       (funcall #',f ,tree))))

(defun fix-relative-link (href)
  "Returns HREF, changing relative Markdown (.md) links to point to resulting HTML (.html) files"
  (ppcre:regex-replace "^([^:]*).md(#.*)?$"
		       href
		       "\\1.html\\2"))

(defun fix-relative-links (tree)
  "Updates all relative links to Markdown (.md) files to point to resulting HTML (.html) files"
  (process-tree (tree tree)
    (:explicit-link (list :explicit-link
			  (plist-replace (rest tree)
					 :source
					 (fix-relative-link (getf (rest tree) :source)))))))

(defun anchorify (text)
  "Creates an anchor string from TEXT, e.g. \"This is a heading!\" becomes \"this-is-a-heading\""
  (string-downcase
   ; Remove trailing hyphens
   (ppcre:regex-replace
    "-+$"
    ; Collapse non-alphanumeric into hyphens
    (ppcre:regex-replace-all "[^a-zA-Z0-9]+" text "-")
    "")))

(defun get-children (lhtml)
  "Gets child nodes for LHTML tree"
  (loop for child on (rest lhtml) by 'cddr
	while (keywordp (car child))
	finally (return child)))

(defun get-inner-text (tree)
  "Walks LHTML tree TREE and concatenates text nodes"
  (let ((stream (make-string-output-stream)))
    (labels ((walk-tree (tree)
	       (cond ((null tree) nil)
		     ((stringp tree) (write-string tree stream))
		     ((consp tree) (loop for child in (get-children tree)
					 do (walk-tree child))))))
      (walk-tree tree))
    (get-output-stream-string stream)))

(defun document-to-lhtml (tree)
  "Converts a parsed Markdown document to LHTML"
  (cond ((null tree) nil)
	((consp tree)
	 (let ((children (rest tree)))
	   (macrolet ((recurse (nodes) `(mapcar #'document-to-lhtml ,nodes)))
	     (ecase (first tree)
	       (:fragment (cons :fragment (recurse children)))
	       (:heading (let ((processed-children (recurse (getf children :contents))))
			   `(,(intern (format nil "H~a" (getf children :level)) 'keyword)
			     :name ,(anchorify (get-inner-text (cons :fragment processed-children)))
			     ,@processed-children)))
	       (:paragraph (cons :p (recurse children)))
	       (:counted-list (cons :ol (recurse children)))
	       (:bullet-list (cons :ul (recurse children)))
	       (:list-item (cons :li (recurse children)))
	       (:explicit-link `(:a :href ,(getf (cadr tree) :source)
				    ,@(recurse (getf (cadr tree) :label))))
	       (:strong (cons :strong (recurse children)))
	       (:emph (cons :em (recurse children)))
	       (:code (cons :code (recurse children)))
	       (:plain (cons :p (recurse children))))))) ; TODO: What is :PLAIN?
	(t tree)))

(defmethod transform ((node markdown) pathstring input-item)
  (let* ((item (item-clone input-item))
	 (content (item-read-content item :type :string))
	 (document-raw (cons :fragment (3bmd-grammar:parse-doc content)))
	 (document (fix-relative-links document-raw))
	 (lhtml (document-to-lhtml document)))
    (setf (item-content item) lhtml)
    (cons (change-type pathstring "lhtml")
	  item)))

(defclass index-posts (aggregate-node)
  ((include :initform '(:type "md")))
  (:documentation "Adds all post to a list that is ordered by date"))

(defmethod aggregate ((node index-posts) snapshot)
  (let* ((posts (loop for item being the hash-values in snapshot
		      collect (item-metadata item)))
	 (sorted-posts (sort posts (lambda (b a) (string< (alist-path a :date)
							  (alist-path b :date))))))
    (list (cons "#index"
		(make-instance 'item
			       :content sorted-posts)))))

(defparameter *site-title* "Test Blog")

(defun template-base (&key body title description keywords path-to-root)
  "Creates an HTML tree for a page with BODY as content"
  `(:html :lang "en"
	  (:head
	   (:meta :charset "utf-8")
	   (:title ,(or title *site-title*))
	   ,@(when description `((:meta :name "description" :content ,description)))
	   ,@(when keywords `((:meta :name "keywords" :content ,(string-join keywords ","))))
	   (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
	   ;; TODO: CSS link
	   ;; TODO: RSS link on root
	   ;; TODO: LD-JSON stuff?
	   (:body
	    (:header
	     (:h1 (:a :href ,(concatenate 'string path-to-root "index.html")
		      ,*site-title*)))
	    ;; TODO: Site subtitle and maybe links
	    (:main ,@body)))))

(defun partial-navigation (&key tags incomplete tag-current path-to-root)
  "Creates a navigation partial with links to TAGS"
  `(:nav
    (:strong "Topics: ") ; TODO: non-breaking space?
    ,@(loop with first = t
	    for tag in tags
	    for tag-string = (string-downcase (symbol-name tag))
	    nconc `(,@(if first
			  (setf first nil)
			  '(" | "))
		    ,@(if (eql tag tag-current)
			  (list tag-string)
			  (list (list :a :href (concatenate 'string path-to-root "posts/" tag-string "/index.html")
				      tag-string)))))
    ,@(when incomplete (list " | "
			     (list :a :href (concatenate 'string path-to-root "archive.html")
				   "..."))))) ; TODO: ellipsis

(defun partial-date (date)
  "Creates a date partial for DATE"
  `(:p (:time :datetime ,date
	      ,date))) ; TODO: Format date

(defun partial-post-summary (&key title date description path-to-root path-from-root)
  "Creates a partial for a summary of a post"
  `(:article
    ;; TODO: Resolve paths instead of just concatenating?
    (:header (:h1 (:a :href ,(concatenate 'string path-to-root path-from-root)
		      ,title))
	     ,(partial-date date))
    ,@(when description `((:p ,description)))))

(defun partial-post-summary-list (posts)
  "Creates a partial for a list of post summaries"
  `(:ul
    ,@(loop for post across posts
	    collect (list :li (partial-post-summary
			       :title (alist-path post :title)
			       :date (alist-path post :date)
			       :description (alist-path post :description)
			       :path-to-root (alist-path post :path-to-root)
			       :path-from-root (alist-path post :path-from-root))))))

(defclass template-posts (transform-node)
  ((include :initform '(:type "lhtml")))
  (:documentation "Applies template to LHTML posts"))

(defun template-post (metadata body)
  (template-base :title (alist-path metadata :title)
;		 :subtitle (alist-path metadata :date)
		 :body body))

(defmethod transform ((node template-posts) pathstring input-item)
  (let* ((item (item-clone input-item))
	 (content (template-post (item-metadata item)
				 (rest (item-read-content item :type :object)))))
    (setf (item-content item) content)
    (cons pathstring
	  item)))

(defclass template-indexes (aggregate-node)
  ((include :initform "#index"))
  (:documentation "Creates index and tag index pages from the page index"))

(defun template-index (&key pathstring posts title tags)
  (declare (ignore pathstring tags))
  (template-base
   :title title
   :body (list (partial-post-summary-list posts))))

(defun create-index-item (&key pathstring posts title tags)
  (cons pathstring
	(make-instance 'item
		       :content (template-index :title title
						:tags tags
						:posts posts))))

(defmethod aggregate ((node template-indexes) snapshot)
  (let ((results nil)
	(tag-to-posts (make-hash-table :test 'equal))
	(index (item-read-content (gethash "#index" snapshot) :type :object)))
    ;; Create vector of posts for each tag
    (loop for metadata in index do
      (loop for tag in (alist-path metadata :keywords) do
	(let ((vector (gethash-or tag
				  tag-to-posts
				  :initform (make-array 5
							:initial-element 0
							:fill-pointer 0
							:adjustable t))))
	  (vector-push-extend metadata vector))))
    ;; TODO: Sort tags
    ;; TODO: Main index
    ;; TODO: Archive
    (do-hash (tag-to-posts tag posts)
      ;; TODO: Move to helper
      (let ((tag-string (string-downcase (symbol-name tag))))
	(push (create-index-item :pathstring (uiop:unix-namestring
					      (make-pathname :directory (list :relative
									      "posts"
									      tag-string)
							     :name "index"
							     :type "lhtml"))
				 :title (format nil "Posts tagged with: ~a" tag-string)
				 :posts posts)
	      results)))
    results))

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

(defparameter *pipeline*
  '((source :children (front-matter))
    (front-matter :children (markdown
			     index-posts))
    (markdown :children (template-posts))
    (template-posts :children (lhtml))
    (index-posts :children (template-indexes))
    (template-indexes :children (lhtml))
    (lhtml :children (destination))
;    (log-items)
    (destination)))

(defvar *pipeline-instance* nil)

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

(defun reset ()
  (setf *pipeline-instance* (make-pipeline *pipeline*)))

(defun run (&optional (pipeline *pipeline-instance*))
  "Runs PIPELINE"
  (loop for node in pipeline do
    (let ((output-changes (update node)))
      ;; Debug logging
      (when-logging
	(log-update node
		    (node-input node)
		    output-changes))
      ;; Push changes to children
      (propagate-changes output-changes
			 (node-children node)))))

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
