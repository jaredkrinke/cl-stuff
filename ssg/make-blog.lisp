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
