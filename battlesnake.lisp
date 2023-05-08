(defpackage :battlesnake
  (:documentation "Battlesnake implementations and webhook host")
  (:nicknames bs)
  (:use :cl)
  (:import-from :arrow-macros :->))

(in-package :battlesnake)

;;; Utility functions
(defun alist-path-recursive (object keys)
    (if keys
	(alist-path-recursive (cdr (assoc (car keys) object)) (cdr keys))
	object))

(defun alist-path (object &rest keys)
  "Walk down an AList path"
  (alist-path-recursive object keys))

;;; Battlesnake logic helpers
(defvar *verbose* nil "Set to non-nil to enable verbose logging")

(defparameter +delta-to-direction+
  '(((0 1) . "up")
    ((0 -1) . "down")
    ((-1 0) . "left")
    ((1 0) . "right")))

(defmacro spew (&rest args)
  "Logs to the console, if *verbose* is non-nil"
  `(and *verbose* (format t ,@args)))

(defun init-deltas ()
  "Returns all possible deltas in a list"
  ;;; TODO: Could be extracted from +delta-to-direction+
  (list '(0 1) '(0 -1) '(-1 0) '(1 0)))

(defun delta-to-direction (delta)
  "Converts a delta to a direction string"
  (cdr (assoc delta +delta-to-direction+ :test 'equal)))

(defun select-delta (deltas)
  "Chooses randomly from a list of deltas (returns 'up' if the list is empty)"
  (let ((length (length deltas)))
    (if (> length 0)
	(nth (random length) deltas)
	'(0 1))))

(defun point-to-list (point)
  "Converts from Battlesnake coordinates to a list, e.g. '{x: 1, y: 2}' to (1 2)"
  (list (cdr (assoc :x point)) (cdr (assoc :y point))))

(defun add-delta (point delta)
  "Adds a delta to a position"
  (mapcar #'+ point delta))

(defun subtract-delta (point delta)
  "Subtracts a delta from a position"
  (mapcar #'- point delta))

(defun out-of-boundsp (position dimensions)
  "Returns true if the position is out of bounds"
  (loop for x in position
	for length in dimensions
	when (or (< x 0) (>= x length)) return t))

(defun prune-out-of-bounds (deltas data)
  "Prunes deltas that would result in a wall collision"
  (let* ((head-position (point-to-list (alist-path data :you :head)))
	 (board (cdr (assoc :board data)))
	 (dimensions (list (cdr (assoc :width board)) (cdr (assoc :height board)))))
    (remove-if #'(lambda (delta) (out-of-boundsp (add-delta head-position delta) dimensions))
	       deltas)))

(defun prune-self (deltas data)
  "Prunes deltas that would collide with itself"
  (let* ((self (cdr (assoc :you data)))
	 (head-position (point-to-list (alist-path self :head)))
	 (next-positions (mapcar #'(lambda (delta) (add-delta head-position delta)) deltas))
	 (body-positions (mapcar #'(lambda (point) (point-to-list point))
				 (cdr (assoc :body self)))))
    (mapcar #'(lambda (position) (subtract-delta position head-position))
	    (set-difference next-positions body-positions :test 'equal))))

(defun make-board-mask (data)
  "Creates a two-dimensional Boolean array for occupied spaces"
  ;;; TODO: Avoid hazards too?
  (let* ((board (alist-path data :board))
	 (dimensions (list (alist-path board :width) (alist-path board :height)))
	 (mask (make-array dimensions :initial-element nil))
	 (snakes (alist-path board :snakes)))
    (loop for snake in snakes do
      (loop for point in (alist-path snake :body) do
	(let ((position (point-to-list point)))
	  (setf (apply #'aref mask position) t))))
    mask))

(defun prune-occupied (deltas data)
  "Prunes deltas that would result in a wall collision"
  (let* ((head-position (point-to-list (alist-path data :you :head)))
	 (occupied (make-board-mask data)))
    (remove-if #'(lambda (delta)
		   (apply #'aref occupied (add-delta head-position delta)))
	       deltas)))

;;; Battlesnake logic entry points
(defun think-random (data)
  "Moves randomly, possibly even into itself"
  (declare (ignore data))
  (select-delta (init-deltas)))

(defun think-bounds (data)
  "Move randomly, but avoids going out of bounds"
  (select-delta (prune-out-of-bounds (init-deltas) data)))

(defun think-self (data)
  "Move randomly, avoiding walls and self"
  (-> (init-deltas)
    (prune-out-of-bounds data)
    (prune-self data)
    (select-delta)))

(defun think-empty (data)
  "Moves randomly, but tries to avoid occupied spaces"
  (-> (init-deltas)
    (prune-out-of-bounds data)
    (prune-occupied data)
    (select-delta)))

(defparameter *all-snakes*
  (loop for think in (list 'think-random
			   'think-bounds
			   'think-self
			   'think-empty)
	collect (cons (string-downcase (subseq (symbol-name think) (length "think-")))
		      think)))

;;; Battlesnake minimal API implementation
(defvar *snakes* (make-hash-table :test 'equal) "Table of snake ids to logic functions")

(setf hunchentoot:*default-content-type* "application/json")

(defclass server (min-http:server)
  ()
  (:default-initargs :access-log-destination nil)
  (:documentation "Battlesnake proof-of-concept"))

(defun create-root-response (data)
  (declare (ignore data))
  (cl-json:encode-json-alist-to-string
   '(("apiversion" . "1"))))

(defun create-move-response (logic)
  (let* ((json-string (hunchentoot:raw-post-data :force-text t))
	 (json (cl-json:decode-json-from-string json-string)))
    ;;; Note: need to work around latency only being set under "board" and not "you"...
    (spew " Latency: ~ams"
	  (let ((you-id (alist-path json :you :id)))
	    (alist-path (find-if #'(lambda (snake) (string= you-id (alist-path snake :id)))
				 (alist-path json :board :snakes))
			:latency)))
    (cl-json:encode-json-alist-to-string
     (list
      (cons "move" (delta-to-direction (funcall logic json)))))))

(defparameter *handlers*
  (list
   (cons '(:get "/") 'create-root-response)
   (cons '(:post "/move") 'create-move-response)))

(defun parse-uri (uri)
  "Parse a URI (e.g. '/snake1/move') into a snake id and route (e.g. 'snake1' and '/move')"
  (if (>= (length uri) 4)
      (let* ((slash-position (position #\/ uri :start 1))
	     (id (and slash-position (subseq uri 1 slash-position)))
	     (route (and slash-position (subseq uri slash-position))))
	(if (and id route)
	    (cons id route)))))

(defmethod hunchentoot:acceptor-dispatch-request ((srv server) (request hunchentoot:request))
  (let* ((method (hunchentoot:request-method request))
	 (uri (hunchentoot:request-uri request))
	 (id-and-route (parse-uri uri))
	 (logic (and id-and-route (gethash (car id-and-route) *snakes*)))
	 (route (and id-and-route (assoc (list method (cdr id-and-route)) *handlers* :test #'equal))))
    (if (and logic route)
	(progn (spew "~a ~a:" method uri)
	       (let ((response (funcall (cdr route) logic)))
		 (spew "~%")
		 response))
	(progn
	  (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
	  ""))))

;;; Control API
(defvar *server* (make-instance 'server) "Battlesnake webhook server")

(defun start ()
  "Starts the Battlesnake webhook server"
  (hunchentoot:start *server*))

(defun stop ()
  "Stops the Battlesnake webhook server"
  (hunchentoot:stop *server*))

(defun snake-add (id symbol)
  "Adds a snake to the Battlesnake webhook server"
  (setf (gethash id *snakes*) symbol))

(defun snake-remove (id)
  "Removes a snake from the Battlesnake webhook server"
  (remhash id *snakes*))

(defun snake-defaults ()
  "Loads default snakes (all of them)"
  (loop for pair in *all-snakes* do (snake-add (car pair) (cdr pair))))

(defun run ()
  "Loads default snakes and starts the server"
  (snake-defaults)
  (start)

  ;;; Warm up the server
  (format t "Running warm-up query...")
  (let ((response (drakma:http-request (format nil
					       "http://~a:~a/~a/"
					       (hunchentoot:acceptor-address *server*)
					       (hunchentoot:acceptor-port *server*)
					       (car (first *all-snakes*))))))
    (format t "Response length: ~a~%" (length response))))
