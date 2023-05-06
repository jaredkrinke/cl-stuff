;;;; Battlesnake proof-of-concept
(require "cl-json")

(load "min-http.lisp")

(defpackage :battlesnake-poc
  (:use :cl))

(in-package :battlesnake-poc)

;;; Battlesnake logic
(defun think-random (data)
  "Moves randomly, possibly even into itself"
  (declare (ignore data))
  (ecase (random 4)
    (0 "up")
    (1 "down")
    (2 "left")
    (3 "right")))

;;; Battlesnake minimal API implementation
(defvar *snakes* (make-hash-table :test 'equal) "Table of snake ids to logic functions")

(setf hunchentoot:*default-content-type* "application/json")

(defclass server (min-http:server)
  ()
  (:documentation "Battlesnake proof-of-concept"))

(defun create-root-response (data)
  (declare (ignore data))
  (cl-json:encode-json-alist-to-string
   '(("apiversion" . "1"))))

(defun create-move-response (logic)
  (let*
      ((json-string (hunchentoot:raw-post-data :force-text t))
       (json (cl-json:decode-json-from-string json-string)))
    (cl-json:encode-json-alist-to-string
     (list
      (cons "move" (funcall logic json))))))

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
    (if route
	(funcall (cdr route) logic)
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
