;;;; Battlesnake proof-of-concept
(require "cl-json")

(load "min-http.lisp")

(defpackage :battlesnake-poc
  (:use :cl))

(in-package :battlesnake-poc)

;;; Battlesnake logic
(defun think ()
  "Battlesnake main logic function -- just moves randomly for now"
  (ecase (random 4)
    (0 "up")
    (1 "down")
    (2 "left")
    (3 "right")))

;;; Battlesnake minimal API implementation
(setf hunchentoot:*default-content-type* "application/json")

(defclass server (min-http:server)
  ()
  (:documentation "Battlesnake proof-of-concept"))

(defun create-root-response ()
  (cl-json:encode-json-alist-to-string
   '(("apiversion" . "1"))))

(defun create-move-response ()
  (cl-json:encode-json-alist-to-string
   (list
    (cons "move" (think)))))

(defparameter *handlers*
  (list
   (cons '(:get "/") #'create-root-response)
   (cons '(:post "/move") #'create-move-response)))

(defmethod hunchentoot:acceptor-dispatch-request ((srv server) (request hunchentoot:request))
  (let* ((method (hunchentoot:request-method request))
	 (uri (hunchentoot:request-uri request))
	 (route (assoc (list method uri) *handlers* :test #'equal)))
    (if route
	(funcall (cdr route))
	(progn
	  (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
	  ""))))
