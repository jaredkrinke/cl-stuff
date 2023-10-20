(defpackage really-lisp/server
  (:use :cl))

(in-package :really-lisp/server)

(defparameter *port* 23115 "Port on which the server should listen")

(defclass server (hunchentoot:acceptor)
  ()
  (:documentation "Minimal HTTP server that only serves programmatic content")
  (:default-initargs
   :address "127.0.0.1"
   :port *port*
   :document-root nil
   :error-template-directory nil
   :access-log-destination nil))

;;; Disable Hunchentoot default status messages
(defmethod hunchentoot:acceptor-status-message ((acceptor server) http-status-code &rest args)
  (declare (ignore args))
  "")

(defmethod hunchentoot:acceptor-dispatch-request ((server server) request)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((stream (hunchentoot:send-headers)))
    (princ "Hello there!" stream)))

;;; Server management
(defvar *server* (make-instance 'server) "Instance of the server")

(defun start-server ()
  "Runs the server"
  (hunchentoot:start *server*))

(defun stop-server ()
  "Stops the server"
  (hunchentoot:stop *server*))
