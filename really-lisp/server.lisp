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

(defun output-string (string stream)
  "Writes a string and waits for it to be flushed"
  (write-string string stream)
  (finish-output stream))

(defmethod hunchentoot:acceptor-dispatch-request ((server server) request)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let* ((binary-stream (hunchentoot:send-headers))
	 (stream (flexi-streams:make-flexi-stream binary-stream :external-format :utf-8)))
    (output-string "<!DOCTYPE html>
<html><head><style>
.dynamic { display: none }
.dynamic:last-of-type { display: block }
</style></head><body><p class=\"dynamic\">Welcome!</p>" stream)
    (sleep 1)
    (output-string "<p class=\"dynamic\">... to the site!</p>" stream)
    (sleep 1)
    (output-string "<p class=\"dynamic\">That is all.</p></body></html>" stream)))

;;; Server management
(defvar *server* (make-instance 'server) "Instance of the server")

(defun start-server ()
  "Runs the server"
  (hunchentoot:start *server*))

(defun stop-server ()
  "Stops the server"
  (hunchentoot:stop *server*))
