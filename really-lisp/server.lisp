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

;;; Configure CL-WHO for HTML5
(setf (cl-who:html-mode) :html5)
(setf cl-who:*attribute-quote-char* #\")

(defun output-string (string stream)
  "Writes a string and waits for it to be flushed"
  (write-string string stream)
  (finish-output stream))

(defmacro output-format (stream format-string &rest rest)
  "Writes a format string and waits for it to be flushed"
  `(output-string (format nil ,format-string ,@rest) ,stream))

(defun handle-not-found ()
  "Returns a 404 not found error"
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  "")

;;; TODO: Per-player, obviously
(defparameter *test-channel* (make-instance 'calispel:channel))

(defun handle-root ()
  "Handles a request to the root resource"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let* ((binary-stream (hunchentoot:send-headers))
	 (stream (flexi-streams:make-flexi-stream binary-stream :external-format :utf-8)))
    ;; TODO: Use CL-WHO and a subsequence for the start of output?
    (output-format stream
		   "~a~%~a"
		   "<!DOCTYPE html>
<html><head><style>
.dynamic { display: none }
.dynamic:last-of-type { display: block }
</style></head><body>"
		   (cl-who:with-html-output-to-string (s)
		       (:div :id "controls"
			     (:iframe :src "controls"))
		     (:p :class "dynamic" "Welcome CL-WHO!")))
    (let ((action (calispel:? *test-channel*)))
      (output-string (cl-who:with-html-output-to-string (s)
		       (:p :class "dynamic" (cl-who:fmt "... to the site! (WHO: ~a)" action)))
		     stream))))

(defun handle-controls ()
  (cl-who:with-html-output-to-string (s)
    (:html
     (:body
      (:form :action "action" :method "post"
	     (:input :type "hidden" :name "action" :value "go")
	     (:input :type "submit" :value "Push me 2!"))))))

(defun handle-action ()
  "Handles an incoming action request"
  (calispel:! *test-channel* (hunchentoot:post-parameter "action"))
  (handle-controls))

(defparameter *dispatch-table* '(("/" handle-root)
				 ("/controls" handle-controls)
				 ("/action" handle-action)))

(defmethod hunchentoot:acceptor-dispatch-request ((server server) request)
  (let ((row (assoc (hunchentoot:script-name* request) *dispatch-table* :test 'equal)))
    (if row
	(funcall (second row))
	(handle-not-found))))

;;; Server management
(defvar *server* (make-instance 'server) "Instance of the server")

(defun start-server ()
  "Runs the server"
  (hunchentoot:start *server*))

(defun stop-server ()
  "Stops the server"
  (hunchentoot:stop *server*))
