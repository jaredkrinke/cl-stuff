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

;;; Utilities
(defun random-on-interval (min max)
  "Returns a random number on the interval [min, max]"
  (+ min (random (- max min))))

(defun random-letter ()
  "Returns a random lower case (English) letter"
  (code-char (random-on-interval (char-code #\a) (char-code #\z))))

(defun random-identifier (&optional (length 12))
  "Returns a random (12 character by default) identifier composed of lower case (English) letters"
  (let ((string (make-string length)))
    (dotimes (x length)
      (setf (char string x) (random-letter)))
    string))

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

(defvar *channels-lock* (bt:make-recursive-lock) "Lock for *CHANNELS*")
(defvar *channels* nil "List of (channel-name . channel)")

(defmacro with-channel-lock (&body body)
  `(bt:with-recursive-lock-held (*channels-lock*)
     ,@body))

(defun add-channel (id channel)
  (with-channel-lock
    (pushnew (cons id channel) *channels*)))

(defun get-channel (id)
  (with-channel-lock
    (rest (assoc id *channels* :test 'equal))))

(defun remove-channel (id)
  (with-channel-lock
    (setf *channels* (delete-if (lambda (row) (equal id (first row))) *channels*))))

(defun run-instance (id stream channel)
  "Runs the handler for an instance of the application"
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
			 (:iframe :src (format nil "controls?id=~a" id)))
		   (:p :class "dynamic" "Welcome CL-WHO! " id)))
  (loop for action = (calispel:? channel)
	until (equal action "quit") do
	  (output-string (cl-who:with-html-output-to-string (s)
			   (:p :class "dynamic" (cl-who:esc (cl-who:fmt "... to the site! (WHO: ~a)" action))))
			 stream)))

(defun handle-root ()
  "Handles a request to the root resource"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let* ((id (random-identifier))
	 (binary-stream (hunchentoot:send-headers))
	 (stream (flexi-streams:make-flexi-stream binary-stream :external-format :utf-8))
	 (channel (make-instance 'calispel:channel)))
	   (add-channel id channel)
    (unwind-protect (run-instance id stream channel)
      (remove-channel id))))

(defun render-controls (id)
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html
     (:body
      (loop for (label action) in '(("A" "a")
				    ("B" "b")
				    ("C" "c")
				    ("D" "d")
				    ("Quit" "quit"))
	    do (cl-who:htm
		(:form :action "action" :method "post"
		       (:input :type "hidden" :name "id" :value (cl-who:esc id))
		       (:input :type "hidden" :name "action" :value action)
		       (:input :type "submit" :value label))))))))

(defun handle-controls ()
  (render-controls (hunchentoot:parameter "id")))

(defun handle-action ()
  "Handles an incoming action request"
  (let* ((id (hunchentoot:parameter "id"))
	 (action (hunchentoot:parameter "action"))
	 (channel (get-channel id)))
    (if channel
	(progn
	  (calispel:! channel action)
	  (render-controls id))
	(handle-not-found))))

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
