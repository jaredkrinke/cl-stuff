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
(defun char-to-string (character)
  "Converts a character into a string"
  (make-string 1 :initial-element character))

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

;;; Game logic
(defparameter *width* 30)
(defparameter *height* 30)
(defparameter *cells* '((:empty . "blue")
			(:player . "yellow")
			(:goal . "white")
			(:wall . "gray")))

(defun create-board (&optional (width *width*) (height *height*))
  "Creates a two-dimensional array that represents the board, initialized to :empty"
  (make-array (list width height) :element-type 'symbol :initial-element :empty))

(defmacro modify-board (place row column value)
  "Queues a board update to the PLACE list"
  `(pushnew (list ,row ,column ,value) ,place))

;;; Configure CL-WHO for HTML5
(setf (cl-who:html-mode) :html5)
(setf cl-who:*attribute-quote-char* #\")

(defun output-string (string stream)
  "Writes a string and waits for it to be flushed"
  (write-string string stream)
  (finish-output stream))

(defmacro output-html ((stream) &body body)
  `(output-string (cl-who:with-html-output-to-string (s) ,@body) ,stream))

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

(defun update-board (board updates)
  "Applies board modifications and returns code to apply the differences"
  (cl-who:with-html-output-to-string (s)
  (loop for (row column value) in updates do
    (unless (eql (aref board row column) value)
      (setf (aref board row column) value)
      (cl-who:htm
       (:style (cl-who:fmt ".s~a_~a { background-color: ~a }"
			   row
			   column
			   (rest (assoc value *cells*)))))))))

(defun run-instance (id stream channel)
  "Runs the handler for an instance of the application"
  (let ((board (create-board)))
    ;; TODO: Use CL-WHO and a subsequence for the start of output?
    (output-format stream
		   "~a~%~a"
		   "<!DOCTYPE html>
<html><head><style>
.dynamic { display: none }
.dynamic:last-of-type { display: block }
table { border-spacing: 0 }
td { background-color: blue; width: 1em; height: 1em; padding: 0; }
</style></head><body>"
		   (cl-who:with-html-output-to-string (s)
		     (:div :id "controls"
			   (:iframe :src (format nil "controls?id=~a" id)))
		     (:table
		      (loop for row from 0 upto (1- *height*) do
			(cl-who:htm
			 (:tr (loop for column from 0 upto (1- *width*) do
			   (cl-who:htm
			    (:td :class (format nil "s~a_~a" column row)
				 "&nbsp;")))))))))
    (loop for action = (calispel:? channel 0)
	  until (equal action "quit") do
	    (let ((updates nil)
		  (html nil))
	      (modify-board updates (random *width*) (random *height*) (if action :player :wall))
	      (setf html (update-board board updates))
	      (if (and html (> (length html) 0)) (output-string html stream)))
	  (sleep 1))))

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
      (loop for (label action) in `((,(char-to-string #\Clockwise_Open_Circle_Arrow) "clockwise")
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
