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

;;; Game infrastructure
(defparameter *frame-period* 1)
(defparameter *width* 30)
(defparameter *height* 30)
(defparameter *actions* '(("clockwise" . :clockwise)
			  ("quit" . :quit)))
(defparameter *cells* '((:empty . "blue")
			(:player . "yellow")
			(:goal . "white")
			(:wall . "gray")))

(defvar *board* nil)
(defvar *board-updates* nil)
(defvar *channel* nil)
(defvar *done* nil)

(defun create-board (&optional (width *width*) (height *height*))
  "Creates a two-dimensional array that represents the board, initialized to :empty"
  (make-array (list width height) :element-type 'symbol :initial-element :empty))

(defun board-get (row column)
  "Gets the value of the specified cell for the current board"
  (aref *board* row column))

(defun board-set (row column value)
  "Queues a change in the value of the specified cell for the current board"
  (pushnew (list row column value) *board-updates*))

(defun poll-event ()
  "Polls for queued actions"
  (let* ((action-string (calispel:? *channel* 0))
	 (action (and action-string (or (rest (assoc action-string *actions* :test 'equal))
					:unknown))))
    action))

(defun quit ()
  "Quits the current instance of the game"
  (setf *done* t))

;;; Calispel channels for propagating updates to game/request thread
(defvar *channels-lock* (bt:make-recursive-lock) "Lock for *CHANNELS*")
(defvar *channels* nil "List of (channel-name . channel)")

(defun make-unbounded-buffered-channel ()
  "Creates a Calispel channel that uses an unbounded FIFO queue for buffering"
  ;; Use an unbounded, buffered queue to store as many events as needed for processing
  (make-instance 'calispel:channel
		 :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))

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

;;; Configure CL-WHO for HTML5
(setf (cl-who:html-mode) :html5)
(setf cl-who:*attribute-quote-char* #\")

;;; HTML output helpers
(defvar *stream* nil "The current chunked output stream")

(defun output-string (string)
  "Writes a string and waits for it to be flushed"
  (write-string string *stream*)
  (finish-output *stream*))

(defmacro output-html (&body body)
  "Convert to HTML using CL-WHO and output"
  `(output-string (cl-who:with-html-output-to-string (s) ,@body)))

(defmacro output-format (format-string &rest rest)
  "Writes a format string and waits for it to be flushed"
  `(output-string (format nil ,format-string ,@rest)))

;;; HTTP request handlers
(defun handle-not-found ()
  "Returns a 404 not found error"
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  "")

(defun update-board ()
  "Applies board modifications and returns HTML (actually mostly CSS) to apply the differences"
  (cl-who:with-html-output-to-string (s)
  (loop for (row column value) in *board-updates* do
    (unless (eql (aref *board* row column) value)
      (setf (aref *board* row column) value)
      (cl-who:htm
       (:style (cl-who:fmt ".s~a_~a { background-color: ~a }"
			   row
			   column
			   (rest (assoc value *cells*)))))))))

(defun output-start (id)
  "Outputs the start of the HTML page (including the game board and control frame)"
  ;; TODO: Use CL-WHO and a subsequence for the start of output?
  (output-format "~a~%~a"
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
			       "&nbsp;"))))))))))

(defun run-instance (id channel)
  "Runs the handler for an instance of the game"
  (let* ((*board* (create-board))
	 (*channel* channel)
	 (*done* nil))
    (output-start id)
    (run-game)))

(defmacro run-loop (&body body)
  `(loop for *board-updates* = nil until *done* do
	 ,@body
	   (let ((html (update-board)))
	     (if (and html (> (length html) 0)) (output-string html)))))

(defun handle-root ()
  "Handles a request to the root resource"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let* ((id (random-identifier))
	 (binary-stream (hunchentoot:send-headers))
	 (*stream* (flexi-streams:make-flexi-stream binary-stream :external-format :utf-8))
	 (channel (make-unbounded-buffered-channel)))
	   (add-channel id channel)
    (unwind-protect (run-instance id channel)
      (remove-channel id))))

(defun render-controls (id)
  "Render the contents of the controls frame"
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
	  (calispel:! channel action 0)
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

;;; Game logic
(defun run-game ()
  "Runs the actual game logic"
  (let ((q 5))
    (run-loop
      (loop for action = (poll-event) while action do
	(case action
	  (:clockwise (board-set (random *height*) q :player))
	  (:quit (quit)))))))

;;; Server management
(defvar *server* (make-instance 'server) "Instance of the server")

(defun start-server ()
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))
