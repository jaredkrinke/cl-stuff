(defpackage :foolander
  (:use :cl)
  (:export #:start-server
	   #:stop-server))

(in-package :foolander)

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

;;; HTTP server configuration
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

(defmethod hunchentoot:acceptor-status-message ((acceptor server) http-status-code &rest args)
  (declare (ignore args))
  "") ; Disable Hunchentoot default status messages

;;; Server management
(defvar *server* (make-instance 'server) "Instance of the server")

(defun start-server ()
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))

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

;;; Game infrastructure
(defparameter *frame-period* 1/3)
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
(defvar *previous-score* nil)
(defvar *channel* nil)
(defvar *done* nil)

(defun create-board (&optional (width *width*) (height *height*))
  "Creates a two-dimensional array that represents the board, initialized to :empty"
  (make-array (list height width) :element-type 'symbol :initial-element :empty))

(defun board-get (position)
  "Gets the value of the specified cell for the current board"
  (apply #'aref *board* position))

(defun board-set (position value)
  "Queues a change in the value of the specified cell for the current board"
  ;; TODO: Consider only queueing if different from most recent (incl. updates?)
  (push (list position value) *board-updates*))

(defun poll-event ()
  "Polls for queued actions"
  (let* ((action-string (calispel:? *channel* 0))
	 (action (and action-string (or (rest (assoc action-string *actions* :test 'equal))
					:unknown))))
    action))

(defun quit ()
  "Quits the current instance of the game"
  (setf *done* t))

;;; Configure CL-WHO for HTML5
(setf (cl-who:html-mode) :html5)
(setf cl-who:*attribute-quote-char* #\")

;;; HTML output helpers
(defvar *stream* nil "The current chunked output stream")

(defun output-string (string)
  "Writes a string and waits for it to be flushed"
  (write-string string *stream*)
  (finish-output *stream*)
  t)

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
    (loop for (position value) in (nreverse *board-updates*) do
      (unless (eql (apply #'aref *board* position) value)
	(setf (apply #'aref *board* position) value)
	(cl-who:htm
	 (:style (cl-who:fmt ".s~a_~a { background-color: ~a }"
			     (first position)
			     (second position)
			     (rest (assoc value *cells*)))))))))

(defun update-score (new-score)
  "Checks for score updates and returns HTML to apply any differences"
  (unless (equal new-score *previous-score*)
    (setf *previous-score* new-score)
    (cl-who:with-html-output-to-string (s)
      (:p :class "score" (cl-who:fmt "Score: ~a" new-score)))))

(defun output-start (id)
  "Outputs the start of the HTML page (including the game board and control frame)"
  ;; TODO: Use CL-WHO and a subsequence for the start of output?
  (output-format "~a~%~a"
		 "<!DOCTYPE html>
<html><head><meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\" /><link rel=\"stylesheet\" href=\"style.css\"></head><body><main class=\"text\">"
		 (cl-who:with-html-output-to-string (s)
		   (:div :id "controls"
			 (:iframe :src (format nil "controls?id=~a" id)))
		   (:table
		    (loop for row from (1- *height*) downto 0 do
		      (cl-who:htm
		       (:tr (loop for column from 0 upto (1- *width*) do
			 (cl-who:htm
			  (:td :class (format nil "s~a_~a" row column)
			       "&nbsp;")))))))
		   (:p :class "score" "Score: 0"))))

(defun run-instance (id channel)
  "Runs the handler for an instance of the game"
  (let* ((*board* (create-board))
	 (*channel* channel)
	 (*done* nil))
    (output-start id)
    (run-game)))

(defmacro run-loop (&body body)
  "Runs logic as part of a loop, adding visual updates at the end"
  `(loop
     with *previous-score* = 0
     for *board-updates* = nil until *done* do
     ,@body
       (let ((html (concatenate 'string
				(update-board)
				(update-score *score*))))
	 (when (and html (> (length html) 0))
	   ;; Ignore flush errors because the client probably just closed the connection
	   (unless (ignore-errors (output-string html))
	     (setf *done* t))))))

(defun handle-main ()
  "Handles a request to the main resource"
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
     (:head (:link :rel "stylesheet" :href "style.css"))
     (:body
      (loop for (label action access-key) in
	    `((,(char-to-string #\Clockwise_Open_Circle_Arrow) "clockwise" "e"))
	    do (cl-who:htm
		(:form :action "action" :method "post"
		       (:input :type "hidden" :name "id" :value (cl-who:esc id))
		       (:input :type "hidden" :name "action" :value action)
		       (:input :type "submit" :value label :accesskey access-key))))))))

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
				 ("/game" handle-main)
				 ("/style.css" handle-style)
				 ("/controls" handle-controls)
				 ("/action" handle-action)))

(defmethod hunchentoot:acceptor-dispatch-request ((server server) request)
  (let ((row (assoc (hunchentoot:script-name* request) *dispatch-table* :test 'equal)))
    (if row
	(funcall (second row))
	(handle-not-found))))

;;; Game content
(defun handle-style ()
  "Handles a request for the CSS stylesheet"
  (setf (hunchentoot:content-type*) "text/css")
  "
body { margin: 0; padding: 0; font-family: sans-serif; background-color: darkblue; color: #eee; line-height: 1.5; }
h1 { color: yellow; }
h1, h2 { text-align: center; }
a:link, a:visited, a:hover { color: goldenrod; }
main { max-width: 30em; margin: auto; }
main.text { padding: 0.5 em; }
li { padding: 0.25em 0; }
ul.scores { padding: 0; }
ul.scores li { list-style: none; text-align: center; padding: 0.125em; line-height: 1; font-size: 150%; }

.score { display: none }
.score { font-weight: bold; font-size: 200%; }
.score:last-of-type { display: block }
table { border-spacing: 0; margin: auto; }
td { background-color: blue; width: 1em; height: 1em; padding: 0; line-height: 1; }

.box { position: absolute; width: 30em; max-width: 100%; top: 1.5em; background-color: #0808ff; margin: 0; outline: 2px solid goldenrod; }

iframe { position: fixed; bottom: 0; width: 100%; max-width: 30em; border: none; }
form { position: absolute; top: 0; left: 0; width: 100%; height: 100% ;}
input[type=submit] { width: 100%; height: 100%; font-size: 400%; font-weight: bold; background-color: blue; color: yellow;cursor: pointer; border: 3px solid yellow; }
input[type=submit]:active { background-color: #0808ff; }

@media (max-height: 40em) {
 body { font-size: 85%; }
 iframe { height: 6em; }
}
")

(defun handle-root ()
  "Handles a request to the root resource, with info and a link to play the game"
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html
     (:head
      (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
      (:link :rel "stylesheet" :href "style.css"))
     (:body
      (:main
       (:h1 "Foolander")
       (:p "Foolander is a simple snake game:"
	   (:ul
	    (:li (:strong "The goal is to consume as many pieces of food as possible."))
	    (:li "Each morsel causes the snake to grow.")
	    (:li "The snake must stay in bounds and avoid running into itself.")))
       (:p "This is an unoriginal concept, but there's an obnoxious twist: " (:strong "you can only turn clockwise") ".")
       (:p "Note: the motivation for this game was to see if it was possible to create an arcade-style, browser-based game using only HTML and CSS. The game " (:em "does not") " require JavaScript (or WebAssembly).")
       (:h2 (:a :href "game" "Click here to start!")))))))

;;; High score management
(defparameter *high-scores-count* 8)

(defvar *high-scores-lock* (bt:make-recursive-lock))
(defvar *high-scores* nil)

(defun high-scores-load-under-lock ()
  "Loads high scores list from disk (must be called under lock)"
  (setf *high-scores*
	(with-open-file (stream "scores.txt" :if-does-not-exist nil)
	  (if stream
	      (make-array *high-scores-count*
			  :element-type 'integer
			  :initial-contents (read stream))
	      (make-array *high-scores-count*
			  :element-type 'integer
			  :initial-element 0)))))

(defun high-scores-get-under-lock ()
  "Returns a list of high scores (must be called under lock)"
  (loop for score across *high-scores* collect score))

(defun high-scores-save-under-lock ()
  "Saves the high scores list to disk (must be called under lock)"
  (with-open-file (stream "scores.txt" :direction :output
				       :if-exists :supersede)
    (print (high-scores-get-under-lock) stream)))

(defun high-scores-add (score)
  "Adds a new score to the list"
  (bt:with-recursive-lock-held (*high-scores-lock*)
    (unless *high-scores* (high-scores-load-under-lock))
    (let ((last-index (1- (length *high-scores*))))
      (when (> score (elt *high-scores* last-index))
	(setf (aref *high-scores* last-index) score)
	(setf *high-scores* (sort *high-scores* #'>))
	(high-scores-save-under-lock))
      (loop for score across *high-scores* collect score))))

(defun render-game-over (score)
  (cl-who:with-html-output-to-string (s)
    (:div :class "box"
	  (:h1 "Game Over!")
	  (:h2 "Score: " (cl-who:fmt "~a" score))
	  (:h2 "High Scores")
	  (:ul :class "scores"
	       (loop for score in (high-scores-add score)
		     do (cl-who:htm (:li (cl-who:fmt "~a" score)))))
	  (:h2 (:a :href "game" "Click to play again")))))

;;; Game logic
(defparameter *bounds* (list (list 0 0)
			     (list (1- *height*) (1- *width*))))

(defparameter *directions* '((1 0)
			     (0 1)
			     (-1 0)
			     (0 -1)))

(defvar *score* nil)
(defvar *snake* nil "Player's body, as a list of positions with the head being first")
(defvar *goal-position* nil "Location of the goal")
(defvar *direction* nil) ; TODO: *direction-cons*

(defun in-range (value min max)
  "Returns non-NIL if the value is within the range [min, max]"
  (and (>= value min)
       (<= value max)))

(defun in-bounds (position)
  "Returns non-NIL if the position is in bounds"
  (every #'in-range position (first *bounds*) (second *bounds*)))

(defun turn-clockwise ()
  "Turns the player 90 degrees clockwise"
  (setf *direction* (or (rest *direction*)
			*directions*)))

(defun get-next-position ()
  "Gets the player's next position"
  (mapcar #'+ (first *snake*) (first *direction*)))

(defun perform-action (action)
  "Perform an action (input event)"
  (case action
    (:clockwise (turn-clockwise))
    (:quit (quit)))) ; TODO: Consider removing this action

(defun handle-actions ()
  "Performs all queued actions"
  (loop for action = (poll-event)
	while action
	do (perform-action action)))

(defun choose-goal-position ()
  "Finds a place for a goal to spawn that isn't occupied"
  (loop for position = (list (random *height*) (random *width*))
	until (not (member position *snake* :test 'equal))
	finally (return position)))

(defun spawn-goal ()
  "Spawns a new goal"
  (setf *goal-position* (choose-goal-position))
  (board-set *goal-position* :goal))

(defun capture-goal ()
  "Increments score and spawns a new goal"
  (incf *score*)
  (spawn-goal))

(defun update-player ()
  "Moves the player in the given direction and resolves goal/end game events"
  (let ((tail-position (first (last *snake*)))
	(new-head-position (get-next-position)))
    (if (and (in-bounds new-head-position)
	     (not (member new-head-position *snake* :test 'equal)))
	(progn
	  (board-set tail-position :empty)
	  (board-set new-head-position :player)
	  (push new-head-position *snake*)
	  (if (equal new-head-position *goal-position*)
	      (capture-goal)
	      (setf *snake* (nbutlast *snake* 1))))
	(quit))))

(defun run-game ()
  "Runs the actual game logic"
  (let ((*score* 0)
	(*direction* *directions*)
	(*snake* (loop repeat 3 collect (list 14 14)))
	(*goal-position* nil))
    (run-loop
      (sleep *frame-period*)
      (if (not *goal-position*) (spawn-goal))
      (handle-actions)
      (update-player))
    (output-string (render-game-over *score*))))
