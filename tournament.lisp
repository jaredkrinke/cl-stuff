(defpackage :tournament
  (:documentation "Battlesnake local tournament, for ranking different implementations")
  (:nicknames bst)
  (:use :cl))

(in-package :tournament)

(defun make-run-command (snakes)
  "Format name+URI pairs as arguments to Battlesnake local run tool"
  (flet ((sequence-to-arguments (flag sequence)
	   (apply #'concatenate 'string
		  (loop for item in sequence collect (format nil " -~a ~a" flag item)))))
    (format nil "./battlesnake play -s ~a ~a"
	    (sequence-to-arguments "n" (mapcar #'car snakes))
	    (sequence-to-arguments "u" (mapcar #'cdr snakes)))))

(defun run-match (snakes)
  "Run a local Battlesnake game and return the winner (or nil, if tied)"
  (let* ((command (make-run-command snakes))
	 (output (with-output-to-string (stderr) (uiop:run-program command :error-output stderr)))
	 (lines (uiop:split-string output :separator (list #\Newline)))
	 (prefix "Snakes Alive: [")
	 (suffix #\])
	 (line (find-if #'(lambda (line) (search "Snakes Alive: [" line))
			lines
			:from-end t))
	 (winner (subseq line
			 (+ (search prefix line) (length prefix))
			 (position suffix line))))
    (if (> (length winner) 0)
	winner
	nil)))

(defun run-matches (snakes count)
  "Run 'count' local matches and return win count by name"
  (format t "Running")
  (let ((stats (make-hash-table :test #'equal)))
    (dotimes (i count)
      (let ((winner (run-match snakes)))
	(if winner (setf (gethash winner stats) (1+ (or (gethash winner stats) 0))))
	(format t ".")))
    stats))

(defun run-and-report-matches (snakes count)
  "Run 'count' local matches and log results"
  (let* ((stats (run-matches snakes count))
	 (results (sort (loop for k being the hash-keys in stats using (hash-value v)
			      collect (cons k v))
			#'(lambda (a b) (< (cdr a) (cdr b))))))
    (loop for pair in results do (format t "~a: ~a~%" (car pair) (cdr pair)))))

(defun last-n (sequence n)
  "Get at most the last N items of a list"
  (let ((length (length sequence)))
    (subseq sequence (max 0 (- length n)))))

(defun make-local-snakes ()
  "Get the last 4 snakes from the list of all snakes"
  (loop for pair in (last-n bs:*all-snakes* 4)
	collect (let ((name (car pair)))
		  (cons name
			(format nil "http://127.0.0.1:8888/~a/" name)))))
