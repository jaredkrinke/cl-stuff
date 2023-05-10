(defpackage :tournament
  (:documentation "Battlesnake local tournament, for ranking different implementations")
  (:nicknames bst)
  (:use :cl))

(in-package :tournament)

(defun sequence-to-arguments (flag sequence)
  (apply #'concatenate 'string
	 (loop for item in sequence collect (format nil " -~a ~a" flag item))))

(defun make-run-command (snakes)
  (let ((command (format nil "./battlesnake play -s ~a ~a"
			 (sequence-to-arguments "n" (mapcar #'car snakes))
			 (sequence-to-arguments "u" (mapcar #'cdr snakes)))))
    command))

(defun run-match (snakes)
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
    winner))


(defun last-n (sequence n)
  "Get at most the last N items of a list"
  (let ((length (length sequence)))
    (subseq sequence (max 0 (- length n)))))

(defun make-local-snakes ()
  (loop for pair in (last-n bs:*all-snakes* 4)
	collect (let ((name (car pair)))
		  (cons name
			(format nil "http://127.0.0.1:8888/~a/" name)))))

(defun run-local-match ()
  (run-match (make-local-snakes)))
