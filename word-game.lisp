(defpackage :thirteen-letters
  (:documentation "Thirteen-letter word scramble game")
  (:nicknames wg)
  (:use :cl)
  (:export #:play))

(in-package :wg)

;;; General utility functions
(defmacro map-each ((var sequence) &body body)
  "Map a sequence using an implicit anonymous function"
  `(mapcar #'(lambda (,var) ,@body) ,sequence))

(defmacro loop-over-lines ((line-var path) &body body)
  "Run LOOP over each line of a file"
  `(with-open-file (stream ,path) ; TODO: gensym!
     (loop for ,line-var = (read-line stream nil)
	   while ,line-var
	   ,@body)))

(defun read-all-lines (path)
  "Reads all lines of a file into a list of lines"
  (loop-over-lines (line path) collect line))

(defun map-lines (path f)
  "Map each line of a file"
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (funcall f line))))

(defun split-list (list size)
  "Destructively splits LIST into as many lists of at most SIZE elements as necessary"
  (let ((partitions (list list)))
    (loop for i upfrom 1
	  while list
	  do (let* ((tail (cdr list))
		    (split (and tail (= 0 (mod i size)))))
	       (cond (split (push tail partitions)
			    (setf (cdr list) nil)))
	       (setf list tail)))
    (nreverse partitions)))

(defun get-random (sequence)
  "Randomly selects an item from SEQUENCE"
  (nth (random (length sequence)) sequence))

(defun shuffle (sequence &rest rest &key &allow-other-keys)
  "Returns a copy of SEQUENCE (as an array) with its elements shuffled (note: extra arguments are passed to MAKE-ARRAY)"
  (let* ((length (length sequence))
	 (array (apply #'make-array length :initial-contents sequence rest)))
    (loop for i upfrom 0
	  for n downfrom length
	  while (> n 1)
	  do (let ((target (+ i (random n))))
	       (rotatef (aref array i) (aref array target))))
    array))

(defun shuffle-string (string)
  "Returns a copy of STRING with its characters shuffled"
  (shuffle string :element-type 'character))

;;; Word list and frequency tools
(defparameter *puzzle-length* 13)
(defparameter *difficulty-buckets* 10)

(defun parse-frequency (line)
  "Parse a word and frequency pair"
  (let ((word (subseq line 0 (position #\Tab line)))
	(count (subseq line (1+ (position #\Tab line :from-end t)))))
    (cons word (parse-integer count))))

(defun load-word-frequencies ()
  "Load word-frequency pairs (top 1/3 million from Google Trillion Word corpus)  into an a-list"
  (map-lines "count_1w.txt" #'parse-frequency))

(defun load-all-words ()
  "Load unfiltered word list (YAWL)"
  (read-all-lines "yawl.txt"))

(defun load-word-rates ()
  "Load words and their rates into a hash table"
  (let* ((frequencies (load-word-frequencies))
	 (rates (make-hash-table :test 'equal :size (length frequencies)))
	 (total (reduce #'+ (mapcar #'cdr frequencies) :initial-value 0)))
    (loop for (word . count) in frequencies
	  do (setf (gethash word rates) (/ count total)))
    rates))

(defun add-rates (words rates)
  "Attach rates to words, resulting in pairs (removing unknown words)"
  (delete-if-not #'cdr
		 (map-each (word words) (cons word (gethash word rates)))))

(defun load-puzzle-rates ()
  "Load puzzle words with rates"
  (add-rates (delete-if-not #'(lambda (word) (= 13 (length word)))
			    (load-all-words))
	     (load-word-rates)))

(defun load-bucketed-puzzles ()
  "Load puzzle words, bucketed into difficulty groups (by frequency)"
  (let* ((rates (load-puzzle-rates))
	 (sorted-rates (sort rates #'(lambda (a b) (> (cdr a) (cdr b)))))
	 (sorted (mapcar #'car sorted-rates)))
    (split-list sorted (ceiling (/ (length sorted) *difficulty-buckets*)))))

;;; The actual game
(defparameter *bucketed-words* (load-bucketed-puzzles))

(defun play (&optional (difficulty 0))
  (let* ((start-time (get-internal-real-time))
	 (solution (get-random (nth difficulty *bucketed-words*)))
	 ;; TODO: Ensure not the same as solution!
	 (scrambled (shuffle-string solution)))
    (format t "~%~%Unscramble the following word")
    (loop
      (format t " (enter 'q' to give up):~%~%  ~a~%~%> " scrambled)
      (let* ((guess (read-line))
	     (guess-time (get-internal-real-time)))
	(cond ((string= guess solution)
	       (let ((seconds (float (/ (- guess-time start-time)
					internal-time-units-per-second))))
		 (format t "~%~%Correct! (Solved in ~a seconds)~%~%" seconds)
		 (return-from play seconds)))
	      ((string= guess "q") (format t "~%~% Better luck next time!~%~%")
	       (return-from play))
	      (t (format t "~%~%Nope! Guess again")))))))
