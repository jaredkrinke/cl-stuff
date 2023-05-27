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

(defun char-repeat (character times)
  "Returns a string with CHARACTER repeated TIMES times"
  (make-array times :element-type 'character :initial-element character))

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

(defun fully-scrambled-p (scrambled solution)
  "Returns NIL if any character in SCRAMBLED matches a letter in SOLUTION"
  (loop for scrambled-character across scrambled
	for solution-character across solution
	when (char-equal scrambled-character solution-character) do (return nil)
	finally (return t)))

(defun scramble (word)
  "Shuffles a word, ensuring that no character is in the correct place"
  (loop with scrambled = word
	until (fully-scrambled-p scrambled word)
	do (setf scrambled (shuffle-string scrambled))
	finally (return scrambled)))

(defun unscramble (scrambled solution index)
  "Unscrambles the letter at INDEX in SCRAMBLED"
  (let* ((c (aref solution index))
	 (target (position c scrambled :start index)))
    (rotatef (aref scrambled index) (aref scrambled target))))

(defun play (&optional (difficulty 0))
  (let* ((tries 0)
	 (solution (get-random (nth difficulty *bucketed-words*)))
	 (scrambled (scramble solution)))
    (format t "~%~%Unscramble the following word")
    (loop
      (format t
	      " (or 'q' to give up):~%  ~a~a~%  ~a~%~%> "
	      (char-repeat #\Space tries)
	      (char-repeat #\_ (- *puzzle-length* tries))
	      scrambled)
      (incf tries)
      (let* ((guess (read-line)))
	(cond ((string= guess solution)
	       (format t "~%~%Correct! (Solved in ~a tries)~%~%" tries)
	       (return-from play tries))
	      ((string= guess "q") (format t "~%~% Better luck next time!~%~%")
	       (return-from play))
	      (t
	       (format t "~%~%Nope! Guess again")
	       ;; TODO: Fail if unscrambling solves the puzzle
	       (unscramble scrambled solution (1- tries))))))))

(defun menu ()
  "Show the title menu and prompt for difficulty level (or exit)"
  (format t
	  "
=== Thirteen Letters ===

 - Goal: Unscramble a thirteen-letter word in as few guesses as possible.
 - After each incorrect guess, one letter will be unscrambled.
 - There are ten difficulty levels (1 being the easiest and ~a the hardest)~%~%"
	  *difficulty-buckets*)

  (loop with continue = t
	while continue
	do (format t
		   "Enter a difficulty level, 1 - ~a (or 'q' to quit):~%~%> "
		   *difficulty-buckets*)
	   (let* ((input (read-line))
		  (number (parse-integer input :junk-allowed t))
		  (difficulty (and number (>= number 1) (<= number *difficulty-buckets*) number)))
	     (cond ((string= "q" input)
		    (format t "~%~%So long!~%")
		    (setf continue nil))
		   (difficulty
		    (play (1- difficulty))
		   (t
		    (format t "~%Invalid input!~%"))))))
