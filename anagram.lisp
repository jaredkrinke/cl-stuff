(defpackage :anagram
  (:documentation "Library for finding anagrams")
  (:use :cl))

(in-package :anagram)

;;; Note: This is really slow... maybe use a trie instead?

;;; Utility functions (TODO: put somewhere)
(defmacro map-each ((var sequence) &body body)
  "Map a sequence using an implicit anonymous function"
  `(mapcar #'(lambda (,var) ,@body) ,sequence))

(defun get-letter-counts (word)
  "Gets an array of the letter counts within a word"
  (let ((normalized-word (string-downcase word))
	(base (char-code #\a))
	(counts (make-array 26)))
    (loop for letter across normalized-word
	  do (incf (aref counts (- (char-code letter) base))))
    counts))

(defun map-lines (path f)
  "Map each line of a file"
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (funcall f line))))

(defun parse-frequency (line)
  "Parse a word and frequency pair"
  (let ((word (subseq line 0 (position #\Tab line)))
	(count (subseq line (1+ (position #\Tab line :from-end t)))))
    (cons word (parse-integer count))))

(defun load-word-frequencies ()
  "Load word-frequency pairs (top 1/3 million from Google Trillion Word corpus)  into an a-list"
  (map-lines "count_1w.txt" #'parse-frequency))

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

;;; Actual implementation
(defparameter *words-raw*
  (with-open-file (stream "yawl.txt")
    (loop for line = (read-line stream nil)
	  while line collect line))
  "List of words")

(defparameter *min-word-length* 3)

(defparameter *words*
  (remove-if-not (lambda (word) (>= (length word) *min-word-length*))
		 (mapcar #'car
			 (add-rates *words-raw* (load-word-rates)))))

(defparameter *letter-counts*
  (loop for word in *words*
	collect (cons word (get-letter-counts word)))
  "A-list of words and letter counts")

(defvar *anagrams* nil
  "A-list of letter counts to anagrams (each a list of words)")

(defun subset (sub-counts super-counts)
  "Returns non-nil if sub-counts is a subset of super-counts"
  (every #'<= sub-counts super-counts))

(defun subtract (a b)
  "Subtracts two letter count arrays"
  (let ((result (make-array (length a))))
  (loop for x across a
	for y across b
	for i upfrom 0
	do (setf (aref result i) (- x y)))
    result))

;;; TODO: Consider collapsing words into unique letter counts first

;; TODO: Memoize
(defvar *anagrams*)

(defun get-anagrams-internal (remaining-counts words)
  "Recursively compute a list of anagrams (each anagram being a list of words)"
  (cond
    ((every (lambda (x) (eql x 0)) remaining-counts) (push words *anagrams*) )
    ((every (lambda (x) (>= x 0)) remaining-counts)
     (loop for (word . letter-counts) in *letter-counts*
	   do (get-anagrams-internal (subtract remaining-counts letter-counts)
				     (cons word words))))))

(defun get-anagrams (word)
  "Compute a list of anagrams for the word (each anagram is a list of words)"
  (let* ((*anagrams* nil)
	 (letter-counts (get-letter-counts word))
	 (*letter-counts* (remove-if-not (lambda (pair) (subset (cdr pair) letter-counts)) *letter-counts*)))
    (get-anagrams-internal (get-letter-counts word) nil)
    *anagrams*))
