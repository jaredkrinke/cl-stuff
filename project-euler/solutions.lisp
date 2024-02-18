(defpackage :project-euler
  (:use :cl))

(in-package :project-euler)

;;; Helpers
(defmacro ret ((variable value) &body body)
  "Creates a local variable named VARIABLE (initialized to VALUE), executes BODY, and returns the value of the variable"
  `(let ((,variable ,value))
     ,@body
     ,variable))

(defmacro multf (place multiplier)
  "Multiplies PLACE by MULTIPLIER"
  `(setf ,place (* ,place ,multiplier)))

(defun squarep (n)
  "Returns non-NIL if natural number N is a square"
  (let ((x (floor (sqrt n))))
    (= (* x x)
       n)))

(defun divisiblep (x y)
  "Returns non-NIL if X is divisble by Y"
  (zerop (mod x y)))

(defun permutations (a)
  "Returns a list of all permutations of A"
  (ret (result nil)
    (for-each-permutation (lambda (permutation) (push permutation result)) a)))

;;; Problem 29
(defun distinct-powers (min max)
  "Calculates the number of distinct powers (a ^ b) for min <= a <= max, and same for b"
  (let ((values nil))
    (loop for a from min upto max do
      (loop for b from min upto max do
	(pushnew (expt a b) values)))
    (length values)))

;;; Problem 30
(defun digits (x)
  "Returns the base ten digits of X, as a list"
  (let ((digits nil))
    (loop until (zerop x) do
      (push (mod x 10) digits)
      (setf x (floor x 10)))
    digits))

(defun sum-digit-powers (x power)
  "Calculates the sum of the digits of X raised to POWER"
  (reduce #'+ (mapcar (lambda (n) (expt n power)) (digits x))))

(defun digit-powers (power)
  "Sums digits of numbers raised to POWER that equal the original number"
  (loop with sum = 0
	for x upfrom 2
	do (when (= x (sum-digit-powers x power))
	     (incf sum x)
	     (format t "~a: ~a~%" x sum))))

;;; Problem 31
(defparameter *coin-values*
  (nreverse (list 1 2 5 10 20 50 100 200))
  "Values of coins in the United Kingdom, ordered from largest to smallest")

(defun coin-sums (target coins)
  "Returns the number of unique ways of combining the coin values in COINS to add up to TARGET (note: COINS must be in descending order)"
  (cond ((< target 0) 0)
	((zerop target) 1)
	(t (loop for remaining-coins on coins
		 for coin = (car remaining-coins)
		 for remaining = (- target coin)
		 ;; Only allow this coin or smaller coins, to avoid duplicates (just in different order)
		 sum (coin-sums remaining remaining-coins)))))

;;; Problem 32
(defparameter *one-through-nine* (loop for x from 1 upto 9 collect x))

(defun remove-index (index sequence)
  "Returns a new list that is a copy of SEQUENCE with the item at INDEX removed"
  (loop for i upfrom 0
	for item in sequence
	unless (= i index) collect item))

(defun split-by-indices (sequence indices &optional (offset 0))
  "Returns subsequences of SEQUENCE when split at INDICES, optionally adjusted by OFFSET (note: INDICIES must be ascending)"
  (cond ((null indices) (list sequence))
	(t (let ((index (- (first indices) offset)))
	     (cons (subseq sequence 0 index)
		   (split-by-indices (subseq sequence index)
				     (rest indices)
				     (+ offset index)))))))

(defun for-each-permutation (f a &optional tail)
  "Calls F on each permutation of sequence A, optionally consed onto TAIL"
  (cond ((null a) (funcall f tail))
	(t (loop with length = (length a)
		 for i from 0 below length
		 for remaining = (remove-index i a)
		 do (for-each-permutation f remaining (cons (elt a i) tail))))))

(defun digits->value (digits)
  "Returns the value represented by DIGITS, a list of digits in base 10"
  (loop with sum = 0
	with multiplier = (expt 10 (1- (length digits)))
	for digit in digits
	do (incf sum (* digit multiplier))
	   (setf multiplier (/ multiplier 10))
	finally (return sum)))

(defun pandigital-product-sum ()
  "Return the sum of all products whose multiplicand, multiplier, and product are 1 through 9 pandigital"
  (let ((products nil)
	(sum 0))
    (for-each-permutation
     (lambda (permutation)
       ;; Try all possible splits of this permutation (i.e. where the multiplication and equals signs should go in the equation "multiplicand x multiplier = product")
       (loop for multiply-index from 1 below (- (length *one-through-nine*) 2)
	     do (loop for equals-index from (1+ multiply-index) below (1- (length *one-through-nine*)) do
	       (destructuring-bind
		   (multiplicand multiplier product)
		   (mapcar #'digits->value
			   (split-by-indices permutation (list multiply-index equals-index)))
		 ;; Check equation is valid *and* product has not already been counted
		 (when (and (= (* multiplicand multiplier) product)
			    (not (member product products)))
		   (push product products)
		   (incf sum product))))
	     finally (return sum)))
     *one-through-nine*)
    sum))

;;; Problem 33
(defun multiple-of-ten-p (x)
  "Returns non-NIL if X is a multiple of 10"
  (= 0 (mod x 10)))

(defun non-trivial-digit-cancelling-fraction (x y)
  "Returns non-NIL if the digit-cancelling fraction (/ x y) is non-trivial (i.e. numerator and denominator are multiples of 10"
  (not (and (multiple-of-ten-p x)
	    (multiple-of-ten-p y))))

(defun digit-cancelling-fraction-product-denominator ()
  (loop with product = 1
	for x from 10 upto 99
	for x-digits = (digits x)
	do (loop for y from (1+ x) upto 99
		 for y-digits = (digits y)
		 for intersection = (intersection x-digits y-digits)
		 do (when (= (length intersection) 1)
		      (let* ((digit (first intersection))
			     (x-cancelled (digits->value (remove digit x-digits)))
			     (y-cancelled (digits->value (remove digit y-digits))))
			(when (and (not (zerop x-cancelled))
				   (not (zerop y-cancelled))
				   (= (/ x-cancelled y-cancelled)
				      (/ x y))
				   (non-trivial-digit-cancelling-fraction x y))
			  (setf product (* product (/ x y)))))))
	finally (return (denominator product))))

;;; Problem 34
(defun factorial (n)
  (ret (product 1)
    (loop for x from n above 1 do
      (multf product x))))

(defun digit-factorials ()
  (loop with sum = 0
	for x upfrom 3
	for digits = (digits x)
	do (when (= x
		    (reduce #'+ (mapcar #'factorial digits)))
	     (incf sum x)
	     (format t "~a (~a)~%" sum x))))

;;; Problem 35
(defun for-each-rotation (function list)
  "Runs FUNCTION on each rotation of LIST"
  (loop with length = (length list)
	with first-cell = list
	for start-cell on list
	for rotation = (loop for cell = start-cell then (or (rest cell) first-cell)
			     repeat length
			     collect (first cell))
	do (funcall function rotation)))

(defun primep (n)
  "Returns non-NIL if natural number N is prime (i.e. > 1 and not a product of two smaller natural numbers)"
  (when (> n 1)
    (loop for x from 2 upto (floor (sqrt n)) do
      (when (= 0 (mod n x)) (return-from primep nil)))
    t))

(defun circular-prime-p (n)
  "Returns non-NIL if N is a circular prime (i.e. all rotations of digits are themselves prime)"
  (let ((digits (digits n)))
    (for-each-rotation (lambda (digits)
			 (let ((value (digits->value digits)))
			   (unless (primep value)
			     (return-from circular-prime-p nil))))
		       digits))
  t)

(defun circular-primes ()
  (ret (count 0)
    (loop for n from 2 below 1000000 do
      (when (circular-prime-p n)
	(incf count)))))

;;; Problem 36
(defun palindromep (a)
  "Returns non-NIL if sequence A is a palindrome (same forwards as backwards)"
  (equal a (reverse a)))

(defun palindromic-number-p (n base)
  "Returns non-NIL if natural number N is a palindrome in base BASE"
  (palindromep (write-to-string n :base base)))

(defun double-base-palindromes ()
  (ret (sum 0)
    (loop for n from 1 below 1000000 do
      (when (and (palindromic-number-p n 10)
		 (palindromic-number-p n 2))
	(incf sum n)))))

;;; Problem 37
(defun truncatable-prime-p (n)
  (when (primep n)
    (let* ((digits (digits n))
	   (length (length digits)))
      (loop for i from 0 below length
	    for j from length above 0
	    do
	       (unless (and (primep (digits->value (subseq digits i)))
			    (primep (digits->value (subseq digits 0 j))))
		 (return-from truncatable-prime-p nil)))
      t)))

(defun truncatable-primes ()
  (loop with sum = 0
	with count = 0
	for i upfrom 8
	until (>= count 11)
	do (when (truncatable-prime-p i)
	     (incf count)
	     (incf sum i))
	finally (return sum)))

;;; Problem 38
(defun pandigitalp (digits)
  "Returns non-NIL if DIGITS represents a pandigital number"
  (equal *one-through-nine*
	 (sort (copy-seq digits) #'<)))

(defun largest-pandigital-concatenated-product ()
  ;; All 9-digit pandigital numbers, in descending order
  (loop with largest = 0
	for n upfrom 1
	do (loop for x upfrom 1
		 for digits = (digits n) then (append digits (digits (* n x)))
		 for value = (digits->value digits)
		 while (<= value 999999999)
		 do (when (and (pandigitalp digits)
			       (> value largest))
		      (setf largest value)
		      (format t "~a (~a ~a)~%" largest n x)))))

;;; Problem 39
(defun max-integer-right-triangles ()
  (let ((solutions (make-hash-table)))
    (loop for i from 1 upto 1000 do
      (loop for j from 1 upto 1000 do
	(loop for k from 1 upto 1000
	      for perimeter = (+ i j k)
	      do (when (and (<= perimeter 1000)
			    (= (+ (* i i) (* j j))
			       (* k k)))
		   (incf (gethash (+ i j k) solutions 0))))))
    (loop with max = 0
	  with best = nil
	  for perimeter being the hash-keys in solutions using (hash-value count)
	  do (when (> count max)
	       (setf max count)
	       (setf best perimeter))
	  finally (return best))))

;;; Problem 40
(defun make-champernowne-digit-generator ()
  (let* ((number 0)
	 (digits nil))
    (lambda ()
      (unless digits
	(setf digits (digits (incf number))))
      (ret (result (first digits))
	(setf digits (rest digits))))))

(defun champernowne-digit-product ()
  (ret (product 1)
    (let ((indices (loop for i from 0 upto 6 collect (expt 10 i)))
	  (generator (make-champernowne-digit-generator)))
      (loop with i = 1
	    for index in indices
	    do (loop while (< i index)
		     do (funcall generator)
			(incf i))
	       (let ((digit (funcall generator)))
		 (incf i)
		 (multf product digit))))))

;;; Problem 41
(defun get-digits (&optional (max 9))
  (subseq *one-through-nine* 0 max))

(defun largest-pandigital-prime ()
  (loop for count from 9 above 0 do
    (let* ((digits (get-digits count))
	   (permutations (permutations digits))
	   (primes (loop for permutation in permutations
			 for value = (digits->value permutation)
			 when (primep value) collect value)))
      (when primes
	(return-from largest-pandigital-prime (reduce #'max primes))))))

;;; Problem 42
(defun triangular-number-p (n)
  "Returns non-NIL if N (an integer) is a triangular number (e.g. 1, 3, 6, 15, 21, ...)"
  (let ((x (1+ (* 8 n))))
    (squarep x)))

(defun letter->index (letter)
  "Returns the index of (capital) letter LETTER in the alphabet (e.g. #\A is 1)"
  (1+ (- (char-code letter) (char-code #\A))))

(defun word->index-sum (word)
  "Returns the sum of the indexes of the letters in the (capital) string WORD"
  (reduce #'+ (loop for letter across word collect (letter->index letter))))

(defun triangle-word-p (word)
  (triangular-number-p (word->index-sum word)))

(defun count-triangle-words ()
  (let* ((text (uiop:read-file-string "0042_words.txt"))
	 (quoted-words (uiop:split-string text :separator '(#\,)))
	 (words (mapcar (lambda (quoted) (subseq quoted 1 (1- (length quoted))))
			quoted-words)))
    (ret (count 0)
      (loop for word in words do
	(when (triangle-word-p word)
	  (incf count))))))

;;; Problem 43
(defparameter *zero-through-nine* (loop for x from 0 upto 9 collect x))

(defun sum-divisible-pandigitals ()
  (ret (sum 0)
    (let ((tests '((2 2)
		   (3 3)
		   (4 5)
		   (5 7)
		   (6 11)
		   (7 13)
		   (8 17)))
	  (digitses (permutations *zero-through-nine*)))
      (loop for digits in digitses do
	(when (loop for (start-index divisor) in tests
		    do (unless (divisiblep (digits->value (subseq digits
								  (1- start-index)
								  (+ start-index 2)))
					   divisor)
			 (return nil))
		    finally (return t))
	  (incf sum (digits->value digits)))))))

;;; Problem 44
(defun find-pentagonal-pair ()
  (let ((set (make-hash-table)))
    (loop for n upfrom 1
	  for number = (/ (* n (- (* 3 n) 1)) 2)
	  repeat 10000
	  do (setf (gethash number set) t))
    (ret (min 1000000000)
      (loop for pj being the hash-keys in set do
	(loop for pk being the hash-keys in set
	      for sum = (+ pj pk)
	      for difference = (abs (- pj pk))
	      do (when (and (gethash sum set)
			    (gethash difference set)
			    (< difference min))
		   (format t "~a (~a + ~a = ~a; |~a - ~a| = ~a)~%"
			   difference
			   pj pk sum
			   pj pk difference)
		   (setf min difference)))))))

;;; Problem 45
(defun find-next-geometric-number ()
  (let ((triangles (make-hash-table))
	(pentagonals (make-hash-table))
	(hexagonals (make-hash-table)))
    (loop repeat 100000
	  for n upfrom 1
	  for triangle = (/ (* n (+ n 1)) 2)
	  for pentagonal = (/ (* n (- (* 3 n) 1)) 2)
	  for hexagonal = (* n (- (* 2 n) 1))
	  do (setf (gethash triangle triangles) t)
	     (setf (gethash pentagonal pentagonals) t)
	     (setf (gethash hexagonal hexagonals) t))
    (loop with count = 0
	  for number being the hash-keys in triangles
	  do (when (and (gethash number pentagonals)
			(gethash number hexagonals))
	       (incf count)
	       (when (>= count 3)
		 (return-from find-next-geometric-number number))))))
