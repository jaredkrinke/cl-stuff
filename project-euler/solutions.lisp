(defpackage :project-euler
  (:import-from :cl-coroutine
		#:defcoroutine
		#:make-coroutine
		#:yield)
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
  "Returns the (natural number) square root for N, if natural number N is a square"
  (let ((x (floor (sqrt n))))
    (when (= (* x x)
	     n)
      x)))

(defun divisiblep (x y)
  "Returns non-NIL if X is divisble by Y"
  (zerop (mod x y)))

(defun for-each-subset (function n a &optional tail)
  "Calls FUNCTION on each subset of A of size N, optionally consed onto TAIL"
  (cond ((= n 0) (funcall function tail))
	(t (loop for (item . remaining) on a do
	  (for-each-subset function (1- n) remaining (cons item tail))))))

(defun subsets (a n)
  "Returns a list of all subsets of A of size N"
  (ret (result nil)
    (for-each-subset (lambda (subset) (push subset result)) n a)))

(defun for-each-permutation (f a &optional tail)
  "Calls F on each permutation of sequence A, optionally consed onto TAIL"
  (cond ((null a) (funcall f tail))
	(t (loop with length = (length a)
		 for i from 0 below length
		 for remaining = (remove-index i a)
		 do (for-each-permutation f remaining (cons (elt a i) tail))))))

(defun permutations (a)
  "Returns a list of all permutations of A"
  (ret (result nil)
    (for-each-permutation (lambda (permutation) (push permutation result)) a)))

(defun for-each-pair (function list)
  "Runs FUNCTION on each pair of items from LIST, as long as FUNCTION returns non-NIL"
  (loop for i upfrom 0
	for a in list
	do (loop for cell = (nthcdr (1+ i) list) then (rest cell)
		 for b = (first cell)
		 while cell
		 do (unless (funcall function a b)
		      (return-from for-each-pair nil))))
  t)

(defun for-each-subsequence (function sequence length)
  "Runs FUNCTION on each subsequence of SEQUENCE of length LENGTH, as long as the result is non-NIL"
  (loop for offset from 0 upto (- (length sequence) length)
	for subsequence = (subseq sequence offset (+ offset length))
	do (unless (funcall function subsequence)
	     (loop-finish))))

(defun for-each-combination (function list n)
  "Calls FUNCTION with a list for each combination of size N from LIST"
  (labels ((recurse (list n set)
	     (cond ((zerop n) (funcall function set))
		   (t (loop for (item . rest) on list do
		     (recurse rest (1- n) (cons item set)))))))
    (recurse list n nil)))

(defun for-each-combination-from-lists (function lists)
  "Calls FUNCTION with a list for each combination of an item from each of the lists in LISTS"
  (labels ((recurse (lists set)
	     (cond ((null lists) (funcall function set))
		   (t (loop for item in (first lists) do
		     (recurse (rest lists) (cons item set)))))))
    (recurse lists nil)))

(defun list->hash-set (list &key (test 'eql))
  "Returns a hash table with elements of LIST as keys, with values set to T"
  (ret (set (make-hash-table :test test))
    (loop for item in list do
      (setf (gethash item set) t))))

;; Progress indicator
(let ((progress 0)
      (progress-max 100)
      (progress-interval 1000000))
  (defun reset-progress (max)
    (format t "Tracking progress (out of ~a)...~%" max)
    (setf progress 0)
    (setf progress-max max)
    (setf progress-interval (ceiling (/ progress-max 100))))
  (defun note-progress ()
    (when (zerop (mod (incf progress) progress-interval))
      (format t "Progress: ~a (~a%)~%" progress (floor (* 100 (/ progress progress-max)))))
    progress))

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

;;; Problem 46
(defun find-goldbach-contradiction ()
  (loop with primes = (make-hash-table)
	with remainders = (make-hash-table)
	for n upfrom 1
	for remainder = (* 2 n n)
	do (setf (gethash remainder remainders) t)
	   (if (primep n)
	       (setf (gethash n primes) t)
	       (when (and (> n 1)
			  (oddp n)
			  (loop for prime being the hash-keys in primes
				do (when (gethash (- n prime) remainders)
				     (return nil))
				finally (return t)))
		 (return-from find-goldbach-contradiction n)))))

;;; Problem 47
(defun get-prime-factors (n primes)
  "Returns the distinct prime factors of N, using an ascending list of primes, PRIMES"
  (loop with factors = nil
	for prime in primes
	while (and (> n 1)
		   (>= n prime))
	do (loop while (divisiblep n prime)
		 do (pushnew prime factors)
		    (setf n (/ n prime)))
	finally (return (nreverse factors))))

(defun find-consecutive-distinct-prime-factors (&optional (count 3))
  (loop with primes = nil
	with primes-tail = nil
	with relevant = nil
	for n upfrom 2
	for i upfrom 0
	do ;; Create list of primes on the way
	   (when (primep n)
	     (let ((new-tail (list n)))
	       (if primes-tail
		   (setf (rest primes-tail) new-tail)
		   (setf primes new-tail))
	       (setf primes-tail new-tail)))
	   ;; Record relevant prime factors
	   (push (get-prime-factors n primes) relevant)
	   (when (>= i count)
	     (setf (rest (nthcdr (1- count) relevant)) nil)
	     ;; All have required count of distinct prime factors
	     (when (every (lambda (a) (= count (length a))) relevant)
	       (return (- n (1- count)))))))

;;; Problem 48
(defun n-to-the-nth-series-digits ()
  (let* ((value (loop for n from 1 upto 1000 sum (expt n n)))
	 (digits (digits value))
	 (end (last digits 10)))
    (loop for digit in end do (format t "~a" digit))
    (terpri)))

;;; Problem 49
(defun prime-permutations ()
  (loop for a from 1000 upto 9999 do
    (when (primep a)
      (let ((a-digits (sort (digits a) #'<)))
	(loop for increment from 1 upto 3333
	      for b = (+ a increment)
	      for c = (+ a increment increment)
	      do ;; Must all be prime
		 (when (and (primep b)
			    (primep c))
		   (let ((b-digits (sort (digits b) #'<))
			 (c-digits (sort (digits c) #'<)))
		     ;; Must all have the same digits
		     (when (and (equal a-digits b-digits)
				(equal b-digits c-digits))
		       (format t "~a~a~a (~a ~a ~a, by ~a)~%" a b c a b c increment)))))))))

;;; Problem 50
(defun consecutive-prime-sum (&optional (max 42))
  (let* ((prime-list (loop for n from 2 below max when (primep n) collect n))
	 (primes (coerce prime-list 'vector))
	 (sums (make-array (length primes) :initial-element 0)))
    (loop for offset from 0 below (length primes) do
      (loop for i from 0 below (- (length sums) offset) do
	(incf (aref sums i)
	      (aref primes (+ i offset))))
      (loop for i from 0 below (- (length sums) offset)
	    for sum across sums
	    do (when (and (< sum max)
			  (primep sum))
		 (format t "~a (~a terms)~%" sum (1+ offset))
		 (loop-finish))))))

;;; Problem 51
(defun get-primes (&key (min 2) (max 100))
  "Returns all the primes that are at least MIN and below MAX, as a list"
  (loop for n from min below max
	when (primep n) collect n))

(defun pattern-matches-p (pattern digits)
  "Returns non-NIL if PATTERN matches DIGITS (i.e. all digits are the same, and all :WILDs are the same)"
  (loop with wild-digit = nil
	for a in pattern
	for b in digits
	do (unless (equal a b)
	     (if (equal a :wild)
		 (if wild-digit
		     (unless (equal wild-digit b)
		       (return nil))
		     (setf wild-digit b))
		 (return nil)))
	finally (return t)))

(defun prime-digit-replacements (&optional (count 8))
  ;; Check 2 digits, 3 digits, etc.
  (loop for digit-count upfrom 2
	for primes = (get-primes :min (expt 10 (1- digit-count)) :max (expt 10 digit-count))
	for prime-digits = (mapcar #'digits primes)
	do ;; For each distinct digit in each prime, replace all of that digit with :WILD
	   (format t "Checking ~a-digit numbers...~%" digit-count)
	   (loop for prime in primes
		 for digits in prime-digits
		 for distinct-digits = (remove-duplicates digits)
		 do (loop for digit in distinct-digits
			  for pattern = (substitute :wild digit digits)
			  do ;; Count matches amongst prime numbers
			     ;; TODO: Although this finds the solution, it seems flawed in the case that not all of a digit need to be replaced
			     (let* ((first-match nil)
				    (matches (loop with sum = 0
						   for p in primes
						   for p-digits in prime-digits
						   do (when (pattern-matches-p pattern p-digits)
							(unless first-match
							  (setf first-match p))
							(incf sum))
						   finally (return sum))))
			       (when (>= matches count)
				 (format t "~s (~a primes): ~a~%" pattern matches first-match)
				 (return-from prime-digit-replacements prime)))))))

;;; Problem 52
(defun sets-equal-p (a b)
  "Returns non-NIL when (list) sets A and B are equal"
  (null (set-exclusive-or a b)))

(defun permuted-multiples ()
  (loop for x upfrom 1
	for digits = (digits x)
	for result = (loop for i from 2 upto 6
			   do (unless (sets-equal-p digits (digits (* i x)))
				(return nil))
			   finally (return t))
	until result
	finally (return x)))

;;; Problem 53
(defun n-choose-r (n r)
  "Computes N choose R (i.e. the number of ways of selecting R items from N)"
  (/ (factorial n)
     (* (factorial r) (factorial (- n r)))))

(defun combinatoric-selections ()
  (loop with count = 0
	for n from 1 upto 100
	do (loop for r from 1 upto n do
	  (when (> (n-choose-r n r) 1000000)
	    (incf count)))
	finally (return count)))

;;; Problem 54
(defstruct poker-card
  rank
  suit)

(defparameter *poker-suits* '((#\H . hearts)
			      (#\D . diamonds)
			      (#\S . spades)
			      (#\C . clubs))
  "A-list of poker suit letters to suits")

(defparameter *poker-high-ranks* '((#\T . 10)
				   (#\J . 11)
				   (#\Q . 12)
				   (#\K . 13)
				   (#\A . 14))
  "A-list of high rank characters (10, jack, queen, king, ace) to value")

;; TODO: Better to sort here, check for flush, and throw away suits at some point...
(defun parse-poker-hand (string)
  "Parses a poker hand string (e.g. '5H 5C 6S 7S KD') into a list of POKER-CARDs"
  (loop for offset from 0 upto 12 by 3
	for rank-char = (aref string offset)
	for suit-char = (aref string (1+ offset))
	collect (make-poker-card :rank (or (digit-char-p rank-char)
					   (rest (assoc rank-char *poker-high-ranks*)))
				 :suit (rest (assoc suit-char *poker-suits*)))))

(defun poker-n-of-a-kind-internal (ranks n)
  "If there are N of the same rank in RANKS, returns the (highest) matching rank, as well as remaining ranks (in descending order)"
  (let ((sorted-ranks (sort (copy-seq ranks) #'>)))
    (loop with last-rank = (first sorted-ranks)
	  with count = 1
	  for rank in (rest sorted-ranks)
	  do (if (eql rank last-rank)
		 (progn
		   (incf count)
		   (when (= n count)
		     (return (values rank
				     (remove rank sorted-ranks)))))
		 (progn
		   (setf count 1)
		   (setf last-rank rank)))
	  finally (return (values nil nil)))))

(defun poker-n-of-a-kind-p (cards n)
  "Returns non-NIL if there are N of the same rank in RANKS, as well as a tie-breaker list"
  (let ((ranks (mapcar #'poker-card-rank cards)))
    (multiple-value-bind (matching-rank remaining-ranks) (poker-n-of-a-kind-internal ranks n)
      (if matching-rank
	  (values t (cons matching-rank remaining-ranks))
	  (values nil nil)))))

(defun poker-two-pair-p (cards)
  "Returns non-NIL if CARDS contains two pairs of the same rank, as well as a tie-breaker list (high card"
  (let ((ranks (mapcar #'poker-card-rank cards)))
    (multiple-value-bind (matching-rank remaining-ranks) (poker-n-of-a-kind-internal ranks 2)
      (when matching-rank
	(multiple-value-bind (low-matching-rank last-ranks) (poker-n-of-a-kind-internal remaining-ranks 2)
	  (when low-matching-rank
	    (values matching-rank (list matching-rank low-matching-rank (first last-ranks)))))))))

(defun poker-full-house-p (cards)
  "Returns non-NIL if CARDS contains three of a kind and two of a kind, along with tie-breaker rank list"
  (let* ((ranks (mapcar #'poker-card-rank cards)))
    (multiple-value-bind (triple-rank remaining-ranks) (poker-n-of-a-kind-internal ranks 3)
      (when (poker-n-of-a-kind-internal remaining-ranks 2)
	(values triple-rank (list triple-rank))))))

(defun poker-flush-p (cards)
  "Returns non-NIL if all poker cards in CARDS are the same suit, as well as tie-breakers"
  (let ((suit (poker-card-suit (first cards))))
    (values (every (lambda (card) (eql suit (poker-card-suit card)))
		   (rest cards))
	    (sort (mapcar #'poker-card-rank cards) #'>))))

(defun poker-straight-p (cards)
  "Returns non-NIL if all poker cards have consecutive ranks, as well as tie-breaker (high card) list"
  ;; Note: ACE is handled differently for a "low" flush (ace through 5)
  (let ((sorted-ranks (sort (mapcar #'poker-card-rank cards) #'<)))
    (loop with start = (first sorted-ranks)
	  with top-rank = (first (last sorted-ranks))
	  for rank in (rest sorted-ranks)
	  for i upfrom 1
	  do ;; Check for consecutive ranks (with an exception for the "low straight" ace)
	     (unless (or (= rank (+ start i))
			 (and (= start 2)
			      (= i 4)
			      (= rank 14)
			      (setf top-rank 5)))
	       (return (values nil nil)))
	  finally (return (values t (list top-rank))))))

(defun poker-straight-flush-p (cards)
  "Returns non-NIL if both a flush and a straight, along with tie-breaker (high card) list"
  (and (poker-flush-p cards)
       (poker-straight-p cards)))

(defparameter *poker-hands*
  `((straight-flush . poker-straight-flush-p)
    (four-of-a-kind . ,(lambda (cards) (poker-n-of-a-kind-p cards 4)))
    (full-house . poker-full-house-p)
    (flush . poker-flush-p)
    (straight . poker-straight-p)
    (three-of-a-kind . ,(lambda (cards) (poker-n-of-a-kind-p cards 3)))
    (two-pair . poker-two-pair-p)
    (one-pair . ,(lambda (cards) (poker-n-of-a-kind-p cards 2)))
    (high-card . ,(lambda (cards) (values t (sort (mapcar #'poker-card-rank cards) #'>)))))
  "A-list of poker hands, all ranked in descending order, to test functions which return whether there was a match and a list of tie-breakers")

(defun classify-poker-hand (cards)
  "Classifies a list of POKER-CARDs, returning the type of hand, its priority (lower is better), and any tie-breakers"
  (loop for offset upfrom 0
	for (name . test) in *poker-hands*
	do (multiple-value-bind (matched tie-breakers) (funcall test cards)
	     (when matched
	       (return (values name offset tie-breakers))))))

(defmethod print-object ((object poker-card) stream)
  (let ((rank (poker-card-rank object))
	(suit (poker-card-suit object)))
    (case rank
      (11 (write-string " J" stream))
      (12 (write-string " Q" stream))
      (13 (write-string " K" stream))
      (14 (write-string " A" stream))
      (t (format stream "~2d" rank)))
    (ecase suit
      (clubs (write-char #\Black_Club_Suit stream))
      (diamonds (write-char #\Black_Diamond_Suit stream))
      (hearts (write-char #\Black_Heart_Suit stream))
      (spades (write-char #\Black_Spade_Suit stream)))))

(defun compare-poker-hands (left right)
  "Returns 'LEFT if LEFT wins, 'TIE if a tie, and 'RIGHT if RIGHT wins"
  (multiple-value-bind (left-name left-offset left-tie-breakers) (classify-poker-hand left)
    (declare (ignore left-name))
    (multiple-value-bind (right-name right-offset right-tie-breakers) (classify-poker-hand right)
      (declare (ignore right-name))
      (if (= left-offset right-offset)
	  (loop for left-breaker in left-tie-breakers
		for right-breaker in right-tie-breakers
		do (unless (= left-breaker right-breaker)
		     (return (if (> left-breaker right-breaker)
				 'left
				 'right)))
		finally (return 'tie))
	  (if (< left-offset right-offset) ; Note: lower offset is better!
	      'left
	      'right)))))

(defun score-poker-line (line)
  (let ((hand-strings (split-by-indices line '(15))))
    (destructuring-bind (player1 player2) (mapcar #'parse-poker-hand hand-strings)
      (let ((won (eql (compare-poker-hands player1 player2) 'left)))
	(format t "~a ~s~%~s (~s)~%~s (~s~%~%"
		(if won "WON " "LOST")
		line
		(sort (copy-seq player1) #'> :key #'poker-card-rank)
		(classify-poker-hand player1)
		(sort (copy-seq player2) #'> :key #'poker-card-rank)
		(classify-poker-hand player2))
	(if won 1 0)))))

(defun score-poker-hands ()
  (loop for line in (uiop:read-file-lines "0054_poker.txt")
	sum (score-poker-line line)))

;;; Problem 55
(defun lychrel-number-p (n)
  "Returns non-NIL if adding the reverse isn't a palindrome after 50 iterations"
  (loop with first = t
	repeat 50
	do (let ((string (write-to-string n)))
	     (when (and (not first)
			(palindromep string))
	       (return nil))
	     (setf n
		   (+ n
		      (parse-integer (nreverse string))))
	     (setf first nil))
	finally (return t)))

(defun lychrel-numbers ()
  (loop for n from 1 below 10000
	sum (if (lychrel-number-p n) 1 0)))

;;; Problem 56
(defun sum-digits (n)
  "Returns the sum of the digits in N (base 10)"
  (loop for digit in (digits n)
	sum digit))

(defun powerful-digit-sum ()
  (loop for a from 1 below 100
	maximize (loop for b from 1 below 100
		       maximize (sum-digits (expt a b)))))

;;; Problem 57
(defun expand-root-two-term (iterations)
  (if (= iterations 0)
      0
      (/ 1 (+ 2 (expand-root-two-term (1- iterations))))))

(defun expand-root-two (iterations)
  (+ 1 (expand-root-two-term iterations)))

(defun square-root-convergents ()
  (loop for iterations from 1 upto 1000
	for expansion = (expand-root-two iterations)
	sum (if (> (length (digits (numerator expansion)))
		   (length (digits (denominator expansion))))
		1
		0)))

;;; Problem 58
;; Diagonal numbers: 1,
;;                   3, 5, 7, 9, (by 2s)
;;                   13, 17, 21, 25, (by 4s)
;;                   31, 37, 43, 49 (by 6s)
;;                   ... (by 8s)

(defun spiral-primes ()
  (loop with primes = 0
	with index = 1
	for count upfrom 1 by 4
	for side-length upfrom 1 by 2
	for layer upfrom 1
	for fraction-prime = (/ primes count)
	while (or (= count 1)
		  (> fraction-prime
		     0.1))
	do (loop repeat 4
		 do (when (primep (incf index (* 2 layer)))
		      (incf primes)))
	finally (return side-length)))

;;; Problem 59
(defun every-nth (list n &optional (offset 0))
  "Returns every Nth item from LIST (optionally offset by OFFSET), as a new list"
  (loop for item in (nthcdr offset list) by (lambda (l) (nthcdr n l))
	collect item))

(defun xor-decryption ()
  (let ((key-length 3)
	(encrypted-bytes (mapcar #'parse-integer
				 (uiop:split-string (uiop:read-file-string "0059_cipher.txt")
						    :separator '(#\,)))))
    ;; Split up into separate single-byte XOR encryption problems
    (loop for offset from 0 below key-length
	  for bytes = (every-nth encrypted-bytes key-length offset)
	  sum (loop for byte across (halp/crypto:decrypt-single-byte-xor bytes)
		    sum byte))))

;;; Problem 60
(defun concatenatable-primes-p (n m)
  "Returns non-NIL if N and M, when concatenated in either order, result in primes"
  (let ((n-digits (digits n))
	(m-digits (digits m)))
    (and (primep (digits->value (append n-digits m-digits)))
	 (primep (digits->value (nconc m-digits n-digits))))))

(defun concatenatable-primes-set-p (set &key (concatenatable-primes-p 'concatenatable-primes-p))
  "Returns non-NIL if all pairs of numbers in SET when concatenated either way result in primes"
  (flet ((matchp (n m) (funcall concatenatable-primes-p n m)))
    (for-each-pair #'matchp set)))

(defun memoize (function)
  "Returns a memoized version of FUNCTION"
  (let ((input-to-results (make-hash-table :test 'equal)))
    (lambda (&rest input)
      (multiple-value-bind (result found) (gethash input input-to-results)
	(if found
	    result
	    (setf (gethash input input-to-results) (apply function input)))))))

(defun prime-pair-sets (&optional (set-size 4) (max 1000))
  ;; Memoize concatenatable-primes-p to avoid redundant checks
  (let* ((primes (get-primes :max max))
	 (sets (mapcar #'list primes))
	 (fast-pair-test (memoize #'concatenatable-primes-p)))
    ;; Incrementally try larger and larger sets
    (loop for i from 2 upto set-size do
      (format t "Checking set size ~a...~%" i)
      (setf sets (loop for set in sets
		       nconc (loop for prime in primes
				   for new-set = (cons prime set)
				   when (concatenatable-primes-set-p
					 new-set
					 :concatenatable-primes-p fast-pair-test)
				     collect new-set))))
    (loop for set in sets
	  minimize (reduce #'+ set))))

;;; Problem 61
;; Failed approaches: permuting 6 two-digit values and looking for ones in each
;; set, permuting values and checking for cyclicality

;; Approach: Try all permutations of each type/class (triangle, square, etc.)
;; and then find cyclical values, because there are only 720 permutations and,
;; using a tree of prefixes, each permutation can be tested fairly quickly
(defun get-polygonal-numbers (n &key (min 1010) (max 9999))
  "Generates N-agonal numbers between MIN and MAX (inclusive)"
  (let ((s (- n 2))
	(start-index 1))
    (flet ((compute (m)
	     (* 1/2
		m
		(+ (* s m) (- 2 s)))))
      ;; Find where value hits MIN threshold
      (loop for i upfrom 1
	    for number = (compute i)
	    until (>= number min)
	    finally (setf start-index i))
      ;; Collect values
      (loop for i upfrom start-index
	    for number = (compute i)
	    while (<= number max)
	    collect number))))

(defstruct polygonal-set
  values
  prefix-to-values)

(defun create-polygonal-set (sides)
  (let ((values (get-polygonal-numbers sides))
	(prefix-to-values (make-hash-table)))
    (loop for value in values
	  for prefix = (floor (/ value 100))
	  do (push value (gethash prefix prefix-to-values)))
    (make-polygonal-set :values values
			:prefix-to-values prefix-to-values)))

(defun for-each-complete-path (function prefix sets &optional path)
  "Calls FUNCTION on each path through each polygonal set in SETS, ensuring the next value starts with PREFIX"
  (cond ((null sets) (funcall function (reverse path)))
	(t (let* ((set (first sets))
		  (values (if prefix
			      (gethash prefix (polygonal-set-prefix-to-values set))
			      (polygonal-set-values set))))
	     (loop for value in values
		   for suffix = (mod value 100)
		   do (for-each-complete-path function suffix (rest sets) (cons value path)))))))

(defun cyclical-figurate-numbers (&optional (min-sides 3) (max-sides 8))
  (let* ((sets (loop for sides from min-sides upto max-sides
		     collect (create-polygonal-set sides)))
	 (type-offsets (loop for set in sets for i upfrom 0 collect i)))
    ;; Try each permutation of polygonal sets (triangle, square, etc.) and then try to find a cyclical sequence
    (for-each-permutation
     (lambda (offsets)
       (let ((permutation (loop for offset in offsets collect (nth offset sets))))
	 (for-each-complete-path
	  (lambda (path)
	    ;; Ensure path wraps around to the beginning
	    (let ((first (floor (/ (first path) 100)))
		  (last (mod (first (last path)) 100)))
	      (when (= first last)
		(return-from cyclical-figurate-numbers (values (reduce #'+ path) path)))))
	  nil
	  permutation)))
     type-offsets)))

;;; Problem 62
(defun cubic-permutations (&optional (count 5))
  ;; Keep track of mapping of digits -> cubes, in order to count permutations
  (loop with digits-to-count = (make-hash-table :test 'equal)
	for i upfrom 1
	for cube = (expt i 3)
	for digits = (digits cube)
	for sorted-digits = (sort digits #'<)
	do (push cube (gethash sorted-digits digits-to-count))
	   (let ((cubes (gethash sorted-digits digits-to-count)))
	     (when (= (length cubes)
		      count)
	       (return-from cubic-permutations (apply #'min cubes))))))

;;; Problem 63
;; Solved (on paper) with actual math!
(defun powerful-digit-counts ()
  (loop for digit from 1 upto 9
	sum (1+ (floor (/ (- (log digit 10)) (- (log digit 10) 1))))))

;;; Problem 64
(defun compute-floor-of-square-root (n)
  "Returns the floor of the square root of N"
  (loop for last = nil then i
	for i upfrom 1
	when (> (* i i) n)
	  return last))

(defun find-square-root-period (n)
  "Returns the period of the constants in the continued fraction which represents (SQRT N)"
  (loop with states = (make-hash-table :test 'equal)
	with a0 = (compute-floor-of-square-root n)
	with a = a0
	with m = 0
	with d = 1
	for constants = (list a0) then (cons a constants)
	for index upfrom 1
	for state = (list a m d)
	for previous-index = (gethash state states)
	do (when previous-index
	     (return (values (- index previous-index)
			     (reverse (rest constants)))))
	   (setf (gethash state states) index)
	   (setf m (- (* d a) m)) ; By subtracting A, inverting, and moving root to numerator
	   (setf d (/ (- n (* m m)) d)) ; Same process as previous
	   (setf a (floor (/ (+ a0 m) d))))) ; The constant is just the floor of the new fraction (... had to look this one up...)

(defun odd-period-square-roots ()
  (loop for n from 1 upto 10000
	sum (if (and (not (squarep n))
		     (oddp (find-square-root-period n)))
		1
		0)))

;;; Problem 65
(defun compute-continued-fraction (constants)
  "Returns the continued fraction using CONSTANTS"
  (let ((rest (rest constants)))
    (+ (first constants)
       (if rest
	   (/ 1 (compute-continued-fraction rest))
	   0))))

(defcoroutine generate-e-continued-fraction-constants ()
  (yield 2)
  (yield 1)
  (yield 2)
  (loop for i upfrom 2 do
    (yield 1)
    (yield 1)
    (yield (* 2 i))))

(defun convergents-of-e ()
  (let* ((generator (make-coroutine 'generate-e-continued-fraction-constants))
	 (constants (loop repeat 100 collect (funcall generator))))
    (reduce #'+
	    (digits (numerator (compute-continued-fraction constants))))))

;;; Problem 66
;; Note: Naively testing all equations in parallel for increasing X was too
;; slow, so I had to look up the math.
;;
;; (EXPT (/ X Y) 2) is (+ D (EXPT Y -2)), which is approximately D, thus (/ X Y)
;; is approximately (SQRT D), so it might have been worth just trying the
;; continued fraction approximations, but I just looked up the formula...
(defun find-pell-solution (n)
  "Returns (hopefully) a minimal integer solution to Pell's equation with D = N"
  (multiple-value-bind (period constants) (find-square-root-period n)
    (if (evenp period)
	(compute-continued-fraction (subseq constants 0 (1- (length constants))))
	(let ((repeating (rest constants)))
	  (compute-continued-fraction (append constants
					      (subseq repeating 0 (1- (length repeating)))))))))

(defun diophantine-equation ()
  (first (halp:find-max
	  #'second
	  (loop for d from 2 upto 1000
		unless (squarep d) collect (list d
						 (numerator (find-pell-solution d)))))))

;;; Problem 68
(defun all-equal-p (list &key (test 'eql))
  "Returns non-NIL if all items in LIST are equal (according to TEST)"
  (every test list (rest list)))

(defun rotate-list (list offset)
  "Returns a new list that is LIST, but with elements rotated by (nonnegative) OFFSET"
  (loop repeat (length list)
	for cell = (nthcdr offset list) then (or (rest cell) list)
	collect (first cell)))

(defun find-min-index (list &key (key 'identity))
  "Returns the index of the minimum value of LIST, optionally using KEY to retrieve the value from the item"
  (loop with best = nil
	with best-index = nil
	for index upfrom 0
	for item in list
	for value = (funcall key item)
	do (when (or (null best)
		     (< value best))
	     (setf best value)
	     (setf best-index index))
	finally (return best-index)))

(defun magic-n-gon-ring (&optional (n 10) (max-digits 16))
  ;; Represent figure as a list of numbers, with lines represented as lists of
  ;; indices into the first list
  (let* ((values (loop for i from 1 upto n collect i))
	 (line-count (floor (/ n 2)))
	 (lines (loop for i from 0 below line-count
		      collect (list (+ i line-count) i (mod (1+ i) line-count)))))
    (flet ((get-line-values (values line)
	     (loop for index in line collect (elt values index)))
	   (sum (a) (reduce #'+ a)))
      (let* ((strings-raw (loop for permutation in (permutations values) ;'((1 3 2 5 4 6))
				for line-values = (loop for line in lines
							collect (get-line-values permutation line))
				for sums = (mapcar #'sum line-values)
				when (all-equal-p sums)
				  collect line-values))
	     ;; Order clockwise, starting with lowest external node value
	     (ordered-strings (loop for string in strings-raw
				    for min-index = (find-min-index string :key #'first)
				    collect (rotate-list string min-index)))
	     ;; Probably could have filtered out rotations in a smarter way
	     (strings (remove-duplicates ordered-strings :test 'equal))
	     (digit-lists (loop for string in strings
				collect (loop for line in string
					      nconc (loop for n in line nconc (digits n)))))
	     ;; N.B. There is a limit on the number of digits!
	     (relevant-digit-lists (remove-if (lambda (digits) (> (length digits) max-digits))
					      digit-lists))
	     (relevant-numbers (mapcar #'digits->value relevant-digit-lists)))
	(reduce #'max relevant-numbers)))))

;;; Problem 69
;; Empirically, this just looks like a matter of finding the largest
;; product of the first prime numbers
(defun totient-maximum (&optional (max 1000000))
  (loop with value = 1
	for p upfrom 2
	do (when (primep p)
	     (let ((next (* value p)))
	       (if (> next max)
		   (return value)
		   (setf value next))))))

;;; Problem 70
(defun permutationp (a b)
  "Returns non-NIL if A is a permutation of B"
  (equal (sort (copy-seq a) #'<)
	 (sort (copy-seq b) #'<)))

(defun compute-totients (&optional max)
  "Computes totients from 1 up to MAX, returning a vector indexed by N"
  (ret (totients (make-array (1+ max)))
    (reset-progress max)
    ;; Initialize to N
    (loop for n from 1 upto max do
      (setf (aref totients n) n))
    ;; For each prime, multiply applicable totients
    (loop for p from 2 upto max do
      (note-progress)
      (when (primep p)
	(loop for n from p upto max by p do
	  (multf (aref totients n) (/ (1- p) p)))))))

(defun totient-permutation (&optional (max 10000000))
  (let ((totients (compute-totients max)))
    (reset-progress max)
    (first (halp:find-min
	    #'second
	    (loop for n from 2 upto max
		  for digits = (digits n)
		  for totient = (aref totients n)
		  when (progn (note-progress) (permutationp digits (digits totient)))
		    collect (list n (/ n totient)))))))

;; Note: not actually used, but this is what compute-totients was
;; based on
(defun totient (n primes)
  "Returns the 'totient' of N"
  (reduce #'* (cons n (loop for p in (get-prime-factors n primes)
			    collect (- 1 (/ 1 p))))))

;;; Problem 71
;; Very naive approach: just find a fraction that has 1 subtracted
;; from the numerator of a multiple of 3/7
(defun ordered-fractions (&optional (max 1000000))
  (let* ((reference (/ 3 7))
	 (numerator (numerator reference))
	 (denominator (denominator reference)))
    (loop with best = 0
	  for m from 2 upto max
	  for n = (1- (* m numerator))
	  for d = (* m denominator)
	  for fraction = (/ n d)
	  do (when (and (< (denominator fraction) max)
			(< fraction reference)
			(> fraction best))
	       (setf best fraction))
	  finally (return (numerator best)))))

;;; Problem 72
(defun counting-fractions (&optional (max 1000000))
  (reduce #'+
	  (subseq (compute-totients max) 2)))

;;; Problem 73
;; Note: Requires a lot of heap space!
(defun counting-fractions-in-range (&optional (max 12000))
  (let ((count 0)
	(set (make-hash-table)))
    (reset-progress (* max max))
    (loop for n from 1 upto max do
      (loop for d from (1+ n) upto max
	    for fraction = (/ n d)
	    do (note-progress)
	       (when (and (> fraction 1/3)
			  (< fraction 1/2)
			  (not (gethash fraction set)))
		 (setf (gethash fraction set) t)
		 (incf count))))
    count))

;;; Problem 74
(defun sum-factorial-of-digits (n)
  "Returns the sum of the factorial of each digit in N"
  (reduce #'+ (mapcar #'factorial (digits n))))

(defun digit-factorial-chains ()
  ;; Algorithm: just evaluate them all, but memoize to avoid redundant work
  (let ((chains (make-hash-table)))
    (labels ((count-chain (n &optional seen)
	       (or (gethash n chains)
		   (setf (gethash n chains)
			 (cond ((member n seen) 0)
			       (t (1+ (count-chain (sum-factorial-of-digits n)
						   (cons n seen)))))))))
      (loop with count = 0
	    for n from 1 below 1000000
	    sum (if (= 60 (count-chain n))
		    1
		    0)))))

;;; Problem 75
(defun coprimep (m n)
  "Returns non-NIL if M and N are coprime"
  (= 1 (gcd m n)))

;; Algorithm: Calculate primitive Pythagorean triples via Euclid's
;; formula, and count them along with their multiples
(defun singular-integer-right-triangles (&optional (max 1500000))
  (let ((counts (make-array (1+ max) :initial-element 0)))
    ;; From Euclid's formula
    (loop for n from 1 upto max do
      (loop for m from (1+ n) upto (sqrt max) ; sqrt due to m * m on next line
	    for perimeter = (* 2 m (+ m n))
	    do ;; Only for primitive triples
	       (when (and (coprimep m n)
			  (or (evenp m) (evenp n)))
		 ;; Count all multiples as well
		 (loop for p2 from perimeter upto max by perimeter
		       do (note-progress)
			  (incf (aref counts p2))))))
    ;; Find lengths with only a single solution
    (loop for n from 1 upto max
	  sum (if (= (aref counts n) 1)
		  1
		  0))))
