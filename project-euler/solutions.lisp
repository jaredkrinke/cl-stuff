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

(defun for-each-pair (function list)
  "Runs FUNCTION on each pair of items from LIST, as long as FUNCTION returns non-NIL"
  (loop for i upfrom 0
	for a in list
	do (loop for cell = (nthcdr (1+ i) list) then (rest cell)
		 for b = (first cell)
		 while cell
		 do (unless (funcall function a b)
		      (return-from for-each-pair)))))

(defun for-each-subsequence (function sequence length)
  "Runs FUNCTION on each subsequence of SEQUENCE of length LENGTH, as long as the result is non-NIL"
  (loop for offset from 0 upto (- (length sequence) length)
	for subsequence = (subseq sequence offset (+ offset length))
	do (unless (funcall function subsequence)
	     (loop-finish))))

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
