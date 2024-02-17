(defpackage :project-euler
  (:use :cl))

(in-package :project-euler)

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

(defun for-each-permutation-recursive (f a &optional tail)
  "Calls F on each permutation of sequence A, optionally consed onto TAIL"
  (cond ((null a) (funcall f tail))
	(t (loop with length = (length a)
		 for i from 0 below length
		 for remaining = (remove-index i a)
		 do (for-each-permutation-recursive f remaining (cons (elt a i) tail))))))

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
    (for-each-permutation-recursive
     (lambda (permutation)
       ;; Try all possible splits of this permutation (i.e. where the multiplication and equals signs should go in the equation "multiplicand x multiplier = product")
       (loop for multiply-index from 1 below (- (length *one-through-nine*) 2)
	     do (loop for equals-index from (1+ multiply-index) below (1- (length *one-through-nine*)) do
	       (destructuring-bind
		   (multiplicand multiplier product)
		   (mapcar #'digits-to-value
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
			     (x-cancelled (digits-to-value (remove digit x-digits)))
			     (y-cancelled (digits-to-value (remove digit y-digits))))
			(when (and (not (zerop x-cancelled))
				   (not (zerop y-cancelled))
				   (= (/ x-cancelled y-cancelled)
				      (/ x y))
				   (non-trivial-digit-cancelling-fraction x y))
			  (setf product (* product (/ x y)))))))
	finally (return (denominator product))))

;;; Problem 34
(defmacro ret ((variable value) &body body)
  "Creates a local variable named VARIABLE (initialized to VALUE), executes BODY, and returns the value of the variable"
  `(let ((,variable ,value))
     ,@body
     ,variable))

(defmacro multf (place multiplier)
  "Multiplies PLACE by MULTIPLIER"
  `(setf ,place (* ,place ,multiplier)))

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
