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

(defun digits-to-value (digits)
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
