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
