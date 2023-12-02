(defpackage #:aoc23
  (:use #:cl))

(in-package #:aoc23)

(defun read-as-lines ()
  (uiop:split-string (read) :separator +eol+))

;;; Day 1, part 1
(defparameter +zero-code+ (char-code #\0))

(defun digit->number (character)
  (- (char-code character) +zero-code+))

(defun sum-calibration-values ()
  (loop for line in (read-as-lines)
	for first-digit = (find-if #'digit-char-p line)
	for last-digit = (find-if #'digit-char-p line :from-end t)
	for first-number = (digit->number first-digit)
	for last-number = (digit->number last-digit)
	for value = (+ (* 10 first-number) last-number)
	sum value))

;;; Day 1, part 2 (note: overlapping strings are considered separate numbers)
(defun string-starts-with (prefix string &key (start 0))
  (and (>= (length string)
	   (+ start (length prefix)))
       (loop for prefix-character across prefix
	     for string-index upfrom start
	     for string-character = (aref string string-index)
	     if (not (char= prefix-character string-character))
	       do (return nil)
	     finally (return t))))

(defun find-digit (line start-index)
  (macrolet ((check-string (string value)
	       `(when (string-starts-with ,string line :start start-index) ,value)))
    (let ((character (aref line start-index)))
      (if (digit-char-p character)
	  (digit->number character)
	  (case character
	    (#\e (check-string "eight" 8))
	    (#\f (or (check-string "four" 4)
		     (check-string "five" 5)))
	    (#\n (check-string "nine" 9))
	    (#\o (check-string "one" 1))
	    (#\s (or (check-string "six" 6)
		     (check-string "seven" 7)))
	    (#\t (or (check-string "two" 2)
		     (check-string "three" 3))))))))

(defun decode-calibration-line (line)
  (let ((first-number (loop for i upfrom 0
			    for number = (find-digit line i)
			    if number do (return number)))
	(last-number (loop for i downfrom (1- (length line))
			   for number = (find-digit line i)
			   if number do (return number))))
    (+ (* 10 first-number) last-number)))

(defun sum-calibration-values* ()
  (loop for line in (read-as-lines)
	sum (decode-calibration-line line)))