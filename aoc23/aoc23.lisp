(defpackage #:aoc23
  (:use #:cl))

(in-package #:aoc23)

(defparameter *eol* (string #\Newline))

(defun read-as-lines ()
  (uiop:split-string (read) :separator *eol*))

;;; Day 1, part 1
(defconstant +zero-code+ (char-code #\0))

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
(defun substring-starts-with-p (prefix string &key (start 0))
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
	       `(when (substring-starts-with-p ,string line :start start-index) ,value)))
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

;;; Problem 2, part 1
(defun get-game-id (line)
  (parse-integer (ppcre:regex-replace "^Game ([0-9]+):.*$" line "\\1")))

(defun get-max-for-color (line color)
  (let ((max 0))
    (ppcre:do-scans (start end reg-starts reg-ends
		     (format nil "([0-9]+) ~a" (string-downcase (symbol-name color))) line)
      (let ((value (parse-integer (subseq line (elt reg-starts 0) (elt reg-ends 0)))))
	(setf max (max max value))))
    max))

(defun sum-possible-game-ids ()
  (loop with sum = 0
	for line in (read-as-lines)
	do (when (and (<= (get-max-for-color line 'red) 12)
		      (<= (get-max-for-color line 'green) 13)
		      (<= (get-max-for-color line 'blue) 14))
	     (incf sum (get-game-id line)))
	finally (return sum)))

;;; Problem 2, part 2
(defun compute-power-for-game (line)
  (apply #'* (loop for color in '(red green blue)
		   collect (get-max-for-color line color))))

(defun sum-game-powers ()
  (loop for line in (read-as-lines)
	sum (compute-power-for-game line)))

;;; Problem 3, part 1
(defun get-symbol-locations (lines)
  (let ((locations nil))
    (loop for y upfrom 0
	  for line in lines
	  do
	     (loop for x upfrom 0
		   for char across line
		   do
		      (when (and (not (digit-char-p char))
				 (not (eql #\. char)))
			(push (list x y)
			      locations))))
    locations))

(defun get-relevant-locations (lines)
  (let ((relevant nil)
	 (width (length (first lines)))
	 (height (length lines))
	(locations (get-symbol-locations lines)))
    (loop for (x y) in locations do
      (loop for dx from -1 upto 1 do
	(loop for dy from -1 upto 1 do
	  (pushnew (list (max 0 (min (1- width) (+ x dx)))
			 (max 0 (min (1- height) (+ y dy))))
		   relevant
		   :test 'equal))))
    relevant))

(defstruct part-info
  number)

(defun get-number-locations (lines)
  (let ((locations nil)
	(location-to-value nil))
    (loop for y upfrom 0 for line in lines do
      (ppcre:do-matches (start end "[0-9]+" line)
	(let* ((value (parse-integer line :start start :end end))
	       (part-info (make-part-info :number value)))
	  (loop for x from start upto (1- end) do
	    (push (list x y) locations)
	    (push (cons (list x y) part-info) location-to-value)))))
    (values locations location-to-value)))

(defun print-schematic (lines relevant-locations part-locations)
  (loop for y upfrom 0 for line in lines do
    (loop for x upfrom 0 for char across line do
      (let* ((position (list x y))
	     (relevant (member position relevant-locations :test 'equal))
	     (has-part (member position part-locations :test 'equal))
	     (c (cond ((and relevant has-part) "+")
		      (relevant "-")
		      (has-part "|")
		      (t "_"))))
	(format t "~a" c)))
    (fresh-line)))

(defun sum-part-numbers ()
  (let* ((lines (read-as-lines))
	 (relevant-locations (get-relevant-locations lines))
	 (parts nil))
    (multiple-value-bind (part-locations location-to-value) (get-number-locations lines)
;      (print-schematic lines relevant-locations part-locations)
      (loop for location in (intersection relevant-locations part-locations :test 'equal)
	    for value = (rest (assoc location location-to-value :test 'equal))
	    do (pushnew value parts :test 'eq)))
    (loop for part-info in parts sum (part-info-number part-info))))

;;; Problem 3, part 2
(defun get-gear-positions (lines)
  (let ((gears nil))
    (loop for y upfrom 0 for line in lines do
      (loop for x upfrom 0 for char across line do
	(when (char= #\* char)
	  (push
	   (loop for dx from -1 upto 1
		 nconc (loop for dy from -1 upto 1
			     collect (list (+ x dx)
					   (+ y dy))))
	   gears))))
    gears))

(defun sum-gear-ratios ()
  (let* ((sum 0)
	 (lines (read-as-lines))
	 (gears (get-gear-positions lines)))
    (multiple-value-bind (part-locations location-to-info) (get-number-locations lines)
      (loop for gear-positions in gears for parts = nil do
	(loop for location in (intersection gear-positions part-locations :test 'equal)
	      for info = (rest (assoc location location-to-info :test 'equal))
	      do (pushnew info parts))
	(when (equal 2 (length parts))
	  (incf sum (* (part-info-number (first parts))
		       (part-info-number (second parts)))))))
    sum))
