(defpackage :aoc23
  (:use :cl))

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

;;; Problem 4, part 1
(defun string-list-to-numbers (string)
  (mapcar #'parse-integer
	  (ppcre:all-matches-as-strings "[0-9]+" string)))

(defun sum-winners ()
    (loop for line in (read-as-lines)
	  for separator-index = (position #\| line)
	  for left = (string-list-to-numbers (subseq line (1+ (position #\: line)) separator-index))
	  for right = (string-list-to-numbers (subseq line (+ separator-index 2)))
	  for score = (let ((score 0))
			(loop for n in left do
			  (when (member n right)
			    (setf score (if (zerop score) 1 (* score 2)))))
			score)
	  sum score))

;;; Problem 4, part 2
(defun count-winners (line)
  (let* ((separator-index (position #\| line))
	 (left (string-list-to-numbers (subseq line (1+ (position #\: line)) separator-index)))
	 (right (string-list-to-numbers (subseq line (+ separator-index 2))))
	 (score (length (intersection left right))))
    score))

(defun sum-copies ()
  (let* ((lines (read-as-lines))
	 (copies (make-array (length lines) :initial-element 1)))
    (loop for line in lines
	  for index upfrom 0
	  for winners = (count-winners line)
	  do (loop repeat winners
		   for i upfrom (1+ index)
		   do (incf (aref copies i) (aref copies index))))
    (loop for count across copies sum count)))

;;; Day 5, part 1
(defun label-line-p (line)
  (find #\: line))

(defun parse-maps (lines)
  (let ((maps (make-hash-table :test 'equal))
	(from nil)
	(map nil))
    (flet ((push-map-if-needed ()
	     (when from
	       (setf (gethash from maps) (nreverse map)))))
      (loop for line in lines
	    do (if (label-line-p line)
		   (ppcre:register-groups-bind (from-new to) ("^([^-]+)-to-([^ ]+) map:" line)
		     (push-map-if-needed)
		     (setf map (list to))
		     (setf from from-new))
		   (push (string-list-to-numbers line) map))
	    finally (push-map-if-needed)))
    maps))

(defun map-value (value map)
  (let ((mapping (find-if (lambda (mapping)
			    (and (>= value (second mapping))
				 (< value (+ (second mapping) (third mapping)))))
			  map)))
    (if mapping
	(+ (- value (second mapping)) (first mapping))
	value)))

(defun emptyp (vector)
  (eql 0 (length vector)))

(defun find-lowest-location ()
  (let* ((lines-raw (read-as-lines))
	 (lines (remove-if #'emptyp lines-raw))
	 (seeds (string-list-to-numbers (first lines)))
	 (maps (parse-maps (rest lines))))
    (loop with location = "seed"
	  with values = seeds
	  for map = (gethash location maps)
	  until (equal location "location")
	  do (setf values (loop for value in values
				collect (map-value value (rest map))))
	     (setf location (first map))
	  finally (return (apply #'min values)))))

;;; Day 5, part 2
(defun map-from-seed (seed maps)
  (loop with location = "seed"
	with value = seed
	for map = (gethash location maps)
	until (equal location "location")
	do (setf value (map-value value (rest map)))
	   (setf location (first map))
	finally (return value)))

;; Approach: Brute force didn't work, so map backwards from lowest ranges and see if any input seed ranges overlap

(defun overlapp (a-start a-length b-start b-length)
  "If ranges overlap, returns (VALUES MIN MAX) where MIN is the start of the overlap and MAX is 1 past the end of the overlap"
  (let* ((a-end (+ a-start a-length))
	 (b-end (+ b-start b-length))
	 (min (max a-start b-start))
	 (max (min a-end b-end)))
    (when (< min max)
      (values min max))))

(defun find-lowest-seeds (maps &optional min max)
  "Find lowest seeds that are on the range [MIN, MAX)"
  (loop for location in '("humidity" "temperature" "light" "water" "fertilizer" "soil" "seed" :done)
	for info = (gethash location maps)
	for ranges = (rest info)
	for best-range = (halp:find-min #'first
					(remove-if-not (lambda (range)
							 (or (null min)
							     (destructuring-bind (to-start from-start length) range
							       (declare (ignore from-start))
							       (overlapp to-start length min (- max min)))))
						       ranges))
	until (eql location :done)
	do (unless best-range (return nil))
	   (if (or min max)
	       (progn
		 ;; Min/max from best range
		 (destructuring-bind (to-start from-start length) best-range
		   (multiple-value-bind (overlap-min overlap-max) (overlapp to-start length min (- max min))
		     (setf min (+ from-start (- overlap-min to-start)))
		     (setf max (+ min (- overlap-max overlap-min))))))
	       (progn
		 ;; Initial min/max
		 (setf min (second best-range))
		 (setf max (+ min (third best-range)))))
	finally (return (list min max))))

(defun try-find-lowest-location (maps seed-ranges &optional min max)
  "Try to find the lowest seed that ends up in range [MIN, MAX)"
  (let ((lowest-range (find-lowest-seeds maps min max)))
    (when lowest-range
      (let* ((lowest-min (first lowest-range))
	     (lowest-length (- (second lowest-range) lowest-min)))
	(loop for (start length) on seed-ranges by 'cddr
	      if (overlapp start length lowest-min lowest-length)
		collect (multiple-value-bind (min max) (overlapp start length lowest-min lowest-length)
			  (list min (- max min))))))))

(defun find-lowest-seed-ranges (seed-ranges maps)
  (let* ((sorted-last-maps (sort (copy-list (rest (gethash "humidity" maps)))
				 (lambda (a b)
				   ;; Sort by start of destination range, ascending
				   (< (first a) (first b))))))
    (loop for (to-start from-start length) in sorted-last-maps
	  for result = (try-find-lowest-location maps seed-ranges to-start (+ to-start length))
	  until result
	  finally (return result))))

(defun find-lowest-locations-from-ranges ()
  (let* ((lines-raw (read-as-lines))
	 (lines (remove-if #'emptyp lines-raw))
	 (seed-ranges (string-list-to-numbers (first lines)))
	 (maps (parse-maps (rest lines))))
    (loop for (start) in (find-lowest-seed-ranges seed-ranges maps)
	  minimize (map-from-seed start maps))))

;;; Day 8, part 1
(defun parse-moves (string)
  (coerce (loop for char across string
		collect (if (equal char #\L) 'first 'second))
	  'vector))

(defun parse-rule (string)
  (ppcre:register-groups-bind (source left right) ("([A-Z0-9]{3}) = [(]([A-Z0-9]{3}), ([A-Z0-9]{3})"
						   string)
    (list source left right)))

(defun count-steps ()
  (let* ((lines (read-as-lines))
	 (moves (parse-moves (first lines)))
	 (rules (mapcar #'parse-rule (cddr lines))))
    (loop with position = "AAA"
	  with count = 0
	  for direction = (aref moves (mod count (length moves)))
	  do (setf position (funcall direction (rest (assoc position rules :test 'equal))))
	     (incf count)
	     (when (equal position "ZZZ") (loop-finish))
	  finally (return count))))

;;; Day 8, part 2
(defun is-start-p (name)
  (eql (aref name 2) #\A))

(defun is-goal-p (name)
  (eql (aref name 2) #\Z))

(defun ghosts-done-p (ghosts)
  (every #'is-goal-p ghosts))

(defun pairs->hash-table (pairs &key (test 'eql))
  (let ((hash-table (make-hash-table :test test)))
    (loop for (key . value) in pairs
	  do (setf (gethash key hash-table) value))
    hash-table))

;; Not used; this was for checking to ensure the sequence of movements didn't repeat internally
;; (defun min-repeating-subsequence (sequence)
;;   (let ((length (length sequence)))
;;     (loop for sublength upfrom 1 do
;;       (when (zerop (mod length sublength))
;; 	(when (loop with same = t
;; 		    for item across sequence
;; 		    for index upfrom 0
;; 		    for subitem = (aref sequence (mod index sublength))
;; 		    while same
;; 		    do (unless (equal item subitem)
;; 			 (setf same nil))
;; 		    finally (return same))
;; 	  (return-from min-repeating-subsequence
;; 	    (subseq sequence 0 sublength)))))
;;     sequence))

(defun ghost-step (ghost moves rules count)
  (funcall (aref moves (mod count (length moves)))
	   (gethash ghost rules)))

(defun find-cycle (ghost moves rules)
  "Find a cycle in the ghost's movements and return (START END GOAL)"
  (let ((seen (make-hash-table :test 'equal)))
    (loop with goal-positions = nil
	  for position upfrom 0
	  for index = (mod position (length moves))
	  for state = (list ghost index)
	  for previous-position = (gethash state seen)
	  until previous-position
	  do (when (is-goal-p ghost)
	       (push position goal-positions))
	     (setf (gethash state seen) position)
	     (setf ghost (ghost-step ghost moves rules position))
	  finally (return (list previous-position
				position
				goal-positions)))))

(cl-coroutine:defcoroutine find-next-goal (arguments)
  (destructuring-bind (ghost moves rules) arguments
    ;; Note: This assumes all paths have cycles--this isn't true in general, but is true for this input
    (destructuring-bind (loop-start loop-end goal-positions) (find-cycle ghost moves rules)
      (unless (equal 1 (length goal-positions))
	(warn "Multiple goal positions"))
      (let ((goal-position (first goal-positions))
	    (period (- loop-end loop-start)))
	(loop for position upfrom goal-position by period do
	  (cl-coroutine:yield position))))))

(defstruct ghost-info
  state
  coroutine
  next)

(defun count-ghost-steps ()
  "Solve the problem programmatically--takes a while to run!"
  (let* ((lines (read-as-lines))
	 (moves (parse-moves (first lines)))
	 (rule-infos (mapcar #'parse-rule (cddr lines)))
	 (rules (pairs->hash-table rule-infos
				   :test 'equal))
	 (ghosts (remove-if-not #'is-start-p
     				(mapcar #'first rule-infos)))
	 (ghost-infos (mapcar (lambda (state)
				(let ((coroutine (cl-coroutine:make-coroutine 'find-next-goal)))
				  (make-ghost-info :state state
						   :coroutine coroutine
						   :next (funcall coroutine (list state moves rules)))))
			      ghosts)))
    ;; Advance ghosts until *all* are in a goal state at the same time
    (loop for all-equal = (apply #'= (mapcar #'ghost-info-next
					     ghost-infos))
	  for earliest-ghost-info = (halp:find-min #'ghost-info-next ghost-infos)
	  until all-equal
	  do ;; Advance the ghost with the earliest goal state
	     (setf (ghost-info-next earliest-ghost-info)
		   (funcall (ghost-info-coroutine earliest-ghost-info) nil))
	  finally (return (ghost-info-next (first ghost-infos))))))

(defun gcd* (x y)
  "Extended Euclidean Algorithm for finding the greatest common denominator of X and Y, along with coefficients A and B such that (= (+ (* A X) (* B Y)) (GCD X Y)). Returns (VALUES GCD A B)."
  ;; TODO: Rename s -> a and tee -> b
  (loop with old-r = x
	with r = y
	with old-s = 1
	with s = 0
	with old-t = 0
	with tee = 1
	until (= r 0)
	do (multiple-value-bind (quotient remainder) (floor old-r r)
	     (psetf r remainder
		    s (- old-s (* quotient s))
		    tee (- old-t (* quotient tee))
		    old-r r
		    old-s s
		    old-t tee))
	finally (return (values old-r old-s old-t))))

(defun combine-periods (a-period a-offset b-period b-offset)
  "Combine two periods (with offsets) and find when they align"
  ;; Note: For this challenge's input, the offsets (i.e. the hard parts) aren't needed
  (multiple-value-bind (gcd a b) (gcd* a-period b-period)
    (declare (ignore b))
    (let ((offset (- a-offset b-offset)))
      (multiple-value-bind (quotient remainder) (floor offset gcd)
	(unless (zerop remainder) (error "Offsets never overlap"))
	(let* ((combined-period (* (/ a-period gcd) b-period))
	       (combined-offset (mod (- a-offset (* a quotient a-period)) combined-period)))
	  (values combined-period combined-offset))))))

(defun count-ghost-steps-fast ()
  "Solve the problem using shortcuts enabled by the specific input values"
  (let* ((lines (read-as-lines))
	 (moves (parse-moves (first lines)))
	 (rule-infos (mapcar #'parse-rule (cddr lines)))
	 (rules (pairs->hash-table rule-infos
				   :test 'equal))
	 (starts (remove-if-not #'is-start-p
				(mapcar #'first rule-infos)))
	 (infos (loop for start in starts
		      collect (multiple-value-list (find-cycle-length start moves rules))))
	 (cycles (loop for (start end (goal-position)) in infos
		       collect (list (- end start) ; Period
				     goal-position))))
    ;; Observation: All lead-ins to cycles start at what *would* be the goal when found later
    ;; on (and goals are only hit once), so all that is needed in the least common multiple...
    (flet ((combine (a b)
	     (multiple-value-list
	      (combine-periods (first a) (second a) (first b) (second b)))))
      (first (reduce #'combine cycles)))))

;;; Day 15, part 1
(defun hash (chars)
  (let ((value 0))
    (loop for char across chars do
      (incf value (char-code char))
      (setf value (* value 17))
      (setf value (mod value 256))
	  finally (return value))))

(defun hash-init ()
  (let ((lines (read-as-lines))
	(stream (make-string-output-stream)))
    (loop for line in lines do (write-string line stream))
    (loop for string in (ppcre:all-matches-as-strings "[^,]+"
						      (get-output-stream-string stream))
	  sum (hash string))))

;;; Day 17, part 1
(defstruct state
  position
  path
  recent
  heat
  goal
  parent)

(defun at-most (count list)
  (loop repeat count
	for item in list
	collect item))

(defun ordered-insert (obj list less-than-p &key (key #'identity))
  (if (null list)
      (list obj)
      (loop with obj-key = (funcall key obj)
	    for pair on list
	    for first = (car pair)
	    for rest = (cdr pair)
	    for first-key = (funcall key first)
	    do (if (funcall less-than-p obj-key first-key)
		   (progn
		     (setf (car pair) obj)
		     (setf (cdr pair) (cons first rest))
		     (loop-finish))
		   (when (null rest)
		     (setf (cdr pair) (list obj))
		     (loop-finish)))
	    finally (return list))))

(defun a*-search (get-next get-heuristic start)
  (let ((seen (make-hash-table :test 'equal)))
    (flet ((better (state)
	     (let* ((key (list (state-position state)
			       (state-recent state)))
		    (previous (gethash key seen)))
	       (when (or (not previous)
			 (< (funcall get-heuristic state) previous))
		 (setf (gethash key seen) (funcall get-heuristic state))
		 t))))
      (loop with available = (list start)
	    for state = (pop available)
	    until (state-goal state)
	    do (when (better state)
		 (loop for new-state in (funcall get-next state) do
		   (setf available (ordered-insert new-state available #'< :key get-heuristic))))
	    finally (break)(return state)))))

;; (defun a*-search (get-next get-heuristic start)
;;   (loop with available = (list start)
;; 	for state = (pop available)
;; 	until (state-goal state)
;; 	do (loop for new-state in (funcall get-next state) do
;; 	  (setf available (ordered-insert new-state available #'< :key get-heuristic)))
;; 	finally (return state)))

(defparameter *directions*
  '((1 0)
    (0 1)
    (-1 0)
    (0 -1)))

(defun position-add (a b)
  (loop for i in a
	for j in b
	collect (+ i j)))

(defun position-negate (a)
  (loop for i in a collect (- 0 i)))

(defun nonnegativep (x)
  (>= x 0))

(defun in-bounds-p (position dimensions)
  (and (every #'nonnegativep position)
       (every #'< position dimensions)))

(defun not-backtracking-p (position path)
  ;; TODO: Only check that it's not going directly backwards?
  (not (member position path :test 'equal)))

(defun not-too-straight-p (new-direction recent)
  (loop for index upfrom 0
	for direction in recent
	while (equal direction new-direction)
	finally (return (< index 3))))

(defun get-next-states (grid dimensions goal-position state)
  (labels ((valid-position (direction state)
	     (let ((new-position (position-add (state-position state) direction)))
	       (and
		(in-bounds-p new-position dimensions)
		(not-backtracking-p new-position (state-path state))
		(not-too-straight-p direction (state-recent state)))))
	   (move (state direction)
	     (let* ((old-position (state-position state))
		    (new-position (position-add old-position direction)))
	       (when (valid-position direction state)
		 (make-state :position new-position
			     :path (cons new-position (state-path state))
			     :recent (cons direction (at-most 2 (state-recent state)))
			     :heat (+ (state-heat state) (apply #'aref (cons grid new-position)))
			     :goal (equal goal-position new-position))))))
    (loop for direction in *directions*
	  for new-state = (move state direction)
	  if new-state collect new-state)))

(defparameter *start-state*
  (make-state :position (list 0 0)
			     :path (list (list 0 0))
			     :recent nil
			     :heat 0
			     :goal nil))

(defun find-best-path ()
  (let* ((lines (read-as-lines))
	 (width (length (first lines)))
	 (height (length lines))
	 (dimensions (list height width))
	 (goal-position (list (1- height) (1- width)))
	 (grid (make-array dimensions)))
    (loop for y upfrom 0 for line in lines do
      (loop for x upfrom 0 for char across line do
	(setf (aref grid y x) (parse-integer (string char)))))
    (a*-search (lambda (state)
		 (get-next-states grid dimensions goal-position state))
	       #'state-heat
	       ;; (lambda (state)
	       ;; 	 (+ (state-heat state)
	       ;; 	    (reduce #'+ (mapcar #'abs (position-add goal-position
	       ;; 					(position-negate (state-position state)))))))
	       *start-state*)))

;;; Day 17, part 2
;; TODO: Needs to move 4 spaces at once when changing direction
(defun not-too-straight-p* (new-direction recent)
  (loop for index upfrom 0
	for direction in recent
	while (equal direction new-direction)
	finally (return (< index 10))))

(defun straight-enough-p (new-direction recent)
  (if recent
      (let ((most-recent (first recent))
	    (four-most-recent (at-most 4 recent)))
	(if (every (lambda (d) (equal d most-recent)) four-most-recent)
	    t
	    (equal new-direction most-recent)))
      t))

(defun get-next-states* (grid dimensions goal-position state)
  (labels ((valid-position (direction state)
	     (let ((new-position (position-add (state-position state) direction)))
	       (and
		(in-bounds-p new-position dimensions)
		(not-backtracking-p new-position (state-path state))
		(not-too-straight-p* direction (state-recent state))
		(straight-enough-p direction (state-recent state)))))
	   (move (state direction)
	     (let* ((old-position (state-position state))
		    (new-position (position-add old-position direction)))
	       (when (valid-position direction state)
		 (make-state :position new-position
			     :path (cons new-position (state-path state))
			     :recent (cons direction (at-most 9 (state-recent state)))
			     :heat (+ (state-heat state) (apply #'aref (cons grid new-position)))
			     :goal (equal goal-position new-position)
			     :parent state)))))
    (loop for direction in *directions*
	  for new-state = (move state direction)
	  if new-state collect new-state)))

(defun find-best-path* ()
  (let* ((lines (read-as-lines))
	 (width (length (first lines)))
	 (height (length lines))
	 (dimensions (list height width))
	 (goal-position (list (1- height) (1- width)))
	 (grid (make-array dimensions)))
    (loop for y upfrom 0 for line in lines do
      (loop for x upfrom 0 for char across line do
	(setf (aref grid y x) (parse-integer (string char)))))
    (a*-search (lambda (state)
		 (get-next-states* grid dimensions goal-position state))
	       'state-heat
	       *start-state*)))

;;; Day 18, part 1
(defparameter *instructions*
  '((#\R . (0 1))
    (#\L . (0 -1))
    (#\D . (1 0))
    (#\U . (-1 0))))

(defun position-multiply (position multiplier)
  (loop for x in position collect (* x multiplier)))

(defun parse-instructions (lines)
  (loop for line in lines
	collect (ppcre:register-groups-bind (direction amount) ("^([UDLR]) ([0-9]+)" line)
		  (list (aref direction 0)
			(parse-integer amount)))))

(defun get-dimensions (instructions)
  "Returns the required dimensions and initial position (with some buffer for flood-filling the exterior"
  (loop with position = (list 0 0)
	with min-x = 0
	with min-y = 0
	with max-x = 0
	with max-y = 0
	for (instruction amount) in instructions
	for direction = (rest (assoc instruction *instructions*))
	do (setf position (position-add position (position-multiply direction amount)))
	   (setf min-y (min min-y (first position)))
	   (setf min-x (min min-x (second position)))
	   (setf max-y (max max-y (first position)))
	   (setf max-x (max max-x (second position)))
	finally (setf min-y (1- min-y))
		(setf min-x (1- min-x))
		(setf max-y (1+ max-y))
		(setf max-x (1+ max-x))
		(return (values (1+ (- max-y min-y))
				(1+ (- max-x min-x))
				(list (- 0 min-y)
				      (- 0 min-x))))))

(defun print-grid (grid)
  (destructuring-bind (height width) (array-dimensions grid)
    (loop for y from 0 below height do
      (loop for x from 0 below width do
	(format t "~a" (aref grid y x)))
      (format t "~%"))))

(defun boundary-fill (grid start value border-value)
  "Flood fills starting at START with VALUE, stopping at BORDER-VALUE"
  (loop with dimensions = (array-dimensions grid)
	with positions = (list start)
	while positions
	do (let ((position (pop positions)))
	     (when (in-bounds-p position dimensions)
	       (let ((existing (apply #'aref (cons grid position))))
		 (unless (or (equal existing value)
			     (equal existing border-value))
		   (setf (apply #'aref (cons grid position)) value)
		   (loop for (nil . direction) in *instructions* do
		     (push (position-add position direction) positions))))))))

(defun compute-trench-area ()
  (let* ((lines (read-as-lines))
	 (instructions (parse-instructions lines)))
    ;; Get dimensions
    (multiple-value-bind (height width position) (get-dimensions instructions)
      (let* ((dimensions (list height width))
	     (grid (make-array dimensions :initial-element 1)))
	(setf (apply #'aref (cons grid position)) 2)
	;; Dig
	(loop for (instruction amount) in instructions
	      for direction = (rest (assoc instruction *directions*))
	      do (loop repeat amount do
		(setf position (position-add position direction))
		(setf (apply #'aref (cons grid position)) 2)))
	;; Fill
	(boundary-fill grid (list 0 0) 0 2)
	;; Sum
	(print-grid grid)
	(loop for y from 0 below height
	      sum (loop for x from 0 below width
			sum (min 1 (aref grid y x))))))))
