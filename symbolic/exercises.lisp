(defpackage symbolic-computation-exercises
  (:use :cl))

(in-package :symbolic-computation-exercises)

;;; Chapter 2
(defun unary-add1 (x)
  (cons :x x))

(defun unary-zerop (x)
  (null x))

(defun unary-sub1 (x)
  (rest x))

(defun unary-greater-p (x y)
  (> (length x) (length y)))

(defun unary-nonzero-p (x)
  (car x))

;;; Chapter 4
(defun make-even (x)
  (if (evenp x) x (1+ x)))

(defun further (x)
  (if (> x 0)
      (1+ x)
      (if (< x 0)
	  (1- x)
	  0)))

(defun my-not (x)
  (if x nil t))

(defun ordered (x y)
  (if (> y x) (list x y) (list y x)))

(defun emphasize3 (x)
  (cond ((equal 'good (first x)) (cons 'great (rest x)))
	((equal 'bad (first x)) (cons 'horrible (rest x)))
	(t (cons 'very x))))

(defun constrain (x max min)
  (cond ((> x max) max)
	((< x min) min)
	(t x)))

(defun first-zero (l)
  (cond ((equal 0 (first l)) "first")
	((equal 0 (second l)) "second")
	((equal 0 (third l)) "third")
	(t "none")))

(defun how-compute (x y result)
  (cond ((equal (+ x y) result) 'sum-of)
	((equal (* x y) result) 'product-of)
	(t 'beats-me)))

(defun square-if-odd-and-positive (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
	((and (oddp x) (< x 0)) (* x 2))
	(t (/ x 2))))

(defun decide-rock-paper-scissors (x y)
  (cond ((equal x 'rock) (cond ((equal y 'rock) 'tie)
			       ((equal y 'paper) 'second-wins)
			       ((equal y 'scissors) 'first-wins)))
	((equal x 'paper) (cond ((equal y 'rock) 'first-wins)
			       ((equal y 'paper) 'tie)
			       ((equal y 'scissors) 'second-wins)))
	((equal x 'scissors) (cond ((equal y 'rock) 'second-wins)
			       ((equal y 'paper) 'first-wins)
			       ((equal y 'scissors) 'tie)))))

(defun compare-using-if (x y)
  (if (> x y) 'first-is-bigger
      (if (equal x y) 'numbers-are-the-same
	  'first-is-smaller)))

(defun compare-using-and (x y)
  (or (and (> x y) 'first-is-bigger)
      (and (equal x y) 'numbers-are-the-same)
      'first-is-smaller))

;;; Chapter 6
(defun set-equal (x y)
  (and (subsetp x y) (subsetp y x)))

(defun proper-subset (x y)
  (and (subsetp x y) (not (set-equal x y))))

(defun right-side (x)
  (rest (member '-vs- x)))

(defun left-side (x)
  (right-side (reverse x)))

(defun count-common (x)
  (length (intersection (left-side x) (right-side x))))

(defun compare-features (x)
  (list (count-common x) 'common 'features))

(defun swap-first-last (x)
  (let ((first (first x))
	(last (last x))
	(middle (nreverse (rest (reverse (rest x))))))
    (append last middle (list first))))

(defun rotate-left (x)
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  (append (last x) (butlast x)))

(defparameter *rooms*
  '((living-room
     (north front-stairs)
     (south dining-room)
     (east kitchen))
    (upstairs-bedroom
     (west library)
     (south front-stairs))
    (dining-room
     (north living-room)
     (east pantry)
     (west downstairs-bedroom))
    (kitchen
     (west living-room)
     (south pantry))
    (pantry
     (north kitchen)
     (west dining-room))
    (downstairs-bedroom
     (north back-stairs)
     (east dining-room))
    (back-stairs
     (south downstairs-bedroom)
     (north library))
    (front-stairs
     (north upstairs-bedroom)
     (south living-room))
    (library
     (east upstairs-bedroom)
     (south back-stairs))))

(defun choices (room-name)
  (cdr (assoc room-name *rooms*)))

(defun look (direction room-name)
  (second (assoc direction (choices room-name))))

(defvar *location* 'pantry)

(defun set-robbie-location (place)
  (setf *location* place))

(defun upstairsp (room-name)
  (member room-name '(library upstairs-bedroom)))

(defun onstairsp (room-name)
  (member room-name '(front-stairs back-stairs)))

(defun where ()
  (append '(robbie is)
	(if (onstairsp *location*)
	    (list 'on 'the *location*)
	    (list (if (upstairsp *location*) 'upstairs 'downstairs) 'in 'the *location*))))

(defun move (direction)
  (let ((destination (look direction *location*)))
    (if destination
	(progn (set-robbie-location destination) (where))
	'(ouch! robbie hit a wall))))

(defun royal-we (x)
  (subst 'we 'i x))

;;; Chapter 7
(defparameter *note-table*
  (loop for note in '(c c-sharp d d-sharp e f f-sharp g g-sharp a a-sharp b)
	for i upfrom 1
	collect (list i note)))

(defun note-names (numbers)
  (mapcar (lambda (x) (cadr (assoc x *note-table*))) numbers))

(defun note-number (name)
  (let ((entry (find-if (lambda (x)
			  (equal (second x) name))
			*note-table*)))
    (first entry)))

(defun note-numbers (names)
  (mapcar #'note-number names))

(defun raise (n numbers)
  (mapcar (lambda (x) (+ x n)) numbers))

(defun normalize (x)
  (cond ((> x 12) (- x 12))
	((< x 1) (+ x 12))
	(t x)))

(defun transpose (n song)
  (let* ((numbers (note-numbers song))
	 (raised (raise n numbers))
	 (normalized (mapcar #'normalize raised)))
    (note-names normalized)))

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(defvar *hand* '((3 hearts)
		 (5 clubs)
		 (2 diamonds)
		 (4 diamonds)
		 (ace spades)))

(defun count-suit (suit hand)
  (count-if (lambda (card) (equal suit (suit card))) hand))

(defvar *colors* '((clubs black)
		   (diamons red)
		   (hearts red)
		   (spades black)))

(defun color-of (card)
  (second (assoc (suit card) *colors*)))

(defun first-red (hand)
  (find-if (lambda (card) (equal 'red (color-of card))) hand))

(defun black-cards (hand)
  (remove-if-not (lambda (card) (equal 'black (color-of card))) hand))

(defun what-ranks (suit hand)
  (mapcar #'rank (remove-if-not (lambda (card) (equal suit (suit card))) hand)))

(defparameter *ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (a b)
  (let* ((rank-a (rank a))
	 (rank-b (rank b))
	 (sublist-b (member rank-b *ranks*)))
    (member rank-a (cdr sublist-b))))

(defun high-card (hand)
  (reduce (lambda (a b) (if (higher-rank-p a b) a b)) hand))

(defun total-length (lists)
  (reduce #'+ (mapcar #'length lists)))

(defparameter *database* '((b1 shape brick)
			   (b1 color green)
			   (b1 size small)
			   (b1 supported-by b2)
			   (b1 supported-by b3)
			   (b2 shape brick)
			   (b2 color red)
			   (b2 size small)
			   (b2 supports b1)
			   (b2 left-of b3)
			   (b3 shape brick)
			   (b3 color red)
			   (b3 size small)
			   (b3 supports b1)
			   (b3 right-of b2)
			   (b4 shape pyramid)
			   (b4 color blue)
			   (b4 size large)
			   (b4 supported-by b5)
			   (b5 shape cube)
			   (b5 color green)
			   (b5 size large)
			   (b5 supports b4)
			   (b6 shape brick)
			   (b6 color purple)
			   (b6 size large)))

(defun match-element (a b)
  (cond ((equal b '?) t)
	(t (equal a b))))

(defun match-triple (a b)
  (every #'match-element a b))

(defun fetch (pattern)
  (remove-if-not (lambda (row) (match-triple row pattern)) *database*))

(defun supporters (block)
  (fetch (list '? 'supports block)))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'cdr (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))

;;; Chapter 8
(defun laugh (n)
  (cond ((<= n 0) nil)
	(t (cons 'ha (laugh (1- n))))))

(defun add-up (numbers)
  (cond ((null numbers) 0)
	(t (+ (first numbers) (add-up (rest numbers))))))

(defun all-odd-p (numbers)
  (cond ((null numbers) t)
	(t (and (oddp (first numbers)) (all-odd-p (rest numbers))))))

(defun rec-member (item sequence)
  (cond ((null sequence) nil)
	(t (if (equal item (first sequence)) sequence (rec-member item (rest sequence))))))

(defun rec-assoc (item alist)
  (cond ((null alist) nil)
	(t (if (equal item (car (first alist)))
	       (first alist)
	       (rec-assoc item (rest alist))))))

(defun rec-nth (n list)
  (cond ((= 0 n) (car list))
	(t (rec-nth (1- n) (cdr list)))))

(defun add1 (x)
  (1+ x))

(defun sub1 (x)
  (1- x))

(defun rec-+ (x y)
  (cond ((= y 0) x)
	(t (rec-+ (add1 x) (sub1 y)))))
