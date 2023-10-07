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
