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

(defun find-first-odd (l)
  (cond ((null l) nil)
	((oddp (first l)) (first l))
	(t (find-first-odd (rest l)))))

(defun last-element (x)
  (cond ((atom (cdr x)) (car x))
	(t (last-element (cdr x)))))

;; (defun add-nums (n sum)
;;   (cond ((zerop n) sum)
;; 	(t (add-nums (1- n) (+ n sum)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (1- n))))))

(defun all-equal (x)
  (cond ((null (cdr x)) t)
	((equal (first x) (second x)) (all-equal (cdr x)))
	(t nil)))

(defun count-down (x)
  (cond ((zerop x) nil)
	(t (cons x (count-down (1- x))))))

(defun recursive-assoc (item sequence)
  (cond ((null sequence) nil)
	((equal item (caar sequence)) (car sequence))
	(t (recursive-assoc item (cdr sequence)))))

(defun sum-numeric-elements (sequence)
  (cond ((null sequence) 0)
	((numberp (car sequence)) (+ (car sequence) (sum-numeric-elements (cdr sequence))))
	(t (sum-numeric-elements (cdr sequence)))))

(defun recursive-remove (f sequence)
  (cond ((null sequence) nil)
	((funcall f (car sequence)) (recursive-remove f (cdr sequence)))
	(t (cons (car sequence) (recursive-remove f (cdr sequence))))))

(defun recursive-intersection (a b)
  (cond ((null a) nil)
	((rec-member (car a) b) (cons (car a) (recursive-intersection (cdr a) b)))
	(t (recursive-intersection (cdr a) b))))

(defun count-cons (tree)
  (cond ((atom tree) 0)
	(t (+ 1
	      (count-cons (car tree))
	      (count-cons (cdr tree))))))

(defun sum-tree (tree)
  (cond ((numberp tree) tree)
	((atom tree) 0)
	(t (+ (sum-tree (car tree))
	      (sum-tree (cdr tree))))))

(defun my-subst (new old tree)
  (cond ((atom tree) (if (equal tree old) new tree))
	(t (cons (my-subst new old (car tree))
		 (my-subst new old (cdr tree))))))

(defun flatten (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

(defun tree-depth (tree)
  (cond ((atom tree) 0)
	(t (+ 1 (max (tree-depth (car tree))
		     (tree-depth (cdr tree)))))))

(defun paren-depth (x)
  (cond ((atom x) 0)
	(t (max (+ 1 (paren-depth (car x)))
		(paren-depth (cdr x))))))

(defun count-up-internal (n max)
  (cond ((>= n max) (list n))
	(t (cons n (count-up-internal (1+ n) max)))))

(defun count-up (max)
  (count-up-internal 1 max))

(defun count-up* (n)
  (cond ((<= n 1) (list 1))
	(t (append (count-up* (1- n)) (list n)))))

(defun bury (item depth)
  (cond ((<= depth 1) (list item))
	(t (list (bury item (1- depth))))))

(defun pairings (a b)
  (cond ((atom a) nil)
	(t (cons (list (first a) (first b))
		 (pairings (cdr a) (cdr b))))))

(defun sublists (list)
  (cond ((atom list) nil)
	(t (cons list (sublists (cdr list))))))

(defun every-other (list)
  (cond ((atom list) nil)
	(t (cons (first list) (every-other (cddr list))))))

(defun left-half-step (list n)
  (cond ((< n 0) nil)
	(t (cons (first list) (left-half-step (cdr list) (1- n))))))

(defun left-half (list)
  (left-half-step list (/ (length list) 2)))

(defun merge-lists (a b)
  (cond ((and (null a) (null b)) nil)
	((or (null a) (< (first b) (first a))) (cons (first b) (merge-lists a (rest b))))
	(t (cons (first a) (merge-lists (rest a) b)))))

(defparameter *family* '((colin nil nil)
			 (deirdre nil nil)
			 (arthur nil nil)
			 (kate nil nil)
			 (frank nil nil)
			 (linda nil nil)
			 (suzanne colin deirdre)
			 (bruce arthur kate)
			 (charles arthur kate)
			 (david arthur kate)
			 (ellen arthur kate)
			 (george frank linda)
			 (hillary frank linda)
			 (andre nil nil)
			 (tamara bruce suzanne)
			 (vincent bruce suzanne)
			 (wanda nil nil)
			 (ivan george ellen)
			 (julie george ellen)
			 (marie george ellen)
			 (nigel andre hillary)
			 (frederick nil tamara)
			 (zelda vincent wanda)
			 (joshua ivan wanda)
			 (quentin nil nil)
			 (robert quentin julie)
			 (olivia nigel marie)
			 (peter nigel marie)
			 (erica nil nil)
			 (yvette robert zelda)
			 (diane peter erica)))

(defun father (person)
  (second (assoc person *family*)))

(defun mother (person)
  (third (assoc person *family*)))

(defun parents (person)
  (remove nil (rest (assoc person *family*))))

(defun children-helper (person list)
  (cond ((null person) nil)
	((atom list) nil)
	((or (equal person (second (first list)))
	     (equal person (third (first list))))
	 (cons (first (first list)) (children-helper person (rest list))))
	(t (children-helper person (rest list)))))

(defun children (person)
  (children-helper person *family*))

(defun siblings (person)
  (let ((father (father person))
	(mother (mother person)))
    (set-difference (union (children father) (children mother))
		    (list person))))

(defun mapunion (f list)
  (reduce #'union (mapcar f list)))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(defun cousins (person)
  (mapunion #'children (mapunion #'siblings (parents person))))

(defun descended-from (young old)
  (cond ((null young) nil)
	((member old (parents young)) t)
	(t (or (descended-from (father young) old)
	       (descended-from (mother young) old)))))

(defun ancestors (person)
  (cond ((null person) nil)
	(t (append (parents person)
		   (ancestors (father person))
		   (ancestors (mother person))))))

(defun generation-gap-helper (person ancestor gap)
  (cond ((null person) nil)
	((equal person ancestor) gap)
	(t (or (generation-gap-helper (father person) ancestor (1+ gap))
	       (generation-gap-helper (mother person) ancestor (1+ gap))))))

(defun generation-gap (person ancestor)
  (generation-gap-helper person ancestor 0))

(defun tr-count-up-helper (x max result)
  (cond ((> x max) result)
	(t (tr-count-up-helper (1+ x) max (cons x result)))))

(defun tr-count-up (n)
  (reverse (tr-count-up-helper 1 n nil)))

(defun tr-factorial-helper (x result)
  (cond ((<= x 1) result)
	(t (tr-factorial-helper (1- x) (* x result)))))

(defun tr-factorial (x)
  (tr-factorial-helper x 1))

(defun tr-union-helper (a b result)
  (cond ((null a) result)
	((member (first a) b) (tr-union-helper (rest a) b result))
	(t (tr-union-helper (rest a) b (cons (first a) result)))))

(defun tr-union (a b)
  (tr-union-helper a b b))

(defun tr-intersection-helper (a b result)
  (cond ((null a) result)
	((member (first a) b) (tr-intersection-helper (rest a) b (cons (first a) result)))
	(t (tr-intersection-helper (rest a) b result))))

(defun tr-intersection (a b)
  (tr-intersection-helper a b nil))

(defun tr-set-difference-helper (a b result)
  (cond ((null a) result)
	((member (first a) b) (tr-set-difference-helper (rest a) b result))
	(t (tr-set-difference-helper (rest a) b (cons (first a) result)))))

(defun tr-set-difference (a b)
  (tr-set-difference-helper a b nil))

(defun tree-find-if (f tree)
  (cond ((null tree) nil)
	((consp tree) (or (tree-find-if f (car tree))
			  (tree-find-if f (cdr tree))))
	((funcall f tree) tree)
	(t nil)))

(defun arithmetic-eval (expression)
  (cond ((numberp expression) expression)
	((listp expression) (funcall (second expression)
				     (arithmetic-eval (first expression))
				     (arithmetic-eval (third expression))))
	(t (error "Unknown operator"))))

(defun arithmetic-p (expression)
  (cond ((numberp expression) t)
	((listp expression) (and (= 3 (length expression))
				 (arithmetic-p (first expression))
				 (member (second expression) '(+ - / *))
				 (arithmetic-p (third expression))))
	(t nil)))

(defun factor-tree (n)
  (labels ((helper (n p)
	     (cond ((equal n p) n)
		   ((zerop (rem n p)) (list n p (helper (/ n p) p)))
		   (t (helper n (+ p 1))))))
    (helper n 2)))

;;; Chapter 9
(defun space-over (n)
  (cond ((zerop n) nil)
	(t (format t " ")
	   (space-over (1- n)))))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~a~%" plotting-string))

(defun plot-points (plotting-string y-values)
  (cond ((null y-values) nil)
	(t (plot-one-point plotting-string (first y-values))
	   (plot-points plotting-string (rest y-values)))))

(defun generate (m n)
  (cond ((> m n) nil)
	(t (cons m (generate (1+ m) n)))))

(defun make-graph ()
  (let ((func (read)) ; Should input a symbol
	(start (read))
	(end (read))
	(plotting-string (read)))
    (plot-points plotting-string (mapcar func (generate start end)))))

(defun dot-prin1 (x)
  (cond ((null x) (format t "NIL"))
	((atom x) (format t "~s" x))
	(t (format t "(")
	   (dot-prin1 (first x))
	   (format t " . ")
	   (dot-prin1 (rest x))
	   (format t ")"))))

(defun hybrid-prin1-list (x prefix)
  (format t prefix)
  (hybrid-prin1 (car x))
  (hybrid-prin1-cdr (cdr x)))

(defun hybrid-prin1-cdr (x)
  (cond ((null x) (format t ")"))
	((atom x) (format t " . ~s)" x))
	(t (hybrid-prin1-list x " "))))

(defun hybrid-prin1 (x)
  (cond ((atom x) (format t "~s" x))
	(t (hybrid-prin1-list x "("))))

;;; Chapter 10
(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (x)
  (ecase x
    (0 " ")
    (1 "O")
    (10 "X")))

(defun print-row (x y z)
  (format t "~&    ~a | ~a | ~a"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~&")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&  -------------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&  -------------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player position board)
  (setf (nth position board) player)
  board)

(defvar *board* (make-board))
(defparameter *computer* 10)
(defparameter *opponent* 1)

(defparameter *lines*
  '((1 2 3)
    (4 5 6)
    (7 8 9)
    (1 4 7)
    (2 5 8)
    (3 6 9)
    (1 5 9)
    (3 5 7)))

(defun sum-line (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-line board triplet))
	  *lines*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t "~&That space is already occupied")
	   (read-a-legal-move board))
	  (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
	  ((board-full-p new-board) (format t "~&Tie game"))
	  (t (computer-move new-board)))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~s" pos)
    (format t "~&My strategy: ~a~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
	  ((board-full-p new-board) new-board (format t "Tie game"))
	  (t (opponent-move new-board)))))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
			   (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
			   (* 2 *computer*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if (lambda (trip) (equal (sum-line board trip)
						target-sum))
			  *lines*)))
    (when triplet (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if (lambda (pos) (zerop (nth pos board)))
	   squares))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun ugly-fixed (x y)
  (let* ((larger (max x y))
	 (average (/ (+ x y) 2.0))
	 (percentage (* 100 (/ average larger))))
    (list 'average average 'is percentage 'percent 'of 'max larger)))

(defun chop (list)
  (setf (cdr list) nil)
  list)

(defun ntack (list symbol)
  (setf (cdr (last list)) (cons symbol nil))
  list)

;;; Chapter 11
(defun it-member (item list)
  (dolist (x list)
    (if (eql x item) (return t))))

(defun it-assoc (key table)
  (dolist (row table)
    (if (eql (first row) key) (return row))))

(defun check-all-odd (numbers)
  (cond ((null numbers) t)
	((evenp (first numbers)) nil)
	(t (check-all-odd (rest numbers)))))

(defun it-length (list)
  (let ((length 0))
    (dolist (x list)
      (incf length))
    length))

(defun it-nth (index list)
  (dotimes (x index)
    (setf list (cdr list)))
  (first list))

(defun it-union (a b)
  (let ((union a))
    (dolist (x b)
      (pushnew x union))
    union))

(defun it-reverse (list)
  (let ((reversed nil))
    (dolist (x list reversed)
      (push x reversed))))

(defun check-all-odd-do (list)
  (do ((l list (rest l)))
      ((null l) t)
    (when (evenp (first l)) (return nil))))

(defun find-largest (list-of-numbers)
  (do* ((list list-of-numbers (rest list))
	(element (first list) (first list))
	(largest element))
       ((null list) largest)
    (when (> element largest)
      (setf largest element))))

(defun power-of-2 (n)
  (do ((count n (1- count))
       (result 1 (* result 2)))
      ((<= count 0) result)))

(defun first-non-integer (list)
  (dolist (x list)
    (unless (integerp x) (return x))))

(defun it-fib (n)
  (do* ((i 0 (1+ i))
	(f0 0 f1)
	(f1 0 result)
	(result 1 (+ f0 f1)))
       ((= i n) result)))

(defun complement-base (base)
  (case base
    (a 't)
    (c 'g)
    (g 'c)
    (t 'a)))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(defun make-double (strand)
  (loop for base in strand
	collect (list base (complement-base base))))

(defun count-bases (strand)
  (let ((counts (list (cons 'a 0)
		      (cons 'c 0)
		      (cons 'g 0)
		      (cons 't 0))))
    (labels ((add (base) (incf (cdr (assoc base counts)))))
    (dolist (base-or-pair strand)
      (cond ((atom base-or-pair) (add base-or-pair))
	    (t (dolist (base base-or-pair) (add base))))))
    counts))

(defun prefixp (a b)
  (do* ((s1 a (rest s1))
	(s2 b (rest s2)))
       ((null s1) t)
    (unless (eql (first s1) (first s2)) (return nil))))

(defun appearsp (a b)
  (cond ((null b) nil)
	((prefixp a b) t)
	(t (appearsp a (rest b)))))

(defun coverp (a b)
  (do ((s1 a (or (rest s1) a))
       (s2 b (rest s2))
       (expected (first a) (first s1))
       (actual (first b) (first s2)))
      ((equal s1 s2) t)
    (unless (equal expected actual) (return nil))))

(defun prefix (n strand)
  (let ((result nil))
    (dotimes (x n (nreverse result))
      (push (first strand) result)
      (setf strand (rest strand)))))

(defun kernel (strand)
  (do* ((kernel-length 1 (1+ kernel-length))
	(prefix (prefix 1 strand) (prefix kernel-length strand)))
       (nil nil)
    (if (coverp prefix strand) (return prefix))))
