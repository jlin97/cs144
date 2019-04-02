;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );END if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;
; If there are any boxes or goals in the grid, then fail the goal test
;
(defun goal-test (s)
  ; if empty, then true
  (cond ((null s) t)
  		; base cases: return NIL for failing check or t otherwise
  		((and (atom s) (or (isBox s) (isStar s))) NIL)
  		((atom s) t)
  		((null (goal-test (car s))) NIL)
  		(t (goal-test (cdr s))))
  );end defun


;
; Getters and setters
;

; Returns the content at row r and column c inside the grid.
(defun get-square (s r c)
  (cond ((null s) NIL)
  		; index into the right row and then index into the right column
  	    (t (index-into (index-into s r) c)))
  )

; Returns the element at index i inside a list.
(defun index-into (row i)
  ; keep decrementing i until the ith element is the head
  (cond ((or (< i 0) (null row)) NIL)
  	    ((> i 0) (index-into (cdr row) (- i 1)))
        (t (car row)))
  )

; Replaces the element at index i with value v and returns the modified list.
(defun replace-at (row i v rst)
  ; return the result list if entire list has been parsed
  (cond ((null row) rst)
  	    ; return original list for invalid index
		((< i 0) row)
		; keep decrementing i until the ith element is the head
  		((> i 0) (replace-at (cdr row) (- i 1) v (append rst (list (car row)))))
        (t (append rst (list v) (cdr row))))
  )

; Returns the list after replacing the value of element at row r and column c with v
(defun set-square (s r c v)
  (cond ((null s) NIL)
  	    ; 1. index into the right row 
  	    ; 2. replace the element at column c 
  	    ; 3. replace the original row at row r with modified row
  		(t (replace-at s r (replace-at (index-into s r) c v (list)) (list))))
  )

;
; Helper functions for next-states
;

;
; shift-one (r c dir)
; Returns (x y) indicating the position after shifting (r c) in the direction dir.
;
(defun shift-one (r c dir)
  (cond ((equal dir "UP") (list (- r 1) c))
		((equal dir "DOWN") (list (+ r 1) c))
  		((equal dir "LEFT") (list r (- c 1)))
  		((equal dir "RIGHT") (list r (+ c 1)))
  		(t NIL))
  )

;
; isBlocked (v)
; Helper function for get-possible-move
; Returns whether content v blocks the keeper
;
(defun isBlocked (v)
  (cond ((null v) t)
  		((isWall v) t)
  	    (t NIL))
  )

;
; isFree (v)
; Helper function for get-possible-move
; Returns whether content v is free for the keeper to move to
;
(defun isFree (v)
  (cond ((null v) NIL)
  		((or (isBlank v) (isStar v)) t)
  		(t NIL))
  )

;
; canPushBox (v)
; Helper function for get-possible-move
; Returns whether content v has a box that can be pushed into content w
;
(defun canPushBox (v w)
  (cond ((null w) NIL)
  	    ; v has to be a box and w has to be free
  		((and (or (isBox v) (isBoxStar v)) (isFree w)) t)
  		(t NIL))
  )

;
; get-possible-move (s r c dir)
; Helper function for try-move
; Returns the moveId available for the keeper trying to reach (r c) facing direction dir
;
(defun get-possible-move (s r c dir)
  (let* ((v (get-square s r c))
  	 (oneOff (shift-one r c dir))
     (y (car oneOff))
	 (x (cadr oneOff))
	 ; x and y are now the coordinate one away from input in direction dir
	 )
    (cond ((isBlocked v) 0)
    	  ((isFree v) 1)
          ((canPushBox v (get-square s y x)) 2)
          (t 0))
   );end let
  )

;
; move-keeper-out (s r c)
; Helper function for perform-move
; Returns the grid after moving the keeper out of its current position (r c)
;
(defun move-keeper-out (s r c)
  (let* ((v (get-square s r c)))
    (cond ((isKeeper v) (set-square s r c blank))
	      ((isKeeperStar v) (set-square s r c star))
	      (t s))
   );end let
  )

;
; move-keeper-in (s r c)
; Helper function for perform-move
; Returns the grid after moving the keeper into its new position (r c)
;
(defun move-keeper-in (s r c)
  (let* ((v (get-square s r c)))
    (cond ((or (isBlank v) (isBox v)) (set-square s r c keeper))
    	  ((or (isStar v) (isBoxStar v)) (set-square s r c keeperstar))
          (t s))
   );end let
  )

;
; move-box-in (s r c)
; Helper function for perform-move
; Returns the grid after moving the box into its new position (r c)
;
(defun move-box-in (s r c)
  (let* ((v (get-square s r c)))
    (cond ((isBlank v) (set-square s r c box))
    	  ((isStar v) (set-square s r c boxstar))
          (t s))
   );end let
  )

;
; perform-move (s r c dir moveId)
; Helper function for try-move
; Returns the grid after applying move moveId on it with the keeper
; at (r c) facing direction dir
; moveId mappings: 0: stuck, 1: only keeper moves, 2: both keeper and box move
;
(defun perform-move (s r c dir moveId)
  (let* ((oneOff (shift-one r c dir))
     (y (car oneOff))
	 (x (cadr oneOff))
	 (twoOff (shift-one y x dir))
	 (y2 (car twoOff))
	 (x2 (cadr twoOff))
	 ; x and y are now the coordinate one away from input in direction dir
	 ; x2 and y2 are now the coordinate two away from input in direction dir
	 ;
	 )
    ; return NIL if the keeper is stuck
    (cond ((= moveId 0) NIL)
    	  ; move the keeper out of the current position to a position one away
          ((= moveId 1) (move-keeper-in (move-keeper-out s r c) y x))
          ; move the keeper and the adjacent box out of the current position to positions one away
          ((= moveId 2) (move-box-in (move-keeper-in (move-keeper-out s r c) y x) y2 x2))
          (t NIL))
   );end let
  )

;
; try-move (s r c dir)
; Returns the grid if changed or NIL after performing possible move
; given keeper position (r c) and direction faced dir
;
(defun try-move (s r c dir)
  (let* ((oneOff (shift-one r c dir))
     (y (car oneOff))
	 (x (cadr oneOff))
	 ; x and y are now the coordinate one away from input
	 )
    (perform-move s r c dir (get-possible-move s y x dir))
   );end let
  )

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ; x and y are now the coordinate of the keeper in s
	 ; try moving in all four directions
	 (result (list (try-move s y x "UP") (try-move s y x "DOWN") (try-move s y x "LEFT") (try-move s y x "RIGHT")))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; The heuristic is admissible because it does not overestimate the cost
; for any given node. If a box is misplaced, it will take at least one
; cost to move it into a goal.
;
(defun h1 (s)
  ; if empty, then return count
  (cond ((null s) 0)
  		; other base cases for atom inputs for s
   		((and (atom s) (isBox s)) 1)
  		((atom s) 0)
  		(t (+ (count box (car s)) (h1 (cdr s)))))
  );end defun


;
; Admissible heuristic 1: any box in a corner means automatic fail
;

;
; isInCorner (s r c)
; Helper function for check-for-walls
; Returns whether (r c) is in a corner
;
(defun isInCorner (s r c)
  ; every corner blocks an element at (r c) from travelling in that direction
  (cond ((and (isBlocked (get-square s (- r 1) c)) (isBlocked (get-square s r (- c 1)))) t)
  		((and (isBlocked (get-square s (- r 1) c)) (isBlocked (get-square s r (+ c 1)))) t)
  		((and (isBlocked (get-square s (+ r 1) c)) (isBlocked (get-square s r (- c 1)))) t)
  		((and (isBlocked (get-square s (+ r 1) c)) (isBlocked (get-square s r (+ c 1)))) t)
		(t NIL))
  )

;
; check-for-walls (s boxes)
; Helper function for any-box-in-corner
; Returns whether any box in boxes is in a corner
;
(defun check-for-walls (s boxes)
  (cond ((null boxes) NIL)
		((isInCorner s (car (car boxes)) (cadr (car boxes))) t)
		(t (check-for-walls s (cdr boxes))))
  )

;
; any-box-in-corner (s)
; Returns whether any of the boxes in s are in a corner
;
(defun any-box-in-corner (s)
	(check-for-walls s (find-boxes s 0 0 (list)))
  )

;
; Admissible heuristic 2: minimize the total Manhattan distances of all boxes to each's closest goal
;

;
; compute-distance (r c r2 c2)
; Helper function for find-minimum-distance
; Returns the Manhattan distance between (r c) and (r2 c2)
;
(defun compute-distance (r c r2 c2)
  (cond ((and (> r2 r) (> c2 c)) (+ (- r2 r) (- c2 c)))
  	    ((> r2 r) (+ (- r2 r) (- c c2)))
  	    ((> c2 c) (+ (- r r2) (- c2 c)))
  	    (t (+ (- r r2) (- c c2))))
  )

;
; find-minimum-distance (r c l dist)
; Helper function for sum-minimum-distances
; Returns the minimum distance between (r c) and coords in l
;
(defun find-minimum-distance (r c l dist)
  (cond ((null l) dist)
  		; set goalDist to be the Manhattan distance between the head and (r c)
  	    (t (let* ((goalDist (compute-distance r c (car (car l)) (cadr (car l)))))
  	    	; if lower than currently saved dist, replace it and keep searching in tail
	        (cond ((< goalDist dist) (find-minimum-distance r c (cdr l) goalDist))
	        	  (t (find-minimum-distance r c (cdr l) dist)))
		   );end let
  		))
  )

;
; sum-minimum-distances (l l2 dist)
; Helper function for boxes-goals-minimum-manhattan-distance
; Returns the sums of all the minimum Manhattan distances from each box in l to its closest goal in l2
;
(defun sum-minimum-distances (l l2 dist)
  ; return dist when l has been parsed completely
  (cond ((null l) dist)
  	    ; add the minimum distance found for head to result from recursing in tail
  		(t (sum-minimum-distances (cdr l) l2 (+ dist (find-minimum-distance (car (car l)) (cadr (car l)) l2 500)))))
  )

;
; boxes-in-row (row r c rst)
; Helper function for find-boxes
; Returns all the boxes in row as a list of lists of (r c)
;
(defun boxes-in-row (row r c rst)
  (cond ((null row) rst)
  		; for atoms, if isBox, then add to rst
  		((and (atom row) (isBox row)) (cons (list r c) rst))
  		((atom row) rst)
  		; otherwise, if head isBox, then add to rst and recurse on tail
  	    ((and (atom (car row)) (isBox (car row))) (boxes-in-row (cdr row) r (+ 1 c) (cons (list r c) rst)))
  	    (t (boxes-in-row (cdr row) r (+ 1 c) rst)))
  )

;
; find-boxes (s r c rst)
; Helper function for boxes-goals-minimum-manhattan-distance
; Returns all the boxes in s as a list of lists of (r c)
;
(defun find-boxes (s r c rst)
  (cond ((null s) rst)
  		; other base cases for atom inputs for s
  		((atom s) NIL)
  		(t (find-boxes (cdr s) (+ 1 r) 0 (boxes-in-row (car s) r 0 rst))))
  )

;
; goals-in-row (row r c rst)
; Helper function for find-goals
; Returns all the goals in row as a list of lists of (r c)
;
(defun goals-in-row (row r c rst)
  (cond ((null row) rst)
  		((and (atom row) (isStar row)) (cons (list r c) rst))
  		((atom row) rst)
  	    ((and (atom (car row)) (isStar (car row))) (goals-in-row (cdr row) r (+ 1 c) (cons (list r c) rst)))
  	    (t (goals-in-row (cdr row) r (+ 1 c) rst)))
  )

;
; find-goals (s r c rst)
; Helper function for boxes-goals-minimum-manhattan-distance
; Returns all the goals in s as a list of lists of (r c)
;
(defun find-goals (s r c rst)
  (cond ((null s) rst)
  		; other base cases for atom inputs for s
  		((atom s) NIL)
  		(t (find-goals (cdr s) (+ 1 r) 0 (goals-in-row (car s) r 0 rst))))
  )

;
; boxes-goals-minimum-manhattan-distance (s)
; Returns the sum of minimum distances from boxes to each's closest goal in s
;
(defun boxes-goals-minimum-manhattan-distance (s)
  (let* ((boxes (find-boxes s 0 0 (list)))
	 (goals (find-goals s 0 0 (list)))
	 )
    (sum-minimum-distances boxes goals 0);end
   );end let
  );


; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

(defun h204607621 (s)
	(cond ((any-box-in-corner s) 3000)
		  (t (boxes-goals-minimum-manhattan-distance s)))
  )

;
; Unused attempted heuristics
;

(defun h-test1 (s)
	(cond ((any-box-in-corner s) 3000)
		  (t (h1 s)))
  )

(defun h-test2 (s)
	(+ (boxes-goals-minimum-manhattan-distance s) (h1 s))
  )

(defun keeper-boxes-minimum-manhattan-distance (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ; x and y are now the coordinate of the keeper in s
	 )
    (find-minimum-distance y x (find-boxes s 0 0 (list)) 500);end
   );end let
  );

(defun keeper-goals-minimum-manhattan-distance (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ; x and y are now the coordinate of the keeper in s
	 )
    (find-minimum-distance y x (find-goals s 0 0 (list)) 500);end
   );end let
  );

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0  0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
