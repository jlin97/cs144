
; performs a depth-first-search for a tree
(defun dfs (tree)
	;; base cases are empty list or atom
	(cond ((null tree) tree)
		  ((atom tree) (list tree))
		  ;; traverse from right to left
		  (t (append (dfs (cdr tree)) (dfs (car tree))))))

; performs a depth-first iterative-deepening-search for a tree
(defun dfid (tree l)
	;; call lds on tree from l to 1
	(cond ((or (= l 0) (null tree)) NIL)
		  (t (append (dfid tree (- l 1)) (lds tree l)))))

; performs a limited-depth-search for a tree
(defun lds (tree l)
	;; base cases are empty list, atom, or when depth is 0
	(cond ((null tree) NIL)
		  ((atom tree) (list tree))
		  ((= l 0) NIL)
		  (t (append (lds (cdr tree) l) (lds (car tree) (- l 1))))))

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	;; return whether s equals '(3 3 NIL)
	(cond ((equal s '(3 3 NIL)) t)
		  (t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	;; let lft-m, lft-c be the count of missionaries and cannibals on the other side
	;; of the river after moving m missionaries and c cannibals
	(let ((lft-m (+ (- 3 (car s)) m)) (lft-c (+ (- 3 (cadr s)) c)))
		;; prevent moving more missionaries or cannibals than there are on
		;; current side of the river
		(cond ((or (> m (car s)) (> c (cadr s))) NIL)
			  ;; prevent moving more travellers than capacity of the boat
		  	  ((> (+ m c) 2) NIL)
		  	  ;; prevent having fewer missionaries than cannibals on other side
		  	  ;; after moving m, c
		  	  ((and (< (- (car s) m) (- (cadr s) c)) (/= 0 (- (car s) m))) NIL)
		  	  ;; prevent having fewer missionaries than cannibals on current side
		  	  ;; after moving m, c
		  	  ((and (< lft-m lft-c) (/= 0 lft-m)) NIL)
		  	  ;; return state of other side after moving m missionaries and c cannibals
		      (t (list (list lft-m lft-c (not (caddr s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	;; return results of trying every valid combination of pairs that
	;; stay within capacity of the boat
	(append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1)
			(next-state s 0 2) (next-state s 2 0)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond ((null states) NIL)
		  ;; return whether s equals head
  		  ((equal s (car states)) t)
  		  ;; otherwise check in tail
  		  (t (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
	(cond ((null states) NIL)
		  ;; if head of current path has been visited, search for solution in tail
 		  ((on-path (car states) path) (mult-dfs (cdr states) path))
 		  ;; if goal test succeeds, return entire path from current state
 		  ((final-state (car states)) (append path (list (car states))))
		  (t (let ((subtree-result (mult-dfs (succ-fn (car states)) (append path (list (car states))))))
	    		;; if there is no valid path in the subtree to goal state,
	    		;; search in tail of current level of states
 		  		(cond ((null subtree-result) (mult-dfs (cdr states) path))
 		  			  ;; otherwise return path
 		  		 	  (t subtree-result))))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	;; check if s has already been visited
	(cond ((on-path s path) NIL)
		  ;; check goal-ship for initial state
		  ((final-state s) s)
		  ;; otherwise call helper function
		  (t (mult-dfs (succ-fn s) (cons s path)))))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
