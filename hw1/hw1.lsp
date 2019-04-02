;;; 1. given a number N and a list TREE representing an ordered tree, return whether N appears in the tree
(defun TREE-CONTAINS (N TREE) 
	;; if TREE is empty, return empty
	(cond ((null TREE) NIL)
		  ;; if TREE is a singleton, check if it matches N
		  ((atom TREE) (= TREE N))
		  ;; if N is less than the current root (head of the tail), recurse left
		  ((< N (cadr TREE)) (TREE-CONTAINS N (car TREE)))
		  ;; if N is greater than the current root, recurse right
		  ((> N (cadr TREE)) (TREE-CONTAINS N (caddr TREE)))
		  ;; otherwise, it means N equals the current root
		  (t t)))

;;; 2. given a list TREE representing an ordered tree, return the minimum value
(defun TREE-MIN (TREE)
	;; if TREE is a singleton, return itself
	(cond ((atom TREE) TREE)
		  ;; otherwise recurse left
		  (t (TREE-MIN (car TREE)))))

;;; 3. given a list TREE representing an ordered tree, return a pre-ordered list of the numbers
(defun TREE-ORDER (TREE)
	;; if TREE is empty, return empty
	(cond ((null TREE) NIL)
		  ;; if TREE is a singleton, return the singleton
		  ((atom TREE) (list TREE))
		  ;; otherwise, append the lists returned by recursing root, left, and right
		  (t (cons (cadr tree) (append (TREE-ORDER (car TREE)) (TREE-ORDER (caddr TREE)))))))

;;; 4. given a list L and numbers START and LEN, return the sub-list of L 
;;; starting at position START and having length LEN
(defun SUB-LIST (L START LEN)
	;; if L is empty, return empty
	(cond ((null L) NIL)
	  	  ;; while START is greater than 0, recurse on tail as a way of popping head
		  ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
		  ;; while LEN is valid and L has not been completely consumed, 
		  ;; recurse on tail while storing head to return
		  ((and (> LEN 0) (not (equal L NIL))) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
		  ;; otherwise, it means START=0 and LEN=0, return NIL
		  (t NIL)))

;;; 5. given a list L, return a list of two lists L1 and L2, in that order, such that:
;;; a. L is the result of appending L1 and L2
;;; b. Length of L1 minus length of L2 is 0 or 1
(defun SPLIT-LIST (L)
	;; if the length of L is even, then take the sublists from index 0 with 
	;; length len(L)/2 and from index len(L)/2 with length len(L)/2
	(cond ((evenp (length L)) (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
		  ;; otherwise, take sublists from index 0 with length (len(L)+1)/2 and
		  ;; from index (len(L)+1)/2 with length (len(L)-1)/2
		  (t (list (SUB-LIST L 0 (/ (+ (length L) 1) 2)) (SUB-LIST L (/ (+ (length L) 1) 2) (/ (- (length L) 1) 2))))))

;;; 6. given a list TREE representing a binary tree, return the height of the tree
(defun BTREE-HEIGHT (TREE)
	;; if TREE is a singleton, return 0
	(cond ((atom TREE) 0)
		  ;; otherwise take the max of recursing on left and right subtrees and add one to the total number of levels
		  ((< (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))) (+ 1 (BTREE-HEIGHT (cadr TREE))))
		  (t (+ 1 (BTREE-HEIGHT (car TREE))))))

;;; 7. given a non-empty list of atoms LEAVES, return a list representing a binary tree such that
;;; a. the tree leaves are the elements of LEAVES
;;; b. for any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the
;;;    number of leaves in its right branch is 0 or 1
(defun LIST2BTREE (LEAVES)
	;; if LEAVES is empty, return empty
	(cond ((null LEAVES) NIL)
		  ;; if LEAVES has only one element, return the element
		  ((equal (cdr LEAVES) NIL) (car LEAVES))
		  ;; recurse on both the head and tail of the result of passing LEAVES to SPLIT-LIST
		  (t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr (SPLIT-LIST LEAVES)))))))

;;; 8. given a list TREE representing a binary tree, return the list of atoms
(defun BTREE2LIST (TREE)
	;; if TREE is empty, return empty
	(cond ((null TREE) NIL)
		  ;; for a singleton, return itself
		  ((atom TREE) (list TREE))
		  ;; if both the left and right subtrees are atoms, return the tree
		  ((and (atom (car TREE)) (atom (cadr TREE))) TREE)
		  ;; otherwise, append the results of recursing both left and right subtrees
		  (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))

;;; 9. given two LISP expressions E1 and E2 whose atoms are all numbers, return whether the expressions are identical
(defun IS-SAME (E1 E2)
	;; if both E1 and E2 are empty lists, return true
	(cond ((and (null E1) (null E2)) t)
		  ;; if both are atoms, check for equality of values
		  ((and (atom E1) (atom E2)) (= E1 E2))
		  ;; if just one is an atom with the other being a list, return false
		  ((or (atom E1) (atom E2)) NIL)
		  ;; otherwise, recurse on the heads of both expressions and the tails
		  ;; with the requirement that both need to be the same
		  (t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))))