
(defun ids_old (tree l)
	(cond ((< l 0) NIL)
		  ((null tree) NIL)
		  ((atom (car tree)) (append (ids (cdr tree) l) (list (car tree))))
		  ((equal l 0) NIL)
		  (t (append (ids (cdr tree) (- l 1)) (ids (car tree) (- l 1))))))

(defun ids3 (tree l)
	(cond ((equal l 0 NIL)
		  ((t (cond ((atom (cadr tree)) (append (ids3 (cddr tree) l) (list (cadr tree)) (ids2 (car tree) (- l 1))))
		  			(())))))))

(defun ids3 (tree l)
	(print tree)
	(cond ((null tree) NIL)
		  ((atom (car tree)) (append (ids3 (cdr tree) l) (list (car tree))))
		  ((equal l 0) NIL)
		  ((null (cdr tree)) (ids3 (car tree) (- l 1)))
		  (t (append (ids3 (cdr tree) l) (ids3 (list (car tree)) (- l 1))))))

;; working! but not clean
(defun lds (tree l)
	(print tree)
	(print l)
	(cond ((= l 0) NIL)
		  ((null tree) NIL)
		  ((atom (car tree)) (append (lds (cdr tree) l) (list (car tree))))
		  ((and (atom (cadr tree)) (not (null (cadr tree)))) 
		    	(append (lds (cddr tree) l) (list (cadr tree)) (lds (car tree) (- l 1))))
		  (t (append (lds (cdr tree) l) (lds (car tree) (- l 1))))))



(defun get_atoms (tree result)
	(cond ((null tree) result)
		  ((atom (car tree)) (get_atoms (cdr tree) (cons (car tree) result)))
		  (t (get_atoms (cdr tree) result))))



(defun lds_test (tree l)
	(print tree)
	(print l)
	(cond ((null tree) tree)
		  ((atom (car tree)) (append (lds_test (cdr tree) l) (list (car tree))))
		  ((= l 0) NIL)
		  ((null (cdr tree)) (lds_test (car tree) (- l 1)))
		  (t (append (lds_test (cdr tree) l) (lds_test (list (car tree)) (- l 1))))))


