
(defun SUM-EXP (exp)
	(cond ((null exp) 0)
		  ((numberp (car exp)) (+ (car exp) (SUM-EXP (cdr exp))))
		  (t (+ (SUM-EXP (car exp)) (SUM-EXP (cdr exp)))))
	)

(defun SUB-SEQUENCE (SEQ P L)
	(cond ((null SEQ) NIL)
		  ((= L 0) NIL)
		  ((> P 0) (SUB-SEQUENCE (cdr SEQ) (- P 1) L))
		  (t (append (SUB-SEQUENCE (cdr SEQ) P (- L 1)) (list (car SEQ)))))
	)